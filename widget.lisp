(in-package #:gauthali)

(defmacro property-set (property value)
  "Set the property value for children widgets."
  (declare (ignore property value))
  `(error "property-set macro is only allowed inside defwidget."))

(defmacro property-get (property &optional default)
  "Get the property value set by parent/ancestor widgets."
  (declare (ignore property default))
  (error "property-get macro can be called only inside defwidget definition."))

(defstruct widget
  (init-function nil :type (function ((or node null) context) node))
  (render-function nil :type (function (node)))
  (body-function nil :type (function (node context))))

(defmacro defwidget (name (&rest lambda-list)
		     (&key state render)
		     &body body)
  (let ((node (gensym "node"))
	(context (gensym "context"))
	(version (1+ (get name :gauthali.widget.version -1))))
    (setf (get name :gauthali.widget.version) version)
    (let* ((decals (if (and (listp body) (listp (first body)) (eql (first (first body)) 'declare))
		       (first body)))
	   (body (if decals (rest body) body)))
      `(defun ,name (,@lambda-list)
	 ,decals
	 (symbol-macrolet (,@(loop for i from 0
				   for binding in state
				   for var = (if (listp binding) (first binding) binding)
				   collect (list var `(aref (node-state ,node) ,i))))
	   (make-widget
	    :init-function
	    (lambda (,node ,context)
	      (declare (ignorable ,context))
	      (unless (and ,node (eql (node-version ,node) ,version))
		(setf ,node (make-node :class ',name
				       :version ,version
				       :dirty t
				       :state (make-array ,(length state) :initial-element nil)))
		;; Initialize variables
		,@(loop for binding in state
			for i from 0
			when (listp binding)
			  collect `(setf ,(first binding) ,(second binding))))
	      ,node)

	    :body-function
	    (lambda (,node ,context)
	      (declare (ignorable ,node ,context))
	      ;; Set body function
	      (setf (fill-pointer (node-properties ,node)) 0)
	      (macrolet ((property-set (property value)
			    `(vector-push-extend (cons ,property ,value) (node-properties ,',node)))
			  (property-get (property &optional default)
			    `(property-get% ,',context ,property ,default)))
		,@body))

	    :render-function
	    (lambda (,node)
	      (declare (ignorable ,node))
	      ,render)))))))

(defstruct node
  (class nil)
  (version 0 :type fixnum)
  (dirty nil :type boolean)
  (id nil)
  (parent nil)
  (state (vector) :type vector)
  (properties (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (children (make-array 0 :fill-pointer 0 :adjustable t))
  (render-function nil)
  (body-function nil))

(defstruct context
  (properties (make-hash-table)))

(defun property-value (property-stack default)
  (let ((len (length property-stack)))
    (if (> len 0)
	(aref property-stack (1- len))
	default)))

(defun make-property ()
  (make-array 0 :initial-element nil :fill-pointer 0 :adjustable t))

(defun context-get-property% (context property &optional default)
  (let ((entry (gethash property (context-properties context))))
    (if entry
	(property-value entry default)
	default)))

(defun context-set-property% (context property value)
  (let ((entry (gethash property (context-properties context))))
    (unless entry
      (setf entry (make-property))
      (setf (gethash property (context-properties context)) entry))
    (vector-push-extend value entry)))

(defun context-setup (context node)
  (loop for (property . value) across (node-properties node)  do
    (context-set-property% context property value)))

(defun context-restore (context node)
  (loop for (property . value) across (node-properties node)
	for entry = (gethash property (context-properties context))
	do
	   (vector-pop entry)))

(defun create-node (widget context old-node)
  (let ((node (funcall (widget-init-function widget) old-node context)))
    (setf (node-body-function node) (widget-body-function widget)
	  (node-render-function node) (widget-render-function widget))
    node))

(defun update-widget-tree (node context)
  (when (node-dirty node)
    (setf (node-dirty node) nil)

    (let* ((child-nodes (node-children node))
	   (old-length (length child-nodes)))
      (context-setup context node)

      ;; Update node-children
      (setf (fill-pointer child-nodes) 0)
      (loop for i from 0
	    for child-widget in (uiop:ensure-list (funcall (node-body-function node) node context))
	    for child-old-node = (when (< i old-length) (aref child-nodes i))
	    do
	       (let ((child-node (create-node child-widget context child-old-node)))
		 (update-widget-tree child-node context)
		 (vector-push-extend child-node child-nodes)))

      (context-restore context node))))

(defun render (node)
  (when (node-render-function node)
    (funcall (node-render-function node) node))
  (loop for child across (node-children node) do
	(render child)))

(defparameter *state* 1)
(defun start-ui (root-widget context)
  (setf *state* 1)
  (let* ((node (create-node root-widget context nil)))
    (update-widget-tree node context)
    ;; TODO: wait for event
    ;; TODO: process event
    (setf *state* 2)
    ;; Update widget tree
    (update-widget-tree node context)
    ;; Render to screen
    (render node)
    (values node context)))

(defun main ()
  (assert-ret (sdl3-ttf:init))
  (let ((font (sdl3-ttf:open-font (namestring (get-resource-path "res/fonts/Times New Roman.ttf")) 18.0))
	(context (make-context)))
    (unwind-protect
	 (progn
	   (context-set-property% context
				  :layout
				  (list :x (make-layout :type :fixed :size 200.0)
					:y (make-layout :type :fixed :size 100.0 :major-axisp nil)))
	   (context-set-property% context :font font)
	   (start-ui (home-screen) context))
      (sdl3-ttf:close-font font)
      (sdl3-ttf:quit))))
