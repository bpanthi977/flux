(in-package #:gauthali)

;;; CONTEXT

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
    (vector-push-extend value entry)
    value))

(defun context-setup (context widget)
  (loop for (property . value) across (widget-properties widget)  do
    (context-set-property% context property value)))

(defun context-restore (context widget)
  (loop for (property . value) across (widget-properties widget)
	for entry = (gethash property (context-properties context))
	do
	   (vector-pop entry)))

;;;; WIDGET WIDGET

(defstruct widget
  (name nil)
  (version 0 :type fixnum)
  (build-function nil :type (or null (function (widget context))))
  (render-function nil :type (or null (function (widget float float float float))))
  (dirty nil :type boolean)
  (id nil)
  (parent nil)
  (state (vector) :type vector)
  (properties (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (children (make-array 0 :fill-pointer 0 :adjustable t)))

(defmacro property-set (property value)
  "Set the property value for children widgets."
  (declare (ignore property value))
  `(error "property-set macro is only allowed inside defwidget."))

(defmacro property-get (property &optional default)
  "Get the property value set by parent/ancestor widgets."
  (declare (ignore property default))
  (error "property-get macro can be called only inside defwidget definition."))

(defun destructure-defwidget-args (args)
  (let (state build render)
    (flet ((render-lambda-formp (form)
	     (and (= 4 (length form))
		  (every #'symbolp form))))
      (loop for clause in args do
	(cond ((or (not (listp clause))
		   (not (member (first clause) '(:state :build :render))))
	       (error "~a not a list beginning with :state, :build, :render." clause))
	      (t
	       (case (first clause)
		 (:state (setf state (rest clause)))
		 (:build (setf build (rest clause)))
		 (:render
		  (unless (render-lambda-formp (second clause))
		    (error ":render clause must have following form: (:render (x y w h) &body body)."))
		  (setf render (rest clause))))))))
    (values state build render)))

(defmacro defwidget (name (&rest lambda-list)
		     &body args)
  (let ((widget (gensym "widget"))
	(context (gensym "context"))
	(version (1+ (get name :gauthali.widget.version -1))))
    (setf (get name :gauthali.widget.version) version)
    (multiple-value-bind (state build render)
	(destructure-defwidget-args args)
      (let* ((decals (if (and (listp build) (listp (first build)) (eql (first (first build)) 'declare))
			 (first build)))
	     (build (if decals (rest build) build)))
	`(defun ,name (,@lambda-list)
	   ,decals
	   (lambda (,widget ,context)
	     (declare (ignorable ,context))
	     ;; Create widget if necessary
	     (unless (and ,widget (eql (widget-version ,widget) ,version))
	       (symbol-macrolet (,@(loop for i from 0
					 for binding in state
					 for var = (if (listp binding) (first binding) binding)
					 collect (list var `(aref (widget-state ,widget) ,i))))
		 (setf ,widget (make-widget
			      :name ',name
			      :version ,version
			      :dirty t
			      :state (make-array ,(length state) :initial-element nil)
			      :build-function
			      (lambda (,widget ,context)
				(declare (ignorable ,widget ,context))
				;; Set build function
				(setf (fill-pointer (widget-properties ,widget)) 0)
				(macrolet ((property-set (property value)
					     `(vector-push-extend (cons ,property ,value) (widget-properties ,',widget)))
					   (property-get (property &optional default)
					     `(context-get-property% ,',context ,property ,default)))
				  ,@build))
			      :render-function
			      ,(when render
				 `(lambda (,widget ,@(first render))
				    (declare (ignorable ,widget))
				    ,@(rest render)))))
		 ;; Initialize variables
		 ,@(loop for binding in state
			 for i from 0
			 when (listp binding)
			   collect `(setf ,(first binding) ,(second binding)))))
	     ;; Return widget (the same or the newly created one)
	     ,widget))))))

(defun update-widget-tree (widget context)
  (cond ((widget-dirty widget)
	 (setf (widget-dirty widget) nil)

	 (let* ((child-widgets (widget-children widget))
		(old-length (length child-widgets)))
	   (context-setup context widget)

	   ;; Update widget-children
	   (setf (fill-pointer child-widgets) 0)
	   (loop for i from 0
		 for child-widget-func in (uiop:ensure-list (funcall (widget-build-function widget) widget context))
		 for child-old-widget = (when (< i old-length) (aref child-widgets i))
		 do
		    (let ((child-widget (funcall child-widget-func child-old-widget context)))
		      (update-widget-tree child-widget context)
		      (vector-push-extend child-widget child-widgets)))

	   (context-restore context widget)))
	(t ;; widget is not dirty but the children might be
	 (context-setup context widget)
	 (loop for child-widget across (widget-children widget) do
	       (update-widget-tree widget context))
	 (context-restore context widget))))
