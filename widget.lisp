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

(defun context-restore-property% (context property)
  (let ((entry (gethash property (context-properties context))))
    (when entry
      (vector-pop entry))))

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
  (memo-if-function nil :type (or null (function (widget))))
  (build-function nil :type (or null (function (widget context))))
  (on-layout-x-function nil :type (or null (function (widget float float))))
  (on-layout-y-function nil :type (or null (function (widget float float))))
  (render-function nil :type (or null (function (widget t float float float float))))
  (cleanup-function nil :type (or null (function (widget))))
  (dirty nil :type boolean)
  (id nil)
  (parent nil)
  (state (vector) :type vector)
  (layout-x (make-layout :major-axisp t) :type layout)
  (layout-y (make-layout :major-axisp nil) :type layout)
  (properties (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (event-handlers (make-array 0 :fill-pointer 0 :adjustable t))
  (children (make-array 0 :fill-pointer 0 :adjustable t)))

(defvar *context* nil
  "Current Context dynamically bound while running build function of widgets.")

(defun property-set (widget property value)
  "Set the property value that the children widgets will get in their context."
  (vector-push-extend (cons property value) (widget-properties widget)))

(defun property-get (property &optional default)
  "Get the property value set by parent/ancestor widgets from current context."
  (context-get-property% *context* property default))

(defmacro on (event-class-symbol handler)
  "Attach an event `handler' to this `widget'.
`event-class-symbol' is the class of event. "
  (let ((class (find-class event-class-symbol)))
    (unless class
      (error "Class ~a not found." event-class-symbol))
    `(progn
       (vector-push-extend (cons ,class ,handler) (widget-event-handlers (context-get-property% *context* :gauthali.widget.current)))
       nil)))

(define-symbol-macro this
    (error "`this' is only accessible inside defwidget."))

(defun destructure-body (body)
  (let* ((decals (if (and (listp body) (listp (first body)) (eql (first (first body)) 'declare))
		     (first body)))
	 (body (if decals (rest body) body)))
    (list decals body)))

(defun extract-lambda-info (lambda-list)
  "Extract variable names and call form from a lambda list."
  (let ((vars '())
	(call-args '())
	(current-section :required))

    (dolist (item lambda-list)
      (cond
	((member item '(&optional &key &rest &allow-other-keys &aux))
	 (setf current-section item))

	(t (let ((var-name (cond
			     ((symbolp item) item)
			     ((listp item) (first item)))))

	     ;; Add to vars list (except for &allow-other-keys)
	     (unless (eq current-section '&allow-other-keys)
	       (push var-name vars))

	     ;; Add to call args based on section
	     (case current-section
	       ((:required &optional)
		(push var-name call-args))

	       (&key
		(push (intern (symbol-name var-name) :keyword) call-args)
		(push var-name call-args))

	       ;; Don't include &rest or &aux in call form for this use case
	       )))))

    (values (reverse vars)
	    (reverse call-args))))

(defun destructure-defwidget-args (args)
  (let (state memo-if build on-layout-x on-layout-y render cleanup)
    (flet ((render-lambda-formp (form)
	     (and (= 5 (length form))
		  (every #'symbolp form)))
	   (on-layout-lambda-formp (form)
	     (and (= 2 (length form))
		  (every #'symbolp form))))
      (loop for clause in args do
	(cond ((or (not (listp clause))
		   (not (member (first clause) '(:state :memo-if :build :on-layout-x :on-layout-y :render :cleanup))))
	       (error "~a not a list beginning with :state, :memo-if, :build, :on-layout-x, :on-layout-y, :render or :cleanup." clause))
	      (t
	       (case (first clause)
		 (:state (setf state (rest clause)))
		 (:memo-if (setf memo-if (rest clause)))
		 (:build (setf build (rest clause)))
		 (:on-layout-x
		  (unless (on-layout-lambda-formp (second clause))
		    (error ":on-layout-x clause must have following form: (:on-layout-x (x width) &body body)."))
		  (setf on-layout-x (rest clause)))
		 (:on-layout-y
		  (unless (on-layout-lambda-formp (second clause))
		    (error ":on-layout-y clause must have following form: (:on-layout-y (y height) &body body)."))
		  (setf on-layout-y (rest clause)))
		 (:render
		  (unless (render-lambda-formp (second clause))
		    (error ":render clause must have following form: (:render (renderer x y w h) &body body)."))
		  (setf render (rest clause)))
		 (:cleanup
		  (setf cleanup (rest clause))))))))
    (values state memo-if build on-layout-x on-layout-y render cleanup)))

(defun defwidget-create-lambda (widget args-and-body)
  (destructuring-bind (args . body) args-and-body
    (let* ((decals (if (and (listp body) (listp (first body)) (eql (first (first body)) 'declare))
		       (first body)))
	   (body (if decals (rest body) body)))
      `(lambda (,widget ,@args)
	 (declare (ignorable ,widget))
	 ,decals
	 (block nil
	   ,@body)))))

(defmacro defwidget (name (&rest lambda-list)
		     &body args)
  (let* ((widget (gensym "widget"))
	 (version (1+ (get name :gauthali.widget.version -1)))
	 (widget-decals-body (destructure-body args))
	 (widget-decals (first widget-decals-body))
	 (args (second widget-decals-body))
	 (lambda-vars)
	 (lambda-call-form))
    (multiple-value-bind (state memo-if build on-layout-x on-layout-y render cleanup)
	(destructure-defwidget-args args)
      (setf (values lambda-vars lambda-call-form) (extract-lambda-info lambda-list))
      `(progn
	 (defun ,name (,@lambda-list)
	   ,widget-decals
	   (lambda (,widget *context*
		    &aux
		      (use-old-state
		       (and ,widget
			    (eql (widget-version ,widget) ,version)
			    (eql (widget-name ,widget) ',name)))
		      (dont-build
		       (and use-old-state
			    (not (null (widget-memo-if-function ,widget)))
			    (funcall (widget-memo-if-function ,widget) ,widget ,@lambda-call-form))))
	     (declare (ignorable *context*))
	     (symbol-macrolet (,@(loop for i from 0
				       for binding in state
				       for var = (if (listp binding) (first binding) binding)
				       collect (list var `(aref (widget-state ,widget) ,i)))
			       (this
				 ,widget))
	       (unless dont-build
		 (when (not use-old-state)
		   (when ,widget
		     (when (widget-cleanup-function ,widget)
		       (funcall (widget-cleanup-function ,widget) ,widget))))
		 (setf ,widget (make-widget
				:name ',name
				:version ,version
				:dirty t
				:state (if use-old-state
					   (widget-state ,widget)
					   (make-array ,(length state) :initial-element nil))
				:children (if ,widget
					      (widget-children ,widget)
					      (make-array 0 :fill-pointer 0 :adjustable t))
				:memo-if-function nil
				:build-function
				(lambda (,widget *context*)
				  (declare (ignorable ,widget *context*))
				  (block nil
				    ,@build))
				:on-layout-x-function
				,(when on-layout-x
				   (defwidget-create-lambda widget on-layout-x))

				:on-layout-y-function
				,(when on-layout-y
				   (defwidget-create-lambda widget on-layout-y))

				:render-function
				,(when render
				   (defwidget-create-lambda widget render))
				:cleanup-function
				,(when cleanup
				   (defwidget-create-lambda widget `(() ,@cleanup)))))
		 ;; Initialize variables
		 (unless use-old-state
		   ,@(loop for binding in state
			   for i from 0
			   when (listp binding)
			     collect `(setf ,(first binding) ,(second binding))))
		 ;; Set memo-if function
		 ;; Has to be done after the state is initialized
		 ,(when memo-if
		    (let ((lambda-vars-gensyms (loop for var in lambda-vars
						     collect (gensym (symbol-name var)))))
		      `(setf (widget-memo-if-function ,widget)
			     (let (,@(mapcar #'list lambda-vars-gensyms lambda-vars))
			       (declare (ignorable ,@lambda-vars-gensyms))
			       (macrolet ((prev (sym)
					    (unless (find sym ',lambda-vars)
					      (error "Can't call prev on ~a.
It is not an argument to defwidget.
Only one of ~a is allowed inside ~a."
						     sym ',lambda-vars ',name))
					    (nth (position sym ',lambda-vars) ',lambda-vars-gensyms)))
				 (lambda (,widget ,@lambda-list)
				   (declare (ignorable ,widget ,@lambda-vars))
				   ,(trivial-macroexpand-all:macroexpand-all `(progn ,@memo-if))))))))))
	     ;; Return widget (the same or the newly created one)
	     ,widget))
	 (setf (get ',name :gauthali.widget.version) ,version)))))

(declaim (inline create-widget))
(defun create-widget (widget-initializer old-widget context)
  "Calls the widget initializer function `widget-initializer'.

Example:
(defwidget button (text) ...) defines a widget called button.
Calling (button \"Click Me\") actually returns a lambda that creates
the widget. This is the widget initializer.

`create-widget' calls that lambda with proper arguments."
  (funcall widget-initializer old-widget context))

(defun destroy-widget (widget)
  (when (widget-cleanup-function widget)
    (funcall (widget-cleanup-function widget) widget)))

(defun widget-rebuild (widget)
  (setf (widget-dirty widget) t))

(defun widget-bounds (widget)
  "Returns (values x y w h)"
  (let ((lx (widget-layout-x widget))
	(ly (widget-layout-y widget)))
    (values (layout-offset lx) (layout-offset ly) (layout-size lx) (layout-size ly))))


(defmacro wrap-build ((widget widget-form) &body build)
  "Create a widget that overrides the build of the `widget'.
Use `call-original-build' inside the `build' forms to call the original build function."
  (assert (symbolp widget))
  (let ((context (gensym "context"))
	(original-build (gensym "original-build")))
    `(lambda (,widget ,context)
       (setf ,widget (create-widget ,widget-form ,widget ,context))
       (let ((,original-build (widget-build-function ,widget)))
	 (setf (widget-build-function ,widget)
	       (lambda (,widget ,context)
		 (macrolet ((call-original-build ()
			      `(funcall ,',original-build ,',widget ,',context)))
		   ,@build))))
       ,widget)))

(defun recompiledp (widget)
  (not (eql (widget-version widget)
	    (get (widget-name widget) ':gauthali.widget.version))))

(defun update-widget-tree (widget context)
  (cond ((or (widget-dirty widget)
	     (some #'recompiledp (widget-children widget)))
	 (let ((errored t))
	   (unwind-protect
		(progn
		  (setf (widget-dirty widget) nil)
		  (let* ((child-widgets (widget-children widget))
			 (old-length (length child-widgets))
			 (child-widget-funcs))

		    ;; Stuff to do before calling widget-build-function
		    (setf (fill-pointer (widget-properties widget)) 0)
		    (setf (fill-pointer (widget-event-handlers widget)) 0)

		    ;; Build the widget
		    (context-set-property% context :gauthali.widget.current widget)
		    (setf child-widget-funcs (uiop:ensure-list (funcall (widget-build-function widget) widget context)))
		    (context-restore-property% context :gauthali.widget.current)
		    (context-setup context widget)
		    ;; Update widget-children
		    (setf (fill-pointer child-widgets) 0)
		    (loop for i from 0
			  for child-widget-func in child-widget-funcs
			  for child-old-widget = (when (< i old-length) (aref child-widgets i))
			  do
			     (let ((child-widget (create-widget child-widget-func child-old-widget context)))
			       (update-widget-tree child-widget context)
			       (vector-push-extend child-widget child-widgets)))
		    ;; Destory old widget children
		    (loop for i from (length child-widgets) below old-length
			  for child = (aref child-widgets i) do
			  (when (widget-cleanup-function child)
			    (funcall (widget-cleanup-function child) child)))

		    (context-restore context widget))

		  (setf errored nil))
	     (when errored
	       (setf (widget-dirty widget) t)))))
	(t ;; widget is not dirty but the children might be
	 (context-setup context widget)
	 (loop for child-widget across (widget-children widget) do
	       (update-widget-tree child-widget context))
	 (context-restore context widget))))

(defun widget-rebuild-all (widget)
  "Marks all widgets for rebuild."
  (labels ((rec (w)
	     (setf (widget-dirty w) t)
	     (loop for child across (widget-children w) do
	       (rec child))))
    (rec widget)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *layout-set-keys* '(major-axis
				    width width.min width.max flex.x
				    height height.min height.max flex.y
				    padding padding.x padding.y
				    child-gap child-gap.x child-gap.y
				    alignment.x alignment.y)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *layout-set-arg* `(&rest keyword-args
					 &key ,@*layout-set-keys*)))

(defun layout-set (widget
		    &key major-axis
		      width width.min width.max flex.x
		      height height.min height.max flex.y
		      padding padding.x padding.y
		      child-gap child-gap.x child-gap.y
		      alignment.x alignment.y)
  (let ((x (widget-layout-x widget))
	(y (widget-layout-y widget)))
    (when major-axis
      (ecase major-axis
	(:x (setf (layout-major-axisp x) t
		  (layout-major-axisp y) nil))
	(:y (setf (layout-major-axisp x) nil
		  (layout-major-axisp y) t))))
    ;; Width
    (when flex.x
      (setf (layout-type x) :flex
	    (layout-flex-value x) flex.x))
    (when width
      (setf (layout-type x) :fixed
	    (layout-size x) width))
    (when width.min
      (setf (layout-minimum x) width.min))
    (when width.max
      (setf (layout-maximum x) width.max))

    ;; Height
    (when flex.y
      (setf (layout-type y) :flex
	    (layout-flex-value y) flex.y))
    (when height
      (setf (layout-type y) :fixed
	    (layout-size y) height))
    (when height.min
      (setf (layout-minimum y) height.min))
    (when height.max
      (setf (layout-maximum y) height.max))

    ;; Padding
    (when padding
      (setf (layout-padding x) padding
	    (layout-padding y) padding))
    (when padding.x
      (setf (layout-padding x) padding.x))
    (when padding.y
      (setf (layout-padding y) padding.y))

    ;; Child Gap
    (when child-gap
      (setf (layout-child-gap x) child-gap
	    (layout-child-gap y) child-gap))
    (when child-gap.x
      (setf (layout-child-gap x) child-gap.x))
    (when child-gap.y
      (setf (layout-child-gap y) child-gap.y))

    ;; Alignment
    (when alignment.x
      (setf (layout-alignment x) alignment.x))
    (when alignment.y
      (setf (layout-alignment y) alignment.y))))

(defmacro layout (#.`(&rest keyword-args &key ,@*layout-set-keys*) &body widget-form)
  (declare #.`(ignorable ,@*layout-set-keys*))
  `(wrap-build (wrapped-widget (progn ,@widget-form))
     (prog1 (call-original-build)
       (layout-set wrapped-widget ,@keyword-args))))

(defun update-widget-layouts (widget x y w h)
  "Update layout (first X and then Y) of all the widgets rooted at
`widget' recursively."
  (labels ((create-tree (widget accessor)
	     (cons (funcall accessor widget)
		   (loop for child across (widget-children widget)
			 collect (create-tree child accessor))))
	   (call-on-layout (widget on-layout-accessor layout-accessor)
	     (let ((on-layout (funcall on-layout-accessor widget))
		   (layout (funcall layout-accessor widget)))
	       (when on-layout
		 (funcall on-layout widget (layout-offset layout) (layout-size layout)))
	       (loop for child across (widget-children widget)
		     do (call-on-layout child on-layout-accessor layout-accessor)))))

    ;; Set the layout of root widget
    (when w
      (setf (layout-type (widget-layout-x widget)) :fixed)
      (setf (layout-size (widget-layout-x widget)) (coerce w 'single-float)))

    (when h
      (setf (layout-type (widget-layout-y widget)) :fixed)
      (setf (layout-size (widget-layout-y widget)) (coerce h 'single-float)))

    (setf (layout-offset (widget-layout-x widget)) (coerce x 'single-float))
    (setf (layout-offset (widget-layout-y widget)) (coerce y 'single-float))

    ;; Layout X
    (solve-layout-tree (create-tree widget #'widget-layout-x))
    (call-on-layout widget #'widget-on-layout-x-function #'widget-layout-x)
    ;; Layout Y
    (solve-layout-tree (create-tree widget #'widget-layout-y))
    (call-on-layout widget #'widget-on-layout-y-function #'widget-layout-y)))

(defun call-render-funcs (widget renderer &optional (offset-x 0.0) (offset-y 0.0))
  "Call all the render function of widget tree recursively."
  (labels ((rec (widget)
	     (with-slots (render-function layout-x layout-y) widget
	       (when render-function
		 (funcall render-function widget renderer
			  (+ offset-x (layout-offset layout-x))
			  (+ offset-y (layout-offset layout-y))
			  (layout-size layout-x) (layout-size layout-y)))
	       (loop for child across (widget-children widget)
		     do (rec child)))))
    (rec widget)))

(defun call-event-handlers (widget event)
  "Find and class event handlers registered in widget tree recursively.
Stop when the first only returns :stop.

Returns :stop if any handler returned :stop, otherwise nil."
  (let ((event-class (class-of event)))
    (labels ((rec (widget)
	       (when (or (loop for (class . handler) across (widget-event-handlers widget)
			       for result = (when (eql event-class class)
					      (funcall handler event))
			       when (eql result :stop)
				 return t)
			 (loop for child across (widget-children widget)
			       for result = (rec child)
			       when (eql result :stop)
				 return t))
		 :stop)))
      (rec widget))))

(defun build-widget (widget-initializer prev-instance context)
  (let ((maybe-new-instance (create-widget widget-initializer prev-instance context)))
    (update-widget-tree maybe-new-instance context)
    maybe-new-instance))
