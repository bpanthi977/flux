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
  (render-function nil :type (or null (function (widget t float float float float))))
  (on-layout-x-function nil :type (or null (function (widget float float))))
  (on-layout-y-function nil :type (or null (function (widget float float))))
  (dirty nil :type boolean)
  (id nil)
  (parent nil)
  (state (vector) :type vector)
  (layout-x (make-layout :major-axisp t) :type layout)
  (layout-y (make-layout :major-axisp nil) :type layout)
  (properties (make-array 0 :fill-pointer 0 :adjustable t) :type vector)
  (event-handlers (make-array 0 :fill-pointer 0 :adjustable t))
  (children (make-array 0 :fill-pointer 0 :adjustable t)))

(defvar *widget-symbol* nil
  "Symbol currently being used for widget during macroexpansion of defwidget.")
(defvar *context-symbol* nil
  "Symbol currently being used for context during macroexpansion of defwidget.")
(defparameter *allow-widget-access* nil
  "Is accessing widget symbol allowed during this macroexpansion time.")
(defparameter *allow-context-access* nil
  "Is accessing context symbol allowed during this macroexpansion time.")

(defmacro property-set (property value)
  "Set the property value for children widgets."
  (unless (and *allow-widget-access* *allow-context-access*)
    (error "property-get macro can be called only inside defwidget definition in :build."))
  `(vector-push-extend (cons ,property ,value) (widget-properties ,*widget-symbol*)))

(defmacro property-get (property &optional default)
  "Get the property value set by parent/ancestor widgets."
  (unless (and *allow-widget-access* *allow-context-access*)
    (error "property-get macro can be called only inside defwidget definition in :build."))
  `(context-get-property% ,*context-symbol* ,property ,default))

(defmacro layout-set (&rest keyword-args
		      &key major-axis
			width width.min width.max flex.x
			height height.min height.max flex.y
			padding padding.x padding.y
			child-gap child-gap.x child-gap.y
			alignment.x alignment.y)
  "Set layout values."
  (declare (ignorable major-axis
		      width width.min width.max flex.x
		      height height.min height.max flex.y
		      padding padding.x padding.y
		      child-gap child-gap.x child-gap.y
		      alignment.x alignment.y))
  (unless *allow-widget-access*
    (error "You can use layout-set function only inside defwidget."))
  `(layout-set% ,*widget-symbol* ,@keyword-args))

(defmacro widget-rebuild ()
  (unless *allow-widget-access*
    (error "widget-rebuild can only be called from inside defwidget."))
  `(setf (widget-dirty ,*widget-symbol*) t))

(defmacro widget-bounds ()
  "Returns (values x y w h)"
  (unless *allow-widget-access*
    (error "widget-bounds can only be called from inside defwidget."))
  `(let ((lx (widget-layout-x ,*widget-symbol*))
	 (ly (widget-layout-y ,*widget-symbol*)))
     (values (layout-offset lx) (layout-offset ly) (layout-size lx) (layout-size ly))))

(defmacro on (event-class-symbol handler)
  (unless (and *allow-widget-access* *allow-context-access*)
    (error "on can only be called from inside defwidget in :build."))
  `(vector-push-extend (cons (find-class ,event-class-symbol)  ,handler) (widget-event-handlers ,*widget-symbol*)))

(defun destructure-defwidget-args (args)
  (let (state build on-layout-x on-layout-y render)
    (flet ((render-lambda-formp (form)
	     (and (= 5 (length form))
		  (every #'symbolp form)))
	   (on-layout-lambda-formp (form)
	     (and (= 2 (length form))
		  (every #'symbolp form))))
      (loop for clause in args do
	(cond ((or (not (listp clause))
		   (not (member (first clause) '(:state :build :on-layout-x :on-layout-y :render))))
	       (error "~a not a list beginning with :state, :build, :on-layout-x, :on-layout-y:render." clause))
	      (t
	       (case (first clause)
		 (:state (setf state (rest clause)))
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
		  (setf render (rest clause))))))))
    (values state build on-layout-x on-layout-y render)))

(defun defwidget-create-lambda (widget-var args-and-body)
  (destructuring-bind (args . body) args-and-body
    (let* ((decals (if (and (listp body) (listp (first body)) (eql (first (first body)) 'declare))
		       (first body)))
	   (body (if decals (rest body) body)))
      `(lambda (,widget-var ,@args)
	 (declare (ignorable ,widget-var))
	 ,decals
	 ,(trivial-macroexpand-all:macroexpand-all `(progn ,@body))))))

(defmacro defwidget (name (&rest lambda-list)
		     &body args)
  (let ((widget (gensym "widget"))
	(context (gensym "context"))
	(version (1+ (get name :gauthali.widget.version -1))))
    (setf (get name :gauthali.widget.version) version)
    (multiple-value-bind (state build on-layout-x on-layout-y render)
	(destructure-defwidget-args args)
      (let* ((decals (if (and (listp build) (listp (first build)) (eql (first (first build)) 'declare))
			 (first build)))
	     (build (if decals (rest build) build))
	     (*allow-widget-access* t)
	     (*widget-symbol* widget)
	     (*context-symbol* context))
	`(defun ,name (,@lambda-list)
	   ,decals
	   (lambda (,widget ,context &aux (use-old (and ,widget (eql (widget-version ,widget) ,version))))
	     (declare (ignorable ,context))
	     ;; TODO: Create widget if necessary
	     ;; only when :memo says so
	     (symbol-macrolet (,@(loop for i from 0
				       for binding in state
				       for var = (if (listp binding) (first binding) binding)
				       collect (list var `(aref (widget-state ,widget) ,i))))
	       (setf ,widget (make-widget
			      :name ',name
			      :version ,version
			      :dirty t
			      :state (if use-old
					 (widget-state ,widget)
					 (make-array ,(length state) :initial-element nil))
			      :children (if ,widget
					    (widget-children ,widget)
					    (make-array 0 :fill-pointer 0 :adjustable t))
			      :build-function
			      ,(let ((*allow-context-access* t))
				 `(lambda (,widget ,context)
				    (declare (ignorable ,widget ,context))
				    ;; Set build function
				    (setf (fill-pointer (widget-properties ,widget)) 0)
				    (setf (fill-pointer (widget-event-handlers ,widget)) 0)
				    ,(trivial-macroexpand-all:macroexpand-all
				      `(progn ,@build))))
			      :on-layout-x-function
			      ,(when on-layout-x
				 (defwidget-create-lambda widget on-layout-x))

			      :on-layout-y-function
			      ,(when on-layout-y
				 (defwidget-create-lambda widget on-layout-y))

			      :render-function
			      ,(when render
				 (defwidget-create-lambda widget render))))
	       ;; Initialize variables
	       (unless use-old
		 ,@(loop for binding in state
			 for i from 0
			 when (listp binding)
			   collect `(setf ,(first binding) ,(second binding)))))
	     ;; Return widget (the same or the newly created one)
	     ,widget))))))

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
			 (child-widgets-func (uiop:ensure-list (funcall (widget-build-function widget) widget context))))

		    (context-setup context widget)

		    ;; Update widget-children
		    (setf (fill-pointer child-widgets) 0)
		    (loop for i from 0
			  for child-widget-func in child-widgets-func
			  for child-old-widget = (when (< i old-length) (aref child-widgets i))
			  do
			     (let ((child-widget (funcall child-widget-func child-old-widget context)))
			       (update-widget-tree child-widget context)
			       (vector-push-extend child-widget child-widgets)))

		    (context-restore context widget))
		  (setf errored nil))
	     (when errored
	       (setf (widget-dirty widget) t)))))
	(t ;; widget is not dirty but the children might be
	 (context-setup context widget)
	 (loop for child-widget across (widget-children widget) do
	       (update-widget-tree child-widget context))
	 (context-restore context widget))))

(defun layout-set% (widget
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

(defun update-widget-layouts (widget)
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

    ;; Layout X
    (solve-layout-tree (create-tree widget #'widget-layout-x))
    (call-on-layout widget #'widget-on-layout-x-function #'widget-layout-x)
    ;; Layout Y
    (solve-layout-tree (create-tree widget #'widget-layout-y))
    (call-on-layout widget #'widget-on-layout-y-function #'widget-layout-y)))

(defun call-render-funcs (widget renderer)
  "Call all the render function of widget tree recursively."
  (labels ((rec (widget)
	     (with-slots (render-function layout-x layout-y) widget
	       (when render-function
		 (funcall render-function widget renderer
			  (layout-offset layout-x) (layout-offset layout-y)
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
