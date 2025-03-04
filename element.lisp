(in-package :gauthali)

(defstruct element
  (x (make-layout) :type layout)
  (y (make-layout) :type layout)
  (draw-fn nil :type (or null function))
  (parent nil :type (or null element))
  (children (make-array 0) :type array))

(defmethod print-object :around ((obj element) stream)
  (let ((*print-circle* t))
    (call-next-method)))

(defun create-layout (sizing position)
  "Syntactic sugar for layout creation.
Sizing:
x       => type :fixed, value x
:flex x => type :flex,  flex-size x
nil     => type :fit
:min x  => minimum x
:max x  => maximum x

Position:
nil         => alignment :start
align x     => alignment x
padding x   => padding x
child-gap x => child-gap x"
  (let ((layout (make-layout)))
    (with-slots (padding alignment child-gap type size minimum maximum flex-value) layout
      (when position
	(assert (listp position))
	(setf alignment (getf position :align :start)
	      padding (coerce (getf position :padding 0.0) 'single-float)
	      child-gap (coerce (getf position :child-gap 0.0) 'single-float)))

      (cond ((numberp sizing) (setf type :fixed
				    size (coerce sizing 'single-float)))
	    ((listp sizing)
	     (let ((flex (getf sizing :flex)))
	       (setf minimum (coerce (getf sizing :min 0.0) 'single-float)
		     maximum (coerce (getf sizing :max most-positive-short-float) 'single-float)
		     type (if flex :flex :fit)
		     size 0.0
		     flex-value (if flex flex 0.0))))))
    layout))


(defparameter *current-element* nil)
(defmacro <> ((draw-fn app &rest keys &key width height &allow-other-keys)
		   &optional grid-args
		   &body body)
  (destructuring-bind (&key row col (major-axis :row)) grid-args
    (let ((el (gensym "EL-")))
      `(let ((,el (make-element
		   :x (create-layout ,(if (listp width) `(list ,@width) width) ,(when row `(list ,@row)))
		   :y (create-layout ,(if (listp height) `(list ,@height) height) ,(when col `(list ,@col)))
		   :parent *current-element*
		   :draw-fn (lambda (width height x y)
			      (,draw-fn ,app :width width
					     :height height
					     :x x
					     :y y
					     ,@(uiop:remove-plist-keys
						(list :width :height :x :y)
						keys)))
		   :children (make-array 0 :element-type 'element :fill-pointer 0 :adjustable t))))
	 (setf (layout-major-axisp (element-x ,el))
	       ,(if (eql major-axis :row) t nil)
	       (layout-major-axisp (element-y ,el))
	       ,(if (eql major-axis :col) t nil))
	 (when *current-element*
	   (vector-push-extend ,el (element-children *current-element*)))
	 (let ((*current-element* ,el))
	   ,@body
	   ,el)))))

(defun map-elements-tree (fn root)
  (cons (funcall fn root)
	(loop for el across (element-children root)
	      collect (map-elements-tree fn el))))

(defun solve-elements-layout (root)
  (let ((width-layout-tree (map-elements-tree #'element-x root))
	(height-layout-tree (map-elements-tree #'element-y root)))
    (solve-layout-tree width-layout-tree)
    (solve-layout-tree height-layout-tree)
    root))

(defun create-test-elements ()
  (element (draw-nothing nil :width 500.0 :height 500.0)
      (:row (:padding 12.0 :child-gap 3.0))
    (element (draw-nothing nil) nil
      (element (draw-nothing nil :width 100.0 :height 100.0) nil))
    (element (draw-nothing nil :width (:flex 1.0) :height (:flex 1.0)) nil)))

(defun draw-element (el)
  "Call the render function of el and then its children."
  (with-slots (x y) el
    (funcall (element-draw-fn el)
	     (layout-size x) (layout-size y)
	     (layout-offset x) (layout-offset y)))
  (loop for child across (element-children el)
	do (draw-element child)))
