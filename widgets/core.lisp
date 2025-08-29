(in-package #:flux)

(defwidget spacer ()
  (:build
   (layout-set this
	       :flex.x 1.0
	       :flex.y 1.0)))

(defwidget hspace (width)
  (:build
   (layout-set this :width width)))

(defwidget row-widget (layout-args widgets)
  (:build
   (apply #'layout-set this layout-args)
   (remove nil widgets)))

(defmacro row ((&rest layout-args) &body widgets)
  `(row-widget (list ,@layout-args)
	       (list
		,@widgets)))

(defwidget column-widget (layout-args widgets)
  (:build
   (apply #'layout-set this layout-args)
   (layout-set this :major-axis :y)
   (remove nil widgets)))

(defmacro column ((&rest layout-args) &body widgets)
  `(column-widget (list ,@layout-args)
		  (list
		   ,@widgets)))

(defwidget ref (setter widget)
  "Calls `setter' with the instance of `widget' after it is built."
  (:build
   (lambda (prev-instance context)
     (let ((instance (funcall widget prev-instance context)))
       (funcall setter instance)
       instance))))
