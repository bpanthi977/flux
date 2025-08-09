(in-package #:gauthali)

(defwidget spacer ()
  (:build
   (layout-set this
	       :flex.x 1.0
	       :flex.y 1.0)))

(defwidget row-widget (layout-args widgets-func)
  (:build
   (apply #'layout-set this layout-args)
   (funcall widgets-func)))

(defmacro row ((&rest layout-args) &body widgets)
  `(row-widget (list ,@layout-args)
	       (lambda ()
		 (list
		  ,@widgets))))

(defwidget column-widget (layout-args widgets-func)
  (:build
   (apply #'layout-set this layout-args)
   (layout-set this :major-axis :y)
   (funcall widgets-func)))

(defmacro column ((&rest layout-args) &body widgets)
  `(column-widget (list ,@layout-args)
	       (lambda ()
		 (list
		  ,@widgets))))
