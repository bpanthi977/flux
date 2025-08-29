;;;; benchmark-app.lisp
(defpackage #:flux/benchmark
  (:use #:cl #:flux))

(in-package #:flux/benchmark)

(defwidget rainbow-button (i j)
  (:state (r (random 255)) (g (random 255)) (b (random 255)) (hover nil))
  (:build
   (property-set this :bg-color (vector r g b 255))
   (hoverable (lambda (h)
		(setf hover h)
		(widget-rebuild this))
	      (button (if hover
			  (format nil "Hover")
			  (format nil "Button ~a, ~a" i j))
		      (lambda ()
			(setf r (random 255)
			      g (random 255)
			      b (random 255))
			(widget-rebuild this)))))
  (:render (renderer x y w h)
	   (declare (ignore renderer x y w h))
	   (setf r (random 255)
		 g (random 255)
		 b (random 255))
	   (widget-rebuild this)))

(defwidget button-row (i)
  (:build
   (loop repeat 21
	 for j from 0
	 collect (rainbow-button i j))))

(defwidget button-stress-app ()
  (:state (display-all nil))
  (:build
   (layout-set this
	       :padding.x 20.0
	       :padding.y 20.0)
   (column ()
     (button "Click Me!" (lambda ()
			  (setf display-all t)
			  (widget-rebuild this)))
     (when display-all
       (scrollable
	(column-widget
	 ()
	 (loop repeat 100
	       for i from 0
	       collect (button-row i))))))))

(defwidget main-widget ()
  (:build
   (button-stress-app)))

(defun main()
  (start-ui :widget (main-widget)
	    :title "Todo App"
	    :width 600 :height 600))
