(in-package #:gauthali)

(defwidget hoverable (on-hover-change widget-func)
  (:state hover)
  (:build
   (on sdl3:mouse-motion-event
       (lambda (event)
	 (multiple-value-bind (x y w h) (widget-bounds this)
	   (let ((hovering (and (<= x (sdl3:%x event) (+ x w))
				(<= y (sdl3:%y event) (+ y h)))))
	     (unless (eql hover hovering)
	       (setf hover hovering)
	       (funcall on-hover-change hover))))))
   (funcall widget-func)))
