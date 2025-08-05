(in-package #:gauthali)

(defwidget button (name on-press)
  (:state pressed)
  (:build
   (on 'sdl3:mouse-button-event
       (callback (event)
	 (let ((old pressed))
	   (if (sdl3:%down event)
	       (multiple-value-bind (x y w h) (widget-bounds)
		 (if (and (<= x (sdl3:%x event) (+ x w))
			  (<= y (sdl3:%y event) (+ y h)))
		     (setf pressed t)
		     (setf pressed nil)))
	       (setf pressed nil))
	   (unless (eql old pressed)
	     (when pressed
	       (funcall on-press))
	     (widget-rebuild)))))

   (layout-set :flex.x :least
	       :alignment.x :center
	       :alignment.y :center)
   (text name))
  (:render (r x y w h)
	   (sdl3:set-render-draw-color r 125 125 125 125)
	   (sdl3:render-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x))))
