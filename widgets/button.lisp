(in-package #:gauthali)

(defwidget pressable (child-widget on-press &key on-press-down)
  (:state pressed)
  (:build
   (on sdl3:mouse-button-event
       (lambda (event)
	 (let ((old pressed))
	   (if (sdl3:%down event)
	       (multiple-value-bind (x y w h) (widget-bounds this)
		 (if (and (<= x (sdl3:%x event) (+ x w))
			  (<= y (sdl3:%y event) (+ y h)))
		     (setf pressed t)
		     (setf pressed nil)))
	       (setf pressed nil))
	   (unless (eql old pressed)
	     (when (and pressed on-press-down) (funcall on-press-down))
	     (when (not pressed) ;; press up
	       (funcall on-press))))))

   child-widget))

(defwidget button (name on-press &key (highlight-on-hover t))
  (:state pressed hovered)
  (:build

   (layout-set this
	       :padding 2.0
	       :alignment.x :center
	       :alignment.y :center)


   (let ((widget (pressable (text name)
			    (lambda ()
			      (setf pressed nil)
			      (funcall on-press))
			    :on-press-down (lambda () (setf pressed t)))))

     (when highlight-on-hover
       (setf widget (hoverable (lambda (hover) (setf hovered hover)) widget)))

     widget))
  (:render (r x y w h)
	   (cond ((and (not hovered) (not pressed))
		  (sdl3:set-render-draw-color r 125 125 125 255)
		  (sdl3:render-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x)))
		 (pressed
		  (sdl3:set-render-draw-color r 0 0 0 255)
		  (sdl3:render-fill-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x)))
		 (hovered
		  (sdl3:set-render-draw-color r 125 125 125 255)
		  (sdl3:render-fill-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x))))))
