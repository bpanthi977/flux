(in-package #:gauthali)

(defstruct texture-cache
  (texture nil)
  (width 0.0)
  (height 0.0)
  (render-scale 1.0))

(defmethod update-texture-size ((cache texture-cache) r w h)
  "If necessary updates the texture to be of size w * h."
  (setf w (ceiling w)
	h (ceiling h))
  (with-slots (texture width height) cache
    (if texture
	(unless (and (= w width) (= h height))
	  (sdl3:destroy-texture texture)
	  (setf texture nil)
	  (setf texture (sdl3:create-texture r :rgba8888 :target w h)))
	(setf texture (sdl3:create-texture r :rgba8888 :target w h)))
    (setf width w height h)
    (assert (not (cffi:null-pointer-p texture)))))

(defwidget scrollable (widget-func &key (x-speed 4.0) (y-speed 4.0))
  (:state widget (texture-cache (make-instance 'texture-cache)) (yscroll 0.0) (xscroll 0.0) (render-scale 1.0) bg-color)
  (:build
   (setf bg-color (property-get :bg-color))
   (setf render-scale (property-get :render-scale))
   (on t (lambda (event)
	   (typecase event
	     ;; Pass mouse events only if they occur inside the widget
	     ((or sdl3:mouse-motion-event sdl3:mouse-button-event)
	      (let ((x (sdl3:%x event))
		    (y (sdl3:%y event)))
		(multiple-value-bind (wx wy w h) (widget-bounds this)
		  (when (and (<= wx x (+ wx w))
			     (<= wy y (+ wy h)))
		    (incf (sdl3:%x event) (- xscroll wx))
		    (incf (sdl3:%y event) (- yscroll wy))
		    (prog1 (call-event-handlers widget event)
		      (setf (sdl3:%x event) x
			    (sdl3:%y event) y))))))
	     ;; Pass other events as it is
	     (t (call-event-handlers widget event)))))

   (on sdl3:mouse-wheel-event
       (lambda (event)
	 (call-event-handlers widget event)
	 (let ((x (sdl3:%x event))
	       (y (sdl3:%y event)))
	   (when (eql (sdl3:%direction event) :flipped)
	     (setf x (- x) y (- y)))
	   (multiple-value-bind (_x _y w h) (widget-bounds this)
	     (declare (ignore _x _y))
	     (setf yscroll (alexandria:clamp (+ yscroll (* y-speed y)) 0 (max 0 (- (/ (texture-cache-height texture-cache) render-scale) h))))
	     (setf xscroll (alexandria:clamp (+ xscroll (* x-speed x)) 0 (max 0 (- (/ (texture-cache-width texture-cache) render-scale) w))))))))


   (layout-set this
	       :flex.x 1.0
	       :flex.y 1.0)
   (setf (widget-detach-children this) t)
   (ref (lambda (w) (setf widget w))
	(funcall widget-func)))

  (:render (r x y w h)
     "Layout - Render"
     ;; Layout
     (layout-set widget :width.min w :height.min h)
     (update-widget-layouts widget 0.0 0.0 nil nil)
     ;; Render to texture
     (multiple-value-bind (_x _y ww wh) (widget-bounds widget)
       (declare (ignore _x _y))
       (unless (or (= ww 0.0) (= wh 0.0))
	 (update-texture-size texture-cache r (* render-scale ww) (* render-scale wh))
	 (assert-ret (sdl3:set-render-target r (texture-cache-texture texture-cache)))
	 (assert-ret (sdl3:set-render-scale r render-scale render-scale))
	 (set-render-draw-color r bg-color)
	 (sdl3:render-clear r)
	 (call-render-funcs widget r)
	 (assert-ret (sdl3:set-render-target r (cffi:null-pointer)))
	 ;;(print (list x y w h ww wh yscroll))
	 ;; Blit the texture at appropriate location
	 (sdl3:set-texture-scale-mode (texture-cache-texture texture-cache) :nearest)
	 (sdl3:render-texture r (texture-cache-texture texture-cache)
			      (make-instance 'sdl3:frect :%x 0.0 :%y (* render-scale (min wh yscroll)) :%w (* render-scale (min w ww)) :%h (* render-scale (max 0.0 (- wh yscroll))))
			      (make-instance 'sdl3:frect :%x x :%y y :%w (min w ww) :%h (max 0.0 (- wh yscroll))))))))
