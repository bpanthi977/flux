(in-package #:gauthali)

(defwidget text (text)
  (:state _text ttf-text width height)
  (:build
   ;; Compute layout ranges
   ;; no further children widgets
   (let ((font (property-get :font))
	 (text-engine (property-get :text-engine)))
     (assert font)
     (assert text-engine)

     ;; Compute properties if cache is invalid
     (when (or (not ttf-text)
	       (not (string-equal _text text)))
       (setf _text text)
       (cffi:with-foreign-string ((str bytes) text)
	 (setf ttf-text (sdl3-ttf:create-text text-engine font str bytes))
	 (let ((ptr ttf-text))
	   (trivial-garbage:finalize ttf-text (lambda () (sdl3-ttf:destroy-text ptr))))
	 (multiple-value-bind (ret w h) (sdl3-ttf:get-text-size ttf-text)
	   (assert-ret ret)
	   (setf width (coerce w 'single-float))
	   (setf height (coerce h 'single-float)))))

     (layout-set :flex.x 1.0
		 :width.max width
		 :height.min height)

     nil))
  (:on-layout-x (x w)
		(declare (ignore x))
		(sdl3-ttf:set-text-wrap-width ttf-text (floor w))
		(multiple-value-bind (ret w h) (sdl3-ttf:get-text-size ttf-text)
		  (declare (ignore w))
		  (assert-ret ret)
		  (setf height (coerce h 'single-float))
		  (layout-set :height.min height)))
  (:render (r x y w h)
	   (declare (ignorable r h))
	   (sdl3-ttf:set-text-wrap-width ttf-text (floor w))
	   (sdl3-ttf:draw-renderer-text ttf-text x y)))

(defwidget button (name)
  (:build
   (layout-set :flex.x :least
	       :padding.x 5.0
	       :padding.y 20.0
	       :alignment.x :start
	       :alignment.y :center)
   (text name))
  (:render (r x y w h)
	   (sdl3:set-render-draw-color r 125 125 125 125)
	   (sdl3:render-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x))))

(defwidget spacer ()
  (:build
   (layout-set :flex.x 1.0
	       :flex.y 1.0)))

(defun seconds-elapsed (since)
  (* 1.0 (- (get-internal-real-time) since) (/ internal-time-units-per-second)))

(defwidget countdown ()
  (:state (mount-time (get-internal-real-time))
	  refresh-time)
  (:build
   (layout-set :flex.x :least)
   (text (format nil "~,3f" (seconds-elapsed mount-time))))
  (:render (r x y w h)
	   (declare (ignore r x y w h))
	   (if (not refresh-time)
	       (setf refresh-time (get-internal-real-time)))
	   (let ((delta (- 1.0 (seconds-elapsed refresh-time))))
	     (if (< delta 0)
		 (progn (setf refresh-time (get-internal-real-time))
			(widget-rebuild))
		 (sb-ext:schedule-timer (sb-ext:make-timer
					 (lambda ()
					   (setf refresh-time (get-internal-real-time))
					   (widget-rebuild)))
					delta)))))

(defwidget home-screen ()
  (:build
   (layout-set :flex.x 1.0)
   (list (button "Start!")
	 (button "Stop!!")
	 (spacer)
	 (countdown))))
