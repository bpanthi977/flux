(in-package #:gauthali)

(defwidget text (text)
  (:state _text ttf-text)
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
       (cffi:with-foreign-string ((str bytes) text)
	 (setf ttf-text (sdl3-ttf:create-text text-engine font str bytes))
	 (let ((ptr ttf-text))
	   (trivial-garbage:finalize ttf-text (lambda () (sdl3-ttf:destroy-text ptr))))
	 (multiple-value-bind (ret w h) (sdl3-ttf:get-text-size ttf-text)
	   (assert-ret ret)
	   (layout-set :flex.x 1.0
		       :width.max (coerce w 'single-float)
		       :height.min (coerce h 'single-float)))))

       nil))
  (:on-layout-x (x w)
		(declare (ignore x))
		(sdl3-ttf:set-text-wrap-width ttf-text (floor w))
		(multiple-value-bind (ret w h) (sdl3-ttf:get-text-size ttf-text)
		  (declare (ignore w))
		  (assert-ret ret)
		  (layout-set :height.min (coerce h 'single-float))))
  (:render (r x y w h)
	   (declare (ignorable r h))
	   (sdl3-ttf:set-text-wrap-width ttf-text (floor w))
	   (sdl3-ttf:draw-renderer-text ttf-text x y)))

(defwidget button (name on-click)
  (:build
   (declare (ignorable name on-click))
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

(defwidget home-screen ()
  (:build
   (layout-set :flex.x 1.0)
   (list (button "Start!" (lambda () (print 'stopped)))
	 (button "Stop!!" (lambda () (print 'started)))
	 (spacer))))
