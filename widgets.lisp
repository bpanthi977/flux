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
	   (property-set :width.max (coerce w 'single-float))
	   (property-set :height.min (coerce h 'single-float)))))

       nil))
  (:render (x y w h)
	   (declare (ignorable h))
	   (sdl3-ttf:set-text-wrap-width ttf-text w)
	   (sdl3-ttf:draw-renderer-text ttf-text x y)))

(defwidget button (name on-click)
  (:build
   (declare (ignorable name on-click))
   (property-set :padding.x 10.0)
   (property-set :padding.y 10.0)
   (text name)))



(defwidget home-screen ()
  (:build
   (list (button "Start" (lambda () (print 'started)))
	 (button "Stop" (lambda () (print 'stopped))))))
