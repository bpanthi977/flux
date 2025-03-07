(in-package #:gauthali)

(defun create-text-element (app text &key width font-size color)
  (when font-size
    (sdl3-ttf:set-font-size (font app) font-size))
  (cffi:with-foreign-string ((str bytes) text)
    (let* ((ttf-text (sdl3-ttf:create-text (text-engine app) (font app) str bytes)))
      (sdl3-ttf:set-text-wrap-width ttf-text (or width 0))
      (multiple-value-bind (ret w h) (sdl3-ttf:get-text-size ttf-text)
	(assert-ret ret)
	(let ((el (make-element
		   :x (make-layout :type :fixed :size (coerce w 'single-float))
		   :y (make-layout :type :fixed :size (coerce h 'single-float))
		   :draw-fn (lambda (w h x y)
			      (declare (ignore w h))
			      (when color
				(sdl3-ttf:set-text-color ttf-text
							 (slot-value color 'sdl3:%r)
							 (slot-value color 'sdl3:%g)
							 (slot-value color 'sdl3:%b)
							 (slot-value color 'sdl3:%a)))
			      (sdl3-ttf:draw-renderer-text ttf-text x y)))))
	  (trivial-garbage:finalize el (lambda () (sdl3-ttf:destroy-text ttf-text)))
	  el)))))

(defun draw-rectangle (app &key width height x y color)
  (declare (type float x y width height)
	   (type (or null sdl3:color) color))
  (when color
    (sdl3:set-render-draw-color (renderer app)
				(slot-value color 'sdl3:%r)
				(slot-value color 'sdl3:%g)
				(slot-value color 'sdl3:%b)
				(slot-value color 'sdl3:%a)))
  (sdl3:render-fill-rect (renderer app) (make-instance 'sdl3:frect :%x x :%y y :%w width :%h height)))

(defun draw-rectangle-frame (app &key width height x y color)
  (declare (type float x y width height)
	   (type (or null sdl3:color) color))
  (when color
    (sdl3:set-render-draw-color (renderer app)
				(slot-value color 'sdl3:%r)
				(slot-value color 'sdl3:%g)
				(slot-value color 'sdl3:%b)
				(slot-value color 'sdl3:%a)))
  (sdl3:render-rect (renderer app) (make-instance 'sdl3:frect :%x x :%y y :%w width :%h height)))

(defun draw-nothing (app &key width height x y)
  (declare (ignore app width height x y)))
