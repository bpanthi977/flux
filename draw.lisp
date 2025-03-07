(in-package #:gauthali)

(defun create-text-element (text-engine text &key width color font)
  (cffi:with-foreign-string ((str bytes) text)
    (let* ((ttf-text (sdl3-ttf:create-text text-engine font str bytes)))
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

(defun draw-rectangle (renderer &key width height x y color)
  (declare (type float x y width height)
	   (type (or null sdl3:color) color))
  (when color
    (sdl3:set-render-draw-color renderer
				(slot-value color 'sdl3:%r)
				(slot-value color 'sdl3:%g)
				(slot-value color 'sdl3:%b)
				(slot-value color 'sdl3:%a)))
  (sdl3:render-fill-rect renderer (make-instance 'sdl3:frect :%x x :%y y :%w width :%h height)))

(defun draw-rectangle-frame (renderer &key width height x y color)
  (declare (type float x y width height)
	   (type (or null sdl3:color) color))
  (when color
    (sdl3:set-render-draw-color renderer
				(slot-value color 'sdl3:%r)
				(slot-value color 'sdl3:%g)
				(slot-value color 'sdl3:%b)
				(slot-value color 'sdl3:%a)))
  (sdl3:render-rect renderer (make-instance 'sdl3:frect :%x x :%y y :%w width :%h height)))

(defun draw-nothing (renderer &key width height x y)
  (declare (ignore renderer width height x y)))
