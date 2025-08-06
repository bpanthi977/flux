(in-package #:gauthali)

(defun get-resource-path (resource)
  (asdf:system-relative-pathname :gauthali resource))

(defun assert-ret (return-value)
  (when (null return-value)
    (error "Error: ~a" (sdl3:get-error))))

(defun sdl3-color (color)
  (make-instance 'sdl3:color
		 :%r (aref color 0)
		 :%g (aref color 1)
		 :%b (aref color 2)
		 :%a (aref color 3)))

(defun set-render-draw-color (r color)
  (sdl3:set-render-draw-color r (aref color 0) (aref color 1) (aref color 2) (aref color 3)))

(defmacro with-render-scale-off (r (sx sy) &body body)
  (assert (symbolp sx))
  (assert (symbolp sy))
  (let ((renderer (gensym "renderer"))
	(ret (gensym "ret")))
    `(let ((,renderer ,r))
       (multiple-value-bind (,ret ,sx ,sy) (sdl3:get-render-scale ,renderer)
	 (declare (ignorable ,sx ,sy))
	 (assert-ret ,ret)
	 (when (and (= ,sx ,sy) (not (= ,sx 1.0)))
	   (sdl3:set-render-scale ,renderer 1.0 1.0))
	 ,@body
	 (when (and (= ,sx ,sy) (not (= ,sx 1.0)))
	   (sdl3:set-render-scale ,renderer ,sx ,sy))))))
