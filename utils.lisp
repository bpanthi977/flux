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
