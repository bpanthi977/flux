(in-package #:gauthali)

(defun get-resource-path (resource)
  (asdf:system-relative-pathname :gauthali resource))

(defun assert-ret (return-value)
  (when (null return-value)
    (error "Error: ~a" (sdl3:get-error))))

(defun color (r g b a)
  (make-instance 'sdl3:color :%r r :%g g :%b b :%a a))
