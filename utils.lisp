(in-package #:gauthali)

(defun assert-ret (return-value)
  (when (null return-value)
    (error "Error: ~a" (sdl3:get-error))))
