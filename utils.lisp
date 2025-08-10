(in-package #:gauthali)

(defun make-hook-store ()
  (list :count 0
	:table (make-hash-table)))

(defun add-hook (hook-store fn)
  (let ((count (incf (getf hook-store :count))))
    (setf (gethash count (getf hook-store :table)) fn)
    count))

(defun remove-hook (hook id)
  (remhash id (getf hook :table)))

(defun run-hooks (hook-store args)
  (maphash (lambda (key value)
	     (declare (ignore key))
	     (apply value args))
	   (getf hook-store :table)))

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

(defun sdl3-rect (x y w h)
  (make-instance 'sdl3:rect :%x x :%y y :%w w :%h h))

(defun sdl3-frect (x y w h)
  (make-instance 'sdl3:frect :%x x :%y y :%w w :%h h))

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
