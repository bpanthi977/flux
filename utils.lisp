(in-package #:gauthali/utils)

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

(defparameter *callbacks* (list :count 0
				:table (make-hash-table))
  "A way to associate callback functions with integers.
Usefull in callbacks passed to C side.")

(defun register-callback (callback)
  (let ((id (incf (getf *callbacks* :count))))
    (setf (gethash id (getf *callbacks* :table)) callback)
    id))

(defun remove-callback (id)
  (remhash id (getf *callbacks* :table)))

(defun get-callback (id)
  (gethash id (getf *callbacks* :table)))

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

(defun map-tree (map-function tree children-function)
  (labels ((rec (el)
	     (cons (funcall map-function el)
		   (map 'list #'rec (funcall children-function el)))))
    (rec tree)))

(in-package #:gauthali)

(defun within-widget-bounds (widget event-x event-y)
  (multiple-value-bind (x y w h) (widget-bounds widget)
    (and (<= x event-x (+ x w))
	 (<= y event-y (+ y h)))))

(defun get-current-font ()
  "Get the current font from context."
  (get-font (property-get :font-manager) (property-get :font) (property-get :font-size)))

(defun traverse-widget-tree (widget path)
  "Path is a string of names and numbers separated by a ."
  (loop for component in (uiop:split-string path :separator ".")
	for children = (widget-children widget) do
    (cond ((every #'digit-char-p component)
	   (setf widget (aref children (parse-integer component))))
	  (t
	   (setf widget (find component children :key #'widget-name :test #'string-equal)))))
  widget)
