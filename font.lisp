(in-package #:gauthali)

(defstruct font-info
  (ptr)
  (size 0 :type float)
  (name "" :type string))

(defclass font-manager ()
  ((open-font-func :initarg :open-font-func)
   (close-font-func :initarg :close-font-func)
   (fonts :initform (make-array 0 :element-type 'font-info :fill-pointer 0 :adjustable t))
   (text-engine :initarg :text-engine)))

(defun find-font (fonts name size)
  (declare (type (vector font-info) fonts))
  (loop for font across fonts
	when (and (string-equal name (font-info-name font))
		  (= size (font-info-size font)))
	  return font))

(defmethod get-font ((fm font-manager) (name string) (size float))
  (with-slots (open-font-func fonts) fm
    (let ((font (find-font fonts name size)))
      (unless font
	;; Load font
	(let ((font-ptr (funcall open-font-func name size)))
	  (assert font-ptr)
	  (setf font (make-font-info :ptr font-ptr
				     :name name
				     :size size))
	  (vector-push-extend font fonts)))
      (font-info-ptr font))))

(defmethod get-text-engine ((fm font-manager))
  (slot-value fm 'text-engine))

(defmethod destory-font-manager ((fm font-manager))
  (with-slots (fonts close-font-func) fm
    (loop for font across fonts do
	  (funcall close-font-func (font-info-ptr font)))
    (setf (fill-pointer fonts) 0)
    t))
