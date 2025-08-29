(in-package #:flux/tests)

(defmacro create-tree (tree)
  (labels ((rec (tree)
	     (if (listp tree)
		 `(list ,(first tree)
			,@(mapcar #'rec (rest tree)))
		 `(list ,tree))))
    (rec tree)))

(defun layout-without-defaults (obj)
  (let* ((class (find-class 'g::layout)))
    (loop for slot in (closer-mop:class-slots class)
	  for slot-name = (closer-mop:slot-definition-name slot)
	  for val = (slot-value obj slot-name)
	  for default = (funcall (closer-mop:slot-definition-initfunction slot))
	  for collectp = (and (not (eql val default))
			      (not (eql slot-name 'scratchpad)))
	  when collectp
	    collect (intern (symbol-name slot-name) (find-package :keyword))
	  when collectp
	    collect val)))

(defun pp-layout-tree (tree)
  (labels ((create-tree (tree)
	     (cons (layout-without-defaults (first tree))
		   (loop for child in (rest tree)
			 collect (create-tree child)))))
    (create-tree tree)))

(defun get-layout-constructor (w axis)
  (flet ((map-fn (accessor)
	   (lambda (widget)
	     (cons (widget-name widget) (layout-without-defaults (funcall accessor widget))))))
    (ecase axis
      (:x (map-tree (map-fn #'widget-layout-x) w #'widget-children))
      (:y (map-tree (map-fn #'widget-layout-y) w #'widget-children)))))


(defmacro with-layout-tree (tree-var tree &body body)
  (let ((bindings nil))
    (labels ((rec (tree)
	       (let* ((node (first tree))
		      (var (first node))
		      (constructor `(make-layout ,@(rest node))))
		 (push (list var constructor) bindings)
		 `(list ,var
			,@(loop for child in (rest tree)
				collect (rec child))))))
      (let ((tree-form (rec tree)))
	`(let* (,@(reverse bindings)
		(,tree-var ,tree-form))
	   ,@body)))))
