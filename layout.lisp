(in-package :gauthali)

(defstruct layout
  (type :fit :type (member :fit :flex :fixed))
  (size 0.0 :type single-float)
  (minimum 0.0 :type single-float)
  (maximum most-positive-single-float :type single-float)
  (major-axisp t :type boolean)
  (flex-value 0.0 :type single-float)
  (offset 0.0 :type single-float)
  (padding 0.0 :type single-float)
  (child-gap 0.0 :type single-float)
  (alignment :start :type (member :start :center :end)))

(defun traverse-bfs (fn root)
  (funcall fn (car root) (cdr root))
  (loop for el in (cdr root) do
	(traverse-bfs fn el)))

(defun traverse-reverse-bfs (fn root)
  (loop for el in (cdr root) do
	(traverse-reverse-bfs fn el))
  (funcall fn (car root) (cdr root)))

(defun clamp-size (el size)
  (min (layout-maximum el)
       (max (layout-minimum el) size)))

(defun remaining-size (el children)
  "Space remaining in element `el' after allocating space for
`children', padding and gap between children."
  (- (layout-size el)
     (+ (loop for child in children
	      summing (layout-size (car child)))
	(* 2 (layout-padding el))
	(if children
	    (* (layout-child-gap el) (1- (length children)))
	    0))))

(defun compute-fit-size (el children)
  "If element layout type is FIT
size = ∑   size(children) [If dimension is major axis]
size = max size(children) [If dimension is minor aixs]"
  (unless (eql (layout-type el) :fixed)
    (setf (layout-size el) 0.0)
    (flet ((compute (reduce-fn)
	     (let* ((children-size
		      (loop for child in children
			    for l = (car child)
			    with acc = 0.0
			    when (not (eql (layout-alignment l) :relative))
			      do (setf acc (funcall reduce-fn acc (layout-size l)))
			    finally (return acc)))
		    (fit-size (clamp-size el (+ children-size
						(* 2 (layout-padding el))
						(or
						 (and (layout-major-axisp el)
						      (* (1- (length children))
							 (layout-child-gap el)))
						 0.0)))))
	       (case (layout-type el)
		 (:fit (setf (layout-size el) fit-size))
		 (:flex (setf (layout-minimum el) fit-size))))))
      (if (layout-major-axisp el)
	  (compute #'+)
	  (compute #'max)))))

(defun compute-flex-size-major (el children)
  "If element children have FLEX layout, then FLEX them.
1. Compute spare size in el [= remaining]
2. Spread the remaining size in proportional to flex-value of flex elements
3. If the size for flex child goes out of bounds (min, max) then clamp it, and remove that
   child from flex calculation.
4. Iterate until all remaining size is distributed."
  (let* ((flex-els (loop for child in children
			 when (and (eql (layout-type (car child)) :flex)
				   (not (= (layout-flex-value (car child)) 0.0)))
			   collect (car child)))
	 (remaining (remaining-size el children)))
    (flet ((flex-sum ()
	     "Sum of all flex values in flex elements."
	     (loop for el in flex-els
		   when el
		     summing (layout-flex-value el)))
	   (remove-el (ref-cons value)
	     "Set the size to `value' and remove from flex elements."
	     (setf (layout-size (car ref-cons)) value)
	     (decf remaining value)
	     (setf (car ref-cons) nil))

	   (proportionate-size (el flex-sum)
	     "New size increment that can be assigned to an `el' by using remaining size."
	     (* (/ (layout-flex-value el)
		   flex-sum)
		remaining)))

      (when flex-els
	;; Unless remaning size is below threshold
	;; distribute the remaining size to flex elements
	(loop
	  with no-updates = nil
	  for flex-sum = (flex-sum)
	  until (or (< (abs remaining) 0.001)
		    no-updates)
	  do
	     (setf no-updates t)
	     (loop for el in flex-els
		   for ref-cons on flex-els
		   when el
		     do
			(setf no-updates nil)
			(let* ((increment (proportionate-size el flex-sum))
			       (new-size (+ (layout-size el) increment)))
			  (cond ((> (layout-minimum el) new-size)
				 (remove-el ref-cons (layout-minimum el)))
				((< (layout-maximum el) new-size)
				 (remove-el ref-cons (layout-maximum el)))
				(t
				 (setf (layout-size el) new-size)
				 (decf remaining increment))))))))))

(defun compute-flex-size (el children)
  (if (layout-major-axisp el)
      (compute-flex-size-major el children)
      (let ((size (- (layout-size el)
		     (* 2 (layout-padding el)))))
	(loop for child in children
	      when (eql (layout-type (car child)) :flex)
		do
		   (setf (layout-size (car child)) size)))))

(defun compute-offset (el children)
  "Compute offset of children using parent element's offset."
  (if (layout-major-axisp el)
      (loop for child in children
	    with offset = (+ (layout-offset el)
			     (layout-padding el)
			     (case (layout-alignment el)
			       (:start 0.0)
			       (:end   (remaining-size el children))
			       (:center (/ (remaining-size el children) 2))))
	    do
	       (setf (layout-offset (car child)) offset)
	       (incf offset (+ (layout-size (car child))
			       (layout-child-gap el))))
      (loop for child in children
	    for child-size = (layout-size (car child))
	    do
	    (setf (layout-offset (car child))
		  (+ (layout-offset el)
		     (ecase (layout-alignment el)
		       (:start (layout-padding el))
		       (:end   (- (layout-size el) (layout-padding el) child-size))
		       (:center (- (/ (layout-size el) 2)
				   (/ child-size 2)))))))))

(defun solve-layout-tree (layout-tree)
  (traverse-reverse-bfs #'compute-fit-size  layout-tree)
  (traverse-bfs         #'compute-flex-size layout-tree)
  (traverse-bfs         #'compute-offset    layout-tree)
  layout-tree)
