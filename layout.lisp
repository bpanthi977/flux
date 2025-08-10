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
  (alignment :start :type (member :start :center :end))
  (scratchpad (make-array 4 :element-type 'single-float) :type (simple-array single-float (4))))

(defun traverse-dfs (fn root)
  (funcall fn (car root) (cdr root))
  (loop for el in (cdr root) do
    (traverse-dfs fn el)))

(defun traverse-reverse-dfs (fn root)
  (loop for el in (cdr root) do
    (traverse-reverse-dfs fn el))
  (funcall fn (car root) (cdr root)))

(defun clamp-size (el size)
  (min (layout-maximum el)
       (max (layout-minimum el) (coerce (min size most-positive-single-float)
					'single-float))))

(defun remaining-size (el children)
  "Space remaining in element `el' after allocating space for
`children', padding and gap between children."
  (- (layout-size el)
     (max 0.0
	  (+ (loop for child in children
		   summing (layout-size (car child)))
	     (* 2 (layout-padding el))
	     (* (layout-child-gap el) (max 0 (1- (length children))))))))

(defun compute-fit-size (el children)
  "If element layout type is FIT
size = ∑   size(children) [If dimension is major axis]
size = max size(children) [If dimension is minor aixs]"
  (unless (eql (layout-type el) :fixed)
    (setf (layout-size el) (layout-minimum el))

    (labels ((accumulate (reduce-fn accessor)
	       (loop for child in children
		     for l = (car child)
		     with acc = 0.0d0
		     do (setf acc (funcall reduce-fn acc (funcall accessor l)))
		     finally (return acc)))

	     (has-flex-child ()
	       (loop for child in children
		     when (eql (layout-type (car child)) :flex)
		       return t))

	     (total-padding ()
	       (+ (* 2 (layout-padding el))
		  (if (layout-major-axisp el)
		      (* (max 0 (1- (length children)))
			 (layout-child-gap el))
		      0.0)))

	     (children-max (acc)
	       (clamp-size el (+ (total-padding) (accumulate acc #'layout-maximum))))

	     (children-total (acc)
	       (clamp-size el (+ (total-padding) (accumulate acc #'layout-size)))))

      (if (layout-major-axisp el)
	  ;; Major Axis
	  (case (layout-type el)
	    (:fit
	     (if (not (has-flex-child))
		 (setf (layout-size el) (children-total #'+))

		 (setf (layout-type el) :flex
		       (layout-flex-value el) 1.0
		       (layout-maximum el) (children-max #'+)
		       (layout-minimum el) (children-total #'+))))

	     (:flex (setf (layout-minimum el) (children-total #'+))))

	  ;; Minor Axis
	  (case (layout-type el)
	    (:fit
	     (if (not (has-flex-child))
		 (setf (layout-size el) (children-total #'max))

		 (setf (layout-type el) :flex
		       (layout-flex-value el) 1.0
		       (layout-maximum el) (children-max #'max)
		       (layout-minimum el) (children-total #'max))))
	    (:flex (setf (layout-minimum el) (children-total #'max))))))))


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
	     (setf (car ref-cons) nil))

	   (proportionate-size (el flex-sum remaining)
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
	  for prev-remaining = remaining
	  until (or (< (abs remaining) 0.001)
		    no-updates)
	  do
	     (setf no-updates t)
	     (loop for el in flex-els
		   for ref-cons on flex-els
		   when el
		     do
			(setf no-updates nil)
			(let* ((increment (proportionate-size el flex-sum prev-remaining))
			       (old-size (layout-size el))
			       (new-size (+ (layout-size el) increment)))
			  (cond ((> (layout-minimum el) new-size)
				 (remove-el ref-cons (layout-minimum el)))
				((< (layout-maximum el) new-size)
				 (remove-el ref-cons (layout-maximum el)))
				(t
				 (setf (layout-size el) new-size)))
			  (decf remaining (- (layout-size el) old-size)))))))))

(defun compute-flex-size (el children)
  (if (layout-major-axisp el)
      (compute-flex-size-major el children)
      (let ((available-size (- (layout-size el)
			       (* 2 (layout-padding el)))))
	(loop for node in children
	      for child = (car node)
	      when (eql (layout-type child) :flex)
		do
		   (setf (layout-size child)
			 (alexandria:clamp
			  available-size (layout-minimum child) (layout-maximum child)))))))

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

(defun save-some-state (layout children)
  (declare (ignore children))
  (setf (aref (layout-scratchpad layout) 0) (layout-minimum layout)
	(aref (layout-scratchpad layout) 1) (layout-maximum layout)
	(aref (layout-scratchpad layout) 2) (layout-flex-value layout)
	(aref (layout-scratchpad layout) 3) (ecase (layout-type layout)
					      (:flex 0.0)
					      (:fit 1.0)
					      (:fixed 2.0))))

(defun restore-state (layout children)
  (declare (ignore children))
  (setf (layout-minimum layout) (aref (layout-scratchpad layout) 0)
	(layout-maximum layout) (aref (layout-scratchpad layout) 1)
	(layout-flex-value layout) (aref (layout-scratchpad layout) 2)
	(layout-type layout) (ecase (aref (layout-scratchpad layout) 3)
			       (0.0 :flex)
			       (1.0 :fit)
			       (2.0 :fixed))))

(defun solve-layout-tree (layout-tree)
  (traverse-dfs #'save-some-state layout-tree)
  (traverse-reverse-dfs #'compute-fit-size  layout-tree)
  (traverse-dfs         #'compute-flex-size layout-tree)
  (traverse-dfs         #'compute-offset    layout-tree)
  (traverse-dfs #'restore-state layout-tree)
  layout-tree)
