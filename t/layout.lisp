(in-package #:flux/tests)

(def-suite layout
  :description "Tests the layout engine: layout.lisp")

(in-suite layout)

(def-test flex-max ()
  (with-layout-tree tree
      ((root :type :fixed :size 100.0)
       ((child :type :flex :flex-value 1.0 :maximum 40.0)))
    (solve-layout-tree tree)
    (is (= (layout-size child) 40.0) "flex el size exceeds maximum allowed. root ~a, child ~a" (layout-size root) (layout-size child))))

(def-test flex-max2 ()
  (with-layout-tree tree
    ((l0 :type :fixed :size 100.0 :flex-value 1.0 :alignment :center)
     ((l1 :type :fit)
      ((l2 :type :flex :maximum 40.0 :flex-value 1.0))))
    (solve-layout-tree tree)
    (is (= (layout-size l2) 40.0)
	"flex el size exceeds maximum allowed. l1 ~a, l2 ~a"
	(layout-size l1) (layout-size l2))
    (is (= (layout-size l1) 40.0)
	"fit el size exceeds total child size. l1 ~a, l2 ~a"
	(layout-size l1) (layout-size l2))))

(def-test flex-max3 ()
  "Fit containing flex in minor direction"
  (with-layout-tree tree
    ((column :type :fixed :size 100.0 :major-axisp nil :flex-value 1.0)
     ((button :type :fit)
      ((text :type :flex :maximum 50.0 :flex-value 1.0))))
    (solve-layout-tree tree)
    (is (= (layout-size button) 50.0)
	"fit size ~a exceeds the max size required by flex child ~a."
	(layout-size button) (layout-maximum text))))

(def-test speed ()
  "Test performance of layout engine."
  (labels ((el (name layouts &rest children)
	     (cons (cons name layouts)
		   children))
	   (create-button ()
	     (el "button" '(:padding 2.0 :alignment :center)
		 (el "pressable" nil
		     (el "text" '(:type :flex :maximum 119.0 :flex-value 1.0)))))
	   (create-row (n-columns)
	     (apply #'el "row" nil
		    (loop repeat n-columns
			  collect (create-button))))
	   (create-table (n-rows n-columns)
	     (apply #'el "column" '(:major-axisp nil)
		    (loop repeat n-rows
			  collect (create-row n-columns)))))
    (let* ((tree (el "main" '(:type :fixed :size 1512.0)
			(create-table 40 20)))
	   (layout-tree (map-tree (lambda (el)
				    (apply #'make-layout (rest (first el))))
				  tree
				  #'cdr))
	   (start (get-internal-real-time))
	   (n 10))
      (loop repeat n do (solve-layout-tree layout-tree))
      (let ((diff (float (/ (- (get-internal-real-time) start) internal-time-units-per-second n))))
	(is (< diff 0.0001)
	    "Time to solve layout exceeded 0.001 seconds. Took ~,4f seconds." diff)))))
