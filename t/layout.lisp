(in-package #:gauthali/tests)

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
