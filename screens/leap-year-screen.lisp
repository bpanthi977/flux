(in-package #:gauthali)

(defun leap-year-p (year)
  (and (= (mod year 4) 0)
       (or (not (= (mod year 100) 0))
	   (= (mod year 400) 0))))

(defwidget leap-year-result (input-text)
  (:build
   (multiple-value-bind (year parsed-length) (parse-integer input-text :junk-allowed t)
     (let* ((valid-number (and (> (length input-text) 0)
			       year
			       (> year 0)
			       (= parsed-length (length input-text))))
	    (leap (and valid-number (leap-year-p year))))
       (cond ((not valid-number)
	      (property-set :fg-color #(255 0 0 0))
	      (text "Input is not a valid number."))
	     ((and valid-number leap)
	      (text "Leap year."))
	     (t (text "Not a leap year.")))))))

(defwidget leap-year-screen ()
  (:state (input "2024"))
  (:build
   (layout-set this :alignment.y :center)
   (column (:alignment.x :center)
     (row (:child-gap.x 10.0)
       (row (:flex.x 1.0
	     :alignment.x :end)
	 (text "Enter year:"))
       (row (:flex.x 1.0
	     :alignment.x :start)
	 (layout (:width 75.0)
	   (text-entry input (lambda (value)
			       (setf input value)
			       (widget-rebuild this))))))
     (leap-year-result input))))
