(in-package #:gauthali)

(defun seconds-elapsed (since)
  (* 1.0 (- (get-internal-real-time) since) (/ internal-time-units-per-second)))

(defwidget countdown ()
  (:state (mount-time (get-internal-real-time))
	  refresh-time)
  (:build
   (text (format nil "~,3f" (seconds-elapsed mount-time))))
  (:render (r x y w h)
   (declare (ignore r x y w h))
   (widget-rebuild)))

(defwidget home-screen ()
  (:state (text "hello world"))
  (:build
   (layout-set :flex.x 1.0
	       :flex.y 1.0
	       :alignment.x :center
	       :alignment.y :center)
   (list (row (:flex.x 1.0)
	   (layout (:width.min 150.0
		    :width.max 200.0
		    :height.min 40.0)
	     (text-entry text (lambda (txt)
				(setf text txt)
				(widget-rebuild)))))
	 (column (:flex.x 1.0)
	   (layout (:width.min 150.0
		    :padding.x 5.0
		    :padding.y 20.0)
	     (button "Start!!" (lambda () (print "Start clicked!"))))
	   (layout (:width.min 150.0
		    :padding.x 5.0
		    :padding.y 20.0)
	     (button "Stop!!" (lambda () (print "Stop clicked!")))))
	 (row (:alignment.x :end
		:width.min 100.0
		:flex.x 1.0)
	      (countdown)))))
