(in-package #:gauthali)

(defwidget button (name on-click)
    (:state (width texture (_name ""))
     :render (render-text name))
  (declare (ignorable name on-click))
  (unless (string-equal _name name)
    (multiple-value-bind (ret w h) (sdl3-ttf:measure-string (property-get :font) name 0 0)
      (declare (ignore h))
      (assert-ret ret)
      (setf _name name
	    width w
	    texture t)))
    nil)


(defwidget home-screen ()
    ()
  (list (button "Start" (lambda () (print 'started)))
	(button "Stop" (lambda () (print 'stopped)))))
