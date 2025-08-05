(in-package #:gauthali)

(defun sdl3-color (color)
  (make-instance 'sdl3:color
		 :%r (aref color 0)
		 :%g (aref color 1)
		 :%b (aref color 2)
		 :%a (aref color 3)))

(defwidget text (text)
  (:state (update t) _text font surface texture fg bg render-scale max-width min-height width height)
  (:build
   ;; Compute layout ranges
   ;; no further children widgets
   (setf font (property-get :font))

   ;; Compute properties if cache is invalid
   (when (or (not (string-equal _text text))
	     (not (equal fg (property-get :fg-color)))
	     (not (equal bg (property-get :bg-color)))
	     (not (= render-scale (property-get :render-scale))))
     (setf update t
	   _text text
	   bg (property-get :bg-color)
	   fg (property-get :fg-color)
	   render-scale (property-get :render-scale))

     (multiple-value-bind (ret w h) (sdl3-ttf:get-string-size font text 0)
       (assert-ret ret)
       (setf max-width (/ (coerce w 'single-float) render-scale)
	     width max-width)
       (setf min-height (/ (coerce h 'single-float) render-scale)
	     height min-height)))

   (layout-set :flex.x 1.0
	       :width.max max-width
	       :height.min min-height)
   nil)

  (:on-layout-x (x w)
		(declare (ignore x))
		(when (or update
			  (not (eql width w)))
		  (setf width w)
		  (multiple-value-bind (ret w h) (sdl3-ttf:get-string-size-wrapped font text 0 (floor (* render-scale width)))
		    (declare (ignore w))
		    (assert-ret ret)
		    (setf height (/ (coerce h 'single-float) render-scale))
		    (when surface
		      (sdl3:destroy-surface surface)
		      (setf surface nil))
		    (setf surface (sdl3-ttf:render-text-lcd-wrapped font _text 0 (sdl3-color fg) (sdl3-color bg) (floor (* render-scale width))))))

		(layout-set :height.min height))
  (:render (r x y w h)
	   (when update
	     (setf update nil)
	     (when texture (sdl3:destroy-texture texture))
	     (setf texture (sdl3:create-texture-from-surface r surface)))
	   (multiple-value-bind (ret sx sy) (sdl3:get-render-scale r)
	     (assert-ret ret)
	     (when (and (= sx sy) (not (= sx 1.0)))
		 (sdl3:set-render-scale r 1.0 1.0))
	     (sdl3:render-texture r texture nil (make-instance 'sdl3:frect :%x (* sx x) :%y (* sy y) :%w (* w sx) :%h (* h sy)))
	     (when (and (= sx sy) (not (= sx 1.0)))
	       (sdl3:set-render-scale r sx sy))))
  (:cleanup
   (when texture
     (sdl3:destroy-texture texture))
   (when surface
     (sdl3:destroy-surface surface))))

(defwidget button (name on-press)
  (:state pressed)
  (:build
   (on 'sdl3:mouse-button-event
       (callback (event)
	 (let ((old pressed))
	   (if (sdl3:%down event)
	       (multiple-value-bind (x y w h) (widget-bounds)
		 (if (and (<= x (sdl3:%x event) (+ x w))
			  (<= y (sdl3:%y event) (+ y h)))
		     (setf pressed t)
		     (setf pressed nil)))
	       (setf pressed nil))
	   (unless (eql old pressed)
	     (when pressed
	       (funcall on-press))
	     (widget-rebuild)))))

   (layout-set :flex.x :least
	       :alignment.x :center
	       :alignment.y :center)
   (text name))
  (:render (r x y w h)
	   (sdl3:set-render-draw-color r 125 125 125 125)
	   (sdl3:render-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x))))

(defwidget spacer ()
  (:build
   (layout-set :flex.x 1.0
	       :flex.y 1.0)))

(defun seconds-elapsed (since)
  (* 1.0 (- (get-internal-real-time) since) (/ internal-time-units-per-second)))

(defwidget countdown ()
  (:state (mount-time (get-internal-real-time))
	  refresh-time)
  (:build
   (layout-set :flex.x :least)
   (text (format nil "~,3f" (seconds-elapsed mount-time))))
  (:render (r x y w h)
	   (declare (ignore r x y w h))
	   (widget-rebuild )))

(defwidget row (layout-args &rest widgets)
  (:build
   (apply #'layout-set layout-args)
   widgets))

(defwidget column (layout-args &rest widgets)
  (:build
   (apply #'layout-set layout-args)
   (layout-set :major-axis :y)
   widgets))

(defwidget home-screen ()
  (:build
   (layout-set :flex.x 1.0
	       :flex.y 1.0
	       :alignment.x :center
	       :alignment.y :center)
   (list (row '(:flex.x 1.0))
	 (column '(:flex.x 1.0)
		 (layout (:width.min 150.0
			  :padding.x 5.0
			  :padding.y 20.0)
		   (button "Start!!" (lambda () (print "Start clicked!"))))
		 (layout (:width.min 150.0
			  :padding.x 5.0
			  :padding.y 20.0)
		   (button "Stop!!" (lambda () (print "Stop clicked!")))))
	 (row '(:alignment.x :end
		:width.min 100.0
		:flex.x 1.0)
	      (countdown)))))
