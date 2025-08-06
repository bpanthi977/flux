(in-package #:gauthali)

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
     (cond ((= 0 (length _text))
	    (setf max-width 0.0
		  width 0.0
		  min-height 0.0
		  height 0.0))
	   (t
	    (multiple-value-bind (ret w h) (sdl3-ttf:get-string-size font text 0)
	      (assert-ret ret)
	      (setf max-width (/ (coerce w 'single-float) render-scale)
		    width max-width)
	      (setf min-height (/ (coerce h 'single-float) render-scale)
		    height min-height)))))

   (layout-set :flex.x 1.0
	       :width.max max-width
	       :height.min min-height)
   nil)

  (:on-layout-x (x w)
		(declare (ignore x))
		(when (and (or update
			       (not (eql width w)))
			   (not (= 0 (length text) 0)))
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
	     (when surface
	       (setf texture (sdl3:create-texture-from-surface r surface))))
	   (when texture
	     (multiple-value-bind (ret sx sy) (sdl3:get-render-scale r)
	       (assert-ret ret)
	       (when (and (= sx sy) (not (= sx 1.0)))
		 (sdl3:set-render-scale r 1.0 1.0))
	       (sdl3:render-texture r texture nil (make-instance 'sdl3:frect :%x (* sx x) :%y (* sy y) :%w (* w sx) :%h (* h sy)))
	       (when (and (= sx sy) (not (= sx 1.0)))
		 (sdl3:set-render-scale r sx sy)))))
  (:cleanup
   (when texture
     (sdl3:destroy-texture texture))
   (when surface
     (sdl3:destroy-surface surface))))

#+nil(defwidget text-entry (initial-text on-change)
  (:state (text initial-text) (focus t) border-color (cursor (length initial-text)))
  (:build
   (on 'sdl3:text-input-event
       (callback (event)
	 (when focus
	   (setf text (concatenate 'string text (sdl3:%text event)))
	   (funcall on-change text)
	   (widget-rebuild))))

   (on 'sdl3:keyboard-event
       (callback (event)
	 (when (and focus
		    (eql (sdl3:%key event) :backspace)
		    (> (length text) 0))
	   (setf text (subseq text 0 (max 0 (1- (length text)))))
	   (funcall on-change text)
	   (widget-rebuild))))

   (layout-set :flex.x :least :flex.y :least
	       :alignment.y :center :alignment.x :start
	       :padding.x 1.0
	       :padding.y 1.0)
   (setf border-color (or (property-get :border-color)
			  (property-get :border-color)
			  #(0 0 0 255)))
   (text text))
  (:render (r x y w h)
	   (set-render-draw-color r border-color)
	   (sdl3:render-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x))
	   (when focus
	     (sdl3:render-line r (+ x w )))))
