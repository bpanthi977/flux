(in-package #:gauthali)


(defwidget text (text)
  (:state (update t) ttf-text
	  (text-engine (get-text-engine (property-get :font-manager)))
	  font fg bg render-scale max-width min-height width height)
  (:memo-if (when (and (eql font (get-current-font))
		       (string= text (prev text))
		       (= render-scale (property-get :render-scale)))
	      (setf fg (property-get :fg-color))
	      (setf bg (property-get :bg-color))
	      t))
  (:build
   (setf font (get-current-font)
	 bg (property-get :bg-color)
	 fg (property-get :fg-color)
	 render-scale (property-get :render-scale)
	 update t)

   ;; Update text string
   (cond (ttf-text
	  (sdl3-ttf:set-text-string ttf-text text 0))
	 (t
	  (setf ttf-text (sdl3-ttf:create-text text-engine font text 0))
	  (assert-ret ttf-text)))

   ;; Compute layout ranges
   (cond ((= 0 (length text))
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
		  height min-height))))

   (layout-set this
	       :flex.x 1.0
	       :width.min 0.0
	       :width.max max-width
	       :height.min min-height)
   ;; No children
   (values))

  (:on-layout-x (x w)
		(declare (ignore x))
		(when (or update
			  (not (= width w)))
		  (setf update nil
			width w)

		  (sdl3-ttf:set-text-wrap-width ttf-text (floor (* width render-scale)))
		  (multiple-value-bind (ret w h) (sdl3-ttf:get-text-size ttf-text)
		    (declare (ignore w))
		    (assert-ret ret)
		    (setf height (/ (coerce h 'single-float) render-scale))))
		(layout-set this :height.min height))

  (:render (r x y w h)
	   ;; Draw background
	   (set-render-draw-color r bg)
	   (sdl3:render-fill-rect r (sdl3-frect x y w h))
	   ;; Draw text
	   (with-render-scale-off r (sx sy)
	     (sdl3-ttf:set-text-color ttf-text (aref fg 0) (aref fg 1) (aref fg 2) (aref fg 3))
	     (sdl3-ttf:draw-renderer-text ttf-text (* sx x) (* sy y))))

  (:cleanup
   (when ttf-text
     (sdl3-ttf:destroy-text ttf-text))))
