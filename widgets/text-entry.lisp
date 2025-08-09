(in-package #:gauthali)

#+nil(defun clamp (value min max)
  (max min (min value max)))

;; Text segment widget for rendering a portion of text
#+nil(defwidget text-segment (text start end)
  (:state font surface texture fg bg render-scale update segment-text)
  (:build
   ;; Get properties
   (setf font (property-get :font))
   (setf fg (property-get :fg-color))
   (setf bg (property-get :bg-color))
   (setf render-scale (property-get :render-scale))

   ;; Extract the text segment
   (let* ((text-len (if text (length text) 0))
	  (safe-start (clamp start 0 text-len))
	  (safe-end (clamp end safe-start text-len)))
     (setf segment-text (if (and text (> text-len 0) (< safe-start safe-end))
			    (subseq text safe-start safe-end)
			    "")))

   ;; Mark for update if text changed
   (setf update t)

   ;; No layout constraints - parent will handle sizing
   nil)

  (:render (r x y w h)
    (declare (ignorable w h))
    ;; Update texture if needed
    (when update
      (setf update nil)

      ;; Clean up old resources
      (when texture
	(sdl3:destroy-texture texture)
	(setf texture nil))
      (when surface
	(sdl3:destroy-surface surface)
	(setf surface nil))

      ;; Create new surface and texture if we have text
      (when (and segment-text (> (length segment-text) 0) font fg)
	(setf surface (sdl3-ttf:render-text-lcd font segment-text 0 (sdl3-color fg) (sdl3-color bg)))
	(when surface
	  (setf texture (sdl3:create-texture-from-surface r surface)))))

    ;; Render the texture
    (when texture
      (multiple-value-bind (ret sx sy) (sdl3:get-render-scale r)
	(assert-ret ret)
	(when (and (= sx sy) (not (= sx 1.0)))
	  (sdl3:set-render-scale r 1.0 1.0))
	(multiple-value-bind (ret w h) (sdl3:get-texture-size texture)
	  (assert-ret ret)
	  (sdl3:render-texture r texture nil (make-instance 'sdl3:frect :%x (* sx x) :%y (* sy y) :%w w :%h h)))

	(when (and (= sx sy) (not (= sx 1.0)))
	  (sdl3:set-render-scale r sx sy)))))

  (:cleanup
    (when texture
      (sdl3:destroy-texture texture))
    (when surface
      (sdl3:destroy-surface surface))))

(defclass text-texture-cache ()
  ((text :initarg :text)
   (texture :initform nil)
   (surface :initform nil)
   (font-size :initform nil)))

(defun text-cache-update-surface (cache new-text fg font)
  (with-slots (text texture surface w h font-size) cache
    (let (new-font-size)
      (when (or (not (equal text new-text))
		(not font-size)
		(not (= (setf new-font-size (sdl3-ttf:get-font-size font)) font-size))
		(null surface))
	(setf text new-text
	      font-size new-font-size)
	(when surface
	  (sdl3:destroy-surface surface)
	  (setf surface nil))
	(when texture
	  (sdl3:destroy-texture texture)
	  (setf texture nil))
	(when (not (zerop (length text)))
	  (setf surface (sdl3-ttf:render-text-blended font text 0 (sdl3-color fg)))
	  surface)))))

(defun text-cache-update-texture (r cache)
  (with-slots (surface texture) cache
    (when (and surface (not texture))
      (setf texture (sdl3:create-texture-from-surface r surface)))))

(defun text-cache-cleanup (cache)
  (with-slots (text surface texture) cache
    (when surface
      (sdl3:destroy-surface surface)
      (setf surface nil))
    (when texture
      (sdl3:destroy-texture texture)
      (setf texture nil))
    (setf text nil)))

;; Enhanced text entry widget using text-segment
;; It is single line input
(defwidget text-entry0 (initial-text on-change focus &optional (cursor-blink-rate 1.5))
  (:state (text initial-text)
	  (cursor-pos (length initial-text))
	  (texture-cache (make-instance 'text-texture-cache :text initial-text))
	  cursor-x
	  render-scale
	  fg bg char-height
	  (cursor-last-shown 0))
  (:build
   ;; Handle text input events
   (on 'sdl3:text-input-event
       (callback (event)
	 (when focus
	   (let* ((input-text (sdl3:%text event))
		  (before (subseq text 0 cursor-pos))
		  (after (subseq text cursor-pos)))
	     (setf text (concatenate 'string before input-text after))
	     (incf cursor-pos (length input-text))
	     (when on-change (funcall on-change text))
	     (widget-rebuild)))))

   ;; Handle keyboard events
   (on 'sdl3:keyboard-event
       (callback (event)
	 (when (and focus (sdl3:%down event))
	   (case (sdl3:%key event)
	     ;; Backspace - delete character before cursor
	     (:backspace
	      (when (> cursor-pos 0)
		(let* ((before (subseq text 0 (1- cursor-pos)))
		       (after (subseq text cursor-pos)))
		  (setf text (concatenate 'string before after))
		  (decf cursor-pos)
		  (when on-change (funcall on-change text))
		  (widget-rebuild))))

	     ;; Delete - delete character after cursor
	     (:delete
	      (when (< cursor-pos (length text))
		(let* ((before (subseq text 0 cursor-pos))
		       (after (subseq text (1+ cursor-pos))))
		  (setf text (concatenate 'string before after))
		  (when on-change (funcall on-change text))
		  (widget-rebuild))))

	     ;; Left arrow - move cursor left
	     (:left
	      (when (> cursor-pos 0)
		(decf cursor-pos)
		(widget-rebuild)))

	     ;; Right arrow - move cursor right
	     (:right
	      (when (< cursor-pos (length text))
		(incf cursor-pos)
		(widget-rebuild)))

	     ;; Home - move cursor to beginning
	     (:home
	      (setf cursor-pos 0)
	      (widget-rebuild))

	     ;; End - move cursor to end
	     (:end
	      (setf cursor-pos (length text))
	      (widget-rebuild))))))

   ;; Get properties
   (let ((font (property-get :font)))
     (setf render-scale (property-get :render-scale)
	   fg (property-get :fg-color)
	   bg (property-get :bg-color)
	   char-height (float (sdl3-ttf:get-font-height font)))
     ;; Get cursor-x i.e. string width upto cursor-pos
     (multiple-value-bind (ret w h) (sdl3-ttf:get-string-size font (subseq text 0 cursor-pos) 0)
       (declare (ignore h))
       (assert-ret ret)
       (setf cursor-x (float w)))

     (text-cache-update-surface texture-cache text #(0 0 0 255) font)

     (with-slots (surface) texture-cache
       (if surface
	   (layout-set :width.max (float (/ (cffi:foreign-slot-value surface '(:struct sdl3:surface) 'sdl3:%w) render-scale))
		       :height.min (float (/ (cffi:foreign-slot-value surface '(:struct sdl3:surface) 'sdl3:%h) render-scale)))))

     (layout-set :flex.x 1.0)))

  (:render (r x y w h)
    ;; Compute start-x, end-x: the clipping range of texture we can
    ;; display `cursor-x' lies in between these two, and `end-x' -
    ;; `start-x' can't exceed `available-x' and `total-x'
    (let ((start-x 0.0))
      (with-slots (surface texture) texture-cache
	(when surface
	  (let ((available-x (* w render-scale))
		(total-x (float (cffi:foreign-slot-value surface '(:struct sdl3:surface) 'sdl3:%w)))
		(total-y (float (cffi:foreign-slot-value surface '(:struct sdl3:surface) 'sdl3:%h)))
		end-x)
	    (cond ((>= available-x total-x)
		   (setf start-x 0.0
			 end-x total-x))
		  ((<= cursor-x available-x)
		   (setf end-x (min available-x total-x)
			 start-x 0.0))
		  (t
		   (setf start-x (- cursor-x available-x)
			 end-x cursor-x)))
	    (text-cache-update-texture r texture-cache)
	    ;;(print (list x y w h start-x end-x cursor-x total-x total-y available-x))
	    (when texture
	      (with-render-scale-off r (sx sy)
		(sdl3:render-texture r texture
				     (make-instance 'sdl3:frect :%x start-x :%y 0.0 :%w (- end-x start-x) :%h (min (* sy h) total-y))
				     (make-instance 'sdl3:frect :%x (* sx x) :%y (* sy y) :%w (- end-x start-x) :%h (min (* sy h) total-y))))))))
      ;; Draw cursor
      (when focus
	(let* ((now (get-internal-real-time))
	       (diff (- now cursor-last-shown))
	       (req-diff (cond ((<= cursor-blink-rate 0.0) 0.0)
			       (t (/ internal-time-units-per-second cursor-blink-rate))))
	       (show (cond ((= req-diff 0.0) t)
			   ((> diff req-diff) t)
			   ((< diff (/ req-diff 2)) nil)
			   (t t))))
	  (when show
	    (set-render-draw-color r fg)
	    (sdl3:render-line r (+ x (/ (- cursor-x start-x) render-scale)) y
			      (+ x (/ (- cursor-x start-x) render-scale)) (+ y (/ char-height render-scale)))
	    (when (> diff req-diff)
	      (setf cursor-last-shown (get-internal-real-time))))))))

  (:cleanup
    ;; Child widgets are automatically cleaned up
   (when texture-cache
     (text-cache-cleanup texture-cache))))

(defwidget text-entry (text on-change &key
		       (cursor-blink-rate 1.5)
		       (padding 2.0))
  (:state focus fg)
  (:build

   (on 'sdl3:mouse-button-event
       (callback (event)
	 (let ((prev-focus focus))
	   (when (sdl3:%down event)
	     (multiple-value-bind (x y w h) (widget-bounds)
	       (if (and (<= x (sdl3:%x event) (+ x w))
			(<= y (sdl3:%y event) (+ y h)))
		   (setf focus t)
		   (setf focus nil))))
	   (unless (eql focus prev-focus)
	     (widget-rebuild)))))
   (setf fg (property-get :fg-color))
   (layout-set :alignment.y :center
	       :padding padding)
   (text-entry0 text on-change focus cursor-blink-rate))
  (:render (r x y w h)
    (set-render-draw-color r fg)
    (sdl3:render-rect r (make-instance 'sdl3:frect :%h h :%w w :%y y :%x x))))
