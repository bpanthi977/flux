(in-package #:gauthali)

(defun clamp (value min max)
  (max min (min value max)))

(defun render-fill-circle (renderer cx cy radius)
  "Renders a filled circle using horizontal lines."
  (loop for dy from (- (floor radius)) to (floor radius)
        for dx = (sqrt (max 0 (- (* radius radius) (* dy dy))))
        do (sdl3:render-line renderer (- cx dx) (+ cy dy) (+ cx dx) (+ cy dy))))

(defwidget slider (on-change &key (min 0.0) (max 1.0) (initial-value min))
  (:state (value initial-value)
          (dragging nil)
          (knob-radius 10.0))
  (:build
   (flet ((update-value-from-event (event)
            (multiple-value-bind (x y w h) (widget-bounds this)
              (declare (ignore y h))
              (let* ((track-width (- w (* 2 knob-radius)))
                     (mouse-x-on-track (clamp (sdl3:%x event)
                                              (+ x knob-radius)
                                              (+ x w (- knob-radius))))
                     (relative-pos (- mouse-x-on-track x knob-radius))
                     (value-ratio (if (> track-width 0) (/ relative-pos track-width) 0.0))
                     (new-value (+ min (* value-ratio (- max min)))))
                (unless (= value new-value)
                  (setf value new-value)
                  (when on-change (funcall on-change value))
                  (widget-rebuild this))))))

     (layout-set this 
		 :flex.x 1.0
		 :flex.y 1.0
		 :width.min (* 3 knob-radius)
                 :height.min (* 2 knob-radius))
     (on sdl3:mouse-button-event
         (callback (event)
           (multiple-value-bind (x y w h) (widget-bounds this)
             (if (sdl3:%down event)
                 (when (and (<= x (sdl3:%x event) (+ x w))
                            (<= y (sdl3:%y event) (+ y h)))
                   (setf dragging t)
                   (update-value-from-event event))
                 (when dragging
                   (setf dragging nil)
                   (widget-rebuild this))))))
     (on sdl3:mouse-motion-event
         (callback (event)
           (when dragging
             (update-value-from-event event))))))
  (:render (r x y w h)
;;	   (print (list x y w h))
    (let* ((bar-height 4.0)
           (bar-y (+ y (/ (- h bar-height) 2)))
           (track-width (- w (* 2 knob-radius)))
           (value-ratio (if (> (- max min) 0) (/ (- value min) (- max min)) 0.0))
           (knob-center-x (+ x knob-radius (if (> track-width 0) (* value-ratio track-width) 0.0)))
           (knob-center-y (+ y (/ h 2))))

      ;; Draw bar
      (set-render-draw-color r #(125 125 125 255))
      (when (> track-width 0)
        (sdl3:render-fill-rect r (sdl3-frect (+ x knob-radius) bar-y track-width bar-height)))

      ;; Draw knob
      (set-render-draw-color r #(200 200 200 255))
      (render-fill-circle r knob-center-x knob-center-y knob-radius))))
