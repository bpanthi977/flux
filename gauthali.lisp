;;;; gauthali.lisp

(in-package #:gauthali)

(defparameter *app* nil)

(defclass state ()
  ((clipboard :initform nil :accessor clipboard)))

(defclass app ()
  ((window :initarg :window :accessor window)
   (renderer :initarg :renderer :accessor renderer)
   (frame-rate :initform 1/60 :accessor frame-rate)
   (font :initarg :font :accessor font)
   (text-engine :initarg :text-engine :accessor text-engine)
   (last-frame :initform (get-internal-real-time) :accessor last-frame)
   (state :initform (make-instance 'state) :accessor state)
   (drawcache :initform (make-drawcache) :accessor drawcache)))

(defun get-font (app &optional font-size)
  (when font-size
    (sdl3-ttf:set-font-size (font app) font-size))
  (font app))

(defun get-render-drivers ()
  (loop for i from 0 below (sdl3:get-num-render-drivers)
	collect (sdl3:get-render-driver i)))

(defun create-texture (app w h)
  (declare (type integer w h))
  (let ((texture (sdl3:create-texture (renderer app)
				      (sdl3:get-window-pixel-format (window app))
				      :target
				      w h)))
    (assert (not (cffi:null-pointer-p texture)))
    texture))

(defun init ()
  (assert-ret (sdl3:set-app-metadata "Gauthali" "1.0" "com.bpanthi.gauthali"))
  (assert-ret (sdl3:init :video))

  (multiple-value-bind (ret window renderer)
      (sdl3:create-window-and-renderer
       "Gauthali"
       400 400
       '(:always-on-top :resizable))
    (assert-ret ret)
    (assert-ret (sdl3-ttf:init))
    (let ((font (sdl3-ttf:open-font (namestring (get-resource-path "res/fonts/Times New Roman.ttf")) 18.0))
	  (text-engine (sdl3-ttf:create-renderer-text-engine renderer)))
      (assert-ret (not (cffi:null-pointer-p font)))
      (assert-ret (not (cffi:null-pointer-p text-engine)))
      (sdl3:add-event-watch (cffi:get-callback 'event-watcher) (cffi:null-pointer))
      (make-instance 'app
		     :window window
		     :renderer renderer
		     :font font
		     :text-engine text-engine))))

(defun render (app)
  (let ((r (renderer app))
	(text-engine (text-engine app)))
    (multiple-value-bind (ret w h) (sdl3:get-window-size (window app))
      (assert-ret ret)
      (sdl3:set-render-draw-color r 255 255 255 255)
      (sdl3:render-clear r)
      (let ((fps (/ internal-time-units-per-second
		    (- (get-internal-real-time) (last-frame app)))))
	(setf (last-frame app) (get-internal-real-time))
	(draw-element-tree
	 (create-text-element text-engine
			      (format nil "FPS: ~,3f [~,3f]" fps (/ (frame-rate app)))
			      :font (get-font app 18.0)
			      :color (color 0 0 0 255))))

      (let ((purple (color 100 0 100 255))
	    (red (color 255 0 0 255))
	    (green (color 0 255 0 255))
	    (white (color 0 0 0 255)))
	(with-drawcache ((drawcache app))
	  (draw-element-tree
	   (solve-elements-layout
	    (<> (draw-nothing r :width w :height h) ;; ROOT ELEMENT
		(:major-axis :col :col (:child-gap 10.0))
	      (register-element
	       (cache-element r t nil
			      (lambda ()
				(let ((*current-element* nil))
				  (<> (draw-nothing r :width w :height 50.0)
				      nil)))))
	      (<> (draw-rectangle-frame r :width :fit :height 10.0)
		  nil
		(<> (draw-rectangle-frame r :width :flex :height 10.0 :color green)
		    nil
		  (<> (draw-rectangle-frame r :width 10.0 :height 10.0 :color red))))
	      (<!> (t)
		  (draw-rectangle-frame r :color green)
		  (:row (:child-gap 5.0) :col (:padding 3.0))

		(<> (draw-rectangle r :width 100.0 :height 50.0 :color purple))
		(<> (draw-rectangle r :width 100.0 :height 60.0 :color red)))

	      (<!> (w)
		  (draw-rectangle-frame r :width (:flex 1.0) :color red)
		  (:col (:align :center))
		(<> (draw-rectangle r :width (:flex 1.0) :height 100.0 :color green))
		(<> (draw-rectangle r :width (:flex 3.0) :height 60.0 :color purple)))
	      (register-element
	       (cache-element
		r
		(list w h)
		nil
		(lambda ()
		  (create-text-element text-engine "Hello!"
				       :font (get-font app 40.0)
				       :color white
				       :width w))))
	      (register-element
	       (cache-element
		r
		(list w h)
		nil
		(lambda ()
		  (create-text-element text-engine "Bibek dai ko UI library maa text wrapping pani garinxa."
				       :font (get-font app 40.0)
				       :color white
				       :width w)))))))))

      (sdl3:render-present r))))

(defun handle-event (app event)
  (declare (ignorable app event))
  (let ((s (state app)))
    (typecase event
      (sdl3:quit-event
       nil)
      (sdl3:keyboard-event
       (case (slot-value event 'sdl3:%scancode)
	 (:q nil)
	 (:v (when (and (slot-value event 'sdl3:%down)
			(member (slot-value event 'sdl3:%mod) '(:lctrl :rctrl)))
	       "Ctrl + V"
	       (setf (clipboard s) (sdl3:get-clipboard-text))))
	 (t t)))
      (t t))))

(defun handle-quit (app)
  (destroy-drawcache (drawcache app))
  (sdl3:destroy-renderer (renderer app))
  (sdl3:destroy-window (window app))
  (sdl3-ttf:close-font (font app))
  (sdl3-ttf:quit)
  (sdl3:pump-events)
  (sdl3:quit-sub-system :video)
  (sdl3:quit))

(cffi:defcallback event-watcher :bool ((userdata :pointer) (*event (:pointer (:union sdl3:event))))
  (declare (ignore userdata))
  ;; To rerender on resize
  (let* ((event (sdl3:event-unmarshal *event))
	 (type (slot-value event 'sdl3:%type)))
    (when (or (eql type :window-resized)
	      (eql type :window-exposed))
      (render *app*)))
    t)

(defun run-app ()
  (let ((app (init))
	(last-frame-update 0))
    (setf *app* app)
    (unwind-protect
	 (loop named outer do
	   (loop named event-loop
		 for event = (sdl3:poll-event*)
		 while event
		 do
		    (print (slot-value event 'sdl3:%type))
		    (unless (handle-event app event)
		      (return-from outer)))
	   (let ((render-time-diff (/ (- (get-internal-real-time) last-frame-update)
				      internal-time-units-per-second)))
	     (cond ((> render-time-diff (frame-rate app))
		    (setf last-frame-update (get-internal-real-time))
		    (render app))
		   (t (sleep (/ (- (frame-rate app) render-time-diff) 2))))))
      (handle-quit app))))

(defun start-app ()
  #+darwin
  (progn
    (trivial-main-thread:with-body-in-main-thread (:blocking t)
      (float-features:with-float-traps-masked t
	(run-app))))
  #-darwin
  (run-app))
(export 'start-app)
