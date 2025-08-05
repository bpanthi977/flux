(require 'sdl3)
(in-package #:gauthali)

(defun handle-event (event root context renderer)
  (unless (eql (call-event-handlers root event) :stop)
    (typecase event
      (sdl3:keyboard-event
       (case (sdl3:%key event)
	 (:q (cffi:with-foreign-object (ev '(:struct sdl3:quit-event))
	       (setf (cffi:foreign-slot-value ev '(:struct sdl3:quit-event) 'sdl3:%type) :quit)
	       (sdl3:push-event ev))))))
    (case (sdl3:%type event)
      (:window-display-scale-changed
       (let ((display-scale (sdl3:get-window-display-scale (sdl3:get-window-from-id (sdl3:%window event)))))
	 (context-set-property% context :render-scale display-scale)
	 (sdl3:set-render-scale renderer display-scale display-scale)
	 (sdl3-ttf:set-font-size (context-get-property% context :font)
				 (* (context-get-property% context :font-size) display-scale))
	 ;; Mark all widgets as dirty
	 (widget-rebuild-all root))))))

(defun update-ui (window renderer root-widget root-widget-symbol context)
  (declare (ignorable window))
  ;; Clear display
  (let ((bg-color (context-get-property% context :bg-color)))
    (sdl3:set-render-draw-color renderer (aref bg-color 0) (aref bg-color 1) (aref bg-color 2) (aref bg-color 3)))
  (sdl3:render-clear renderer)
  (multiple-value-bind (ret w h) (sdl3:get-window-size window)
    (assert-ret ret)
    (setf root-widget
	  (build-layout-render root-widget root-widget-symbol renderer context
			       :x 0 :y 0 :w w :h h)))
  (sdl3:render-present renderer)
  root-widget)

(defparameter *root* nil)
(defun main0 (root-widget-symbol)
  ;; INIT
  (assert-ret (sdl3:init '(:video)))
  (let ((mouse-x nil)
	(mouse-y nil))
    (multiple-value-bind (ret window renderer)
	(sdl3:create-window-and-renderer "Leap Year" 400 200 '(:input-focus :mouse-focus :resizable :high-pixel-density))
      (assert-ret ret)
      (sdl3:raise-window window)
      ;; Init font
      (sdl3-ttf:init)
      (let ((font (sdl3-ttf:open-font (namestring (get-resource-path "res/fonts/Times New Roman.ttf")) 24.0))
	    (text-engine (sdl3-ttf:create-renderer-text-engine renderer)))
	(assert-ret (not (cffi:null-pointer-p font)))
	(assert-ret (not (cffi:null-pointer-p text-engine)))

	(let ((context (make-context))
	      (root))
	  (context-set-property% context :window window)
	  (context-set-property% context :font font)
	  (context-set-property% context :font-size 24.0)
	  (context-set-property% context :render-scale 1.0)
	  (context-set-property% context :text-engine text-engine)
	  (context-set-property% context :bg-color #(255 255 255 255))
	  (context-set-property% context :fg-color #(0 0 0 255))

	  ;; EVENT LOOP
	  (unwind-protect
	       (progn
		 (setf root (update-ui window renderer root root-widget-symbol context))
		 (setf *root* root)
		 (loop for event = (sdl3:wait-event-timeout* 100) do
		   (when event
		     (if (typep event 'sdl3:quit-event)
			 (return))
		     (handle-event event root context renderer)
		     (case (sdl3:%type event)
		       (:mouse-button-down
			(if (and mouse-x mouse-y)
			    (setf mouse-x nil mouse-y nil)
			    (setf mouse-x (sdl3:%x event)
				  mouse-y (sdl3:%y event))))))
		   (setf root (update-ui window renderer root root-widget-symbol context))
		   (setf *root* root)))

	    ;; QUIT
	    (sdl3-ttf:close-font font)
	    (sdl3-ttf:destroy-renderer-text-engine text-engine)
	    (sdl3:destroy-renderer renderer)
	    (sdl3:destroy-window window)
	    ;; The window may not actually be destroyed until the event loop
	    ;; is pumped again.
	    (sdl3:pump-events)
	    ;; And since we pumped, we flush so that it doesn't affect next
	    ;; time we create window
	    (sdl3:flush-events 0 (cffi:foreign-enum-value 'sdl3:event-type :last))))))))

(defun main ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (float-features:with-float-traps-masked t
      (main0 'home-screen))))
