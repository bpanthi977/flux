(require 'sdl3)
(in-package #:gauthali)

(defun handle-event (event)
  (format t "Event: ~a~%" (sdl3:%type event))
  (typecase event
    (sdl3:keyboard-event
     (case (sdl3:%key event)
       (:q (cffi:with-foreign-object (ev '(:struct sdl3:quit-event))
	     (setf (cffi:foreign-slot-value ev '(:struct sdl3:quit-event) 'sdl3:%type) :quit)
	     (sdl3:push-event ev)))))))


(defun update-ui (window renderer root-widget context)
  (declare (ignorable window))
  ;; Clear display
  (sdl3:set-render-draw-color renderer 0 0 0 0)
  (sdl3:render-clear renderer)
  ;; Update widgets
  (update-widget-tree root-widget context)
  (update-widget-layouts root-widget)
  ;; Render
  (call-render-funcs root-widget renderer)
  (sdl3:render-present renderer))

(defparameter *root* nil)

(defwidget root-widget (width height child-widget-func)
  (:build
   (layout-set :width (coerce width 'float)
	       :height (coerce height 'float))
   (funcall child-widget-func)))

(defun main0 (root-widget-func)
  ;; INIT
  (assert-ret (sdl3:init '(:video)))
  (let ((width 400)
	(height 200))
    (multiple-value-bind (ret window renderer)
	(sdl3:create-window-and-renderer "Leap Year" 400 200 '(:input-focus :mouse-focus :resizable))
      (assert-ret ret)
      (sdl3:raise-window window)
      ;; Init font
      (sdl3-ttf:init)
      (let ((font (sdl3-ttf:open-font (namestring (get-resource-path "res/fonts/Times New Roman.ttf")) 24.0))
	    (text-engine (sdl3-ttf:create-renderer-text-engine renderer)))
	(assert-ret (not (cffi:null-pointer-p font)))
	(assert-ret (not (cffi:null-pointer-p text-engine)))

	;; Init root widget
	(let ((context (make-context))
	      root)
	  (context-set-property% context :window window)
	  (context-set-property% context :font font)
	  (context-set-property% context :text-engine text-engine)

	  ;; EVENT LOOP
	  (unwind-protect
	       (progn
		 (setf root (funcall (root-widget width height root-widget-func)
				     nil context))
		 (setf *root* root)
		 (update-ui window renderer root context)
		 (loop for event = (sdl3:wait-event-timeout* 100) do
		   (when event
		     (if (typep event 'sdl3:quit-event)
			 (return))
		     (handle-event event)
		     (case (sdl3:%type event)
		       (:window-resized (setf width (sdl3:%data-1 event)
					      height (sdl3:%data-2 event)
					      root (funcall (root-widget width height root-widget-func)
							    nil context)))))
		   (if (eql (update-ui window renderer root context) :quit)
		       (return))))

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
      (main0 (lambda ()
	       (home-screen))))))
