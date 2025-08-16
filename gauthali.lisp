(require 'sdl3)
(in-package #:gauthali)

(defparameter *uis* nil)
(defparameter *window-positions* (make-hash-table))

(defun update-window-position (event-type widget-name &optional data1 data2)
  (flet ((set-val (key val)
	   (let ((entry (gethash widget-name *window-positions*)))
	     (unless entry
	       (setf entry (list :xy nil :size nil))
	       (setf (gethash widget-name *window-positions*) entry))
	     (setf (getf entry key) val))))
    (case event-type
      (:window-moved
       (set-val :xy (cons data1 data2)))
      (:window-resized
       (set-val :size (cons data1 data2))))))

(defstruct UI
  (window)
  (window-id)
  (renderer)
  (widget-initializer)
  (widget)
  (context)
  (debugger-render-hooks (make-hook-store)))

(defun init-font-manager (renderer)
  (make-instance 'font-manager
		 :open-font-func (lambda (name size)
				   (cond ((string-equal name "times-new-roman")
					  (multiple-value-bind (ret sx sy) (sdl3:get-render-scale renderer)
					    (declare (ignore sy))
					    (assert-ret ret)
					    (let ((font (sdl3-ttf:open-font (namestring (get-resource-path "res/fonts/Times New Roman.ttf")) (* sx size))))
					      (assert-ret (not (cffi:null-pointer-p font)))
					      font)))
					 (t (error "Can't load font ~a" name))))
		 :close-font-func #'sdl3-ttf:close-font))

(defun init-ui (window renderer root-widget-initializer)
  (let ((fm (init-font-manager renderer))
	(context (make-context)))

    (context-set-property% context :font-manager fm)
    (context-set-property% context :font "times-new-roman")
    (context-set-property% context :font-size 24.0)
    (context-set-property% context :render-scale 1.0)
    (context-set-property% context :bg-color #(255 255 255 255))
    (context-set-property% context :fg-color #(0 0 0 255))

    (let* ((widget (build-widget root-widget-initializer nil context))
	   (entry (gethash (widget-name widget) *window-positions*)))

      (when entry
	(when (getf entry :size)
	  (sdl3:set-window-size window (car (getf entry :size)) (cdr (getf entry :size))))
	(when (getf entry :xy)
	  (sdl3:set-window-position window (car (getf entry :xy)) (cdr (getf entry :xy)))))

      (make-ui
       :window window
       :window-id (sdl3:get-window-id window)
       :renderer renderer
       :widget-initializer root-widget-initializer
       :widget widget
       :context context))))

(defun handle-event (event uis)
  (let* ((window-id (if (slot-exists-p event 'sdl3:%window-id)
			(sdl3:%window-id event)))
	 (stop nil))
    ;; Pass to UIs
    (loop for ui across uis do
      (if window-id
	  (when (and (not stop)
		     (eql window-id (ui-window-id ui)))
	    (setf stop (or stop (call-event-handlers (ui-widget ui) event))))))
    ;; If none say stop, handle it myself
    (unless stop
      (typecase event
	(sdl3:keyboard-event
	 (case (sdl3:%key event)
	   (:q (cffi:with-foreign-object (ev '(:struct sdl3:quit-event))
		 (setf (cffi:foreign-slot-value ev '(:struct sdl3:quit-event) 'sdl3:%type) :quit)
		 (sdl3:push-event ev)))
	   (:d (when (and (member :lctrl (sdl3:%mod event))
			  (sdl3:%down event)
			  (not (sdl3:%repeat event))
			  (< (count 'debugger-screen uis :key (lambda (ui) (widget-name (ui-widget ui)))) 2))
		 (multiple-value-bind (ret w r) (sdl3:create-window-and-renderer "Debugger" 200 400 '(:resizable :high-pixel-density))
		   (assert-ret ret)
		   (vector-push-extend (init-ui w r (row () (debugger-screen))) uis)))))))

      (case (sdl3:%type event)
	(:window-display-scale-changed
	 (let* ((window-id (sdl3:%window-id event))
		(display-scale (sdl3:get-window-display-scale (sdl3:get-window-from-id window-id)))
		(ui (find window-id uis :key #'ui-window-id))
		(context (when ui (ui-context ui))))
	   (when ui
	     (context-set-property% context :render-scale display-scale)
	     (sdl3:set-render-scale (ui-renderer ui) display-scale display-scale)
	     (loop for font across (slot-value (context-get-property% context :font-manager) 'fonts) do
	       (sdl3-ttf:set-font-size (font-info-ptr font)
				       (* (font-info-size font) display-scale)))
	     ;; Mark all widgets as dirty
	     (widget-rebuild-all (ui-widget ui)))))
	((:window-moved :window-resized)
	 (let* ((window-id (sdl3:%window-id event))
		(ui (find window-id uis :key #'ui-window-id)))
	   (when ui
	     (update-window-position (sdl3:%type event) (widget-name (ui-widget ui)) (sdl3:%data-1 event) (sdl3:%data-2 event)))))))))

(defun update-ui0 (ui)
  (with-slots (window renderer widget widget-initializer context) ui
    ;; Clear display
    (let ((bg-color (context-get-property% context :bg-color)))
      (sdl3:set-render-draw-color renderer (aref bg-color 0) (aref bg-color 1) (aref bg-color 2) (aref bg-color 3)))
    (sdl3:render-clear renderer)

    ;; Build - Layout - Render
    (multiple-value-bind (ret w h) (sdl3:get-window-size window)
      (assert-ret ret)
      (setf widget (build-widget widget-initializer widget context))
      (update-widget-layouts widget 0 0 w h)
      (call-render-funcs widget renderer))

    ;; Present
    (run-hooks (ui-debugger-render-hooks ui) (list ui))
    (sdl3:render-present renderer)
    (values)))

(defun update-ui (ui)
  (restart-case (update-ui0 ui)
    (retry ()
      :report "Retry updating ui again."
      (update-ui ui))))



(defun main0 (root-widget-initializer &key title width height)
  ;; INIT
  (assert-ret (sdl3:init '(:video)))
  (let ((mouse-x nil)
	(mouse-y nil)
	(uis (make-array 0 :element-type 'ui :adjustable t :fill-pointer 0)))
    (setf *uis* uis)

    (sdl3-ttf:init)
    (multiple-value-bind (ret window renderer)
	(sdl3:create-window-and-renderer title width height '(:input-focus :mouse-focus :resizable :high-pixel-density))
      (assert-ret ret)
      (sdl3:raise-window window)
      (sdl3:start-text-input window)
      (vector-push-extend (init-ui window renderer root-widget-initializer) uis))

    ;; EVENT LOOP
    (unwind-protect
	 (progn
	   (map nil #'update-ui uis)
	   (loop for event = (sdl3:wait-event-timeout* 100) do
	     (when event
	       (if (typep event 'sdl3:quit-event)
		   (return))
	       (handle-event event uis)
	       (case (sdl3:%type event)
		 (:mouse-button-down
		  (if (and mouse-x mouse-y)
		      (setf mouse-x nil mouse-y nil)
		      (setf mouse-x (sdl3:%x event)
			    mouse-y (sdl3:%y event))))))
	     (map nil #'update-ui uis)))

      ;; QUIT
      (loop for ui across uis do
	(sdl3:destroy-renderer (ui-renderer ui))
	(sdl3:destroy-window (ui-window ui))
	(when (context-get-property% (ui-context ui) :font-manager)
	  (destory-font-manager (context-get-property% (ui-context ui) :font-manager)))
	(cleanup-widget (ui-widget ui)))
      ;; The window may not actually be destroyed until the event loop
      ;; is pumped again.
      (sdl3:pump-events)
      ;; And since we pumped, we flush so that it doesn't affect next
      ;; time we create window
      (sdl3:flush-events 0 (cffi:foreign-enum-value 'sdl3:event-type :last)))))

(defun start-ui (&key (widget (row () (leap-year-screen))) (title "Leap Year") (width 400) (height 200))
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (float-features:with-float-traps-masked t
      (main0 widget :title title :width (floor width) :height (floor height)))))
