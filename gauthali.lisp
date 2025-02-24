;;;; gauthali.lisp

(in-package #:gauthali)

(defparameter *app* nil)

(defclass app ()
  ((window :initarg :window :accessor window)
   (renderer :initarg :renderer :accessor renderer)
   (frame-rate :initform 1000/60 :accessor frame-rate)
   (font :initarg :font :accessor font)))


(defun init ()
  (assert-ret (sdl3:set-app-metadata "Gauthali" "1.0" "com.bpanthi.gauthali"))
  (assert-ret (sdl3:init :video))

  (multiple-value-bind (ret window renderer)
      (sdl3:create-window-and-renderer
       "Gauthali"
       400 400
       nil)
    (assert-ret ret)
    (make-instance 'app
		   :window window
		   :renderer renderer)))

(defun render (app)
  (declare (ignore app)))

(defun handle-event (app event)
  (declare (ignorable app event))
  (typecase event
    (sdl3:keyboard-event 
     (case (slot-value event 'sdl3:%scancode)
       (:q nil)
       (t t)))
    (t t)))

(defun handle-quit (app)
  (sdl3:destroy-renderer (renderer app))
  (sdl3:destroy-window (window app))
  (sdl3:pump-events)
  (sdl3:quit-sub-system :video)
  (sdl3:quit))

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
                    (unless (handle-event app event)
		      (return-from outer)))
	   (let ((render-time-diff (/ (- (get-internal-real-time) last-frame-update)
				      internal-time-units-per-second)))
	     (cond ((> render-time-diff (frame-rate app))
		    (render app)
		    (setf last-frame-update (get-internal-real-time)))
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
