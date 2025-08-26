(in-package #:gauthali)

(cffi:defcallback file-select-callback :void ((user-data :pointer) (filelist (:pointer :string)) (filter :int))
  (declare (ignore filter))
  (cond ((cffi:null-pointer-p filelist)
	 (print "Null filelist. Some error."))
	(t
	 (let ((files (loop for i from 0
			    for str = (cffi:mem-aref filelist :string i)
			    while str
			    collect str))
	       (callback-id (cffi:mem-ref user-data :int)))
	   (funcall (get-callback callback-id) files))))
  (cffi:foreign-free user-data))

(defwidget select-file (widget-fn callback &key filter (default-location "") (allow-many nil))
  "widget-fn is a function that takes a trigger function to show the file selection dialog and returns a widget.
callback is function that get the list of files.

widget-fn = (function (trigger) widget)
callback = (function (files))

Filter is a cons of (name . pattern)
pattern can be * or semi-colon separated list of extensions."
  (:state callback-id)
  (:build
   (when callback-id
     (remove-callback callback-id)
     (setf callback-id nil))
   (unless callback-id
     (setf callback-id (register-callback callback)))
   (let ((window (property-get :sdl3.window)))
     (funcall widget-fn
	      (lambda ()
		(let ((int-ptr (cffi:foreign-alloc :int :count 1)))
		  ;; The callback function has do free the int-ptr
		  (setf (cffi:mem-aref int-ptr :int) callback-id)
		  (sdl3:show-open-file-dialog (cffi:callback file-select-callback) int-ptr  window
					      (map 'vector (lambda (x)
							     (make-instance 'sdl3:dialog-file-filter :%name (car x) :%pattern (cdr x)))
						   filter)
					      default-location
					      allow-many))))))
  (:cleanup
   (remove-callback callback-id)))
