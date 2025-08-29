(in-package #:flux)

(defwidget tabs-header (label-widgets on-change)
  "`label-widgets' are a list of tab labels. When one of them is clicked
`on-change' is called with the index."
  (:build
   (loop for label in label-widgets
	 for i from 0
	 collect (let ((k i))
		   (pressable label (lambda () (funcall on-change k)))))))

(defwidget tabs-body (selected-idx tabs &aux (len (length tabs)))
  "`tabs' is a list of widgets that we want to select from.
The first argument `selected-idx' selectes the tab that is currently shown.

This widget can be used with `tab-header' to create `tabs' widget."
  (:state widgets)
  (:build

   ;; Ensure widgets array matches widget-funcs size
   (unless widgets
     (setf widgets (make-array len :initial-element nil)))
   (unless (= (length widgets) len)
     (let ((old widgets))
       (setf widgets (make-array len :initial-element nil))
       (replace widgets old)))
   ;; Ensure selected-idx is correct
   (unless (and (>= selected-idx 0)
		(< selected-idx len))
     (setf selected-idx nil))

   ;; Tab body
   (when selected-idx
     (lambda (prev-widget context)
       (declare (ignore prev-widget))
       (setf (aref widgets selected-idx)
	     (funcall (elt tabs selected-idx)
		      (aref widgets selected-idx) context))))))

(defwidget tabs (initial-idx &rest tabs)
  "Show one of the tabs at a time and maintains the state in previously
opened tabs. `initial-idx' is the index of the tab is shown
in the beginning.

The arguments to tabs are cons cell: (label . body)
Label is the label widget (usually a text widget or an icon) and body
widget is the content of tab."
  (:state (selected-idx initial-idx))
  (:build
   (column ()
     (tabs-header (mapcar #'car tabs) (lambda (idx)
				       (setf selected-idx idx)
				       (widget-rebuild this)))
     (tabs-body selected-idx (mapcar #'cdr tabs)))))
