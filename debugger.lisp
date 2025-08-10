(in-package #:gauthali)

(defun layout-without-defaults (obj)
  (let* ((class (find-class 'layout)))
    (loop for slot in (closer-mop:class-slots class)
	  for slot-name = (closer-mop:slot-definition-name slot)
	  for val = (slot-value obj slot-name)
	  for default = (funcall (closer-mop:slot-definition-initfunction slot))
	  for collectp = (and (not (eql val default))
			      (not (eql slot-name 'scratchpad)))
	  when collectp
	    collect (intern (symbol-name slot-name) (find-package :keyword))
	  when collectp
	    collect val)))

(defwidget ui-selection (on-select)
  (:state selected)
  (:build
   (layout-set this
	       :flex.x 1.0
	       :child-gap 5.0)
   (loop for ui across *uis*
	 collect (button (format nil "~a~a"
				 (if (eql selected ui) "-> " "")
				 (string (ui-widget-symbol ui)))
			 (let ((copy ui))
			   (lambda ()
			     (funcall on-select copy)
			     (setf selected copy)
			     (widget-rebuild this)))))))

(defwidget box (children-func)
  (:build
   (layout-set this :padding 2.0)
   (funcall children-func))
  (:render (r x y w h)
   (set-render-draw-color r #(125 125 125 0))
   (sdl3:render-rect r (sdl3-frect x y w h))))

(defwidget hspace (width)
  (:build
   (layout-set this :width width)))

(defwidget ui-tree (widget)
  (:state expanded (space/2 5.0))
  (:build
   (when widget
     (list
      (hspace (* 2 space/2))
      (column-widget nil
		      (lambda ()
			(append (list
				 (button (symbol-name (widget-name widget))
					 (lambda ()
					   (setf expanded (not expanded))
					   (widget-rebuild this))))
				(when expanded
				  (list
				   (text (format nil "~s" (layout-without-defaults (widget-layout-x widget))))
				   (text (format nil "~s" (layout-without-defaults (widget-layout-y widget))))))
				(when expanded
				  (loop for el across (widget-children widget)
					collect (ui-tree el)))))))))
  (:render (r x y w h)
    "Render a line of left side in the hspace"
    (when expanded
      (set-render-draw-color r #(125 125 125 0))
      (sdl3:render-line r (+ x space/2) y (+ x space/2) (+ y h)))))

(defwidget debugger ()
  (:state (selected-window))
  (:build
   (column ()
     (row ()
       (ui-selection (lambda (s)
		       (setf selected-window s)
		       #+nl(let ((*print-circle* t))
			 (print (gauthali/tests::get-layout-constructor (ui-widget selected-window) :x)))
		       (widget-rebuild this))))
     (ui-tree (when selected-window
		(ui-widget selected-window)))))
  (:render (r w h x y)
    (declare (ignore r w h x y))
    (widget-rebuild this)))
