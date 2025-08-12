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
				 ;; Replace with Title
				 (string (widget-name (ui-widget ui))))
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

(defwidget layout-description (widget)
  (:build
   (property-set this :font-size 16.0)
   (layout-set this :major-axis :y)
   (list
    (text (format nil "~s" (layout-without-defaults (widget-layout-x widget))))
    (text (format nil "~s" (layout-without-defaults (widget-layout-y widget)))))))

(defun highlight-widget (widget ui)
  (let* ((r (ui-renderer ui))
	 (lx (widget-layout-x widget))
	 (ly (widget-layout-y widget)))
    (set-render-draw-color r #(255 0 0 0))
    (sdl3:render-rect r (sdl3-frect (layout-offset lx) (layout-offset ly)
				    (layout-size lx) (layout-size ly)))))

(defwidget ui-tree (widget render-hooks)
  (:state expanded (space/2 5.0) hook-handle)
  (:build
   (when widget
     (list
      (hspace (* 2 space/2))
      (column-widget nil
		      (lambda ()
			(append (list
				 (hoverable
				  (lambda (hover)
				    (when hook-handle
				      (remove-hook render-hooks hook-handle)
				      (setf hook-handle nil))
				    (when hover
				      (setf hook-handle (add-hook render-hooks
								  (lambda (ui)
								    (highlight-widget widget ui))))))
				  (lambda ()
				    (button (symbol-name (widget-name widget))
					    (lambda ()
					      (setf expanded (not expanded))
					      (widget-rebuild this))))))
				(when expanded
				  (cons (layout-description widget)
					(loop for el across (widget-children widget)
					      collect (ui-tree el render-hooks))))
				(when (and expanded (eql (widget-name widget) 'scrollable))
				  (loop for state-val across (widget-state widget)
					when (widget-p state-val)
					  collect (ui-tree state-val render-hooks)))))))))
  (:render (r x y w h)
    (declare (ignorable w))
    "Render a line of left side in the hspace"
    (when expanded
      (set-render-draw-color r #(125 125 125 0))
      (sdl3:render-line r (+ x space/2) y (+ x space/2) (+ y h))))
  (:cleanup
   (when hook-handle
     (remove-hook render-hooks hook-handle))))

(defwidget debugger-screen ()
  (:state (selected-window))
  (:build
   (property-set this :current-ui selected-window)
   (scrollable
    (lambda ()
      (column ()
	(row ()
	  (ui-selection (lambda (s)
			  (setf selected-window s)
			  (widget-rebuild this))))
	(when selected-window
	  (ui-tree (ui-widget selected-window)
		   (ui-debugger-render-hooks selected-window)))))
    :x-speed 5.0
    :y-speed 5.0))
  (:render (r w h x y)
    (declare (ignore r w h x y))
    (widget-rebuild this)))
