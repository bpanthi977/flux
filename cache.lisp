(in-package #:gauthali)

(defstruct drawcache
  (used nil :type boolean)
  (tag nil :type (or null string))
  (props nil :type t)
  (test #'equalp :type function)
  (draw-tree-fn nil :type (or null (function (element))))
  (texture nil :type (or null cffi:foreign-pointer))
  (width 0.0 :type single-float)
  (height 0.0 :type single-float)
  (child-pointer 0 :type integer)
  (children
   (make-array 0 :element-type 'element :fill-pointer 0 :adjustable t)
   :type (array drawcache)))

(defun destroy-drawcache (cache)
  (when (drawcache-texture cache)
    (sdl3:destroy-texture (drawcache-texture cache)))
  (setf (drawcache-texture cache) nil)
  (loop for child across (drawcache-children cache)
	do (destroy-drawcache child)))

(defun texture-size-eql (texture w h)
  (multiple-value-bind (ret ww hh) (sdl3:get-texture-size texture)
    (assert-ret ret)
    (and (= (ceiling w) ww) (= (ceiling h) hh))))

(defun ensure-valid-texture (renderer cache w h)
  (with-slots (texture) cache
    ;; Destory invalid texture
    (when (and texture
	       #+nil(not (texture-size-eql texture w h)))
      (sdl3:destroy-texture texture)
      (setf texture nil))
    ;; Create new texture if needed
    (unless texture
      (setf texture (sdl3:create-texture renderer :argb8888 :target (ceiling w) (ceiling h))
	    (drawcache-width cache) w
	    (drawcache-height cache) h))

    texture))

(defun find-child-cache (root tag)
  "Find the next child cache in the root cache.
If tag is given, child with same tag is found else
next child is returned and child-pointer in root cache is updated."
  (if tag
      (loop for child across (drawcache-children root) do
	(when (eql tag (drawcache-tag child))
	  (return child)))
      (loop with children = (drawcache-children      root)
	    with offset   = (drawcache-child-pointer root)
	    do
	       (incf (drawcache-child-pointer root))
	       (cond ((>= offset (length children))
		      (return nil))
		     ((drawcache-tag (aref children offset))
		      (incf offset))
		     (t
		      (return (aref children offset)))))))

(defun destroy-unused-cache (root)
  "Destorys and removes the unused children drawcaches in root."
  (let* ((children (drawcache-children root))
	 (len (length children)))
    (setf (fill-pointer children) 0)
    (loop for i from 0 below len
	  for child = (aref children i) do
	    (if (drawcache-used child)
		(vector-push child children)
		(destroy-drawcache child)))))

(defun create-cache-draw-tree-fn (renderer cache el)
  "Return a draw-tree-fn for a cache element that wraps element `el'.

On the first we need to render to a texture and cache it, and
later we resuse the texture."
  (let ((drawn-once nil))
    (lambda (new-el)
      (let ((old-target (sdl3:get-render-target renderer))
	    (x (layout-offset (element-x new-el)))
	    (y (layout-offset (element-y new-el)))
	    (w (layout-size   (element-x new-el)))
	    (h (layout-size   (element-y new-el))))

	(if drawn-once
	    (sdl3:render-texture renderer
				 (drawcache-texture cache) nil
				 (make-instance 'sdl3:frect
						:%w w :%h h :%x x :%y y))
	    (unwind-protect
		 (progn
		   ;; Change render target
		   (ensure-valid-texture renderer cache w h)
		   (sdl3:set-render-target renderer (drawcache-texture cache))

		   ;; Draw to texture
		   (translate-element-tree el (- x) (- y))
		   (draw-element-tree el)

		   ;; Draw texture onto the screen
		   (sdl3:set-render-target renderer old-target)
		   (sdl3:render-texture renderer
				 (drawcache-texture cache) nil
				 (make-instance 'sdl3:frect
						:%w w :%h h :%x x :%y y))
		   (setf drawn-once t))
	      (sdl3:set-render-target renderer old-target)))))))

(defparameter *current-drawcache* nil)
(defun cache-element (renderer props tag el-fn &aux (parent-cache *current-drawcache*))
  "Create an wrapper element that caches the rendering of element tree `el-fn'
done using `renderer' and reuses the texture next time if the `props' are same.

`el-fn' is a function that returns an element.

This function returns an dummy element if cache is upto date, otherwise it
evalutates the element `el-fn' and returns a wrapper element."
  (let (cache el)
    (cond
      ;; No cache => Return the element
      ((null parent-cache)
       (funcall el-fn))

      ;; Found cache with same props
      ;; => Return dummy element that renders from cached texture
      ((and (setf cache (find-child-cache parent-cache tag))
	    (funcall (drawcache-test cache) (drawcache-props cache) props))

       (setf (drawcache-used cache) t)
       (make-element
	:x (make-layout :type :fixed :size (drawcache-width cache))
	:y (make-layout :type :fixed :size (drawcache-height cache))
	:draw-tree-fn (drawcache-draw-tree-fn cache)))

      ;; Found cache with different props Or didn't find the cache
      ;; => Return element tree replica that caches the render
      (t
       ;; Ensure cache exits, evaluate the element tree (el-fn)
       ;; and if we are reusing old cache, destroy unused children caches
       (cond (cache
	      (loop for child across (drawcache-children cache) do
		(setf (drawcache-used child) nil))
	      (let ((*current-drawcache* cache))
		(setf el (funcall el-fn)))
	      (destroy-unused-cache cache))

	     ((not cache)
	      (setf cache (make-drawcache
			   :used t
			   :tag tag
			   :props props))
	      (vector-push-extend cache (drawcache-children parent-cache))
	      (incf (drawcache-child-pointer parent-cache))
	      (let ((*current-drawcache* cache))
		(setf el (funcall el-fn)))))

       (assert (typep el 'element))
       (setf (drawcache-used cache) t
	     (drawcache-props cache) props
	     (drawcache-child-pointer cache) 0
	     (drawcache-draw-tree-fn cache) (create-cache-draw-tree-fn renderer cache el))

       (make-element
	:x (element-x el)
	:y (element-y el)
	:children (element-children el)
	:draw-tree-fn (drawcache-draw-tree-fn cache))))))

(defmacro with-drawcache ((cache) &body body)
  `(let ((*current-drawcache* ,cache))
     (prog1 (progn ,@body)
       (when *current-drawcache*
	 (setf (drawcache-child-pointer *current-drawcache*) 0)))))


(defmacro <!> ((props &optional tag)
	       (draw-fn renderer &rest keys &key width height &allow-other-keys)
	       &optional grid-args
	       &body body)
  (declare (ignorable width height))
  `(register-element
    (cache-element
     ,renderer ,props ,tag
     (lambda ()
       (let ((*current-element* nil))
	 (<> (,draw-fn ,renderer ,@keys) ,grid-args ,@body))))))
