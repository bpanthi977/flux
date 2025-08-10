(in-package #:gauthali)

(defwidget fill-background-widget (widgets-func)
  (:state color)
  (:build
   (setf color (property-get :bg-color))
   (remove nil (funcall widgets-func)))
  (:render (r x y w h)
    (set-render-draw-color r color)
    (sdl3:render-fill-rect r (sdl3-frect x y w h))))

(defmacro fill-background (&rest body)
  `(fill-background-widget
    (lambda ()
      (list
       ,@body))))
