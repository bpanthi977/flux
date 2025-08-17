;;;; package.lisp

(defpackage #:gauthali/utils
  (:use #:cl)
  (:export
   #:make-hook-store
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:get-resource-path
   #:assert-ret
   #:sdl3-color
   #:sdl3-rect
   #:sdl3-frect
   #:set-render-draw-color
   #:with-render-scale-off
   #:map-tree))

(defpackage #:gauthali
  (:use #:cl #:gauthali/utils)
  (:export
   #:defwidget
   #:widget-rebuild
   #:this
   #:prev
   #:on
   #:property-set
   #:property-get
   #:start-ui
   #:layout-set
   #:widget-bounds
   #:cleanup-widget
   #:build-context
   #:build-widget
   #:*context*
   #:update-widget-layouts
   #:call-render-funcs
   #:call-event-handlers

   #:column
   #:column-widget
   #:row
   #:row-widget
   #:layout
   #:pressable
   #:hoverable
   #:button
   #:text
   #:text-entry
   #:slider
   #:scrollable))
