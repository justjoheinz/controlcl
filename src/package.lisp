(in-package :cl-user)

(uiop:define-package :controlcl
  (:use :cl :cl-sdl2-hershey)
  (:import-from :alexandria
   :clamp)
  (:import-from :assoc-utils
   :aget
                :alist-values)
  (:import-from :event-emitter
   :event-emitter
                :on
   :emit
                :once)
  (:export
   #:*theme-retro*
   #:*theme-cp52014*
   #:*theme-cp5blue*
   #:*theme-red*
   #:*theme-grey*
   #:*theme-a*
   #:*current-theme*
   #:theme-fg-color
   #:theme-bg-color
   #:theme-active-color
   #:theme-caption-color
   #:theme-value-color
   #:color-values
   #:*colors*
   #:set-color-from-theme
   #:set-color-from-code
   #:set-color-from-palette
   #:v-factor
   #:with-theme
   #:point-in-rect
   #:controlcl-version
   #:controlcl-init
   #:render-text
   #:slider
   #:slider-name
   #:slider-min-value
   #:slider-max-value
   #:bang
   #:bang-name
   #:controller-mouse-over-p
   #:controller-hide
   #:controller-show
   #:controller-draw
   #:controller
   #:controller-x
   #:controller-y
   #:controller-w
   #:controller-h
   #:controller-value
   #:controller-id
   #:controller-visible
   #:controller-mouse-over
   #:controller-set-mouse-over-color
   #:controlcl
   #:controlcl-controllers
   #:controlcl-renderer
   #:with-controlcl
   #:controlcl-draw
   #:controlcl-show
   #:controlcl-hide
   #:controlcl-add-bang
   #:controlcl-add-slider
   #:controlcl-mouse-over
   #:controlcl-add-image
   #:*controlcl*
   #:from
   #:controlcl-get-ctrl
   #:controlcl-move-to
   #:controller-move-to
   #:event
   #:event-id
   #:event-source
   #:event-target
   #:event-mouse
   #:event-mouse-x
   #:event-mouse-y
   #:event-mouse-button
   #:event-mouse-modifiers
   #:event-key
   #:event-key-key
   #:event-key-modifiers
   #:event-value
   #:event-value-old
   #:event-value-new
   #:controlcl-emit-event
   #:on-event
   #:checkbox
   #:controlcl-add-checkbox))
