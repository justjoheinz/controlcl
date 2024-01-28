(in-package :cl-user)

(uiop:define-package :controlcl
  (:use :cl :cl-sdl2-hershey)
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
   #:set-color-from-code))
