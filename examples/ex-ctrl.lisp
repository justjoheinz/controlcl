(in-package :cl-user)

(uiop:define-package controlcl/ex-ctrl
  (:use :cl :controlcl :sdl2)
  (:export
   #:main))

(in-package :controlcl/ex-ctrl)

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "ControlCL Testing" :w 800 :h 600 :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (controlcl:with-controlcl renderer
          (let ((slider-x (controlcl-add-slider :name "Rocket-X"
                                                :x 20 :y 50
                                                :value 50 :min-value 0 :max-value 400))
                (slider-y (controlcl-add-slider :name "Rocket-Y"
                                                :x 20 :y 100
                                                :value 50 :min-value 0 :max-value 400))
                (bang (controlcl-add-bang :name "Launch"
                                          :x 20 :y 150))
                (img (controlcl-add-image :name "Rocket"
                                          :x 150 :y 60 :w nil :h nil
                                          :rel-image-path "docs/rocket.png")))
            (sdl2:with-event-loop (:method :poll)
              (:mousemotion (:x x :y y)
                            (controlcl-mouse-over :x x :y y))
              (:keyup (:keysym keysym)
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                        (sdl2:push-quit-event))
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-h)
                        (controlcl-hide))
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
                        (controlcl-show))
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-1)
                        (decf (slider-value slider-x)))
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-2)
                        (incf (slider-value slider-x))))
              (:quit () t)
              (:idle ()
                     (sdl2:mouse-state)
                     (sdl2:render-clear renderer)
                     (controlcl-draw)
                     (set-color-from-palette renderer :black)
                     (sdl2:render-present renderer)
                     (sdl2:delay 20)))))))))
