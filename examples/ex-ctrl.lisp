(in-package :cl-user)

(uiop:define-package controlcl/ex-ctrl
  (:use :cl :controlcl :sdl2)
  (:export
   #:main))

(in-package :controlcl/ex-ctrl)

(defun main ()
  ;; Themes can be set globally or with lexical scoping
  (setq *current-theme* *theme-cp5blue*)
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "ControlCL Testing" :w 800 :h 600 :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        ;; set up the lexical scope of the controlcl instance
        (controlcl:with-controlcl renderer
          (let ((r-x 150)
                (r-y 60))
            (controlcl-add-slider :id 'rocket-x
                                  :name "Rocket-X"
                                  :x 20 :y 50
                                  :value r-x :min-value 0 :max-value 400)
            (controlcl-add-slider :id 'rocket-y
                                  :name "Rocket-Y"
                                  :x 20 :y 100
                                  :value r-y :min-value 0 :max-value 400)
            (controlcl-add-bang :id 'launch :name "Launch"
                                :x 20 :y 150)
            (controlcl-add-image :id 'rocket :name "Rocket"
                                 :x r-x :y r-y :w nil :h nil
                                 :rel-image-path "docs/rocket.png"))
          (sdl2:with-event-loop (:method :poll)
            (:mousemotion (:x x :y y)
                          (controlcl-mouse-over :x x :y y))
            (:keyup (:keysym keysym)
                    ;; ESC - quit
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-quit-event))
                    ;; h - hide all controllers
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-h)
                      (controlcl-hide))
                    ;; s - show all controllers
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
                      (controlcl-show))
                    ;; Due to the lack of the event system for controlcl
                    ;; we directly move the rocket instead of listening to
                    ;; changes
                    ;; 1 - decrease controller 'rocket-x
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-1)
                      (let ((rocket (controlcl-get-ctrl :id 'rocket)))
                        (decf (slider-value (controlcl-get-ctrl :id 'rocket-x)))
                        (controller-move-to rocket
                                            :x (slider-value (controlcl-get-ctrl :id 'rocket-x))
                                            :y (slider-value (controlcl-get-ctrl :id 'rocket-y)))))
                    ;; 2 - increase controller 'rocket-x
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-2)
                      (let ((rocket (controlcl-get-ctrl :id 'rocket)))
                        (incf (slider-value (controlcl-get-ctrl :id 'rocket-x)))
                        (controller-move-to rocket
                                            :x (slider-value (controlcl-get-ctrl :id 'rocket-x))
                                            :y (slider-value (controlcl-get-ctrl :id 'rocket-y))))))
            (:quit () t)
            (:idle ()
                   (sdl2:mouse-state)
                   (sdl2:render-clear renderer)
                   (controlcl-draw)
                   (set-color-from-palette renderer :black)
                   (sdl2:render-present renderer)
                   (sdl2:delay 20))))))))
