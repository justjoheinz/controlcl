(in-package :cl-user)

(uiop:define-package controlcl/ex-ctrl
  (:use :cl :controlcl :sdl2)
  (:export
   #:main))

(in-package :controlcl/ex-ctrl)

(defmethod on-event ((ctrl controller) (evt event-value))
  (let ((source (event-source evt))
        (target (controller-id ctrl)))
    (if (and (eq target 'rocket)
             (or (eq source 'rocket-x) (eq source 'rocket-y)))
        (let ((x (controller-value (controlcl-get-ctrl 'rocket-x)))
              (y (controller-value (controlcl-get-ctrl 'rocket-y))))
          (controller-move-to (controlcl-get-ctrl 'rocket) :x x :y y)))))

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
                          (log4cl:log-info "Handling mouse motion event")
                          (let ((evt (make-instance 'event-mouse :x x :y y)))
                            (controlcl-emit-event evt)))
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

                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-1)
                      (let ((ctrl (controlcl-get-ctrl 'rocket-x)))
                        (decf (controller-value ctrl))))

                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-2)
                      (let ((ctrl (controlcl-get-ctrl 'rocket-x)))
                        (incf (controller-value ctrl))))

                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-3)
                      (let ((ctrl (controlcl-get-ctrl 'rocket-y)))
                        (decf (controller-value ctrl))))

                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-4)
                      (let ((ctrl (controlcl-get-ctrl 'rocket-y)))
                        (incf (controller-value ctrl)))))

            (:quit () t)
            (:idle ()
                   (sdl2:mouse-state)
                   (sdl2:render-clear renderer)
                   (controlcl-draw)
                   (set-color-from-palette renderer :black)
                   (sdl2:render-present renderer)
                   (sdl2:delay 10))))))))
