(uiop:define-package controlcl/ex-theme
  (:use :cl :controlcl :sdl2 :cl-sdl2-hershey)
  (:export
   #:main))

(in-package :controlcl/ex-theme)

(hershey-init)


(defun render-theme (renderer y &optional (*current-theme* *current-theme*))

  (sdl2:with-rects ((rect-000 0 y 100 100)
                    (rect-100 100 y 100 100)
                    (rect-200 200 y 100 100)
                    (rect-300 300 y 100 100)
                    (rect-400 400 y 100 100))
    ;; fg, active, value
    (set-color-from-theme renderer :fg)
    (sdl2:render-fill-rect renderer rect-000)

    (with-font *roman-simplex-font* 0.35
      (set-color-from-theme renderer :value)
      (render-hershey-string renderer 25 (+ y 50) "VALUE"))

    ;;bg
    (set-color-from-theme renderer :bg)
    (sdl2:render-fill-rect renderer rect-100)

    ;; active
    (set-color-from-theme renderer :active)
    (sdl2:render-fill-rect renderer rect-200)

    ;; caption
    (set-color-from-theme renderer :caption)
    (sdl2:render-fill-rect renderer rect-300)

    ;; value
    (set-color-from-theme renderer :value)
    (sdl2:render-fill-rect renderer rect-400)))


(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "ControlCL Example-Theme" :w 500 :h 600 :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (sdl2:render-clear renderer)
                 (loop for theme in (list *theme-retro* *theme-cp52014* *theme-cp5blue*
                                          *theme-red* *theme-grey* *theme-a*)
                       for y = 0 then (+ y 100)
                       do (with-theme theme
                            (render-theme renderer y)))

                 ;; draw background
                 (sdl2:set-render-draw-color renderer 0 0 0 0)
                 (sdl2:render-present renderer)
                 (sdl2:delay 40))
          (:quit () t))))))
