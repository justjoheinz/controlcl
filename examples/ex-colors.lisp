(in-package :cl-user)

(uiop:define-package controlcl/ex-colors
  (:use :cl :controlcl :cl-sdl2-hershey)
  (:export
   #:main))

(in-package :controlcl/ex-colors)

(defun main ()
  (controlcl-init)
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "ControlCL Example-Colors" :w 1105 :h 100 :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (sdl2:render-clear renderer)
                 (loop for (name color-code) on controlcl:*colors* by #'cddr
                       for i from 0 below (length controlcl:*colors*)
                       with width = (floor (/ 2210 (length controlcl:*colors*)))
                       with text = (format nil "~a" name)

                       do (progn
                            (controlcl:set-color-from-code renderer color-code)
                            (sdl2:with-rects ((rect (* i width) 0 width 100))
                              (sdl2:render-fill-rects renderer rect 1))
                            (with-font cl-sdl2-hershey:*roman-simplex-font* 0.35
                              (controlcl:set-color-from-theme renderer :value)
                              (let ((x (+ (* i width) 10))
                                    (y 20)
                                    (text (format nil ":~a" name)))
                                (render-text renderer x y text)))))
                 (sdl2:render-present renderer)
                 (sdl2:delay 40))
          (:quit () t))))))
