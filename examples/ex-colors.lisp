(in-package :cl-user)

(uiop:define-package controlcl/ex-colors
  (:use :cl :controlcl :cl-sdl2-hershey)
  (:export
   #:main))

(in-package :controlcl/ex-colors)

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "ControlCL Example-Colors" :w 700 :h 300 :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (controlcl-init)
        (sdl2:with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (sdl2:render-clear renderer)
                 (let ((num-colors (/ (length controlcl:*colors*) 2)))
                   ;; we have 21 colors - I would like to create a 7 x 3 grid when iterating
                   ;; over the colors
                   ;;
                   (loop for (name color-code) on controlcl:*colors* by #'cddr
                         for i from 0 below num-colors
                         for y = (floor (/ i 7))
                         for x = (mod i 7)
                         for text = (format nil "~a" name)
                         do (progn
                              (set-color-from-code renderer color-code)
                              (sdl2:with-rects ((rect (* x 100) (* y 100) 100 100))
                                (sdl2:render-fill-rects renderer rect 1))))
                   (sdl2:render-present renderer)
                   (sdl2:delay 40)))
          (:quit () t))))))
