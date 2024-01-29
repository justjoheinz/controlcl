(in-package :cl-user)

(uiop:define-package controlcl/ex-ctrl
  (:use :cl :controlcl :sdl2)
  (:export
   #:main))

(in-package :controlcl/ex-ctrl)

(defun main ()
  (controlcl-init)
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "ControlCL Testing" :w 800 :h 600 :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (let ((slider (make-instance 'slider :name "test-slider" :x 100 :y 200 :min-value 0 :max-value 100 :value 50))
              (bang (make-instance 'bang :name "test" :x 100 :y 100)))
          (sdl2:with-event-loop (:method :poll)
            (:mousemotion (:x x :y y)
                          (setf (controller-mouse-over bang) (controller-mouse-over-p bang x y))
                          (setf (controller-mouse-over slider) (controller-mouse-over-p slider x y)))

            (:keyup (:keysym keysym)
                    (format t "keysym: ~a~%" keysym)
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-quit-event))
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-h)
                      (controller-hide bang)
                      (format t "visible: ~a~%" (controller-visible bang)))
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
                      (controller-show bang)
                      (format t "visible: ~a~%" (controller-visible bang)))
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-1)
                      (decf (slider-value slider))
                      (format t "value: ~a~%" (slider-value slider)))
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-2)
                      (incf (slider-value slider))
                      (format t "value: ~a~%" (slider-value slider))))
            (:quit () t)
            (:idle ()
                   (sdl2:mouse-state)
                   (sdl2:render-clear renderer)
                   (controller-draw bang renderer)
                   (controller-draw slider renderer)
                   (set-color-from-palette renderer :black)
                   (sdl2:render-present renderer)
                   (sdl2:delay 20))))))))