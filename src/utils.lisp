(in-package :controlcl)


;; UTILITIES

(defvar *controlcl-event* (sdl2:register-user-event-type :controlcl))

(defun point-in-rect (x y w h x1 y1)
  "Return true if the point x1,y1 is inside the rectangle x,y,w,h"
  (and (<= x x1 (+ x w))
       (<= y y1 (+ y h))))


(defun v-factor ( value min-v max-v)
  "Return a value between 0 and 1, expressing the relation of value to min-v and max-v."
  (let ((v (- value min-v))
        (max (- max-v min-v)))
    (/ v max)))

;; helper methods to encapsulate calls to cl-sdl2-hershey

(defgeneric render-text (renderer x y text)
  (:documentation "Render text at x,y"))

(defmethod render-text (renderer  x y (text string))
  (cl-sdl2-hershey:render-hershey-string renderer x y text))

(defun controlcl-init ()
  "Initialize the controlcl library"
  (hershey-init))

(defun controlcl-version ()
  "Return the version of the controlcl library"
  (asdf:component-version (asdf:find-system :controlcl)))
