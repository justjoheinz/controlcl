(in-package :controlcl)


;; UTILITIES

(defun point-rect (x y w h x1 y1)
  "Return true if the point x1,y1 is inside the rectangle x,y,w,h"
  (and (<= x x1 (+ x w))
       (<= y y1 (+ y h))))


;; given min-v, v and max-v
;; return a value between 0 and 1
(defun v-factor ( value min-v max-v)
  "Return a value between 0 and 1, expressing the relation of value to min-v and max-v."
  (let ((v (- value min-v))
        (max (- max-v min-v)))
    (/ v max)))
