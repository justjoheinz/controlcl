(in-package :controlcl)

;; BANG

(defclass checkbox (controller)
  ((name :initarg :name :accessor bang-name :initform nil)
   (value :initform t)
   (w :initform (error "you must provide a width"))
   (h :initform (error "you must provide a height"))))

(defmethod controller-draw ((ctrl checkbox))
  (with-slots (x y w h renderer) ctrl
    (controller-set-mouse-over-color ctrl)
    (sdl2:with-rects ((rect x y w h))
      (sdl2:render-fill-rect renderer rect))
    (set-color-from-theme renderer :caption)
    (with-font (*roman-plain-font* 0.6)
      (render-text renderer x (+ y h 10) (bang-name ctrl)))
    (if (controller-value ctrl)
        (progn
          (set-color-from-theme renderer :value)
          (sdl2:render-draw-line renderer x y (+ x w) (+ y h))
          (sdl2:render-draw-line renderer (+ x w) y x (+ y h))))))
