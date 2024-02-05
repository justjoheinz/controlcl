(in-package :controlcl)

;; BANG

(defclass checkbox (controller)
  ((name :initarg :name :accessor bang-name :initform nil)
   (w :initform (error "you must provide a width"))
   (h :initform (error "you must provide a height"))))

(defun set-checkbox-color (ctrl)
  (with-slots (value renderer) ctrl
    (if value
        (set-color-from-theme renderer :active)
        (if (controller-mouse-over ctrl)
            (set-color-from-theme renderer :fg)
            (set-color-from-theme renderer :bg)))))

(defmethod controller-draw ((ctrl checkbox))
  (with-slots (x y w h renderer) ctrl
    (set-checkbox-color ctrl)
    (sdl2:with-rects ((rect x y w h))
      (sdl2:render-fill-rect renderer rect))
    (set-color-from-theme renderer :caption)
    (with-font (*roman-plain-font* 0.6)
      (render-text renderer x (+ y h 10) (bang-name ctrl)))))

(defmethod on-event ((ctrl checkbox) (evt event-mouse-clicked))
  (with-slots (x y) evt
    (if (controller-mouse-over-p ctrl :x x :y y)
        (setf (controller-value ctrl) (not (controller-value ctrl))))))
