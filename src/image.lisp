(in-package :controlcl)

;; IMAGE

(defclass image (controller)
  ((name :initarg :name :accessor image-name :initform nil)
   (surface :initarg :surface :accessor image-surface :initform nil)
   (w :initform nil)
   (h :initform nil)))

(defmethod controller-draw ((ctrl image))
  (with-slots (x y w h surface renderer) ctrl
    (sdl2:with-rects ((rect x y w h))
      (let* ((texture (sdl2:create-texture-from-surface renderer surface)))
        (sdl2:render-copy renderer texture :dest-rect rect)
        (sdl2:destroy-texture texture)))))
