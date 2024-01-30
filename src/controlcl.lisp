(in-package :controlcl)

(defvar *controlcl* nil)


;; CONTROLCL

(defclass controlcl ()
  ((controllers :initarg :controllers :accessor controlcl-controllers :initform nil)
   (renderer :initarg :renderer :accessor controlcl-renderer :initform nil)))

(defmacro with-controlcl (renderer &body body)
  `(let ((*controlcl* (make-instance 'controlcl :renderer ,renderer)))
     (controlcl-init)
     (sdl2-image:init '(:png))
     (unwind-protect
          (progn ,@body))
     (sdl2-image:quit)))

(defun controlcl-draw ()
  (dolist (ctrl (controlcl-controllers *controlcl*))
    (controller-draw ctrl)))

(defun controlcl-show ()
  (dolist (ctrl (controlcl-controllers *controlcl*))
    (controller-show ctrl)))

(defun controlcl-hide ()
  (dolist (ctrl (controlcl-controllers *controlcl*))
    (controller-hide ctrl)))

(defun controlcl-add-bang (&key name x y)
  (let ((bang (make-instance 'bang :name name :x x :y y
                                   :renderer (controlcl-renderer *controlcl*))))
    (push bang (controlcl-controllers *controlcl*))
    bang))

(defun controlcl-add-slider (&key name x y value  min-value max-value)
  (assert (<= min-value value max-value)
          (min-value value max-value)
          "It must be T that MIN-VALUE <= VALUE <= MAX-VALUE  (~S <= ~S <= ~S)." min-value value max-value)
  (let ((slider (make-instance 'slider :name name :x x :y y
                                       :value value
                                       :min-value min-value :max-value max-value
                                       :renderer (controlcl-renderer *controlcl*))))
    (push slider (controlcl-controllers *controlcl*))
    slider))

(defun controlcl-add-image (&key name x y w h rel-image-path)
  (let* ((image-path (asdf:system-relative-pathname :controlcl rel-image-path))
         (surface (sdl2-image:load-image image-path))
         (w1 (or w (sdl2:surface-width surface)))
         (h1 (or h (sdl2:surface-height surface)))
         (image (make-instance 'image :name name :x x :y y :w w1 :h h1
                                      :surface surface
                                      :renderer (controlcl-renderer *controlcl*))))
    (push image (controlcl-controllers *controlcl*))
    image))

(defun controlcl-mouse-over (&key x y)
  (dolist (ctrl (controlcl-controllers *controlcl*))
    (setf (controller-mouse-over ctrl) (controller-mouse-over-p ctrl :x x :y y))))
