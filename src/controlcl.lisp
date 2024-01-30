(in-package :controlcl)

(defvar *controlcl* nil)

;; CONTROLLER


(defclass controller ()
  ((x :initarg :x :accessor controller-x :initform nil)
   (y :initarg :y :accessor controller-y :initform nil)
   (w :initarg :w :accessor controller-w :initform nil)
   (h :initarg :h :accessor controller-h :initform nil)
   (value :initarg :value :accessor controller-value :initform nil)
   (id :initarg :id :accessor controller-id :initform nil)
   (visible :initarg :visible :accessor controller-visible :initform t)
   (mouse-over :initarg :mouse-over :accessor controller-mouse-over :initform nil)
   (renderer :initarg :renderer :accessor controller-renderer :initform nil)))

;; generic function definitions

(defgeneric controller-draw  (controller)
  (:documentation "draw the controller to the renderer"))

(defmethod controller-draw :around ((ctrl controller))
  (when (and (controller-visible ctrl) (next-method-p))
    (call-next-method)))

(defgeneric controller-show (controller)
  (:documentation "show the controller.
By default it sets the VISIBLE flag to T.
")
  (:method ((ctrl controller))
    (setf (controller-visible ctrl) t)))

(defgeneric controller-hide (controller)
  (:documentation "hide the controller.
By default it sets the VISIBLE flag to NIL.")
  (:method ((ctrl controller))
    (setf (controller-visible ctrl) nil)))

(defgeneric controller-move-to (controller &key x y)
  (:documentation "move the controller to the position x,y")
  (:method ((ctrl controller) &key x y)
    (setf (controller-x ctrl) x
          (controller-y ctrl) y)))

(defgeneric controller-mouse-over-p (controller &key x y)
  (:documentation "return T if the point x,y is over the controller")
  (:method ((ctrl controller) &key x y)
    (point-in-rect (controller-x ctrl) (controller-y ctrl)
                   (controller-w ctrl) (controller-h ctrl) x y)))

(defgeneric controller-set-mouse-over-color (controller)
  (:documentation "decide if the active or foreground color should be returned")
  (:method ((ctrl controller))
    (let ((color-key (if (controller-mouse-over ctrl) :active :fg)))
      (set-color-from-theme (controller-renderer ctrl) color-key))))

;; BANG

(defclass bang (controller)
  ((name :initarg :name :accessor bang-name :initform nil)
   (w :initform 20)
   (h :initform 20)))

(defmethod controller-draw ((ctrl bang))
  (with-slots (x y w h renderer) ctrl
    (controller-set-mouse-over-color ctrl)
    (sdl2:with-rects ((rect x y w h))
      (sdl2:render-fill-rect renderer rect))
    (set-color-from-theme renderer :caption)
    (with-font (*roman-plain-font* 0.6)
      (render-text renderer x (+ y h 10) (bang-name ctrl)))))

;; SLIDER

(defclass slider (controller)
  ((name :initarg :name :accessor slider-name :initform nil)
   (min-value :initarg :min-value :accessor slider-min-value :initform 0)
   (max-value :initarg :max-value :accessor slider-max-value :initform 100)
   (value :initarg :value :accessor slider-value :initform 0)
   (w :initform 100)
   (h :initform 20)))


(defmethod controller-draw ((ctrl slider))
  (with-slots (x y w h min-value max-value value renderer) ctrl
    (let* ((cv (clamp value min-value max-value))
           (v-factor (v-factor cv min-value max-value))
           (h2 (/ h 2)))
      (sdl2:with-rects ((rect-bg x y w h)
                        (rect-fg x y (floor (* w v-factor)) h))
        (set-color-from-theme renderer :bg)
        (sdl2:render-fill-rect renderer rect-bg)
        (controller-set-mouse-over-color ctrl)
        (sdl2:render-fill-rect renderer rect-fg))

      (with-font (*roman-plain-font* 0.6)
        (set-color-from-theme renderer :caption)
        (render-text renderer (+ x w 10) (+ y h2) (slider-name ctrl))
        (set-color-from-theme renderer :value)
        (render-text renderer (+ x 5 ) (+ y h2) (format nil "~a" cv))))))

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
