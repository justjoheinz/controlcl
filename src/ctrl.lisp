(in-package :controlcl)

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


(defmethod (setf slider-value) (value (ctrl slider))
  (setf (controller-value ctrl)
        (clamp value
               (slider-min-value ctrl)
               (slider-max-value ctrl))))


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

