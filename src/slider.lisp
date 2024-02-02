(in-package :controlcl)


;; SLIDER

(defclass slider (controller)
  ((name :initarg :name :accessor slider-name :initform nil)
   (min-value :initarg :min-value :accessor slider-min-value :initform 0)
   (max-value :initarg :max-value :accessor slider-max-value :initform 100)
   (value :initform 0)
   (w :initform (error "you must provide a width"))
   (h :initform (error "you must provide a height"))))


(defmethod (setf controller-value) (value (ctrl slider))
  (setf (slot-value ctrl 'value)
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
