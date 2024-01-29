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
   (mouse-over :initarg :mouse-over :accessor controller-mouse-over :initform nil)))

;; generic function definitions

(defgeneric controller-draw  (controller renderer)
  (:documentation "draw the controller to the renderer"))

(defmethod controller-draw :around ((ctrl controller) renderer)
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


(defgeneric controller-mouse-over-p (controller x y)
  (:documentation "return T if the point x,y is over the controller")
  (:method ((ctrl controller) x y)
    (point-in-rect (controller-x ctrl) (controller-y ctrl)
                   (controller-w ctrl) (controller-h ctrl) x y)))


;; BANG

(defclass bang (controller)
  ((name :initarg :name :accessor bang-name :initform nil)
   (w :initform 20)
   (h :initform 20)))

(defmethod controller-draw ((ctrl bang) renderer)
  (with-slots (x y w h) ctrl
    (let ((color-key (if (controller-mouse-over ctrl) :active :fg)))
      (set-color-from-theme renderer color-key)
      (sdl2:with-rects ((rect x y w h))
        (sdl2:render-fill-rect renderer rect))
      (set-color-from-theme renderer :caption)
      (with-font *roman-plain-font* 0.6
        (render-text renderer x (+ y h 10) (bang-name ctrl))))))

;; SLIDER

(defclass slider (controller)
  ((name :initarg :name :accessor slider-name :initform nil)
   (min-value :initarg :min-value :accessor slider-min-value :initform 0)
   (max-value :initarg :max-value :accessor slider-max-value :initform 100)
   (value :initarg :value :accessor slider-value :initform 0)
   (w :initform 100)
   (h :initform 20)))


(defmethod controller-draw ((ctrl slider) renderer)
  (with-slots (x y w h min-value max-value value) ctrl
    (let* ((cv (alexandria:clamp value min-value max-value))
           (v-factor (v-factor cv min-value max-value))
           (color-key (if (controller-mouse-over ctrl) :active :fg))
           (h2 (/ h 2)))
      (sdl2:with-rects ((rect-bg x y w h)
                        (rect-fg x y (floor (* w v-factor)) h))
        (set-color-from-theme renderer :bg)
        (sdl2:render-fill-rect renderer rect-bg)
        (set-color-from-theme renderer color-key)
        (sdl2:render-fill-rect renderer rect-fg))

      (with-font *roman-plain-font* 0.6
        (set-color-from-theme renderer :caption)
        (render-text renderer (+ x w 10) (+ y h2) (slider-name ctrl))
        (set-color-from-theme renderer :value)
        (render-text renderer (+ x 5 ) (+ y h2) (format nil "~a" cv))))))


;; CONTROLCL



;; Main


(defun main ()
  (controlcc-init)
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

(defun test-me (x &key zap )
  (when zap
    (format t "zap")))
