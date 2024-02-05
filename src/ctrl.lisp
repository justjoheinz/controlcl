(in-package :controlcl)

;; CONTROLLER

(defclass controller (event-emitter)
  ((x :initarg :x :accessor controller-x :initform nil)
   (y :initarg :y :accessor controller-y :initform nil)
   (w :initarg :w :accessor controller-w :initform nil)
   (h :initarg :h :accessor controller-h :initform nil)
   (value :initarg :value :accessor controller-value :initform nil)
   (id :initarg :id :accessor controller-id
       :initform (error "An id must be provided to a controller"))
   (visible :initarg :visible :accessor controller-visible
            :initform t)
   (active :initarg :active :accessor controller-active
           :initform t)
   (mouse-over :initarg :mouse-over :accessor controller-mouse-over
               :initform nil)
   (controlcl :initarg :controlcl :accessor controller-controlcl
              :initform nil)
   (renderer :initarg :renderer :accessor controller-renderer
             :initform nil)))

;; check the types after instantiation
(defmethod initialize-instance :after ((ctrl controller) &key)
  (with-slots (x y w h id renderer controlcl) ctrl
    (unless (and (integerp x) (integerp y) (integerp w) (integerp h))
      (error "x,y,w,h must be integers"))
    (unless (symbolp id)
      (error "id must be a symbol"))
    (unless (renderer-p renderer)
      (error "renderer must be a sdl2 renderer"))
    (unless (typep controlcl 'controlcl)
      (error "controlcl must be a controlcl instance"))
    (on :controlcl-event ctrl (lambda (ctrl evt)
                                (log4cl:log-info "event ~S~&controller ~S~&"
                                                 (event-id evt)
                                                 (controller-id ctrl))
                                (on-event ctrl evt))))
  )

(defmethod (setf controller-value) :around (new-value (ctrl controller))
  (let ((old-value (controller-value ctrl)))
    (when (next-method-p)
      (when (and (controller-controlcl ctrl) (not (equal old-value new-value)))
        (let ((event (make-instance 'event-value
                                    :source (controller-id ctrl)
                                    :old old-value
                                    :new new-value)))
          (controlcl-emit-event event)))
      (call-next-method))))

;; generic function definitions

(defgeneric on-event (controller event))

(defmethod on-event ((ctrl t) (evt t))
  nil)

(defmethod on-event :around ((ctrl controller) (evt event-mouse))
  (log4cl:log-info "on-event ~S~&controller ~S~&" (event-id evt) (controller-id ctrl))
  (with-slots (x y) evt
    (let* ((active (controller-active ctrl))
           (mouse-over (controller-mouse-over-p ctrl :x x :y y)))
      (setf (controller-mouse-over ctrl) (and active mouse-over))
      (when (next-method-p)
        (call-next-method)))
    )
  )

(defgeneric controller-draw (controller)
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
