(in-package :controlcl)

;; CONTROLLER

(defclass controller (event-emitter)
  ((x :initarg :x
      :accessor controller-x :initform nil
      :documentation "x position of the controller")
   (y :initarg :y
      :accessor controller-y :initform nil
      :documentation "y position of the controller")
   (w :initarg :w
      :accessor controller-w :initform nil
      :documentation "width of the controller")
   (h :initarg :h
      :accessor controller-h :initform nil
      :documentation "height of the controller")
   (value :initarg :value
          :accessor controller-value :initform nil
          :documentation "value of the controller")
   (id :initarg :id :accessor controller-id
       :initform (error "An id must be provided to a controller")
       :documentation "id of the controller.

IDs are symbols and have to be unique across all controllers.
They are used to identify the source and targets of events.")
   (visible :initarg :visible :accessor controller-visible
            :initform t
            :documentation "visible flag of the controller.

Visible controllers are drawn by the renderer.")
   (active :initarg :active :accessor controller-active
           :initform t
           :documentation "active flag of the controller.

Active controllers are currently selected and can decide to render or behave differently.")
   (sticky :initarg :sticky :accessor controller-sticky
           :initform nil
           :documentation "sticky flag of the controller.

Sticky controllers are not hidden when the VISIBLE flag is set to NIL.")
   (mouse-over :initarg :mouse-over :accessor controller-mouse-over
               :initform nil
               :documentation "mouse-over flag of the controller.

Indicates that the mouse hovers over the controller.")
   (renderer :initarg :renderer :accessor controller-renderer
             :initform nil
             :documentation "The renderer associated with the controller."))

  (:documentation "A controller is a visual element that can be drawn and interacted with."))

;; check the types after instantiation
(defmethod initialize-instance :after ((ctrl controller) &key)
  (with-slots (x y w h id renderer) ctrl
    (unless (and (integerp x) (integerp y) (integerp w) (integerp h))
      (error "x,y,w,h must be integers"))
    (unless (symbolp id)
      (error "id must be a symbol"))
    (unless (renderer-p renderer)
      (error "renderer must be a sdl2 renderer"))
    (on :controlcl-event ctrl (lambda (ctrl evt)
                                (log4cl:log-info "event ~S~&controller ~S~&"
                                                 (event-id evt)
                                                 (controller-id ctrl))
                                (on-event ctrl evt))))
  )

(defmethod (setf controller-value) :around (new-value (ctrl controller))
  (let ((old-value (controller-value ctrl)))
    (when (next-method-p)
      (when (not (equal old-value new-value))
        (let ((event (make-instance 'event-value
                                    :source (controller-id ctrl)
                                    :old old-value
                                    :new new-value)))
          (prog1
              (call-next-method)
            (controlcl-emit-event event)))))))

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
  (:documentation "Hide the controller.
By default it sets the VISIBLE flag to NIL, unless the STICKY flag for the controller is set.")
  (:method ((ctrl controller))
    (setf (controller-visible ctrl) (or nil (controller-sticky ctrl)))))

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
