(in-package :controlcl)

(defclass event ()
  ((id :reader event-id :initarg :id :initform nil :allocation :class)
   (source :initarg :source :accessor event-source :initform nil)
   (target :initarg :target :accessor event-target :initform nil))
  (:documentation "An event is a message sent from one object to another.
If the target is nil the event is considered to be a broadcast event."))

(defclass event-mouse (event)
  ((id :initform :event-mouse)
   (x :initarg :x :accessor event-mouse-x)
   (y :initarg :y :accessor event-mouse-y)
   (button :initarg :button :accessor event-mouse-button :initform nil)
   (modifiers :initarg :modifiers :accessor event-mouse-modifiers :initform nil))
  (:documentation "A mouse event is an event that is generated by the mouse."))

(defclass event-mouse-clicked (event)
  ((id :initform :event-mouse-clicked)
   (x :initarg :x :accessor event-mouse-clicked-x)
   (y :initarg :y :accessor event-mouse-clicked-y))
  (:documentation "A mouse clicked event is an event that is generated by the mouse when a button is clicked."))

(defclass event-key (event)
  ((id :initform :event-key)
   (key :initarg :key :accessor event-key-key)
   (modifiers :initarg :modifiers :accessor event-key-modifiers))
  (:documentation "A key event is an event that is generated by the keyboard."))

(defclass event-value (event)
  ((id :initform :event-value)
   (old :initarg :old :accessor event-value-old)
   (new :initarg :new :accessor event-value-new))
  (:documentation "A value event is an event that is generated by a value changing."))
