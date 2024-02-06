(in-package :controlcl)

(defvar *controlcl* nil)


;; CONTROLCL

(defclass controlcl (event-emitter)
  ((controllers :initarg :controllers :accessor controlcl-controllers :initform nil)
   (renderer :initarg :renderer :accessor controlcl-renderer :initform nil)))

(defmacro with-controlcl (renderer &body body)
  `(let ((*controlcl* (make-instance 'controlcl :renderer ,renderer)))
     (controlcl-init)
     (sdl2-image:init '(:png))
     (unwind-protect
          (progn ,@body))
     (sdl2-image:quit)))


(defmacro do-ctrls ((ctrl controlcl) &body body)
  `(dolist (,ctrl (alist-values (controlcl-controllers ,controlcl)))
     ,@body))

(defun controlcl-draw ()
  (do-ctrls (ctrl *controlcl*)
    (controller-draw ctrl)))

(defun controlcl-show ()
  (do-ctrls (ctrl *controlcl*)
    (controller-show ctrl)))

(defun controlcl-hide ()
  (do-ctrls (ctrl *controlcl*)
    (controller-hide ctrl)))

(defun controlcl-get-ctrl (id)
  "get the CTRL from *CONTROLCL* with the id"
  (aget (controlcl-controllers *controlcl*) id))

(defgeneric controlcl-emit-event (event))

(defmethod controlcl-emit-event ((event event))
  (emit-event event))

(defun controlcl-add-bang (&key id name x y (w 20) (h 20) sticky)
  (let ((bang (make-instance 'bang :id id
                                   :name name :x x :y y :w w :h h
                                   :sticky sticky
                                   :renderer (controlcl-renderer *controlcl*))))
    (setf (aget (controlcl-controllers *controlcl*) id) bang)
    bang))

(defun controlcl-add-checkbox (&key id name x y (w 20) (h 20) sticky)
  (let ((checkbox (make-instance 'checkbox :id id
                                           :name name :x x :y y :w w :h h
                                           :sticky sticky
                                           :renderer (controlcl-renderer *controlcl*))))
    (setf (aget (controlcl-controllers *controlcl*) id) checkbox)
    checkbox))


(defun controlcl-add-slider (&key id name x y (w 100) (h 20) value  min-value max-value sticky)
  (assert (<= min-value value max-value)
          (min-value value max-value)
          "It must be T that MIN-VALUE <= VALUE <= MAX-VALUE  (~S <= ~S <= ~S)." min-value value max-value)
  (let ((slider (make-instance 'slider :id id
                                       :name name
                                       :x x :y y :w w :h h
                                       :value value
                                       :min-value min-value :max-value max-value
                                       :sticky sticky
                                       :renderer (controlcl-renderer *controlcl*))))
    (setf (aget (controlcl-controllers *controlcl*) id) slider)
    slider))

(defun controlcl-add-image (&key id name x y w h sticky rel-image-path)
  (let* ((image-path (asdf:system-relative-pathname :controlcl rel-image-path))
         (surface (sdl2-image:load-image image-path))
         (w1 (or w (sdl2:surface-width surface)))
         (h1 (or h (sdl2:surface-height surface)))
         (image (make-instance 'image :id id
                                      :name name :x x :y y :w w1 :h h1
                                      :sticky sticky
                                      :surface surface
                                      :renderer (controlcl-renderer *controlcl*))))
    (setf (aget (controlcl-controllers *controlcl*) id) image)
    image))

(defun controlcl-mouse-over (&key x y)
  (do-ctrls (ctrl *controlcl*)
    (setf (controller-mouse-over ctrl) (controller-mouse-over-p ctrl :x x :y y))))

(defun controlcl-move-to (&key x y)
  (do-ctrls (ctrl *controlcl*)
    (controller-move-to ctrl :x x :y y)))


;; EVENTS DISPATCHER
;;
(defun emit-event (event)
  (with-slots (id source target) event
    (log4cl:log-info "emit-event ~S : ~S -> ~S" id source target)
    (if (null target)
        (do-ctrls (ctrl *controlcl*)
          (unless (eq (controller-id ctrl) source)
            (emit :controlcl-event ctrl ctrl event))) ; broadcast
        (let ((ctrl (controlcl-get-ctrl target)))
          (unless (eq (controller-id ctrl) source)
            (emit :controlcl-event ctrl ctrl event))))))
