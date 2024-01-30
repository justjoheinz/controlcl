(in-package :controlcl/tests)

(defvar *slider* nil)

(defhook setup :before
  (setq *slider* (make-instance 'slider :value 50 :min-value 0 :max-value 100)))

(deftest slider-controller
  (testing "Slider is correctly initialised"
    (ok (= (slider-value *slider*) 50)))

  (testing "Setter for value, min-value, max-value. Value cannot be set out of bounds."
    (ok (= (setf (slider-value *slider*) -1) 0))
    (ok (= (setf (slider-value *slider*) 101) 100))
    (ok (= (setf (slider-value *slider*) 50) 50)))

  (testing "Instantiation via controlcl-add-slider"
    ;; since value is out of bounds
    (ok (signals  (controlcl-add-slider :name "asdf" :x 10 :y 10 :value -5 :min-value 0 :max-value 100)))))
