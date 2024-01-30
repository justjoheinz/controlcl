(in-package :controlcl/tests)

(defvar *slider* nil)


(defhook setup :before
  (setq *controlcl* (make-instance 'controlcl:controlcl))
  (setq *slider* (make-instance 'slider :id 'slider :name "test-slider"
                                        :x 10 :y 10 :w 100 :h 20
                                        :value 50 :min-value 0 :max-value 100
                                        :controlcl *controlcl*)))

(deftest slider-controller
  (testing "Slider is correctly initialised"
    (ok (= (slider-value *slider*) 50)))

  (testing "Setter for value, min-value, max-value. Value cannot be set out of bounds."
    (ok (= (setf (slider-value *slider*) -1) 0))
    (ok (= (setf (slider-value *slider*) 101) 100))
    (ok (= (setf (slider-value *slider*) 50) 50)))

  (testing "Instantiation via controlcl-add-slider"
    ;; since value is out of bounds
    (ok (signals  (controlcl-add-slider :id 'slider
                                        :name "asdf"
                                        :x 10 :y 10 :w 100 :h 20
                                        :value -5 :min-value 0 :max-value 100)))
    ;; id is missing
    (ok (signals (controlcl-add-slider  :name "asdf"
                                        :x 10 :y 10 :w 100 :h 20
                                        :value -5 :min-value 0 :max-value 100)))))
