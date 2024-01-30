
;; internal tests
(in-package :controlcl/tests)



(deftest v-factor-tests
  (testing "v-factor"
    (ok (= (v-factor 50 0 100) 1/2))
    (ok (= (v-factor 100 0 100) 1))
    (ok (= (v-factor 0 0 100) 0))))
