(asdf:defsystem controlcl
  :version "0.0.1"
  :depends-on ("sdl2" "cl-sdl2-hershey" "alexandria")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "utils")
                             (:file "theme")
                             (:file "controlcl"))))
  :in-order-to ((test-op (test-op :controlcl/tests))))

(asdf:defsystem controlcl/tests
  :version "0.1"
  :serial t
  :depends-on ("controlcl" "rove")
  :components ((:module "t"
                :components ((:file "tests-controlcl"))))
  :perform (test-op (op c) (symbol-call :rove :run c :style :spec)))

(asdf:defsystem controlcl/examples
  :version "0.1"
  :serial t
  :depends-on ("controlcl")
  :components ((:module "examples"
                :components ((:file "ex-theme")))))
