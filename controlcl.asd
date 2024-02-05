(asdf:defsystem controlcl
  :version "0.0.1"
  :depends-on ("sdl2"
               "cl-sdl2-hershey"
               "sdl2-image"
               "event-emitter"
               "log4cl"
               "alexandria"
               "assoc-utils")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "utils")
                             (:file "theme")
                             (:file "events")
                             (:file "ctrl")
                             (:file "bang")
                             (:file "slider")
                             (:file "checkbox")
                             (:file "image")
                             (:file "controlcl"))))
  :in-order-to ((test-op (test-op :controlcl/tests))))

(asdf:defsystem controlcl/tests
  :version "0.1"
  :serial t
  :depends-on ("controlcl" "rove")
  :components ((:module "t"
                :components ((:file "package")
                             (:file "tests-controlcl")
                             (:file "tests-slider"))))
  :perform (test-op (op c) (symbol-call :rove :run c :style :dot)))

(asdf:defsystem controlcl/examples
  :version "0.1"
  :depends-on ("controlcl" "closer-mop")
  ;; closer-mop is just required to explore on the repl
  :components ((:module "examples"
                :components ((:file "ex-theme")
                             (:file "ex-colors")
                             (:file "ex-ctrl")))))
