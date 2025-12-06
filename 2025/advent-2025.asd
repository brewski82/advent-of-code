(asdf:defsystem "advent-2025"
  :depends-on ("alexandria"
               "transducers"
               "metabang-bind"
               "serapeum"
               "cl-ppcre"
               "fset")
  :in-order-to ((test-op (test-op "advent-2025/test")))
  :components ((:file "package")
               (:file "advent-2025-day-1")))

(asdf:defsystem "advent-2025/test"
  :depends-on ("fiveam"
               "advent-2025")
  :components ((:module "tests"
                        :serial t
                :components ((:file "suite")
                             (:file "advent-2025-day-1-test"))))
  :perform (asdf:test-op
            (op c)
            (uiop:symbol-call :advent-2025-suite :run-tests)))
