(asdf:defsystem "advent-2025"
  :depends-on ("alexandria"
               "transducers"
               "metabang-bind"
               "serapeum"
               "cl-ppcre"
               "fset"
               "str"
               "advent-utils")
  :in-order-to ((test-op (test-op "advent-2025/test")))
  :components ((:file "package")
               (:file "advent-2025-day-1")
               (:file "advent-2025-day-2")
               (:file "advent-2025-day-3")
               (:file "advent-2025-day-4")
               (:file "advent-2025-day-5")
               (:file "advent-2025-day-6")
               (:file "advent-2025-day-7")
               (:file "advent-2025-day-8")
               (:file "advent-2025-day-9")
               (:file "advent-2025-day-10")))

(asdf:defsystem "advent-2025/test"
  :depends-on ("fiveam"
               "advent-2025")
  :components ((:module "tests"
                        :serial t
                :components ((:file "suite")
                             (:file "advent-2025-day-1-test")
                             (:file "advent-2025-day-2-test")
                             (:file "advent-2025-day-3-test")
                             (:file "advent-2025-day-4-test")
                             (:file "advent-2025-day-5-test")
                             (:file "advent-2025-day-6-test")
                             (:file "advent-2025-day-7-test")
                             (:file "advent-2025-day-8-test")
                             (:file "advent-2025-day-9-test")
                             (:file "advent-2025-day-10-test"))))
  :perform (asdf:test-op
            (op c)
            (uiop:symbol-call :advent-2025-suite :run-tests)))
