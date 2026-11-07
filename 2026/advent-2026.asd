(asdf:defsystem "advent-2026"
  :depends-on ("alexandria"
               "transducers"
               "metabang-bind"
               "serapeum"
               "cl-ppcre"
               "fset"
               "str"
               "fare-memoization"
               "advent-utils")
  :in-order-to ((test-op (test-op "advent-2026/test")))
  :components ((:file "package")
               (:file "advent-2026-day-1")
               (:module "downloads")))

(asdf:defsystem "advent-2026/test"
  :depends-on ("fiveam"
               "advent-2026")
  :components ((:module "tests"
                        :serial t
                :components ((:file "suite"))))

  :perform (asdf:test-op (op c)
            (uiop:symbol-call :advent-2026-suite :run-tests)))
