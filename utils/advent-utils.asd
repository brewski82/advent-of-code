(asdf:defsystem #:advent-utils
                :depends-on ("alexandria"
                             "transducers"
                             "metabang-bind"
                             "serapeum"
                             "cl-ppcre"
                             "fset"
                             "str")
  :in-order-to ((test-op (test-op "advent-utils/test")))
  :components ((:file "utils")))

(asdf:defsystem "advent-utils/test"
  :depends-on ("fiveam"
               "advent-utils")
  :components ((:file "utils-tests"))
  :perform (asdf:test-op
            (op c)
            (uiop:symbol-call :advent-utils-suite :run-tests)))
