(asdf:defsystem "advent-2024"
  :depends-on ("alexandria"
               "arrow-macros"
               "str"
               "metabang-bind"
               "iterate"
               "serapeum"
               "cl-ppcre")
  :components ((:file "advent-2024-day-1")
               (:file "advent-2024-day-2")
               (:file "advent-2024-day-3")
               (:file "advent-2024-day-4")))
