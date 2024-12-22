(asdf:defsystem "advent-2024"
  :depends-on ("alexandria"
               "arrow-macros"
               "str"
               "metabang-bind"
               "iterate"
               "serapeum"
               "cl-ppcre"
               "cl-graph"
               "array-operations"
               "fset"
               "nclasses"
               "access"
               "bordeaux-threads"
               "damn-fast-priority-queue")
  :components ((:file "advent-2024-day-1")
               (:file "advent-2024-day-2")
               (:file "advent-2024-day-3")
               (:file "advent-2024-day-4")
               (:file "advent-2024-day-5")
               (:file "advent-2024-day-6")
               (:file "advent-2024-day-7")
               (:file "advent-2024-day-8")
               (:file "advent-2024-day-9")
               (:file "advent-2024-day-10")
               (:file "advent-2024-day-11")
               (:file "advent-2024-day-12")
               (:file "advent-2024-day-13")
               (:file "advent-2024-day-14")
               (:file "advent-2024-day-15")
               (:file "advent-2024-day-16")))
