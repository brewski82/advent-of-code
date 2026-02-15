(defpackage #:advent-2025-day-12-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-12)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind))

(in-package #:advent-2025-day-12-test)

(def-suite* advent-2025-day-12-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-12-test-system)

(test part-1
  (is (= 526 (advent-2025-day-12::day-12-part-1))))
