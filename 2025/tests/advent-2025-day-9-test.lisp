(defpackage #:advent-2025-day-9-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-9)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind))

(in-package #:advent-2025-day-9-test)

(def-suite* advent-2025-day-9-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-9-test-system)

(defparameter *test* (list "7,1"
                           "11,1"
                           "11,7"
                           "9,7"
                           "9,5"
                           "2,5"
                           "2,3"
                           "7,3"))

(test process-part-1
  (is (= 50 (advent-2025-day-9::process-part-1 *test*))))

(test part-1
  (is (= 4749672288 (advent-2025-day-9::day-9-part-1))))

(test process-part-2
  (is (= 24 (advent-2025-day-9::process-part-2 *test*))))

(test part-2
  (is (= 1479665889
         (advent-2025-day-9::day-9-part-2))))
