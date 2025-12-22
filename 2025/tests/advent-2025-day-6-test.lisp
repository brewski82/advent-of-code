(defpackage #:advent-2025-day-6-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-6))

(in-package #:advent-2025-day-6-test)

(def-suite* advent-2025-day-6-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-6-test-system)


(defparameter *test* (list "123 328  51 64 "
                           " 45 64  387 23 "
                           "  6 98  215 314"
                           "*   +   *   +  "))

(test process-part-1
  (is (= 4277556 (advent-2025-day-6::process-part-1 *test*))))

(test part-1
  (is (= 4364617236318 (advent-2025-day-6::part-1))))

(test process-part-2
  (is (= 3263827 (advent-2025-day-6::process-part-2 *test*))))

(test part-2
  (is (= 9077004354241 (advent-2025-day-6::part-2))))
