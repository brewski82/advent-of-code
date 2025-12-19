(defpackage #:advent-2025-day-5-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-5))

(in-package #:advent-2025-day-5-test)

(def-suite* advent-2025-day-5-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-5-test-system)

(defparameter *test-input-part-1* (list "3-5"
                                        "10-14"
                                        "16-20"
                                        "12-18"
                                        ""
                                        "1"
                                        "5"
                                        "8"
                                        "11"
                                        "17"
                                        "32"))

(test process-part-1
      (is (= 3 (advent-2025-day-5::process-part-1 *test-input-part-1*))))

(test part-1
  (is (= 525 (advent-2025-day-5::part-1))))

(test process-part-2
      (is (= 14 (advent-2025-day-5::process-part-2 *test-input-part-1*))))

(test part-2
  (is (= 333892124923577 (advent-2025-day-5::part-2))))
