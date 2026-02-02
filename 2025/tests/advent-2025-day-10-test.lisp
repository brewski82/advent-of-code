(defpackage #:advent-2025-day-10-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-10)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind))

(in-package #:advent-2025-day-10-test)

(def-suite* advent-2025-day-10-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-10-test-system)

(defparameter *test* (list
                      "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                      "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                      "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"))

(test process-part-1
  (is (= 7 (advent-2025-day-10::process-part-1 *test*))))

;; (test part-1
;;   (is (= 0 (advent-2025-day-10::day-10-part-1))))

(test process-part-2
  (is (advent-2025-day-10::process-part-2 *test*)))

;; (test part-2
;;   (is (= 0 (advent-2025-day-10::day-10-part-2))))
