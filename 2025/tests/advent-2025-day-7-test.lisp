(defpackage #:advent-2025-day-7-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-7))

(in-package #:advent-2025-day-7-test)

(def-suite* advent-2025-day-7-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-7-test-system)

(defparameter *test* `(
                       ".......S......."
                       "..............."
                       ".......^......."
                       "..............."
                       "......^.^......"
                       "..............."
                       ".....^.^.^....."
                       "..............."
                       "....^.^...^...."
                       "..............."
                       "...^.^...^.^..."
                       "..............."
                       "..^...^.....^.."
                       "..............."
                       ".^.^.^.^.^...^."
                       "..............."))

(test process-part-1 (is (= 21 (advent-2025-day-7::process-part-1 *test*))))

(test day-7-part-1
  (is (= 1690 (advent-2025-day-7::day-7-part-1))))

(test day-7-part-2
  (is (= 221371496188107 (advent-2025-day-7::day-7-part-2))))
