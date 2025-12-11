(defpackage #:advent-2025-day-3-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-3))

(in-package #:advent-2025-day-3-test)

(def-suite* advent-2025-day-3-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-3-test-system)

(test largest-joltage
  (is (= 98 (advent-2025-day-3::largest-joltage 2 "987654321111111")))
  (is (= 89 (advent-2025-day-3::largest-joltage 2 "811111111111119")))
  (is (= 78 (advent-2025-day-3::largest-joltage 2 "234234234234278")))
  (is (= 92 (advent-2025-day-3::largest-joltage 2 "818181911112111"))))

(test total-output-joltage
  (is (= 357 (advent-2025-day-3::total-output-joltage 2 (list "987654321111111"
                                                              "811111111111119"
                                                              "234234234234278"
                                                              "818181911112111")))))
(test part-1
  (is (= 17766 (advent-2025-day-3::solution-part-1))))

(test part-2
  (is (= 176582889354075 (advent-2025-day-3::solution-part-2))))
