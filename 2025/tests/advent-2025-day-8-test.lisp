(defpackage #:advent-2025-day-8-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-8))

(in-package #:advent-2025-day-8-test)

(def-suite* advent-2025-day-8-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-8-test-system)

(defparameter *test* (list "162,817,812"
                           "57,618,57"
                           "906,360,560"
                           "592,479,940"
                           "352,342,300"
                           "466,668,158"
                           "542,29,236"
                           "431,825,988"
                           "739,650,466"
                           "52,470,668"
                           "216,146,977"
                           "819,987,18"
                           "117,168,530"
                           "805,96,715"
                           "346,949,466"
                           "970,615,88"
                           "941,993,340"
                           "862,61,35"
                           "984,92,344"
                           "425,690,689"))

(test process-part-1
  (is (= 40 (advent-2025-day-8::process-part-1 10 *test*))))

(test part-1
  (is (= 123234 (advent-2025-day-8::day-8-part-1))))

(test process-part-2
  (is (= 25272 (advent-2025-day-8::process-part-2 *test*))))

(test part-2
  (is (= 9259958565 (advent-2025-day-8::day-8-part-2))))
