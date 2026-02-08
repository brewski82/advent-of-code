(defpackage #:advent-2025-day-11-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-11)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind))

(in-package #:advent-2025-day-11-test)

(def-suite* advent-2025-day-11-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-11-test-system)

(defparameter *test* (list "aaa: you hhh"
                           "you: bbb ccc"
                           "bbb: ddd eee"
                           "ccc: ddd eee fff"
                           "ddd: ggg"
                           "eee: out"
                           "fff: out"
                           "ggg: out"
                           "hhh: ccc fff iii"
                           "iii: out"))

(test process-part-1
  (is (= 5 (advent-2025-day-11::process-part-1 *test*))))

(defparameter *test2* (list "svr: aaa bbb"
                            "aaa: fft"
                            "fft: ccc"
                            "bbb: tty"
                            "tty: ccc"
                            "ccc: ddd eee"
                            "ddd: hub"
                            "hub: fff"
                            "eee: dac"
                            "dac: fff"
                            "fff: ggg hhh"
                            "ggg: out"
                            "hhh: out"))

(test process-part-2
  (is (= 2 (advent-2025-day-11::process-part-2 *test2*))))
