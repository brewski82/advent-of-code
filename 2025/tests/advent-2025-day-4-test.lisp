(defpackage #:advent-2025-day-4-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-4))

(in-package #:advent-2025-day-4-test)

(def-suite* advent-2025-day-4-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-4-test-system)

(test count-accessible
  (is (= 13
         (advent-2025-day-4::count-accessible

          (advent-utils:to-char-array (list "..@@.@@@@."
                                            "@@@.@.@.@@"
                                            "@@@@@.@.@@"
                                            "@.@@@@..@."
                                            "@@.@@@@.@@"
                                            ".@@@@@@@.@"
                                            ".@.@.@.@@@"
                                            "@.@@@.@@@@"
                                            ".@@@@@@@@."
                                            "@.@.@@@.@."))))))

(test part-1
  (is (= 1356 (advent-2025-day-4::part-1))))

(test count-accessible-part-2
  (is (= 43
         (advent-2025-day-4::count-accessible-part-2

          (advent-utils:to-char-array (list "..@@.@@@@."
                                            "@@@.@.@.@@"
                                            "@@@@@.@.@@"
                                            "@.@@@@..@."
                                            "@@.@@@@.@@"
                                            ".@@@@@@@.@"
                                            ".@.@.@.@@@"
                                            "@.@@@.@@@@"
                                            ".@@@@@@@@."
                                            "@.@.@@@.@."))))))

(test part-2
  (is (= 8713 (advent-2025-day-4::part-2))))
