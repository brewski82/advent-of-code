(defpackage #:advent-2025-day-2-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-2))

(in-package #:advent-2025-day-2-test)

(def-suite* advent-2025-day-2-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-2-test-system)

(test sum-invalid-ids
  (is (= 33 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 11 :end 22))))
  (is (= 99 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 95 :end 115))))
  (is (= 1010 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 998 :end 1012))))
  (is (= 1188511885 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 1188511880 :end 1188511890))))
  (is (= 222222 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 222220 :end 222224))))
  (is (= 0 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 1698522 :end 1698528))))
  (is (= 446446 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 446443 :end 446449))))
  (is (= 38593859 (advent-2025-day-2::sum-invalid-ids (advent-2025-day-2::make-range :start 38593856 :end 38593862)))))

(defparameter *test-input* "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(test process-part-1
  (is (= 1227775554 (advent-2025-day-2::process-part-1 *test-input*))))

(defun call-sum-invalid-ids-part-2 (start end)
  (advent-2025-day-2::sum-invalid-ids-part-2 (advent-2025-day-2::make-range :start start
                                                                            :end end)))
(test sum-invalid-ids-part-2
  (is (= 33 (call-sum-invalid-ids-part-2  11  22)))
  (is (= 210 (call-sum-invalid-ids-part-2  95  115)))
  (is (= 2009 (call-sum-invalid-ids-part-2  998  1012)))
  (is (= 1188511885 (call-sum-invalid-ids-part-2  1188511880  1188511890)))
  (is (= 222222 (call-sum-invalid-ids-part-2  222220  222224)))
  (is (= 0 (call-sum-invalid-ids-part-2  1698522  1698528)))
  (is (= 446446 (call-sum-invalid-ids-part-2  446443  446449)))
  (is (= 38593859 (call-sum-invalid-ids-part-2  38593856  38593862)))
  (is (= 565656 (call-sum-invalid-ids-part-2  565653 565659)))
  (is (= 824824824 (call-sum-invalid-ids-part-2 824824821 824824827)))
  (is (= 2121212121 (call-sum-invalid-ids-part-2 2121212118 2121212124))))

(test part-2
  (is (= 14582313461 (advent-2025-day-2::solution-part-2))))
