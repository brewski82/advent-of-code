(defpackage #:advent-2025-day-1-test
  (:use #:common-lisp #:fiveam #:advent-2025-day-1))

(in-package #:advent-2025-day-1-test)

(def-suite* advent-2025-day-1-test-system :in advent-2025-suite::advent-2025-test-system)

(in-suite advent-2025-day-1-test-system)

(test part-1
  (is (= 1055 (advent-2025-day-1::solution-part-1))))

(defun password-for-calculate-zero-clicks (dial-state distance)
  (advent-2025-day-1::dial-state-password
   (advent-2025-day-1::next-dial-state-part-2
    (advent-2025-day-1::make-dial-state :value dial-state :password 0) distance)))

(test calculate-zero-clicks
  (is (= 0 (password-for-calculate-zero-clicks 1 1)))
  (is (= 0 (password-for-calculate-zero-clicks 0 5)))
  (is (= 0 (password-for-calculate-zero-clicks -10 5)))
  (is (= 0 (password-for-calculate-zero-clicks -10 -5)))
  (is (= 0 (password-for-calculate-zero-clicks 0 -99)))
  (is (= 0 (password-for-calculate-zero-clicks -1 -98)))
  (is (= 0 (password-for-calculate-zero-clicks 99 0)))
  (is (= 0 (password-for-calculate-zero-clicks -99 0)))
  (is (= 0 (password-for-calculate-zero-clicks -98 -1)))
  (is (= 0 (password-for-calculate-zero-clicks 98 1)))
  (is (= 1 (password-for-calculate-zero-clicks 0 100)))
  (is (= 1 (password-for-calculate-zero-clicks 1 99)))
  (is (= 1 (password-for-calculate-zero-clicks 1 150)))
  (is (= 2 (password-for-calculate-zero-clicks 0 200)))
  (is (= 1 (password-for-calculate-zero-clicks 5 -10)))
  (is (= 1 (password-for-calculate-zero-clicks 5 -5)))
  (is (= 1 (password-for-calculate-zero-clicks 5 95)))
  (is (= 1 (password-for-calculate-zero-clicks 5 -100)))
  (is (= 2 (password-for-calculate-zero-clicks 5 -200)))
  (is (= 2 (password-for-calculate-zero-clicks 5 -105)))
  (is (= 3 (password-for-calculate-zero-clicks 5 -250)))
  (is (= 2 (password-for-calculate-zero-clicks 5 250))))

(test part-2
  (is (= 6386 (advent-2025-day-1::solution-part-2))))
