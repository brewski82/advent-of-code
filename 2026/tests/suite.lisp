(defpackage #:advent-2026-suite
  (:use #:cl
        #:fiveam)
  (:export #:run-tests))

(in-package #:advent-2026-suite)

(def-suite advent-2026-test-system)

(defun run-tests ()
  (run! 'advent-2026-test-system))

(in-suite advent-2026-test-system)

(test smoke-test (is (= 1 1)))
