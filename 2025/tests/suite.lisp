(defpackage #:advent-2025-suite
  (:use #:cl
        #:fiveam)
  (:export #:run-tests))

(in-package #:advent-2025-suite)

(def-suite advent-2025-test-system)

(defun run-tests ()
  (fiveam:run! 'advent-2025-test-system))
