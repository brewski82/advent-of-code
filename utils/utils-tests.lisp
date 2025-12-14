(defpackage #:advent-utils-suite
  (:use #:cl
        #:fiveam)
  (:export #:run-tests))

(in-package #:advent-utils-suite)

(def-suite advent-utils-test-system)

(defun run-tests ()
  (fiveam:run! 'advent-utils-test-system))

(in-suite advent-utils-test-system)

(test mapc-array
  (let ((result 0))
    (advent-utils:mapc-array (lambda (value &rest indexes)
                               (declare (ignore indexes))
                               (incf result value))
                             #2A((10 20 30)
                                 (40 50 60)))
    (is (= 210 result))))


(test reduce-array
  (is (= 210 (advent-utils:reduce-array (lambda (acc value &rest indexes)
                                          (declare (ignore indexes))
                                        (+ acc value))
                                      #2A((10 20 30)
                                          (40 50 60))))))

(test neighbors
  (is (equalp (list 10 20 30 60 40)
              (advent-utils:neighbors #2A((10 20 30)
                                          (40 50 60))
                                      1 1))))
