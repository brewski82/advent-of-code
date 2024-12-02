(defpackage #:advent-2024-day-2
  (:use #:common-lisp #:alexandria)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:arrow-macros #:->> #:->))

(in-package #:advent-2024-day-2)

(defun day-2-part-1 ()
  (with-open-file (in "~/Downloads/input-2")
    (loop for line = (read-line in nil nil)
          for report = (parse-report line)
          while line
          counting (safe-report-p report))))

(defun parse-report (string)
  (mapcar #'parse-integer
          (str:split " " string :omit-nulls t)))

(defun safe-report-p (report)
  (bind ((differences (rest (serapeum:deltas report)))
         (signums (mapcar #'signum differences))
         (abs-differences (mapcar #'abs differences)))
    (and (serapeum:same #'identity signums)
         (every (lambda (x) (and (>= x 1)
                                 (<= x 3)))
                abs-differences))))

(defun day-2-part-2 ()
  (with-open-file (in "~/Downloads/input-2")
    (loop for line = (read-line in nil nil)
          for report = (parse-report line)
          while line
          counting (safe-report-part-2-p report))))

(defun safe-report-part-2-p (report)
  (when (safe-report-p report)
    (return-from safe-report-part-2-p t))
  ;; Check every sublist to see if it is safe. Horribly inefficent :)
  (loop for i from 0 below (length report)
        for subreport = (serapeum:splice-seq report :start i :end (1+ i))
        when (safe-report-p subreport)
          do (return t)
        finally (return nil)))
