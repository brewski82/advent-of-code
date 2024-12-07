(defpackage #:advent-2024-day-7
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:arrow-macros #:->> #:-> #:-> #:some->>))

(in-package #:advent-2024-day-7)

(defun day-7-part-1 ()
  (->> (read-input "~/Downloads/input-7")
    (remove-if-not #'possibly-true-p)
    (mapcar #'first)
    (reduce #'+)))

(defun read-input (path)
  "Reads PATH into a list of lines. Each line is a list with the test value as the
first element. The second element is a list of the operands."
  (->> (uiop:read-file-lines path)
    (mapcar (lambda (line)
              (bind (((result operands) (str:split-omit-nulls ":" line)))
                (list (parse-integer result)
                      (->> operands
                        (str:split-omit-nulls " ")
                        (mapcar #'parse-integer))))))))

(defun possibly-true-p (line)
  "Returns T if this line can produce the desired result."
  (can-equal-p (first line) 0 (cadr line)))

(defun can-equal-p (target result operands)
  "Recursive function to check a line. TARGET is the number we are aiming for,
RESULT is the running total, and OPERANDS is a list of the remaing operands."
  (if operands
      (or (can-equal-p target (+ result (first operands)) (rest operands))
          (can-equal-p target (* result (first operands)) (rest operands)))
      (= target result)))

(defun day-7-part-2 ()
  (->> (read-input "~/Downloads/input-7")
    (remove-if-not #'possibly-true-part-2-p)
    (mapcar #'first)
    (reduce #'+)))

(defun possibly-true-part-2-p (line)
  (can-equal-part-2-p (first line) 0 (cadr line)))

(defun can-equal-part-2-p (target result operands)
  (if operands
      (or (can-equal-part-2-p target (+ result (first operands)) (rest operands))
          (can-equal-part-2-p target (* result (first operands)) (rest operands))
          (can-equal-part-2-p target (concat-numbers result (first operands))
                              (rest operands)))
      (= target result)))

(defun concat-numbers (a b)
  (+ (* a (expt 10 (digit-count b)))
     b))

(defun digit-count (n)
  (length (write-to-string (abs n))))
