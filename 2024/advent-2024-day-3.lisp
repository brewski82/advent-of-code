(defpackage #:advent-2024-day-3
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:arrow-macros #:->> #:->))

(in-package #:advent-2024-day-3)

(defun day-3-part-1 ()
  (bind ((input (uiop:read-file-string "~/Downloads/input-3"))
         (answer 0))
    (do-register-groups ((#'parse-integer x) (#'parse-integer y))
        ("mul\\((\\d{1,3}),(\\d{1,3})\\)" input)
      (incf answer (* x y)))
    answer))

(defun day-3-part-2 ()
  (bind ((input (uiop:read-file-string "~/Downloads/input-3"))
         (enabled-p t)
         (answer 0))
    (do-register-groups (op (#'parse-integer x) (#'parse-integer y))
        ("(mul\\((\\d{1,3}),(\\d{1,3})\\)|(do\\(\\))|(don't\\(\\)))" input)
      (cond ((string-equal op "don't()") (setf enabled-p nil))
            ((string-equal op "do()") (setf enabled-p t))
            (enabled-p (incf answer (* x y)))))
    answer))
