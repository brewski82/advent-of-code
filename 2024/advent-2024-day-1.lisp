(defpackage #:advent-2024-day-1
  (:use #:common-lisp #:alexandria)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:arrow-macros #:->> #:->))

(in-package #:advent-2024-day-1)

(defun day-1-part-1 ()
  (->> (read-file-into-sorted-lists "~/Downloads/input-1")
    (apply (curry #'mapcar (compose #'abs #'-)))
    (reduce #'+)))

(defun day-1-part-2 ()
  (bind (((list-1 list-2)
          (read-file-into-sorted-lists "~/Downloads/input-1"))
         (occurence-table (make-hash-table))
         (similarity (lambda (x) (* x (gethash x occurence-table 0)))))
    (mapc (lambda (x) (incf (gethash x occurence-table 0)))
          list-2)
    (->> list-1
      (mapcar similarity)
      (reduce #'+))))

(defun read-file-into-sorted-lists (path)
  (with-open-file (in path)
    (loop with list-1 = (list)
          with list-2 = (list)
          for line = (read-line in nil nil)
          while line
          do (bind (((list-1-element list-2-element)
                     (string-to-integer-pair line)))
               (push list-1-element list-1)
               (push list-2-element list-2))
          finally (return (list (sort list-1 #'<) (sort list-2 #'<))))))

(defun string-to-integer-pair (string)
  (mapcar #'parse-integer
          (str:split " " string :omit-nulls t)))
