(defpackage #:advent-2024-day-4
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:arrow-macros #:->> #:->))

(in-package #:advent-2024-day-4)

(defun day-4-part-1 ()
  (-> (read-file-to-matrix "~/Downloads/input-4")
      (make-matrix)
      (perform-search)))

(defun perform-search (matrix)
  (loop with count = 0
        with (row-count column-count) = (array-dimensions matrix)
        for r from 0 below row-count do
          (loop
            for c from 0 below column-count
            for horizontal = (collect-horizontal matrix r c)
            for vertical = (collect-vertical matrix r c)
            for right-diagonal = (collect-right-diagonal matrix r c)
            for left-diagonal = (collect-left-diagonal matrix r c)
            when (is-xmas-p horizontal)
              do (incf count)
            when (is-xmas-p vertical)
              do (incf count)
            when (is-xmas-p right-diagonal)
              do (incf count)
            when (is-xmas-p left-diagonal)
              do (incf count))
        finally (return count)))

(defun is-xmas-p (string)
  (or (string-equal "xmas" string)
      (string-equal "samx" string)))

(defun collect-horizontal (matrix r c)
  (bind ((col-count (array-dimension matrix 1)))
    (when (<= c (- col-count 4))
      (serapeum:concat (loop for i from c below (+ c 4)
                             collect (aref matrix r i))))))

(defun collect-vertical (matrix r c)
  (bind ((row-count (array-dimension matrix 0)))
    (when (<= r (- row-count 4))
      (serapeum:concat (loop for i from r below (+ r 4)
                             collect (aref matrix i c))))))

(defun collect-right-diagonal (matrix r c)
  (bind (((row-count column-count) (array-dimensions matrix)))
    (when (and (<= r (- row-count 4))
               (<= c (- column-count 4)))
      (serapeum:concat (loop for i from r below (+ r 4)
                             for j from c below (+ c 4)
                             collect (aref matrix i j))))))

(defun collect-left-diagonal (matrix r c)
  (bind ((row-count (array-dimension matrix 0)))
    (when (and (<= r (- row-count 4))
               (>= c 3))
      (serapeum:concat (loop for i from r below (+ r 4)
                             for j from c above (- c 4)
                             collect (aref matrix i j))))))

(defun make-matrix (lines)
  (make-array (list (length lines) (length (first lines)))
              :initial-contents lines))

(defun read-file-into-lines (path)
  (with-open-file (in path)
    (loop with lines = (list)
          for line = (read-line in nil nil)
          while line
          do (setf lines (serapeum:append1 lines line))
          finally (return lines))))

(defun day-4-part-2 ()
  (-> (read-file-to-matrix "~/Downloads/input-4")
      (make-matrix)
      (perform-search-part-2)))

(defun perform-search-part-2 (matrix)
  (loop with count = 0
        with (row-count column-count) = (array-dimensions matrix)
        for r from 1 below (1- row-count) do
          (loop for c from 1 below (1- column-count)
                for (left-diagonal right-diagonal) = (collect-x matrix r c)
                when (and (is-mas-p left-diagonal)
                          (is-mas-p right-diagonal))
                  do (incf count))
        finally (return count)))

(defun collect-x (matrix r c)
  (list (serapeum:concat (list (aref matrix (1- r) (1- c))
                               (aref matrix r c)
                               (aref matrix (1+ r) (1+ c))))
        (serapeum:concat (list (aref matrix (1- r) (1+ c))
                               (aref matrix r c)
                               (aref matrix (1+ r) (1- c))))))

(defun is-mas-p (string)
  (or (string-equal "mas" string)
      (string-equal "sam" string)))
