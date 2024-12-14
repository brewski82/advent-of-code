(defpackage #:advent-2024-day-10
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:serapeum #:append1 #:~>>)
  (:import-from #:nclasses #:define-class)
  (:import-from #:access #:access #:accesses #:set-access)
  (:import-from #:array-operations/utilities #:nested-loop))

(in-package #:advent-2024-day-10)

(defparameter *map* nil)
(defparameter *visited* nil)
(defparameter *score* 0)
(defparameter *part* nil)

(defun day-10-part-1 ()
  (setf *part* 1)
  (day-10))

(defun day-10-part-2 ()
  (setf *part* 2)
  (day-10))

(defun day-10 ()
  (read-input-into-matrix "~/Downloads/input-10")
  (init-visited-matrix)
  (setf *score* 0)
  (search-trails)
  *score*)

(defun read-input-into-matrix (path)
  (bind ((contents (uiop:read-file-lines path))
         (rows (length contents))
         (cols (length (first contents))))
    (setf *map*
          (make-array (list rows cols)
                      :initial-contents
                      (mapcar (curry #'map 'vector #'digit-char-p)
                              contents)))))

(defun init-visited-matrix ()
  (setf *visited*
        (make-array (array-dimensions *map*) :initial-element 0)))

(defun search-trails ()
  (nested-loop (row col) (array-dimensions *map*)
    (when (zerop (height row col))
      (init-visited-matrix)
      (search-trail row col))))

(defun search-trail (row col)
  (visit row col)
  (if (= 9 (height row col))
      (incf *score*)
      (~>> (gradual-uphill-neighbors row col)
           (mapc (curry #'apply #'search-trail))))
  (when (= 2 *part*)
    (unvisit row col)))

(defun gradual-uphill-neighbors (row col)
  (bind ((next-height (1+ (height row col))))
    (~>> (list (unvisited-with-height (1+ row) col next-height)
               (unvisited-with-height row (1+ col) next-height)
               (unvisited-with-height (1- row) col next-height)
               (unvisited-with-height row (1- col) next-height))
         (remove-if #'null))))

(defun unvisited-with-height (row col height)
  (when (and (<= 0 row (1- (array-dimension *map* 0)))
             (<= 0 col (1- (array-dimension *map* 1)))
             (not (visitedp row col))
             (= height (height row col)))
    (list row col)))

(defun visitedp (row col)
  (plusp (aref *visited* row col)))

(defun visit (row col)
  (setf (aref *visited* row col) 1))

(defun unvisit (row col)
  (setf (aref *visited* row col) 0))

(defun height (row col)
  (aref *map* row col))
