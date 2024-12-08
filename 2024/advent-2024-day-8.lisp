(defpackage #:advent-2024-day-8
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:arrow-macros #:->> #:-> #:-> #:some->>)
  (:import-from #:serapeum #:append1))

(in-package #:advent-2024-day-8)

(defparameter *antenna-map* (make-hash-table)
  "Maps antenna frequencies, represented as chars, to a list of their locations on
the map. Each location is a CONs of (row . col).")

(defparameter *map-dimensions* nil)

(defparameter *antinodes* (fset:empty-set))

(defun day-8-part-1 ()
  (load-antenna-map "~/Downloads/input-8")
  (maphash-values (rcurry #'add-antinodes-from-list #'add-antinodes-part-1)
                  *antenna-map*)
  (fset:size *antinodes*))

(defun load-antenna-map (path)
  (clrhash *antenna-map*)
  (setf *antinodes* (fset:empty-set))
  (with-open-file (in path)
    (loop for row from 0
          for line = (read-line in nil nil)
          for col-count = (length line) then col-count
          while line do
            (loop for col from 0
                  for char across line
                  unless (equal #\. char)
                    do (add-antenna char row col))
          finally (setf *map-dimensions* (list row col-count)))))

(defun add-antenna (antenna row col)
  (bind ((list (gethash antenna *antenna-map*)))
    (setf (gethash antenna *antenna-map*)
          (append1 list (cons row col)))))

(defun add-antinodes-from-list (list fn)
  (map-combinations fn list :length 2 :copy nil))

(defun add-antinodes-part-1 (antennae)
  (bind ((antenna-1 (first antennae))
         (antenna-2 (second antennae))
         (row-distance (- (car antenna-1) (car antenna-2)))
         (col-distance (- (cdr antenna-1) (cdr antenna-2))))
    (add-antinode (+ (car antenna-1) row-distance)
                  (+ (cdr antenna-1) col-distance))
    (add-antinode (- (car antenna-2) row-distance)
                  (- (cdr antenna-2) col-distance))))

(defun add-antinode (row col)
  (bind (((row-count col-count) *map-dimensions*))
    (when (and (>= row 0)
               (>= col 0)
               (< row row-count)
               (< col col-count))
      (fset:includef *antinodes* (cons row col)))))

(defun day-8-part-2 ()
  (load-antenna-map "~/Downloads/input-8")
  (maphash-values (rcurry #'add-antinodes-from-list #'add-antinodes-part-2)
                  *antenna-map*)
  (fset:size *antinodes*))

(defun add-antinodes-part-2 (antennae)
  (bind ((antenna-1 (first antennae))
         (antenna-2 (second antennae))
         (row-distance (- (car antenna-1) (car antenna-2)))
         (col-distance (- (cdr antenna-1) (cdr antenna-2))))
    (loop for row = (car antenna-1) then (+ row row-distance)
          for col = (cdr antenna-1) then (+ col col-distance)
          while (add-antinode row col))
    (loop for row = (car antenna-2) then (- row row-distance)
          for col = (cdr antenna-2) then (- col col-distance)
          while (add-antinode row col))))
