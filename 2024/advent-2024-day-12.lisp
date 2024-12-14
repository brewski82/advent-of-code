(defpackage #:advent-2024-day-12
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:serapeum #:append1 #:~>> #:~>)
  (:import-from #:nclasses #:define-class)
  (:import-from #:access #:access #:accesses #:set-access)
  (:import-from #:array-operations/utilities #:nested-loop))

(in-package #:advent-2024-day-12)

(defparameter *map* nil)
(defparameter *visited* nil)
(defparameter *last-visited* nil)

(defun day-12 ()
  (read-input-into-map "~/Downloads/input-12")
  (init-visited-matrix)
  (calculate-price))

(defun calculate-price ()
  (bind ((price 0))
    (nested-loop (row col) (array-dimensions *map*)
      (unless (visitedp row col)
        (bind (((area perimeter) (region-area-perimeter row col)))
          (incf price (* area perimeter)))))
    price))

(defun region-area-perimeter (row col)
  (when (visitedp row col)
    (return-from region-area-perimeter (list 0 0)))
  (visit row col)
  (bind ((this-perimeter (perimeter row col))
         (neighbor-area-perimeters
          (loop for (neighbor-row neighbor-col) in (unvisted-neighbors row col)
                while neighbor-row
                collecting (region-area-perimeter neighbor-row neighbor-col)))
         (total (apply #'mapcar #'+ (or neighbor-area-perimeters '((0 0)))))
         (area (1+ (or (first total) 0)))
         (perimeter (+ (or (second total) 0)
                       this-perimeter)))
    (list area perimeter)))

(defun perimeter (row col)
  (bind ((plant-type (plant-type row col)))
    (+ (include-perimeter (1+ row) col plant-type)
       (include-perimeter (1- row) col plant-type)
       (include-perimeter row (1+ col) plant-type)
       (include-perimeter row (1- col) plant-type))))

(defun include-perimeter (row col plant-type)
  (unless (and (<= 0 row (1- (row-count)))
               (<= 0 col (1- (col-count))))
    (return-from include-perimeter 1))
  (when (equalp plant-type (plant-type row col))
    (return-from include-perimeter 0))
  1)

(defun unvisted-neighbors (row col)
  (bind ((plant-type (plant-type row col)))
    (~>> (list (unvisited-with-plant-type (1+ row) col plant-type)
               (unvisited-with-plant-type row (1+ col) plant-type)
               (unvisited-with-plant-type (1- row) col plant-type)
               (unvisited-with-plant-type row (1- col) plant-type))
         (remove-if #'null))))

(defun unvisited-with-plant-type (row col plant-type)
  (when (and (<= 0 row (1- (row-count)))
             (<= 0 col (1- (col-count)))
             (not (visitedp row col))
             (equalp plant-type (plant-type row col)))
    (list row col)))

(defun init-visited-matrix ()
  (setf *visited*
        (make-array (array-dimensions *map*) :initial-element 0)))

(defun read-input-into-map (path)
  (bind ((contents (uiop:read-file-lines path))
         (rows (length contents))
         (cols (length (first contents))))
    (setf *map*
          (make-array (list rows cols)
                      :initial-contents contents))))

(defun plant-type (row col)
  (aref *map* row col))

(defun visitedp (row col)
  (plusp (aref *visited* row col)))

(defun visit (row col)
  (setf (aref *visited* row col) 1)
  (setf (aref *last-visited* row col) 1))

(defun day-12-part-2 ()
  (read-input-into-map "~/Downloads/input-12")
  (init-visited-matrix)
  (calculate-price-part-2))

(defun calculate-price-part-2 ()
  (bind ((price 0))
    (nested-loop (row col) (array-dimensions *map*)
      (unless (visitedp row col)
        (init-last-visited-matrix)
        (bind ((area (first (region-area-perimeter row col)))
               (top-sides (count-top-sides))
               (bottom-sides (count-bottom-sides))
               (right-sides (count-right-sides))
               (left-sides (count-left-sides)))
          (incf price (* area (+ top-sides bottom-sides right-sides left-sides))))))
    price))

(defun count-top-sides ()
  (loop with sides = 0
        for tracking-side = nil
        for row from 0 below (row-count) do
        (loop for col from 0 below (col-count)
              for top-side = (top-side row col)
              when (and top-side
                        (last-col-p col))
                do (incf sides)
              when (and (not top-side)
                        tracking-side)
                do (incf sides)
              do (setf tracking-side top-side))
        finally (return sides)))

(defun count-bottom-sides ()
  (loop with sides = 0
        for tracking-side = nil
        for row from 0 below (row-count) do
        (loop for col from 0 below (col-count)
              for bottom-side = (bottom-side row col)
              when (and bottom-side
                        (last-col-p col))
                do (incf sides)
              when (and (not bottom-side)
                        tracking-side)
                do (incf sides)
              do (setf tracking-side bottom-side))
        finally (return sides)))

(defun count-right-sides ()
  (loop with sides = 0
        for tracking-side = nil
        for col from 0 below (col-count) do
        (loop for row from 0 below (row-count)
              for right-side = (right-side row col)
              when (and right-side
                        (last-row-p row))
                do (incf sides)
              when (and (not right-side)
                        tracking-side)
                do (incf sides)
              do (setf tracking-side right-side))
        finally (return sides)))

(defun count-left-sides ()
  (loop with sides = 0
        for tracking-side = nil
        for col from 0 below (col-count) do
        (loop for row from 0 below (row-count)
              for left-side = (left-side row col)
              when (and left-side
                        (last-row-p row))
                do (incf sides)
              when (and (not left-side)
                        tracking-side)
                do (incf sides)
              do (setf tracking-side left-side))
        finally (return sides)))

(defun in-visiting-region (row col)
  (plusp (aref *last-visited* row col)))

(defun top-side (row col)
  (cond ((not (in-visiting-region row col))
         nil)
        ((zerop row)
         t)
        (t
         (not (equalp (plant-type row col)
                      (plant-type (1- row) col))))))

(defun bottom-side (row col)
  (cond ((not (in-visiting-region row col))
         nil)
        ((last-row-p row)
         t)
        (t
         (not (equalp (plant-type row col)
                      (plant-type (1+ row) col))))))

(defun right-side (row col)
  (cond ((not (in-visiting-region row col))
         nil)
        ((last-col-p col)
         t)
        (t
         (not (equalp (plant-type row col)
                      (plant-type row (1+ col)))))))

(defun left-side (row col)
  (cond ((not (in-visiting-region row col))
         nil)
        ((zerop col)
         t)
        (t
         (not (equalp (plant-type row col)
                      (plant-type row (1- col)))))))

(defun init-last-visited-matrix ()
  (setf *last-visited*
        (make-array (array-dimensions *map*) :initial-element 0)))

(defun row-count ()
  (array-dimension *map* 0))

(defun col-count ()
  (array-dimension *map* 1))

(defun last-row-p (row)
  (= row (1- (row-count))))

(defun last-col-p (col)
  (= col (1- (col-count))))
