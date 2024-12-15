(defpackage #:advent-2024-day-14
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:serapeum #:append1 #:~>> #:~> #:batches)
  (:import-from #:nclasses #:define-class)
  (:import-from #:access #:access #:accesses #:set-access)
  (:import-from #:array-operations/utilities #:nested-loop))

(in-package #:advent-2024-day-14)

(defun day-14 ()
  (~>> (uiop:read-file-lines "~/Downloads/input-14")
       (mapcar #'parse-input-line)
       (mapcar #'next-position)
       (mapcar #'quadrant)
       (remove-if #'null)
       serapeum:frequencies
       hash-table-values
       (apply #'*)))

(defun parse-input-line (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer px) (#'parse-integer py)
                                  (#'parse-integer vx) (#'parse-integer vy))
      ("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)" line)
    (list (cons px py)
          (cons vx vy))))

(defun next-position (robot)
  (bind ((x (mod (+ (px robot)
                    (* 100
                       (vx robot)))
                 101))
         (y (mod (+ (py robot)
                    (* 100
                       (vy robot)))
                 103)))
    (cons (if (minusp x)
              (+ 101 x)
              x)
          (if (minusp y)
              (+ 103 y)
              y))))

(defun quadrant (coordinate)
  (bind ((x (x coordinate))
         (y (y coordinate)))
    (cond ((and (< x 50) (< y 51)) :q1)
          ((and (> x 50) (< y 51)) :q2)
          ((and (< x 50) (> y 51)) :q3)
          ((and (> x 50) (> y 51)) :q4)
          (t nil))))

(defun px (robot)
  (car (first robot)))

(defun py (robot)
  (cdr (first robot)))

(defun vx (robot)
  (car (second robot)))

(defun vy (robot)
  (cdr (second robot)))

(defun x (coordinate)
  (car coordinate))

(defun y (coordinate)
  (cdr coordinate))

(defun day-14-part-2 ()
  (loop for robots = (~>> (uiop:read-file-lines "~/Downloads/input-14")
                          (mapcar #'parse-input-line))
        then (mapcar #'next-second robots)
        for s from 0 to 1000000
        when (is-christmas-tree robots)
          do (print-tree robots)
          and return s))


;;; https://www.reddit.com/r/adventofcode/comments/1hehu14/comment/m24ient/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;;; Essentially we assume that if all the robots are in a unique position, then
;;; they form a xmas tree :-/
(defun is-christmas-tree (robots)
  (init-map)
  (loop for robot in robots
        when (exists robot)
          do (return nil)
        do (load-robot robot)
        finally (return t)))

(defun next-second (robot)
  (bind ((x (mod (+ (px robot)
                    (vx robot))
                 101))
         (y (mod (+ (py robot)
                    (vy robot))
                 103)))
    (list (cons (if (minusp x)
                    (+ 101 x)
                    x)
                (if (minusp y)
                    (+ 103 y)
                    y))
          (cons (vx robot)
                (vy robot)))))

(defparameter *map* (make-hash-table))

(defun init-map ()
  (setf *map* (make-hash-table)))

(defun load-robot (robot)
  (bind ((set (gethash (px robot) *map* (fset:empty-set))))
    (setf (gethash (px robot) *map*)
          (fset:includef set (py robot)))))

(defun exists (robot)
  (when-let ((set (gethash (px robot) *map*)))
    (fset:contains? set (py robot))))

;;; for fun :-)
(defun print-tree (robots)
  (bind ((array (make-array '(101 103) :initial-element " ")))
    (mapc (lambda (robot)
            (setf (aref array (px robot) (px robot)) "#"))
          robots)
    (fresh-line)
    (loop for y from 0 to 102
          do (fresh-line)
          do (loop for x from 0 to 100 do (princ (aref array x y))))))
