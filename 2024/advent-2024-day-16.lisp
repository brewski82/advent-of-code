(defpackage #:advent-2024-day-16
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:serapeum #:append1 #:~>> #:~> #:batches)
  (:import-from #:nclasses #:define-class)
  (:import-from #:access #:access #:accesses #:set-access)
  (:import-from #:array-operations/utilities #:nested-loop))

(in-package #:advent-2024-day-16)

(defparameter *debug* nil)
(defparameter *maze* nil)
(defparameter *priority-queue* nil)
(defparameter *visited* nil)
(defparameter *parents* nil)
(defparameter *scores* nil)

(define-class pos () (row col direction score))

(defmethod fset:compare ((p1 pos) (p2 pos))
  (fset:compare-slots p1 p2 'row 'col 'direction))

(defun load-maze (path)
  (bind ((contents (uiop:read-file-lines path))
         (dimensions (list (length contents)
                           (length (first contents)))))
    (setf *maze*
          (make-array dimensions :initial-contents contents))))

(defun day-16 ()
  (load-maze "~/Downloads/test")
  (setf *priority-queue* (damn-fast-priority-queue:make-queue))
  (setf *visited* (fset:empty-set))
  (setf *parents* (make-hash-table :test #'equalp))
  (setf *scores* (make-hash-table :test #'equalp))
  (bind (((start-row . start-col) (find-in-maze #\S)))
    (enqueue (make-instance 'pos
                            :row start-row
                            :col start-col
                            :direction 'east
                            :score 0))
    (bind ((score (search-maze)))
      (update-and-print-maze)
      (format t "~&part 1 score: ~A~%" (score score))
      (format t "~&part 2 score: ~A~%" (part-2-score)))))

(defun part-2-score ()
  (bind ((tiles (fset:empty-set)))
    (loop with (end-row . end-col) = (find-in-maze #\E)
          with (start-row . start-col) = (find-in-maze #\S)
          for parents = (get-parent end-row end-col)
            then (flatten (mapcar (lambda (parent)
                                    (get-parent (row parent) (col parent)))
                                  parents))
          while parents
          until (and (equal (row (first parents)) start-row)
                     (equal (col (first parents)) start-col))
          do (loop for parent in parents do (setf tiles (fset:includef tiles parent))))
    (fset:size tiles)))

(defun update-and-print-maze ()
  (loop with (end-row . end-col) = (find-in-maze #\E)
        with (start-row . start-col) = (find-in-maze #\S)
        for parents = (get-parent end-row end-col) then
                                                   (flatten (mapcar (lambda (parent)
                                                                      (get-parent (row parent) (col parent)))
                                                                    parents))
        while parents
        until (and (equal (row (first parents)) start-row)
                   (equal (col (first parents)) start-col))
        do (loop for parent in parents
                 do (setf (aref *maze* (row parent) (col parent))
                          (ecase (direction parent)
                            (north #\^)
                            (south #\V)
                            (east #\>)
                            (west #\<)))))
  (terpri)
  (loop for row from 0 below (array-dimension *maze* 0) do (terpri) do
    (loop for col from 0 below (array-dimension *maze* 1) do
      (princ (aref *maze* row col)))))

(defun add-score (row col score)
  (setf (gethash (cons row col) *scores*) score))

(defun get-score (row col)
  (gethash (cons row col) *scores*))

(defun add-parent (row col pos)
  (bind ((parents (gethash (cons row col) *parents*)))
    (setf (gethash (cons row col) *parents*)
          (push pos parents))))

(defun get-parent (row col)
  (gethash (cons row col) *parents*))

(defun enqueue (pos)
  (damn-fast-priority-queue:enqueue *priority-queue*
                                    pos
                                    (score pos)))

(defun search-maze ()
  (loop with (end-row . end-col) = (find-in-maze #\E)
        for pos = (damn-fast-priority-queue:dequeue *priority-queue*)
        while pos
        when (and (equal (row pos) end-row)
                  (equal (col pos) end-col))
          do (return pos)
        unless (fset:contains? *visited* pos)
          do (setf *visited* (fset:includef *visited* pos))
          and do (loop for neighbor in (find-neighbors pos)
                       for prev-score = (or (get-score (row neighbor) (col neighbor))
                                            most-positive-fixnum)
                       when (and (equal 7 (row neighbor))
                                 (equal 6 (col neighbor)))
                         do (format t "~&pos = ~A neigh = ~A"
                                    (score pos) (score neighbor))
                         when (<= (score neighbor) prev-score)
                         do (add-score (row neighbor) (col neighbor) (score neighbor))
                         and do (add-parent (row neighbor) (col neighbor)
                                            pos)
                         and do (enqueue neighbor))))

(defun find-neighbors (pos)
  ;; (remove-if #'null
  ;;            (list ))
  (~>> '(east west north south)
       (mapcar (curry #'find-neighbor pos))
       (remove-if #'null)))

(defun find-neighbor (pos direction)
  (bind ((row (case direction
                (north (1- (row pos)))
                (south (1+ (row pos)))
                (t (row pos))))
         (col (case direction
                (east (1+ (col pos)))
                (west (1- (col pos)))
                (t (col pos))))
         (tile (aref *maze* row col)))
    (when (and (<= 0 row (1- (array-dimension *maze* 0)))
               (<= 0 col (1- (array-dimension *maze* 1)))
               (char-not-equal #\# tile))
      (make-instance 'pos
                     :row row
                     :col col
                     :direction direction
                     :score (if (equal direction (direction pos))
                                (1+ (score pos))
                                (+ 1001 (score pos)))))))

(defun find-in-maze (char)
  (array-operations/utilities:nested-loop (row col) (array-dimensions *maze*)
    (when (char-equal char (aref *maze* row col))
      (return-from find-in-maze (cons row col)))))
