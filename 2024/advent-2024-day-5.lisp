(defpackage #:advent-2024-day-5
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:arrow-macros #:->> #:-> #:-> #:some->>))

(in-package #:advent-2024-day-5)

(defparameter *graph* nil)

(defun day-5-part-1 ()
  (->> (read-input "~/Downloads/input-5")
    (remove-if-not #'correct-order-p)
    (mapcar #'extract-middle)
    (reduce #'+)))

(defun day-5-part-2 ()
  (->> (read-input "~/Downloads/input-5")
    (remove-if #'correct-order-p)
    (mapcar #'correct-update)
    (mapcar #'extract-middle)
    (reduce #'+)))

(defun correct-order-p (update)
  "Recursively checks if the first item of UPDATE is out of order. Recurses for
each element."
  (and (notany (lambda (ancestor)
                 (ancestor-p (cl-graph:find-vertex *graph* (first update))
                             (cl-graph:find-vertex *graph* ancestor)))
               (rest update))
       (or (null update)
           (correct-order-p (rest update)))))

(defun correct-update (update)
  "Loops through UPDATEs elements and swaps those that are out of order. Repeats
until UPDATE is correct."
  (loop until (correct-order-p update)
        do (loop for i from 0 below (1- (length update))
                 for cur = (elt update i)
                 for next = (elt update (1+ i))
                 when (ancestor-p (cl-graph:find-vertex *graph* cur)
                                  (cl-graph:find-vertex *graph* next))
                   do (setf (nth i update) next
                            (nth (1+ i) update) cur)))
  update)

(defun extract-middle (update)
  (elt update (floor (/ (length update) 2))))

(defun ancestor-p (source ancestor)
  (some->> (cl-graph:parent-vertexes source)
    (equalp ancestor)))

(defun read-input (path)
  (bind ((graph (cl-graph:make-graph 'cl-graph:graph-container :default-edge-type :directed)))
    (with-open-file (in path)
      (loop with lines = (list)
            for line = (read-line in nil nil)
            while line
            until (emptyp line)
            do (bind (((parent child) (split-line-to-numbers line "|")))
                 (cl-graph:add-edge-between-vertexes graph parent child)))
      (setf *graph* graph)
      (loop for line = (read-line in nil nil)
            while line
            collect (split-line-to-numbers line ",")))))

(defun split-line-to-numbers (line separator)
  (->> line
    (str:split-omit-nulls separator)
    (mapcar #'parse-integer)))
