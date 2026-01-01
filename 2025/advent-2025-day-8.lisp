;; Advent of Code 2025 Day 8: Playground
;; Problem: Connect junction boxes (3D points) to form circuits.
;; Part 1: Connect the 1000 closest pairs and multiply the three largest circuit sizes.
;; Part 2: Keep connecting until all boxes are in one circuit, multiply final X coordinates.

(defpackage #:advent-2025-day-8
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-8)

(defun to-point (line)
  "Parse a line of the form 'X,Y,Z' into a 3D point structure."
  (cl-ppcre:register-groups-bind ((#'parse-integer x)
                                  (#'parse-integer y)
                                  (#'parse-integer z))
      ("(\\d+),(\\d+),(\\d+)" line)
    (point :x x :y y :z z)))

(defun to-points (lines)
  "Convert a list of coordinate strings into point structures."
  (mapcar #'to-point lines))

;;; Data structure to represent a pairwise distance between two points
(defstruct distance point-a point-b value)

(defun compute-distance (point-a point-b)
  "Create a distance record between two points with their Manhattan distance."
  (make-distance :point-a point-a
                 :point-b point-b
                 :value (distance point-a point-b)))

(defun compute-distances (point points)
  "Compute distances from a single point to a list of other points."
  (mapcar (curry #'compute-distance point) points))

(defun compute-all-distances (points)
  "Compute distances for all pairs of points (each pair computed once)."
  (mapcon (lambda (list)
            (compute-distances (first list)
                               (rest list)))
          points))

(defun sort-distances (distances)
  "Sort all pairwise distances in ascending order."
  (sort distances #'< :key #'distance-value))

;;; Union-Find style operation: merge two circuits/boxes together
(defun union-boxes (map distance)
  "Merge two circuits when their junction boxes are connected.
   Map: fset map of point -> circuit (set of points).
   Returns updated map where both points and their circuits point to the merged set."
  (bind (((:structure distance- point-a point-b) distance)
         ;; Get or create a new single-point circuit for each point
         (point-a-box (or (fset:@ map point-a) (fset:set point-a)))
         (point-b-box (or (fset:@ map point-b) (fset:set point-b)))
         ;; Merge the two circuits into one
         (connected-box (fset:union point-a-box point-b-box)))
    ;; Update the map so all points in the merged circuit point to the combined set
    (fset:reduce (lambda (m k)
                   (fset:with m k connected-box))
                 connected-box
                 :initial-value map)))

(defun merge-first-n-connections (limit distances)
  "Merge circuits by connecting the first N closest pairs of junction boxes."
  (t:transduce (t:take limit)
               (t:fold #'union-boxes (fset:empty-map))
               distances))

(defun sort-boxes (boxes)
  "Sort circuits (sets of boxes) by size in descending order."
  (fset:sort boxes #'> :key #'fset:size))

(defun multiply-largest-three-sizes (boxes)
  "Multiply together the sizes of the three largest circuits."
  (reduce #'* (mapcar #'fset:size (serapeum:take 3 boxes))))

(defun process-part-1 (limit input)
  "Solve Part 1: Connect the N closest pairs and multiply the three largest circuit sizes."
  (~>> input
       to-points
       compute-all-distances
       sort-distances
       (merge-first-n-connections limit)
       fset:range                         ; Get all distinct circuits
       sort-boxes
       (fset:convert 'list)
       multiply-largest-three-sizes))

;;; Part 2: Keep connecting until all boxes form a single circuit
(defun all-connected-p (box-count map)
  "Check if all boxes are now in a single circuit. Returns true if there's only one
unique circuit set and it contains all boxes."
  (and (= 1 (fset:size (fset:range map)))  ; Only one unique circuit set
       (= box-count (~>> map
                         fset:range
                         (funcall (rcurry #'fset:at-index 0))
                         fset:size))))       ; Circuit contains all boxes

(defun union-boxes-until-complete (box-count map distance)
  "Merge circuits, stopping when all boxes are connected. Returns the updated map,
or a 'reduced' value when complete. The reduced value is the product of the X
coordinates of the final two boxes."
  (bind (((:structure distance- point-a point-b) distance)
         ;; Get or create circuits for each point
         (point-a-box (or (fset:@ map point-a) (fset:set point-a)))
         (point-b-box (or (fset:@ map point-b) (fset:set point-b)))
         ;; Merge the two circuits
         (connected-box (fset:union point-a-box point-b-box))
         ;; Update map so all points in merged circuit point to it
         (map (fset:reduce (lambda (m k)
                             (fset:with m k connected-box))
                           connected-box
                           :initial-value map)))
    ;; Check if we're done - if so, return the product of X coordinates
    (if (all-connected-p box-count map)
        (t:reduced (* (x point-a) (x point-b)))
        map)))

(defun merge-connections (box-count distances)
  "Keep merging circuits until all boxes are in a single circuit. Returns the
product of the X coordinates of the final two connected boxes."
  (t:transduce #'t:pass
               (t:fold (curry #'union-boxes-until-complete box-count)
                       (fset:empty-map))
               distances))

(defun process-part-2 (input)
  "Solve Part 2: Connect boxes until all are in one circuit, return product of
final X coordinates."
  (~>> input
       to-points
       compute-all-distances
       sort-distances
       (merge-connections (length input))))

(defun day-8-part-1 ()
  "Solve Advent of Code 2025 Day 8 Part 1."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-8-part-1")
       (process-part-1 1000)))

(defun day-8-part-2 ()
  "Solve Advent of Code 2025 Day 8 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-8-part-1")
       process-part-2))
