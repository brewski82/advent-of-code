;; Advent of Code 2025 Day 9

(defpackage #:advent-2025-day-9
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1 - Largest rectangle between any two red tiles
;;;
;;; Find the largest axis-aligned rectangle that has red tiles at two opposite
;;; corners. There are no restrictions on what tiles can be included.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-9-part-1 ()
  "Solve Advent of Code 2025 Day 9 Part 1."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-9-part-1")
       process-part-1))

(defun process-part-1 (input)
  "Parse input and find the maximum rectangle area between any two red tiles."
  (~>> input
       csv-lines-to-2d-points
       largest-areas
       (remove-if #'null)
       (reduce #'max)))

(defun largest-areas (points)
  "For each point, find the maximum area rectangle between it and any other point.
Returns a list of areas (one per point), or NIL if only one point remains."
  (maplist (lambda (list)
             (largest-area-for-point (first list) (rest list)))
          points))

(defun largest-area-for-point (point points)
  "Find the maximum area rectangle between POINT and any point in POINTS."
  (alexandria:extremum (mapcar (curry #'area point)
                               points)
                       #'>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2 - Largest rectangle restricted to red and green tiles
;;;
;;; Red tiles form a closed polygon. Green tiles are the polygon edges and all
;;; tiles inside the polygon. Find the largest axis-aligned rectangle with red
;;; tiles at opposite corners, where all other tiles must be red or green.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-9-part-2 ()
  "Solve Advent of Code 2025 Day 9 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-9-part-1")
       process-part-2))

(defun process-part-2 (input)
  "Parse input, compute polygon edges, and find the maximum valid rectangle area."
  (~>> input
       csv-lines-to-2d-points
       compute-edges
       largest-valid-area))

(defstruct points-and-edges
  "Container holding the red tile points and the polygon edges."
  points edges)

(defun compute-edges (points)
  "Build a closed polygon by creating edges between consecutive points.
The polygon closes by connecting the last point back to the first."
  (bind  ((butlast-edges (maplist (lambda (list)
                                   (when (and (first list) (second list))
                                     (make-edge (first list) (second list))))
                                 points))
          (last-edge (make-edge (lastcar points) (first points))))

    (make-points-and-edges :points points
                           :edges (remove-if #'null
                                             (serapeum:append1 butlast-edges last-edge)))))

(defun largest-valid-area (points-and-edges)
  "Find the largest rectangle with red tiles at opposite corners that doesn't
cross the polygon boundary. Optimization: only checks point pairs that haven't
been checked yet by enumerating with indices."
  (bind (((:structure points-and-edges- points edges) points-and-edges))
    (t:transduce #'t:enumerate
                 (t:fold (lambda (largest-so-far point-enumeration)
                           (bind (((index . point) point-enumeration))
                             ;; For each point, check all subsequent points as the
                             ;; opposite corner (avoiding duplicate checks)
                             (max largest-so-far
                                  (largest-valid-area-for-point edges
                                                                largest-so-far
                                                                point
                                                                (subseq points index)))))
                         0)
                 points)))

(defun largest-valid-area-for-point (edges largest point points)
  "Find the maximum area rectangle between POINT and any point in POINTS."
  (t:transduce #'t:pass
               (t:fold (lambda (largest-so-far p)
                         (max largest-so-far
                              (valid-area edges largest-so-far point p)))
                       largest)
               points))

(defun valid-area (edges largest-area point-a point-b)
  "Calculate rectangle area between two points, returning 0 if invalid. Optimizes
by skipping boundary validation if the area cannot beat the current largest."
  (bind ((area (area point-a point-b)))
    (cond ((<= area largest-area)
           ;; Early exit: if this area can't beat the current max, skip validation
           0)
          ((valid-rectangle-p edges point-a point-b)
           ;; Rectangle doesn't cross polygon boundary, so it's valid
           area)
          (t
           ;; Rectangle crosses polygon boundary, invalid
           0))))

(defun valid-rectangle-p (edges point-a point-b)
  "Check if a rectangle doesn't intersect any polygon edge. Valid means the
rectangle is entirely outside or entirely inside the polygon, ensuring all
interior tiles are green (inside) or the rectangle contains only red/green
tiles."
  (bind ((min-col (min (col point-a) (col point-b)))
         (max-col (max (col point-a) (col point-b)))
         (min-row (min (row point-a) (row point-b)))
         (max-row (max (row point-a) (row point-b))))
    ;; Check that the rectangle bounding box doesn't overlap with any edge's bounding box.
    ;; If all edges are outside the rectangle, the rectangle interior is safe.
    (t:transduce #'t:pass
                 (t:all? (lambda (edge)
                           (bind (((:accessors point-a point-b) edge)
                                  (edge-min-col (min (col point-a) (col point-b)))
                                  (edge-max-col (max (col point-a) (col point-b)))
                                  (edge-min-row (min (row point-a) (row point-b)))
                                  (edge-max-row (max (row point-a) (row point-b))))
                             ;; True if edge is completely outside rectangle bounds
                             (or (>= min-col edge-max-col)
                                 (<= max-col edge-min-col)
                                 (>= min-row edge-max-row)
                                 (<= max-row edge-min-row)))))
                 edges)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug - Visualization helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debug-print-grid (points-and-edges)
  "Create a grid visualization of the red tile polygon. Red tiles are marked with
#, all other tiles with ."
  (bind (((:structure points-and-edges- points) points-and-edges)
         (right-most (find-extreme points :right-most))
         (bottom-most (find-extreme points :bottom-most))
         (grid (make-grid (1+ bottom-most)
                          (1+ right-most)
                          :initial-element #\.)))
    (fset:image (lambda (point)
                  (setf (aref grid (row point) (col point)) #\#))
                points)
    grid))

(defun find-extreme (points direction)
  "Find the extreme coordinate in the given direction. DIRECTION is one of
:LEFT-MOST, :RIGHT-MOST, :TOP-MOST, :BOTTOM-MOST"
  (ecase direction
    (:left-most (fset:reduce #'min points :key #'x))
    (:right-most (fset:reduce #'max points :key #'x))
    (:top-most (fset:reduce #'min points :key #'y))
    (:bottom-most (fset:reduce #'max points :key #'y))))
