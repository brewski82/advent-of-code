(defpackage #:advent-2025-day-7
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>> #:append1)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-7-part-1 ()
  "Solve Advent of Code 2025 Day 7 Part 1."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-7-part-1")
       process-part-1))

(defun process-part-1 (rows)
  "Part 1: Count total number of beam splits in the manifold."
  (~>> rows
       count-manifold-splits
       manifold-state-split-count))

(defstruct manifold-state split-count incoming-beams)
(defstruct manifold-row-state split-count incoming-beams beams splitters)
(defstruct beam column path-count)
(defstruct row index value)
(defstruct item index value)

(defmethod fset:compare ((b1 beam) (b2 beam))
  (fset:compare (beam-column b1) (beam-column b2)))

(defun count-manifold-splits (rows)
  "Process all rows of the manifold, tracking beam propagation and splits. Returns
final manifold-state containing split count and active beams."
  (t:transduce #'t:pass
               (t:fold #'process-row
                       (make-manifold-state :split-count 0
                                            :incoming-beams (fset:empty-set)))
               rows))

(defun process-row (manifold-state row)
  "Process a single row, updating the manifold state. Beams that hit splitters are
stopped, new beams continue from adjacent columns. Returns updated
manifold-state with new incoming beams for the next row."
  (bind ((manifold-row-state (count-manifold-row-splits manifold-state row))
         ((:structure manifold-row-state- split-count incoming-beams beams splitters)
          manifold-row-state)
         (new-incoming-beams (fset:set-difference (fset:reduce #'merge-beam-into beams :initial-value incoming-beams)
                                              splitters)))
    (make-manifold-state :split-count split-count :incoming-beams new-incoming-beams)))


(defun count-manifold-row-splits (manifold-state row)
  "Process a single row of the manifold, propagating beams through each cell.
Returns a manifold-row-state with updated beams and split counts."
  (bind (((:structure manifold-state- split-count incoming-beams)
          manifold-state))
    (t:transduce (t:comp #'t:enumerate (t:map #'to-item))
                 (t:fold #'process-cell
                         (make-manifold-row-state :split-count split-count
                                                  :beams (fset:empty-set)
                                                  :incoming-beams incoming-beams
                                                  :splitters (fset:empty-set)))
                 row)))

(defun process-cell (manifold-row-state item)
  "Process a single cell in the manifold row. Handles starting beams (S),
splitters (^), and empty space (.). Updates beam positions and counts splits."
  (bind (((:structure manifold-row-state- split-count incoming-beams beams splitters)
          manifold-row-state)
         ((:structure item- index value)
          item)
         (split-p (find-beam incoming-beams index))
         (path-count (if split-p
                         (beam-path-count split-p)
                         1)))
    (ecase value
      (#\S (make-manifold-row-state :incoming-beams incoming-beams
                                    :beams (merge-beams beams index 1)
                                    :split-count split-count
                                    :splitters splitters))
      (#\^ (make-manifold-row-state :incoming-beams incoming-beams
                                    :beams (if split-p
                                               (merge-beams (merge-beams beams (1- index) path-count)
                                                            (1+ index) path-count)
                                               beams)
                                    :split-count (if split-p
                                                     (1+ split-count)
                                                     split-count)
                                    :splitters (fset:with splitters (make-beam :column index))))
      (#\. manifold-row-state))))

(defun merge-beam-into (set beam)
  "Merge BEAM into SET, combining path counts if a beam already exists at that column.
Used as a reducer function with fset:reduce."
  (let ((existing (find-beam set (beam-column beam))))
    (if existing
        (fset:with (fset:less set existing)
                   (make-beam :column (beam-column beam)
                              :path-count (+ (beam-path-count existing)
                                             (beam-path-count beam))))
        (fset:with set beam))))

(defun merge-beams (beams column count)
  "Merge a beam with COUNT paths at COLUMN into BEAMS set, combining path counts if
beam exists."
  (bind ((beam (find-beam beams column)))
    (if beam
        (fset:with (fset:less beams beam)
                   (make-beam :column column
                              :path-count (+ count (beam-path-count beam))))
        (fset:with beams (make-beam :column column
                                    :path-count count)))))

(defun find-beam (beams column)
  "Find a beam at COLUMN in the BEAMS set."
  (fset:find column beams :key #'beam-column))

(defun to-item (enumeration)
  (bind (((index . value) enumeration))
    (make-item :index index :value value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-7-part-2 ()
  "Solve Advent of Code 2025 Day 7 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-7-part-1")
       process-part-2))

(defun process-part-2 (rows)
  "Part 2: Count distinct quantum timelines (many-worlds interpretation). Each beam
tracks how many different paths reach that position."
  (~>> rows
       count-manifold-splits
       manifold-state-incoming-beams
       count-paths))

(defun count-paths (beams)
  "Sum the path counts of all beams, giving total number of distinct timelines."
  (fset:reduce #'+ beams :key #'beam-path-count))
