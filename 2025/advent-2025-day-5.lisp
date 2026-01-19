(defpackage #:advent-2025-day-5
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar)
  (:import-from #:serapeum #:~>> #:append1)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-5)

;;; Advent of Code 2025 Day 5: Cafeteria
;;;
;;; Problem: The kitchen's new inventory management system tracks fresh ingredients
;;; using ID ranges. The database has two sections:
;;;   1. Fresh ingredient ID ranges (inclusive, e.g., "3-5" means IDs 3, 4, and 5)
;;;   2. Available ingredient IDs to check
;;;
;;; Part 1: Count how many available ingredient IDs are fresh (fall within any range)
;;; Part 2: Count the total number of ingredient IDs considered fresh by the ranges

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun part-1 ()
  "Solve part 1: Count how many available ingredient IDs are fresh."
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-5-part-1")
       process-part-1))

(defun process-part-1 (lines)
  "Process the input lines and count fresh ingredient IDs.

LINES is a list of strings containing:
  - Range definitions (e.g., \"3-5\")
  - A blank line separator
  - Individual ingredient IDs to check"
  (~>> lines
       to-ranges-ids
       count-fresh-ids))

(defstruct range
  "Represents an inclusive range of fresh ingredient IDs."
  lower  ; Minimum ID in the range
  upper) ; Maximum ID in the range

(defstruct ranges-ids
  "Holds the parsed database: fresh ingredient ranges and available IDs to check."
  ranges  ; List of RANGE structs
  ids)    ; List of ingredient IDs to validate

(defun to-ranges-ids (lines)
  "Parse input lines into a RANGES-IDS struct.

Uses a transducer with a fold to accumulate ranges and IDs from the input."
  (t:transduce #'t:pass
               (t:fold #'ranges-ids-fold (make-ranges-ids))
               lines))

(defun ranges-ids-fold (prev string)
  "Fold function to accumulate ranges and IDs from each line.

PREV is the accumulated RANGES-IDS struct.
STRING is the current line being processed.

Lines containing '-' are parsed as ranges (e.g., \"3-5\").
Empty lines are ignored.
Other lines are parsed as individual ingredient IDs."
  (bind (((:structure ranges-ids- ranges ids)
          prev))
    (cond ((alexandria:emptyp string) prev)
          ((find #\- string)
           (make-ranges-ids :ranges (cons (to-range string) ranges)
                            :ids ids))
          (t (make-ranges-ids :ranges ranges
                              :ids (cons (parse-integer string) ids))))))

(defun to-range (string)
  "Parse a range string like \"3-5\" into a RANGE struct.

Returns a RANGE with LOWER and UPPER bounds (inclusive)."
  (cl-ppcre:register-groups-bind ((#'parse-integer lower) (#'parse-integer upper))
      ("(\\d+)-(\\d+)" string)
    (make-range :lower lower :upper upper)))

(defun count-fresh-ids (ranges-ids)
  "Count how many ingredient IDs in RANGES-IDS are fresh.

An ID is fresh if it falls within any of the ranges."
  (bind (((:structure ranges-ids- ranges ids)
          ranges-ids))
    (count-if (curry #'fresh-p ranges) ids)))

(defun fresh-p (ranges id)
  "Return T if ID falls within any range in RANGES.

An ingredient ID is fresh if it's between the LOWER and UPPER bounds
(inclusive) of any range."
  (some (lambda (range)
          (<= (range-lower range)
              id
              (range-upper range)))
        ranges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun part-2 ()
  "Solve part 2: Count total ingredient IDs considered fresh by all ranges.

This ignores the available IDs section and counts all IDs covered by the
fresh ingredient ranges (including overlapping ranges counted once)."
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-5-part-1")
       process-part-2))

(defun process-part-2 (lines)
  "Process the input and count total fresh ingredient IDs.

Strategy:
  1. Parse the ranges from input
  2. Sort ranges by lower bound
  3. Consolidate overlapping ranges
  4. Count IDs in each consolidated range
  5. Sum the counts"
  (~>> lines
       to-ranges-ids
       ranges-ids-ranges
       sort-ranges
       consolidate-ranges
       (mapcar #'range-id-count)
       (reduce #'+)))

(defun sort-ranges (ranges)
  "Sort RANGES by lower bound, then by upper bound.

This ordering is necessary for the consolidation algorithm to work correctly."
  (sort ranges #'compare-ranges))

(defun compare-ranges (a b)
  "Compare two ranges for sorting.

Primary sort key: lower bound (ascending)
Secondary sort key: upper bound (ascending)"
  (or (< (range-lower a) (range-lower b))
      (and (= (range-lower a) (range-lower b))
           (< (range-upper a) (range-upper b)))))

(defun consolidate-ranges (ranges)
  "Merge overlapping ranges into a list of non-overlapping ranges.

RANGES must be sorted by lower bound.

Example: Ranges (3-5) and (4-8) become (3-8).
         Ranges (10-14) and (16-20) remain separate."
  (t:transduce #'t:pass
               (t:fold #'consolidate-ranges-fold (list))
               ranges))

(defun consolidate-ranges-fold (ranges current)
  "Fold function to merge overlapping ranges.

RANGES is the list of consolidated ranges accumulated so far.
CURRENT is the next range to process.

If CURRENT overlaps with the last range in RANGES, merge them.
Otherwise, add CURRENT as a new separate range."
  (bind ((prev-range (alexandria:lastcar ranges)))
    (cond
      ;; If this is the first element, set it as the first of the consolidated
      ;; list of ranges.
      ((null prev-range) (list current))
      ;; If the start of the current range is within or adjacent to the previous range,
      ;; consolidate them by extending the upper bound.
      ((<= (range-lower current) (range-upper prev-range))
       (append1 (butlast ranges)
                (make-range :lower (range-lower prev-range)
                            :upper (max (range-upper prev-range)
                                        (range-upper current)))))
      ;; Else we have the start of a new range.
      (t (append1 ranges current)))))

(defun range-id-count (range)
  "Count the number of ingredient IDs in RANGE (inclusive).

For example, range (3-5) contains 3 IDs: 3, 4, and 5."
  (1+ (- (range-upper range)
         (range-lower range))))
