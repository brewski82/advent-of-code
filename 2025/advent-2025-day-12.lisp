;; Advent of Code 2025 Day 12

(defpackage #:advent-2025-day-12
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Ug! (spoiler)
;;; https://www.reddit.com/r/adventofcode/comments/1pks1y5/comment/ntne3e9/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;;;

(defun day-12-part-1 ()
  "Solve Advent of Code 2025 Day 12 Part 1."
  (~>> (uiop:read-file-string #P"~/Downloads/2025-day-12-part-1")
       process-part-1))

(defun process-part-1 (input)
  "Process input for Day 12 Part 1."
  (~>> input
       parse-input
       count-regions))

(defun count-regions (context)
  "Count regions that fit within available shapes.

Returns the number of regions that can fit."
  (t:transduce (t:map (curry #'fits (getf context :shapes)))
               (t:fold #'+)
               (getf context :regions)))

(defun fits (table region)
  "Check if a region's total area is sufficient for its quantities.

Returns 1 if it fits, 0 otherwise."
  (bind ((area (reduce #'* (getf region :dimensions) :initial-value 1))
         (area-needed (area-needed table (getf region :quantities))))
    (if (>= area area-needed)
        1
        0)))

(defun area-needed (table quantities)
  "Calculate total area needed for given quantities across shapes.

TABLE maps shape indices to character arrays.
QUANTITIES is a list of quantities to find area for."
  (t:transduce (t:comp #'t:enumerate
                       (t:map (curry #'area-for-index table)))
               (t:fold #'+)
               quantities))

(defun area-for-index (table index-quantity)
  "Calculate area for a quantity by counting # characters in its shape.

Returns quantity multiplied by the number of # characters in the shape."
  (bind (((index . quantity) index-quantity)
         (array (gethash index table))
         (area (reduce-array (lambda (acc value &rest indexes)
                               (declare (ignore indexes))
                               (+ acc
                                  (if (char-equal value #\#)
                                      1
                                      0)))
                             array :initial-value 0)))
    (* quantity area)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-input (string)
  (~>> string
       group-input
       to-shapes-and-regions
       process-shapes-and-regions))

(defun group-input (string)
  (cl-ppcre:split "\\n\\n" string))

(defun to-shapes-and-regions (groups)
  "Partition groups into shapes (containing 'x') and regions.

Returns a list of (shapes regions)."
  (multiple-value-list (serapeum:partition (compose #'not (curry #'cl-ppcre:scan "x"))
                                           groups)))

(defun process-shapes-and-regions (shapes-and-regions)
  "Build context with shapes table and processed regions.

Returns a property list with :shapes and :regions."
  (bind (((shapes (regions)) shapes-and-regions)
         (table (make-hash-table)))
    (mapc (curry #'add-shape-entry table) shapes)
    `(:shapes ,table
      :regions ,(process-regions regions))))

(defun add-shape-entry (table shape-input)
  "Parse shape input and store as character array in TABLE.

Extracts index and grid from shape-input string."
  (bind (((index shape) (cl-ppcre:split ":\\n" shape-input))
         (array (to-char-array (serapeum:lines shape)))
         (key (parse-integer index)))
    (setf (gethash key table) array)))

(defun process-regions (regions)
  (mapcar #'process-region (serapeum:lines regions)))

(defun process-region (region)
  "Parse region string into dimensions and quantities.

Returns a property list with :dimensions and :quantities."
  (bind (((dimensions quantities) (cl-ppcre:split ":" region)))
    `(:dimensions ,(mapcar #'parse-integer (cl-ppcre:split "x" dimensions))
      :quantities ,(mapcar #'parse-integer (serapeum:tokens quantities)))))
