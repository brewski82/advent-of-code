;;;; Advent of Code 2025 - Day 2 Solution
;;;;
;;;; Finds invalid IDs in given ranges based on digit patterns.
;;;; Supports two validation algorithms for parts 1 and 2.

(defpackage #:advent-2025-day-2
  (:use #:common-lisp)
  (:import-from #:serapeum #:~>>)
  (:import-from #:alexandria #:compose)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-2)

;;; Entry Points

(defun solution-part-1 ()
  "Solve part 1: sum invalid IDs where left half equals right half."
  (process-part-1 (uiop:read-file-string "~/Downloads/2025-day-2-part-1")))

(defun solution-part-2 ()
  "Solve part 2: sum invalid IDs where some digit batch pattern repeats."
  (process-part-2 (uiop:read-file-string "~/Downloads/2025-day-2-part-1")))

;;; Part 1: Half Equality Validation

(defun process-part-1 (input)
  "Parse input and sum invalid IDs using half-equality check."
  (t:transduce
   (t:map (compose #'sum-invalid-ids
                   #'make-range-from-string))
   #'+
   (parse-input input)))

(defun sum-invalid-ids (range)
  "Sum all IDs in RANGE where left half equals right half."
  (t:transduce
   (t:comp (t:take (1+ (- (range-end range) (range-start range))))
           (t:filter #'invalid-id-p))
   #'+
   (t:ints (range-start range))))

(defun invalid-id-p (id)
  "Return T if ID's left half equals right half.
Requires ID to have an even number of digits."
  (bind ((num-digits (1+ (floor (log id 10))))
         (half-digits (/ num-digits 2)))
    (when (evenp num-digits)
      (bind ((divisor (expt 10 half-digits))
             (left-half (floor id divisor))
             (right-half (mod id divisor)))
        (= left-half right-half)))))

;;; Part 2: Batch Pattern Validation

(defun process-part-2 (input)
  "Parse input and sum invalid IDs using batch pattern check."
  (t:transduce
   (t:map (compose #'sum-invalid-ids-part-2
                   #'make-range-from-string))
   #'+
   (parse-input input)))

(defun sum-invalid-ids-part-2 (range)
  "Sum all IDs in RANGE where some digit batch pattern repeats."
  (t:transduce
   (t:comp (t:take (1+ (- (range-end range) (range-start range))))
           (t:filter #'invalid-id-part-2-p))
   #'+
   (t:ints (range-start range))))

(defun invalid-id-part-2-p (id)
  "Return T if ID contains a repeating batch pattern.
Checks if dividing the ID's digits into N batches yields all equal batches,
for any valid batch size from 1 to half the digit count."
  (bind ((num-digits (1+ (floor (log id 10))))
         (half-digits (floor num-digits 2))
         (id-string (serapeum:string+ id))
         ((:flet invalid-p (batches))
          (when (zerop (rem num-digits batches))
            (apply #'serapeum:equalp* (serapeum:batches id-string batches)))))
    (t:transduce
     (t:take half-digits)
     (t:any? #'invalid-p)
     (t:ints 1))))

;;; Input Parsing

(defun parse-input (input)
  "Parse comma-separated ranges from INPUT string."
  (str:split "," input))

(defstruct range
  start  ; Inclusive start of ID range
  end)   ; Inclusive end of ID range

(defun make-range-from-string (string)
  "Parse STRING in format \"NUM-NUM\" into a range structure."
  (cl-ppcre:register-groups-bind ((#'parse-integer start) (#'parse-integer end))
      ("(\\d+)-(\\d+)" string)
    (make-range :start start :end end)))
