;;;; Advent of Code 2025 - Day 3 Solution
;;;;
;;;; This module solves the Day 3 puzzle, which involves processing banks of digits
;;;; to extract the largest digits and calculate joltage values. The solution uses
;;;; transducers for efficient sequence processing and functional programming patterns.

(defpackage #:advent-2025-day-3
  (:use #:common-lisp)
  (:import-from #:alexandria #:compose #:curry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-3)

(defun solution-part-1 ()
  (~>> (read-input)
       (total-output-joltage 2)))

(defun solution-part-2 ()
  (~>> (read-input)
       (total-output-joltage 12)))

(defun read-input ()
  (uiop:read-file-lines "~/Downloads/2025-day-3-part-1"))

(defun total-output-joltage (battery-count banks)
  "Sum the largest joltage values across all banks."
  (t:transduce
   (t:map (curry #'largest-joltage battery-count))
   #'+
   banks))

(defun largest-joltage (battery-count bank)
  (~>> (process-bank battery-count bank)
       state-joltage))

(defstruct state
  start-index    ; Index of current largest digit
  joltage        ; Accumulated joltage value
  bank           ; String of digit characters
  battery-count) ; Total batteries to process

(defun process-bank (battery-count bank)
  "Extract largest digits at progressively expanding search windows in BANK.
Returns final state with accumulated joltage value."
  (t:transduce
   (t:take battery-count)
   (t:fold #'next-state
           (make-state :start-index -1
                       :joltage 0
                       :bank bank
                       :battery-count battery-count))
   (t:ints 0)))

(defun next-state (prev-state step)
  "Find the largest digit in the next search window and append to joltage."
  (bind (((:structure state- start-index joltage bank battery-count)
          prev-state)
         ((:structure digit-and-index- digit index)
          (largest-digit-and-index bank
                                   (1+ start-index)
                                   (+ (- (length bank) battery-count)
                                      (1+ step)))))
    (make-state :start-index index
                :joltage (+ (* 10 joltage)
                            digit)
                :bank bank
                :battery-count battery-count)))

(defstruct digit-and-index
  digit  ; Numeric value 0-9
  index) ; Position in bank


(defun largest-digit-and-index (bank start end)
  "Find the largest digit in BANK between START and END positions."
  (t:transduce
   (t:comp (t:drop start)
           (t:take (- end start))
           #'t:enumerate
           (t:map (curry #'to-digit-and-index start)))
   (t:fold #'max-digit)
   bank))

(defun max-digit (a b)
  (if (> (digit-and-index-digit b)
         (digit-and-index-digit a))
      b
      a))

(defun to-digit-and-index (start enumeration)
  (bind (((index . digit-char) enumeration))
    (make-digit-and-index :digit (digit-char-p digit-char)
                          :index (+ start index))))
