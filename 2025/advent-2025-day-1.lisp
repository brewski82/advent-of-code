;;;; Advent of Code 2025 - Day 1 Solution
;;;;
;;;; Simulates a dial that rotates based on input commands.
;;;; Tracks the dial state and counts crossing zero.

(defpackage #:advent-2025-day-1
  (:use #:common-lisp)
  (:import-from #:alexandria #:compose)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-1)

;;; Entry Points

(defun solution-part-1 ()
  "Solve part 1: extract password from final dial state."
  (dial-state-password (process-part-1)))

(defun solution-part-2 ()
  "Solve part 2: extract password counting zero crossings."
  (dial-state-password (process-part-2)))

;;; Part 1: Simple Zero Counting

(defun process-part-1 ()
  "Process rotation commands and accumulate dial state for part 1."
  (t:transduce
   (t:map (compose #'to-distance
                   #'parse-rotation))
   (t:fold #'next-dial-state (make-dial-state :value 50 :password 0))
   #p"~/Downloads/2025-day-1a"))

(defun next-dial-state (dial-state distance)
  "Update dial state by rotating, incrementing password if crossing zero."
  (bind ((next-value (mod (+ (dial-state-value dial-state)
                             distance)
                          100))
         (next-password (if (zerop next-value)
                            (1+ (dial-state-password dial-state))
                            (dial-state-password dial-state))))
    (make-dial-state :value next-value
                     :password next-password)))

;;; Part 2: Advanced Zero Crossing Detection

(defun process-part-2 ()
  "Process rotation commands and accumulate dial state for part 2."
  (t:transduce
   (t:map (compose #'to-distance
                   #'parse-rotation))
   (t:fold #'next-dial-state-part-2 (make-dial-state :value 50 :password 0))
   #p"~/Downloads/2025-day-1a"))

(defun next-dial-state-part-2 (dial-state distance)
  "Update dial state counting zeros passed during rotation and at crossings."
  (bind ((dial-value (dial-state-value dial-state))
         (sum (+ dial-value distance))
         (next-value (mod sum 100))
         (zeros-while-turning (truncate (abs sum) 100))
         (crossed-zero-p (and (not (zerop dial-value))
                              (/= (signum dial-value)
                                  (signum sum))))
         (zeros-from-crossing (if crossed-zero-p 1 0))
         (next-password (+ (dial-state-password dial-state)
                           zeros-while-turning
                           zeros-from-crossing)))
    (make-dial-state :value next-value
                     :password next-password)))

;;; Dial State

(defstruct dial-state
  value      ; Current dial position (0-99)
  password)  ; Accumulated password count

;;; Input Parsing

(defun parse-rotation (input)
  "Parse STRING into a rotation command (direction + distance)."
  (make-rotation :direction (subseq input 0 1)
                 :distance (parse-integer (subseq input 1))))

(defstruct rotation
  direction  ; \"L\" or \"R\"
  distance)  ; Numeric distance to rotate

(defun to-distance (rotation)
  "Convert rotation to signed distance (negative for left, positive for right)."
  (if (string-equal "L" (rotation-direction rotation))
      (- (rotation-distance rotation))
      (rotation-distance rotation)))
