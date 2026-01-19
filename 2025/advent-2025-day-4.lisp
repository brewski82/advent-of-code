(defpackage #:advent-2025-day-4
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-4)

;;; Advent of Code 2025 Day 4: Printing Department
;;;
;;; Problem: Optimize forklift work by identifying accessible paper rolls.
;;; Paper rolls (@) are arranged on a grid. A forklift can only access a roll
;;; if it has fewer than 4 adjacent rolls (in the 8 surrounding positions).
;;;
;;; Part 1: Count how many rolls are initially accessible
;;; Part 2: Count total rolls that can be removed by repeatedly removing
;;;         accessible rolls until none remain

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun part-1 ()
  "Solve part 1: Count initially accessible paper rolls."
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-4-part-1")
       to-char-array
       count-accessible))

(defun count-accessible (arr)
  "Count accessible paper rolls in ARR.

A roll is accessible if it has fewer than 4 adjacent rolls."
  (reduce-array (curry #'count-location arr)
                arr
                :initial-value 0))

(defun count-location (arr count value row col)
  (if (accessible-p value arr row col)
      (1+ count)
      count))

(defun accessible-p (value arr row col)
  "Return T if position contains an accessible paper roll.

Checks all 8 adjacent positions (orthogonal and diagonal)."
  (and (paper-roll-p value)
       (< (adjacent-rolls arr row col)
          4)))

(defun adjacent-rolls (arr row col)
  (count-if #'paper-roll-p (neighbors arr row col)))

(defun paper-roll-p (char)
  (char-equal char #\@))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun part-2 ()
  "Solve part 2: Count total rolls removable through iterative removal."
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-4-part-1")
       to-char-array
       count-accessible-part-2))

(defun count-accessible-part-2 (arr &optional (total-count 0))
  "Recursively remove accessible rolls and return total count.

Removes accessible rolls repeatedly until none remain."
  (bind ((count (count-accessible arr)))
    (if (zerop count)
        total-count
        (count-accessible-part-2 (remove-accessible arr)
                                 (+ count total-count)))))

(defun remove-accessible (arr)
  "Return new array with accessible rolls replaced by empty spaces."
  (map-array (curry #'maybe-remove-roll arr)
             arr))

(defun maybe-remove-roll (arr value row col)
  (if (accessible-p value arr row col)
      #\.
      value))
