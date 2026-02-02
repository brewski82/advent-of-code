;; Advent of Code 2025 Day 10: Factory
;;
;; This solution solves the button-pressing optimization problem where we must find
;; the minimum number of button presses to configure machine indicator lights and
;; joltage counters.
;;
;; Part 1: Uses breadth-first search (BFS) to find the minimum presses needed to
;;   match target indicator light patterns by toggling lights with buttons.
;;
;; Part 2: Uses an optimized "parity halving" strategy that pre-computes all 2^n
;;   possible button combinations and organizes them by their parity pattern
;;   (even/odd for each counter). This enables:
;;     1. Massive pruning: only try combinations matching current target's parity
;;     2. Exponential speedup: halve the target each recursion (O(log sum) depth)
;;   This strategy reduces complexity from brute-force exponential to tractable.
;;
;; Algorithm credit: https://github.com/JoanaBLate/advent-of-code-js/blob/main/2025/day10-solve2.js

(defpackage #:advent-2025-day-10
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-10)

(defstruct machine
  "Machine with lights, buttons, and target joltages."
  lights           ; bit-array of target indicator light states
  button-bits      ; bit-arrays for Part 1 light toggling
  button-numbers   ; lists of counter indices for Part 2
  joltages)        ; target joltage values

(defstruct item
  "BFS queue item with current state and press count."
  target           ; current state (bit-array for Part 1, list for Part 2)
  count)           ; number of button presses so far

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1
;;; BFS strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-10-part-1 ()
  "Solve Advent of Code 2025 Day 10 Part 1."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-10-part-1")
       process-part-1))

(defun process-part-1 (input)
  "Process input for Day 10 Part 1."
  (~>> input
       parse-lines
       count-pushes
       (reduce #'+)))

(defun count-pushes (machines)
  "Solve Part 1 for all machines and return list of minimum presses."
  (mapcar #'count-pushes-for-machine machines))

(defun count-pushes-for-machine (machine)
  "Find minimum button presses to match target light pattern."
  (bfs :init-item (make-item :target
                             (make-array (length (machine-lights machine))
                                         :element-type 'bit
                                         :initial-element 0)
                             :count 0)
       :done-p (lambda (item)
                 (equalp (item-target item)
                         (machine-lights machine)))
       :next (lambda (queue item)
               (mapc (lambda (button)
                       (serapeum:enq
                        (make-item :target
                                   (push-button (item-target item) button)
                                   :count
                                   (1+ (item-count item)))
                        queue))
                     (machine-button-bits machine)))))

(defun bfs (&key init-item done-p next)
  "Generic breadth-first search.

INIT-ITEM: starting state.
DONE-P: predicate function to check if goal reached.
NEXT: function that enqueues next states, receives queue and current item."
  (bind ((q (serapeum:queue init-item)))
    (loop (bind ((item (serapeum:deq q)))
            (when (funcall done-p item)
              (return-from bfs (item-count item)))
            (funcall next q item)))))

(defun push-button (lights button)
  "Toggle lights by XOR with button's bit-array."
  (bit-xor lights button))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;; DFS with memoization too slow. Used halving strategy from
;;; https://github.com/JoanaBLate/advent-of-code-js/blob/main/2025/day10-solve2.js
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-10-part-2 ()
  "Solve Advent of Code 2025 Day 10 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-10-part-1")
       process-part-2))

(defun process-part-2 (input)
  "Process input for Day 10 Part 2."
  (~>> input
       parse-lines
       count-presses-for-machines
       (reduce #'+)))

(defun count-presses-for-machines (machines)
  "Solve Part 2 for all machines and return list of minimum presses."
  (mapcar (lambda (machine)
            (count-presses (make-parity-map (length (machine-joltages machine))
                                            (machine-button-numbers machine))
                           (machine-joltages machine)))
          machines))

(defstruct pushes-button
  "Pre-computed button combination with press count and joltage effects."
  pushes           ; number of button presses in this combination
  button)          ; resulting joltage changes

(defun make-parity-map (joltage-length buttons)
  "Pre-compute all 2^n button combinations organized by parity pattern.

Returns hash table mapping parity patterns to lists of pushes-button structures.
This enables pruning: only combinations matching the target's parity are tried."
  (bind ((hash-table (make-hash-table :test #'equalp))
         (powerset (serapeum:powerset buttons))
         ((:flet add-entry (set))
          (bind ((combined-buttons (combine-buttons joltage-length set))
                 (parity (joltages-parity combined-buttons))
                 (entry (gethash parity hash-table)))
            (setf (gethash parity hash-table)
                  (serapeum:append1 entry
                                    (make-pushes-button :pushes (length set)
                                                        :button combined-buttons))))))
    (mapc #'add-entry powerset)
    hash-table))

(defun count-presses (map joltages)
  "Find minimum button presses to reach target joltages.

Uses parity halving: matches target's parity pattern, subtracts matching
combination, divides by 2, and recurses. This gives O(log sum) depth instead
of exponential time.

MAP: hash table from make-parity-map.
JOLTAGES: current target joltages."
  (when (every #'zerop joltages)
    (return-from count-presses 0))
  (when (some #'minusp joltages)
    (return-from count-presses most-positive-fixnum))
  (bind ((parity (joltages-parity joltages))
         (pushes-buttons (gethash parity map)))
    (unless pushes-buttons
      ;; No combination exists with this parity pattern - dead end
      (return-from count-presses most-positive-fixnum))
    ;; Try all combinations with matching parity, keep minimum result
    (t:transduce (t:map (lambda (pushes-button)
                          (bind (((:structure pushes-button- pushes button) pushes-button)
                                 (new-joltages (next-joltages joltages button)))
                            (+ pushes (* 2 (count-presses map new-joltages))))))
                 (t:fold #'min most-positive-fixnum)
                 pushes-buttons)))

(defun joltages-parity (joltage)
  "Extract parity pattern: map each joltage to 0 (even) or 1 (odd)."
  (flet ((to-parity (number)
           (if (oddp number) 1 0)))
    (mapcar #'to-parity joltage)))

(defun next-joltages (joltages button)
  "Subtract button effects from joltages and divide by 2.

Used by parity halving: subtracting a parity-matched combination gives all
evens, which we divide by 2 to reduce problem size exponentially."
  (mapcar (lambda (joltage value)
            (/ (- joltage value) 2))
          joltages button))

(defun combine-buttons (joltage-length buttons)
  "Combine a set of buttons into a single joltage effect vector.

Sums the effects of pressing each button in the set once."
  (bind ((combined (make-list joltage-length :initial-element 0)))
    (unless buttons
      (return-from combine-buttons combined))
    (mapc (curry #'combine-button combined) buttons)
    combined))

(defun combine-button (buttons button)
  (mapc (lambda (index)
          (incf (nth index buttons)))
        button))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-lines (lines)
  "Parse all input lines into machine structures."
  (mapcar #'parse-line lines))

(defun parse-line (line)
  "Parse a single machine line into a machine structure.

Input format: [lights] button1 button2 ... {joltages}"
  (cl-ppcre:register-groups-bind ((#'parse-lights lights)
                                  buttons
                                  (#'parse-csv joltages))
      ("\\[(.*)\\] (.*) {(.*)}" line)
    (make-machine :lights lights
                  :button-bits (parse-buttons-bits (length lights) buttons)
                  :button-numbers (parse-buttons-numbers buttons)
                  :joltages joltages)))

(defun parse-lights (lights)
  (make-array (length lights)
              :element-type 'bit
              :initial-contents (map 'list #'parse-light lights)))

(defun parse-light (light)
  (ecase light
    (#\. 0)
    (#\# 1)))

(defun parse-buttons-bits (length buttons)
  "Parse button definitions and convert to bit-arrays for Part 1."
  (mapcar (curry #'parse-button-bits length)
          (cl-ppcre:all-matches-as-strings "[\\d\\,]+" buttons)))

(defun parse-button-bits (length button)
  (bind ((numbers (mapcar #'parse-integer (cl-ppcre:split "," button)))
         (bit-array (make-array length :element-type 'bit)))
    (mapc (lambda (position)
            (setf (bit bit-array position) 1))
          numbers)
    bit-array))

(defun parse-buttons-numbers (buttons)
  "Parse button definitions into lists of counter indices for Part 2."
  (mapcar #'parse-csv
          (cl-ppcre:all-matches-as-strings "[\\d\\,]+" buttons)))

(defun parse-csv (csv)
  "Parse comma-separated number string to list of integers."
  (mapcar #'parse-integer
          (cl-ppcre:split "," csv)))
