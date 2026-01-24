;; Advent of Code 2025 Day 10

(defpackage #:advent-2025-day-10
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-10)

(defun day-10-part-1 ()
  "Solve Advent of Code 2025 Day 10 Part 1."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-10-part-1")
       process-part-1))

(defun day-10-part-2 ()
  "Solve Advent of Code 2025 Day 10 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-10-part-1")
       process-part-2))

(defun process-part-1 (input)
  "Process input for Day 10 Part 1."
  (~>> input
       parse-lines
       count-pushes
       (reduce #'+)))

(defun process-part-2 (input)
  "Process input for Day 10 Part 2."
  (~>> input
       parse-lines))

(defstruct machine lights buttons joltages)
(defstruct item lights count)

(defun count-pushes (machines)
  (mapcar #'bfs machines))

(defun bfs (machine)
  (bind ((length (length (machine-lights machine)))
         (item (make-item :lights (make-array length
                                              :element-type 'bit
                                              :initial-element 0)
                          :count 0))
         (q (serapeum:queue item))
         ((:flet on-p (item)) (equalp (item-lights item) (machine-lights machine)))
         (i 0))
    (loop (bind ((item (serapeum:deq q)))

            (when (on-p item)
              (return-from bfs (item-count item)))
            (incf i)
            (when (= 2000000 i)
              (error "hit limit"))
            (mapc (lambda (button)
                    (serapeum:enq (make-item :lights (push-button (item-lights item) button)
                                             :count (1+ (item-count item)))
                                  q))
                  (machine-buttons machine))))))

(defun parse-lines (lines)
  (mapcar #'parse-line lines))

(defun parse-line (line)
  (cl-ppcre:register-groups-bind ((#'parse-lights lights)
                                  buttons
                                  (#'parse-joltages joltages))
      ("\\[(.*)\\] (.*) {(.*)}" line)
    (make-machine :lights lights
                  :buttons (parse-buttons (length lights) buttons)
                  :joltages joltages)))

(defun parse-lights (lights)
  (make-array (length lights)
              :element-type 'bit
              :initial-contents (map 'list #'parse-light lights)))

(defun parse-light (light)
  (ecase light
    (#\. 0)
    (#\# 1)))

(defun parse-buttons (length buttons)
  (mapcar (curry #'parse-button length)
          (cl-ppcre:all-matches-as-strings "[\\d\\,]+" buttons)))

(defun parse-button (length button)
  (bind ((numbers (mapcar #'parse-integer (cl-ppcre:split "," button)))
         (bit-array (make-array length :element-type 'bit)))
    (mapc (lambda (position)
            (setf (bit bit-array position) 1))
          numbers)
    bit-array))

(defun push-button (lights button)
  (bit-xor lights button ))

(defun parse-joltages (joltages)
  (mapcar #'parse-integer
          (cl-ppcre:split "," joltages)))
