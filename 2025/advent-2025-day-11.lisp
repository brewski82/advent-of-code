;; Advent of Code 2025 Day 11

(defpackage #:advent-2025-day-11
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-11-part-1 ()
  "Solve Advent of Code 2025 Day 11 Part 1."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-11-part-1")
       process-part-1))

(defun process-part-1 (input)
  "Process input for Day 11 Part 1."
  (~>> input
       parse-lines
       to-plist
       to-hash-table
       dfs))

(defun dfs (table &optional (node "you"))
  "Count paths from NODE to \"out\" in adjacency TABLE.

TABLE is a hash table mapping node names to lists of neighbors.
NODE is the starting node (defaults to \"you\").

Returns the total count of paths from NODE to \"out\"."
  (cond ((string= node "out") 1)
        ((null (gethash node table)) 0)
        (t (t:transduce (t:map (curry #'dfs table))
                        (t:fold #'+)
                        (gethash node table)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-11-part-2 ()
  "Solve Advent of Code 2025 Day 11 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-11-part-1")
       process-part-2))

(defun process-part-2 (input)
  "Process input for Day 11 Part 2."
  (~>> input
       parse-lines
       to-plist
       to-hash-table
       dfs-2))

(defun dfs-2 (table &key (node "svr") fft dac)
  "Count paths from NODE to \"out\" passing through FFT and DAC nodes.

TABLE is a hash table mapping node names to lists of neighbors.
NODE is the starting node (defaults to \"svr\").
FFT and DAC track whether we've visited those specific nodes.

Returns count of complete paths that visit both FFT and DAC before \"out\"."
  (cond ((and (string= node "out") fft dac) 1)
        ((string= node "out") 0)
        ((null (gethash node table)) 0)
        (t (t:transduce (t:map (curry #'dfs table
                                      :fft (or fft (string= node "fft"))
                                      :dac (or dac (string= node "dac"))
                                      :node))
                        (t:fold #'+)
                        (gethash node table)))))

(fare-memoization:memoize 'dfs-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-hash-table (plist)
  (alexandria:plist-hash-table plist :test #'equalp))

(defun to-plist (lines)
  (apply #'append lines))

(defun parse-lines (lines)
  (mapcar #'parse-line lines))

(defun parse-line (line)
  (bind (((key values) (cl-ppcre:split ":" line)))
    (list key (serapeum:tokens values))))
