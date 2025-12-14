(defpackage #:advent-2025-day-4
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-4)

(defun paper-roll-p (char)
  (char-equal char #\@))

(defun adjacent-rolls (arr row col)
  (count-if #'paper-roll-p (neighbors arr row col)))

(defun accessible-p (value arr row col)
  (and (paper-roll-p value)
       (< (adjacent-rolls arr row col)
          4)))

(defun count-location (arr count value row col)
  (if (accessible-p value arr row col)
      (1+ count)
      count))

(defun count-accessible (arr)
  (reduce-array (curry #'count-location arr)
                arr
                :initial-value 0))

(defun part-1 ()
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-4-part-1")
       to-char-array
       count-accessible))

(defun maybe-remove-roll (arr value row col)
  (if (accessible-p value arr row col)
      #\.
      value))

(defun remove-accessible (arr)
  (map-array (curry #'maybe-remove-roll arr)
             arr))

(defun count-accessible-part-2 (arr &optional (total-count 0))
  (bind ((count (count-accessible arr)))
    (if (zerop count)
        total-count
        (count-accessible-part-2 (remove-accessible arr)
                                 (+ count total-count)))))

(defun part-2 ()
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-4-part-1")
       to-char-array
       count-accessible-part-2))
