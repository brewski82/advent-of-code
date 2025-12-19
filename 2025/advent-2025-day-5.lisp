(defpackage #:advent-2025-day-5
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar)
  (:import-from #:serapeum #:~>> #:append1)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-5)

(defstruct range lower upper)
(defstruct ranges-ids ranges ids)

(defun to-range (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer lower) (#'parse-integer upper))
      ("(\\d+)-(\\d+)" string)
    (make-range :lower lower :upper upper)))

(defun ranges-ids-fold (prev string)
  (bind (((:structure ranges-ids- ranges ids)
          prev))
    (cond ((alexandria:emptyp string) prev)
          ((find #\- string)
           (make-ranges-ids :ranges (cons (to-range string) ranges)
                            :ids ids))
          (t (make-ranges-ids :ranges ranges
                              :ids (cons (parse-integer string) ids))))))

(defun to-ranges-ids (lines)
  (t:transduce #'t:pass
               (t:fold #'ranges-ids-fold (make-ranges-ids))
               lines))

(defun fresh-p (ranges id)
  (some (lambda (range)
          (<= (range-lower range)
              id
              (range-upper range)))
        ranges))

(defun count-fresh-ids (ranges-ids)
  (bind (((:structure ranges-ids- ranges ids)
          ranges-ids))
    (count-if (curry #'fresh-p ranges) ids)))

(defun process-part-1 (lines)
  (~>> lines
       to-ranges-ids
       count-fresh-ids))

(defun part-1 ()
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-5-part-1")
       process-part-1))

(defun compare-ranges (a b)
  (or (< (range-lower a) (range-lower b))
      (and (= (range-lower a) (range-lower b))
           (< (range-upper a) (range-upper b)))))

(defun sort-ranges (ranges)
  (sort ranges #'compare-ranges))

(defun consolidate-ranges-fold (ranges current)
  (bind ((prev-range (alexandria:lastcar ranges)))
    (cond
      ;; If this is the first element, set it as the first of the consolidated
      ;; list of ranges.
      ((null prev-range) (list current))
      ;; If the start of the current range is within the previous range,
      ;; consolidate them.
      ((<= (range-lower current) (range-upper prev-range))
       (append1 (butlast ranges)
                (make-range :lower (range-lower prev-range)
                            :upper (max (range-upper prev-range)
                                        (range-upper current)))))
      ;; Else we have the start of a new range.
      (t (append1 ranges current)))))

(defun consolidate-ranges (ranges)
  (t:transduce #'t:pass
               (t:fold #'consolidate-ranges-fold (list))
               ranges))

(defun range-id-count (range)
  (1+ (- (range-upper range)
         (range-lower range))))


(defun process-part-2 (lines)
  (~>> lines
       to-ranges-ids
       ranges-ids-ranges
       sort-ranges
       consolidate-ranges
       (mapcar #'range-id-count)
       (reduce #'+)))

(defun part-2 ()
  (~>> (uiop:read-file-lines "~/Downloads/2025-day-5-part-1")
       process-part-2))
