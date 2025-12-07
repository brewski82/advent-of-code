(defpackage #:advent-2025-day-2
  (:use #:common-lisp)
  (:import-from #:serapeum #:~>>)
  (:import-from #:alexandria #:compose)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-2)

(defun invalid-id-p (id)
  (bind ((num-digits (1+ (floor (log id 10))))
         (half-digits (/ num-digits 2)))
    (when (evenp num-digits)
      (bind ((divisor (expt 10 half-digits))
             (left-half (floor id divisor))
             (right-half (mod id divisor)))
        (= left-half right-half)))))

(defun sum-invalid-ids (range)
  (t:transduce
   (t:comp (t:take (1+ (- (range-end range) (range-start range))))
           (t:filter #'invalid-id-p))
   #'+
   (t:ints (range-start range))))

(defun parse-input (input)
  (str:split "," input))

(defstruct range start end)

(defun make-range-from-string (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer start) (#'parse-integer end))
      ("(\\d+)-(\\d+)" string)
    (make-range :start start :end end)))

(defun process-part-1 (input)
  (t:transduce
   (t:map (compose #'sum-invalid-ids
                   #'make-range-from-string))
   #'+
   (parse-input input)))

(defun solution-part-1 ()
  (process-part-1 (uiop:read-file-string "~/Downloads/2025-day-2-part-1")))

(defun invalid-id-part-2-p (id)
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

(defun sum-invalid-ids-part-2 (range)
  (t:transduce
   (t:comp (t:take (1+ (- (range-end range) (range-start range))))
           (t:filter #'invalid-id-part-2-p))
   #'+
   (t:ints (range-start range))))

(defun process-part-2 (input)
  (t:transduce
   (t:map (compose #'sum-invalid-ids-part-2
                   #'make-range-from-string))
   #'+
   (parse-input input)))

(defun solution-part-2 ()
  (process-part-2 (uiop:read-file-string "~/Downloads/2025-day-2-part-1")))
