(defpackage #:advent-2024-day-11
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:serapeum #:append1 #:~>>)
  (:import-from #:nclasses #:define-class)
  (:import-from #:access #:access #:accesses #:set-access)
  (:import-from #:array-operations/utilities #:nested-loop))

(in-package #:advent-2024-day-11)

(defparameter *stone-count* 0)
(defparameter *blink-count* 75)
(defparameter *cache* (make-hash-table))

(defun cache (stone blink-count value)
  (bind ((sub-table (or (gethash stone *cache*) (make-hash-table))))
    (setf (gethash blink-count sub-table) value)
    (setf (gethash stone *cache*)
          sub-table))
  value)

(defun get-cache (stone blink-count)
  (when (gethash stone *cache*)
    (gethash blink-count (gethash stone *cache*))))

(defun day-11 ()
  (setf *stone-count* 0)
  (setf *cache* (make-hash-table))
  (~>> (parse-stones "70949 6183 4 3825336 613971 0 15 182")
       (mapc (rcurry #'blink 0)))
  *stone-count*)

(defun blink (stone count)
  (bind ((cached-value (get-cache stone count)))
    (cond (cached-value
           (incf *stone-count* cached-value)
           cached-value)
          ((= *blink-count* count)
           (incf *stone-count*)
           1)
          ((zerop stone)
           (~>> (blink 1 (1+ count))
                (cache stone count)))
          (t (bind ((digit-count (digit-count stone))
                    (value (if (evenp digit-count)
                               (~>> (split-stone stone digit-count)
                                    (mapcar (rcurry #'blink (1+ count)))
                                    (reduce #'+))
                               (blink (* 2024 stone) (1+ count)))))
               (cache stone count value))))))

(defun digit-count (stone)
  (1+ (truncate (log stone 10))))

(defun split-stone (stone digit-count)
  (multiple-value-bind (l r)
      (truncate stone (expt 10 (/ digit-count 2)))
    (list l (ceiling r))))

(defun parse-stones (string)
  (~>> string
       (str:split-omit-nulls " ")
       (mapcar #'parse-integer)))
