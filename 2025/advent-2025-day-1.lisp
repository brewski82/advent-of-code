(defpackage #:advent-2025-day-1
  (:use #:common-lisp)
  (:import-from #:alexandria #:compose)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-1)

(defstruct rotation direction distance)

(defun parse-rotation (input)
  (make-rotation :direction (subseq input 0 1)
                 :distance (parse-integer (subseq input 1))))

(defun to-distance (rotation)
  (if (string-equal "L" (rotation-direction rotation))
      (- (rotation-distance rotation))
      (rotation-distance rotation)))

(defstruct dial-state value password)

(defun next-dial-state (dial-state distance)
  (bind ((next-value (mod (+ (dial-state-value dial-state)
                             distance)
                          100))
         (next-password (if (zerop next-value)
                            (1+ (dial-state-password dial-state))
                            (dial-state-password dial-state))))
    (make-dial-state :value next-value
                     :password next-password)))

(defun process-part-1 ()
  (t:transduce
   (t:map (compose #'to-distance
                   #'parse-rotation))
   (t:fold #'next-dial-state (make-dial-state :value 50 :password 0))
   #p"~/Downloads/2025-day-1a"))

(defun solution-part-1 ()
  (dial-state-password (process-part-1)))

(defun next-dial-state-part-2 (dial-state distance)
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

(defun process-part-2 ()
  (t:transduce
   (t:map (compose #'to-distance
                   #'parse-rotation))
   (t:fold #'next-dial-state-part-2 (make-dial-state :value 50 :password 0))
   #p"~/Downloads/2025-day-1a"))

(defun solution-part-2 ()
  (dial-state-password (process-part-2)))
