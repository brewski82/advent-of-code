(defpackage #:advent-2025-day-3
  (:use #:common-lisp)
  (:import-from #:alexandria #:compose #:curry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-3)

(defstruct digit-and-index digit index)

(defun max-digit (a b)
  (if (> (digit-and-index-digit b)
         (digit-and-index-digit a))
      b
      a))

(defun to-digit-and-index (start enumeration )
  (bind (((index . digit-char) enumeration))
    (make-digit-and-index :digit (digit-char-p digit-char)
                          :index (+ start index))))

(defun largest-digit-and-index (bank start end)
  (t:transduce
   (t:comp (t:drop start)
           (t:take (- end start))
           #'t:enumerate
           (t:map (curry #'to-digit-and-index start)))
   (t:fold #'max-digit)
   bank))

(defstruct state start-index joltage bank battery-count)

(defun next-state (prev-state step)
  (bind (((:structure state- start-index joltage bank battery-count)
          prev-state)
         ((:structure digit-and-index- digit index)
          (largest-digit-and-index bank
                                   (1+ start-index)
                                   (+ (- (length bank) battery-count)
                                      (1+ step)))))
    (make-state :start-index index
                :joltage (+ (* 10 joltage)
                            digit)
                :bank bank
                :battery-count battery-count)))

(defun process-bank (battery-count bank)
  (t:transduce
   (t:take battery-count)
   (t:fold #'next-state
           (make-state :start-index -1
                       :joltage 0
                       :bank bank
                       :battery-count battery-count))
   (t:ints 0)))

(defun largest-joltage (battery-count bank)
  (~>> (process-bank battery-count bank)
       state-joltage))

(defun total-output-joltage (battery-count banks)
  (t:transduce
   (t:map (curry #'largest-joltage battery-count))
   #'+
   banks))

(defun read-input ()
  (uiop:read-file-lines "~/Downloads/2025-day-3-part-1"))

(defun solution-part-1 ()
  (~>> (read-input)
       (total-output-joltage 2)))


(defun solution-part-2 ()
  (~>> (read-input)
       (total-output-joltage 12)))
