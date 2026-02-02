;; Advent of Code 2025 Day 10

(defpackage #:advent-2025-day-10
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>>)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-10)

(defstruct machine lights button-bits button-numbers joltages)
(defstruct item target count)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1
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
  (mapcar (lambda (machine)
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
          machines))

(defun bfs (&key init-item done-p next)
  (bind ((q (serapeum:queue init-item))
         (i 0))
    (loop (bind ((item (serapeum:deq q)))
            (when (funcall done-p item)
              (return-from bfs (item-count item)))
            (incf i)
            (when (= 200000 i)
              (error "hit limit"))
            (funcall next q item)))))

(defun push-button (lights button)
  (bit-xor lights button ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun day-10-part-2 ()
  "Solve Advent of Code 2025 Day 10 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-10-part-1")
       process-part-2))

(defun process-part-2 (input)
  "Process input for Day 10 Part 2."
  (~>> input
       parse-lines
       count-joltage-pushes
       (reduce #'+)
       ))

;; (defun next-joltages (joltages button)
;;   (bind ((list (copy-list joltages)))
;;     (mapc (lambda (index)
;;             (decf (nth index list)))
;;           button)
;;     list))

(defun button-joltages (joltages button)
  (mapc (lambda (index)
          (incf (nth index joltages)))
        button))

(defun buttons-joltages (joltage-length buttons)
  (bind ((list (make-list joltage-length :initial-element 0)))
    (unless buttons
      (return-from buttons-joltages list))
    (mapc (curry #'button-joltages list)
          buttons)
    list))

(defun joltages-parity (joltage)
  (flet ((to-parity (number)
           (if (oddp number)
               1
               0)))
    (mapcar #'to-parity joltage)))

(defun make-parity-map (joltage-length buttons)
  (bind ((hash-table (make-hash-table :test #'equalp))
         (powerset (serapeum:powerset buttons))
         ((:flet add-entry (set))
          (bind ((buttons-joltages (buttons-joltages joltage-length set))
                 (parity (joltages-parity buttons-joltages))
                 (entry (gethash parity hash-table)))
            (setf (gethash parity hash-table)
                  (serapeum:append1 entry
                                    (make-pushes-button :pushes (length set)
                                                        :button buttons-joltages))))))
    (mapc #'add-entry powerset)
    hash-table))

(defstruct pushes-button pushes button)

(defun next-joltages (joltages button)
  (mapcar (lambda (joltage value)
            (/ (- joltage value)
               2))
          joltages button))

(defun count-presses (map joltages)
  (when (every #'zerop joltages)
    (progn ;; (print "found")
           (return-from count-presses 0)))
  (when (some #'minusp joltages)
    (return-from count-presses most-positive-fixnum))
  (bind ((parity (joltages-parity joltages))
         (values (gethash parity map)))
    (unless values
      (return-from count-presses most-positive-fixnum))
    (reduce (lambda (best value)
              (bind (((:structure pushes-button- pushes button) value)
                     (new-joltages (next-joltages joltages button)))
                ;; (format t "joltages=~A new-joltages=~A pushes=~A button=~A parity=~A~%"
                ;;         joltages new-joltages pushes button parity)
                (min best
                     (+ pushes (* 2 (count-presses map new-joltages))))))
            values
            :initial-value most-positive-fixnum)))

(defun count-joltage-pushes (machines)
  (mapcar (lambda (machine)
            (print machine)
            (count-presses (make-parity-map (length (machine-joltages machine))
                                            (machine-button-numbers machine))
                           (machine-joltages machine))
            ;; (dfs :joltages (machine-joltages machine)
            ;;      :depth 0
            ;;      :cache (make-hash-table :test #'equalp)
            ;;      :best *worst-best*
            ;;      :buttons (machine-button-numbers machine))
            )
          machines))

(defstruct best height depth)

(defvar *worst-best* (make-best :height most-positive-fixnum :depth most-positive-fixnum))

(defun dfs (&key joltages depth cache best buttons)
  (when (>= depth (best-depth best))
    (return-from dfs *worst-best*))
  (alexandria:when-let (cached-best (gethash joltages cache))
    (return-from dfs cached-best))
  (when (every #'zerop joltages)
    (return-from dfs (make-best :height 0 :depth depth)))
  (unless (valid-joltage-state-p joltages)
    (return-from dfs *worst-best*))
  (bind ((next-best (reduce (lambda (next button)
                              (bind ((this-best (dfs :joltages (next-joltages joltages button)
                                                     :depth (1+ depth)
                                                     :buttons buttons
                                                     :cache cache
                                                     :best next
                                                     ;; :last-buttons (serapeum:append1 last-buttons button)
                                                     )))
                                (if (< (best-height this-best) (best-height next))
                                    (make-best :height (1+ (best-height this-best)) :depth (best-depth this-best))
                                    next)))
                            buttons
                            :initial-value best
                            )))
    (setf (gethash joltages cache) (or next-best *worst-best*))
    ;; (when (or (not next-best) (eq :impossible next-best))
    ;;   (setf (gethash joltages cache) :impossible)
    ;;   (return-from dfs :impossible))

    ;; (let ((cached-value (gethash joltages cache)))
    ;;   (when (or (and (not cached-value)
    ;;                  (not (eq :impossible next-best))
    ;;                  (/= most-positive-fixnum (best-value next-best)))
    ;;             (and cached-value
    ;;                  (< (best-value next-best)
    ;;                     (best-value cached-value))))
    ;;     (setf (gethash joltages cache) next-best)))
    next-best))

(defun next-joltages (joltages button)
  (bind ((list (copy-list joltages)))
    (mapc (lambda (index)
            (decf (nth index list)))
          button)
    list))

(defun valid-joltage-state-p (joltages)
  (every (curry #'<= 0) joltages))

(defun push-joltage-button (joltages button)
  (bind ((list (copy-list joltages)))
    (mapc (lambda (index)
            (incf (nth index list)))
          button)
    list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-lines (lines)
  (mapcar #'parse-line lines))

(defun parse-line (line)
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
  (mapcar #'parse-csv
          (cl-ppcre:all-matches-as-strings "[\\d\\,]+" buttons)))

(defun parse-csv (csv)
  (mapcar #'parse-integer
          (cl-ppcre:split "," csv)))
