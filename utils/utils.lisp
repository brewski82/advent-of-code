(defpackage #:advent-utils
  (:use #:common-lisp)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:alexandria #:curry #:compose)
  (:local-nicknames (:t :transducers)
                    (:s :serapeum))
  (:export #:map-array
           #:mapc-array
           #:reduce-array
           #:neighbors
           #:to-char-array
           #:array-to-lists
           #:transpose))

(in-package #:advent-utils)

(defun map-array (fn arr)
  "Return a new array with the same dimensions as ARR. FN is called as (fn value
&rest indexes)."
  (let* ((dims (array-dimensions arr))
         (result (make-array dims
                             :element-type (array-element-type arr))))

    (labels ((walk (dims index-so-far)
               (if (null dims)
                   (let* ((indexes (reverse index-so-far))
                          (value   (apply #'aref arr indexes)))
                     (setf (apply #'aref result indexes)
                           (apply fn value indexes)))
                   (dotimes (i (car dims))
                     (walk (cdr dims) (cons i index-so-far))))))

      (walk dims '())
      result)))


(defun mapc-array (fn arr)
  "Call FN on every element of ARR. FN is called as (fn value &rest indexes)."
  (labels ((walk (dims index-so-far)
             (if (null dims)
                 (let* ((indexes (reverse index-so-far))
                        (value   (apply #'aref arr indexes)))
                   (apply fn value indexes))
                 (dotimes (i (car dims))
                   (walk (cdr dims) (cons i index-so-far))))))
    (walk (array-dimensions arr) '())
    arr))

(defun reduce-array (fn arr &key (initial-value nil initial-p))
  "Reduce over ARR. FN is called as (fn acc value &rest indexes). Returns the final
accumulated value."

  (let* ((dims (array-dimensions arr))
         (acc initial-value)
         (first? t))

    (labels ((walk (dims index-so-far)
               (if (null dims)
                   (let* ((indexes (reverse index-so-far))
                          (value   (apply #'aref arr indexes)))
                     (cond
                       ;; No initial value: first element becomes accumulator
                       ((and (not initial-p) first?)
                        (setf acc value
                              first? nil))
                       ;; Normal reduction
                       (t
                        (setf acc
                              (apply fn acc value indexes)))))
                   (dotimes (i (car dims))
                     (walk (cdr dims) (cons i index-so-far))))))

      (walk dims '())
      acc)))

(defun valid-array-indexes-p (arr row col)
  (let* ((array-dimensions (array-dimensions arr))
         (rows (first array-dimensions))
         (cols (second array-dimensions)))
    (and (< -1 row rows)
         (< -1 col cols))))

(defun neighbors (arr row col)
  "Returns the eight neighbors of a 2D as a list."
  (remove-if #'null
               (list (and (valid-array-indexes-p arr (1- row) (1- col))
                          (aref arr (1- row) (1- col)))
                     (and (valid-array-indexes-p arr (1- row) col)
                          (aref arr (1- row) col))
                     (and (valid-array-indexes-p arr (1- row) (1+ col))
                          (aref arr (1- row) (1+ col)))
                     (and (valid-array-indexes-p arr row (1+ col))
                          (aref arr row (1+ col)))
                     (and (valid-array-indexes-p arr (1+ row) (1+ col))
                          (aref arr (1+ row) (1+ col)))
                     (and (valid-array-indexes-p arr (1+ row) col)
                          (aref arr (1+ row) col))
                     (and (valid-array-indexes-p arr (1+ row) (1- col))
                          (aref arr (1+ row) (1- col)))
                     (and (valid-array-indexes-p arr row (1- col))
                          (aref arr row (1- col))))))

(defun to-char-array (lines)
  (make-array (list (length lines)
                    (length (first lines)))
              :initial-contents lines))

(defun array-to-lists (array)
  (mapcar (lambda (i)
            (mapcar (lambda (j)
                      (aref array i j))
                    (alexandria:iota (array-dimension array 1))))
          (alexandria:iota (array-dimension array 0))))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))
