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
           #:neighbor-points
           #:to-char-array
           #:array-to-lists
           #:transpose
           #:point
           #:distance
           #:x
           #:y
           #:z
           #:csv-line-to-2d-point
           #:csv-lines-to-2d-points
           #:area
           #:make-grid
           #:row
           #:col
           #:make-edge
           #:point-a
           #:point-b))

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

(defun neighbor-points (arr row col)
  "Returns the eight neighbors of a 2D array as a list of list of (row col)."
  (remove-if #'null
               (list (and (valid-array-indexes-p arr (1- row) (1- col))
                          (list (1- row) (1- col)))
                     (and (valid-array-indexes-p arr (1- row) col)
                          (list (1- row) col))
                     (and (valid-array-indexes-p arr (1- row) (1+ col))
                          (list (1- row) (1+ col)))
                     (and (valid-array-indexes-p arr row (1+ col))
                          (list row (1+ col)))
                     (and (valid-array-indexes-p arr (1+ row) (1+ col))
                          (list (1+ row) (1+ col)))
                     (and (valid-array-indexes-p arr (1+ row) col)
                          (list (1+ row) col))
                     (and (valid-array-indexes-p arr (1+ row) (1- col))
                          (list (1+ row) (1- col)))
                     (and (valid-array-indexes-p arr row (1- col))
                          (list row (1- col))))))

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

(defclass point ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defmethod fset:compare ((p1 point) (p2 point))
  (fset:compare-slots p1 p2 'x 'y))

(defclass 2d-point (point) ())

(defmethod print-object ((p 2d-point) stream)
  (print-unreadable-object (p stream :type t :identity nil)
    (format stream "(~a, ~a)" (x p) (y p))))

(defclass 3d-point (point)
  ((z :accessor z :initarg :z)))

(defmethod fset:compare ((p1 3d-point) (p2 3d-point))
  (fset:compare-slots p1 p2 'x 'y 'z))

(defclass edge ()
  ((point-a :accessor point-a :initarg :point-a)
   (point-b :accessor point-b :initarg :point-b)))

(defmethod fset:compare ((e1 edge) (e2 edge))
  (fset:compare-slots e1 e2 'point-a 'point-b))

(defun make-edge (p1 p2)
  (case (fset:compare p1 p2)
    (:greater (make-instance 'edge :point-a p2 :point-b p1))
    (otherwise (make-instance 'edge :point-a p1 :point-b p2))))

(defmethod print-object ((e edge) stream)
  (print-unreadable-object (e stream :type t :identity nil)
    (format stream "edge(~a, ~a)" (point-a e) (point-b e))))

(defgeneric row (point)
  (:method ((point 2d-point))
    (y point)))

(defgeneric col (point)
  (:method ((point 2d-point))
    (x point)))

(defmethod print-object ((p 3d-point) stream)
  (print-unreadable-object (p stream :type t :identity nil)
    (format stream "(~a, ~a, ~a)" (x p) (y p) (z p))))

(defgeneric distance (a b)
  (:method ((a 2d-point) (b 2d-point))
    (sqrt (+ (expt (- (x a) (x b)) 2)
             (expt (- (y a) (y b)) 2))))
  (:method ((a 3d-point) (b 3d-point))
    (sqrt (+ (expt (- (x a) (x b)) 2)
             (expt (- (y a) (y b)) 2)
             (expt (- (z a) (z b)) 2)))))

(defgeneric area (a b)
  (:method ((a 2d-point) (b 2d-point))
    (* (1+ (abs (- (x a) (x b))))
       (1+ (abs (- (y a) (y b)))))))

(defun point (&key x y z row col)
  (if z
      (make-instance '3d-point :x x :y y :z z)
      (make-instance '2d-point :x (or x col) :y (or y row))))

(defun csv-line-to-2d-point (line)
  "Parse a line of the form 'X,Y' into a 2D point structure."
  (cl-ppcre:register-groups-bind ((#'parse-integer x)
                                  (#'parse-integer y))
      ("(\\d+),(\\d+)" line)
    (point :x x :y y )))

(defun csv-lines-to-2d-points (lines)
  (mapcar #'csv-line-to-2d-point lines))

(defun make-grid (row-count col-count &key initial-element)
  (make-array (list row-count col-count) :initial-element initial-element))
