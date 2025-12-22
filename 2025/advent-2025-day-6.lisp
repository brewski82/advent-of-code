(defpackage #:advent-2025-day-6
  (:use #:common-lisp #:advent-utils)
  (:import-from #:alexandria #:compose #:curry #:lastcar #:rcurry)
  (:import-from #:serapeum #:~>> #:append1)
  (:import-from #:metabang-bind #:bind)
  (:local-nicknames (:t :transducers)))

(in-package #:advent-2025-day-6)

(defun tokenize-input (lines)
  "Split each line in LINES into whitespace-separated tokens."
  (mapcar #'serapeum:tokens lines))

(defun group-equations (input)
  "Transpose INPUT, grouping corresponding elements from each list into equations."
  (apply #'mapcar #'list input))

(defun solve-equation (line)
  "Solve a single equation LINE. Parses numbers from all but the last element, and
applies the operator (+ or *) found in the last element."
  (bind ((numbers (mapcar #'parse-integer (butlast line))))
    (ecase (char (lastcar line) 0)
      (#\+ (apply #'+ numbers))
      (#\* (apply #'* numbers)))))

(defun solve-equations (lines)
  "Solve all equations in LINES and sum the results."
  (apply #'+ (mapcar #'solve-equation lines)))

(defun process-part-1 (lines)
  "Process input LINES for part 1: tokenize, group, and solve equations."
  (~>> lines
       tokenize-input
       group-equations
       solve-equations))

(defun part-1 ()
  "Solve Advent of Code 2025 Day 6 Part 1."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-6-part-1")
       process-part-1))

(defun column-p (array column)
  "Return T if COLUMN in ARRAY contains only whitespace characters."
  (t:transduce (t:comp (t:take (array-dimension array 0))
                       (t:map (lambda (row)
                                (aref array row column))))
               (t:all? #'serapeum:whitespacep)
               (t:ints 0)))

(defun find-columns (array)
  "Find all column indices in ARRAY that contain only whitespace."
  (t:transduce (t:comp (t:take (array-dimension array 1))
                       (t:filter (curry #'column-p array)))
               #'t:cons
               (t:ints 0)))

(defun maybe-replace-with-separator (separator-columns value row column)
  "Replace VALUE with pipe separator if it's whitespace in a separator column."
  (declare (ignore row))
  (if (and (serapeum:whitespacep value)
           (member column separator-columns))
      #\|
      value))

(defun inject-separators (array separator-columns)
  "Replace whitespace in SEPARATOR-COLUMNS of ARRAY with pipe characters."
  (map-array (curry #'maybe-replace-with-separator separator-columns)
             array))

(defun to-strings (lists)
  "Convert each list of characters in LISTS to a string."
  (mapcar (rcurry #'coerce 'string)
          lists))

(defun split-columns (lists)
  "Split each string in LISTS on pipe characters."
  (mapcar (curry #'serapeum:split-sequence #\|) lists))

(defun solve-equation-part-2 (equation)
  "Solve a single EQUATION for part 2. Transposes the numbers, converts to strings,
appends the operator, and solves."
  (bind ((transposed (transpose (butlast equation)))
         (strings (to-strings (append transposed (last equation)))))
    (solve-equation strings)))

(defun solve-equations-part-2 (lists)
  "Solve all equations in LISTS for part 2 and sum the results."
  (apply #'+ (mapcar #'solve-equation-part-2 lists)))

(defun process-part-2 (lines)
  "Process input LINES for part 2: find separator columns, inject separators,
split into equations, and solve all problems."
  (bind ((char-array (to-char-array lines))
         (separator-columns (find-columns char-array)))
    (~>> separator-columns
         (inject-separators char-array)
         array-to-lists
         split-columns
         group-equations
         solve-equations-part-2)))

(defun part-2 ()
  "Solve Advent of Code 2025 Day 6 Part 2."
  (~>> (uiop:read-file-lines #P"~/Downloads/2025-day-6-part-1")
       process-part-2))
