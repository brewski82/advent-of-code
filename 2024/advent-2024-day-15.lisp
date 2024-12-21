(defpackage #:advent-2024-day-15
  (:use #:common-lisp #:alexandria #:cl-ppcre)
  (:import-from #:metabang-bind #:bind)
  (:import-from #:serapeum #:append1 #:~>> #:~> #:batches)
  (:import-from #:nclasses #:define-class)
  (:import-from #:access #:access #:accesses #:set-access)
  (:import-from #:array-operations/utilities #:nested-loop))

(in-package #:advent-2024-day-15)

(defparameter *debug* nil)

(defun read-map-and-moves (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          when (str:starts-with-p "#" line)
            collect line into map
          else when (plusp (length line))
                 collect line into moves
          finally (return (list map moves)))))

(defun day-15 ()
  (bind (((map moves) (read-map-and-moves "~/Downloads/input-15"))
         (map (make-array (list (length map)
                                (length (first map)))
                          :initial-contents map))
         (moves (str:join "" moves))
         (initial-robot (find-robot map)))
    (loop for char across moves
          for move = (parse-move char)
          for robot = (move initial-robot move map) then (move robot move map)
          when *debug* do (format t "~&~A ~A~%" char move)
          when *debug* do (print-room map))
    (print-room map)
    (loop for row from 0 below (array-dimension map 0) summing
          (loop for col from 0 below (array-dimension map 1)
                when (char-equal #\O (aref map row col))
                  sum (gps row col)))))

(defun gps (row col)
  (+ (* 100 row) col))

(defun find-robot (map)
  (array-operations/utilities:nested-loop (row col) (array-dimensions map)
    (when (char-equal #\@ (aref map row col))
      (return-from find-robot (cons row col)))))

(defun row (coordinate)
  (car coordinate))

(defun col (coordinate)
  (cdr coordinate))

(defun parse-move (move)
  (cond ((char-equal #\> move) (cons 0 1))
        ((char-equal #\< move) (cons 0 -1))
        ((char-equal #\^ move) (cons -1 0))
        ((char-equal #\v move) (cons 1 0))
        (t (error "unknown move ~A" move))))

(defun move (robot move map)
  (bind ((space (find-space robot move map)))
    (when *debug*
      (format t "~&space = ~A robot = ~A move = ~A~%" space robot move))
    (if space
        (loop for s = space then (cons (+ (row s) (* -1 (row move)))
                                       (+ (col s) (* -1 (col move))))
              when (adjacent-p s robot)
                do (setf (aref map (row s) (col s)) #\@
                         (aref map (row robot) (col robot)) #\.)
                and do (return s)
              else do (setf (aref map (row s) (col s)) #\O)
              finally (return s))
        robot)))

(defun find-space (robot move map)
  (loop for space = robot then (cons (+ (row space) (row move))
                                     (+ (col space) (col move)))
        while (within-bounds-p space map)
        when (char-equal #\. (aref map (row space) (col space)))
          do (return space)))

(defun within-bounds-p (position map)
  (and (<= 0 (row position) (1- (array-dimension map 0)))
       (<= 0 (col position) (1- (array-dimension map 1)))
       (char-not-equal #\# (aref map (row position) (col position)))))

(defun adjacent-p (position robot)
  (and (<= (abs (- (row robot) (row position))) 1)
       (<= (abs (- (col robot) (col position))) 1)))


(defun print-room (room)
  (loop for row from 0 below (array-dimension room 0)
        do (fresh-line)
        do (loop for col from 0 below (array-dimension room 1)
                 do (princ (aref room row col)))))

(defun day-15-part-2 ()
  (bind (((map moves) (read-map-and-moves "~/Downloads/input-15"))
         (map (make-array (list (length map)
                                (length (first map)))
                          :initial-contents map))
         (map (transform-map map))
         (moves (str:join "" moves))
         (initial-robot (find-robot map)))
    (print-room map)
    (loop for char across moves
          for move = (parse-move char)
          for robot = (move-2 initial-robot move map) then (move-2 robot move map)
          when *debug* do (format t "~&~A ~A~%" char move)
            when *debug* do (print-room map))
    (print-room map)
    (loop for row from 0 below (array-dimension map 0) summing
          (loop for col from 0 below (array-dimension map 1)
                when (char-equal #\[ (aref map row col))
                  sum (gps row col)))))

(defun move-2 (robot move map)
  (when *debug*
    (format t "~&robot = ~A move = ~A free-space = ~A" robot move
            (has-free-space-2 robot move map)))
  (if (has-free-space-2 robot move map)
      (bind ((next (do-move robot move map)))
        (setf (aref map (row robot) (col robot)) #\.)
        next)
      robot))

(defun has-free-space-2 (robot move map)
  (bind ((next-position (next-position robot move)))
    (when (within-bounds-p next-position map)
      (ecase (aref map (row next-position) (col next-position))
        (#\. t)
        (#\# nil)
        (#\[ (if (horizontal-move-p move)
                 (has-free-space-2 next-position move map)
                 (and (has-free-space-2 next-position move map)
                      (has-free-space-2 (right-position next-position) move map))))
        (#\] (if (horizontal-move-p move)
                 (has-free-space-2 next-position move map)
                 (and (has-free-space-2 next-position move map)
                      (has-free-space-2 (left-position next-position) move map))))))))

(defun do-move (position move map)
  (bind ((char (aref map (row position) (col position)))
         (next-position (next-position position move))
         (next-position-char (aref map (row next-position) (col next-position))))
    (unless (char-equal #\. next-position-char)
      (do-move next-position move map)
      (when (and (vertical-move-p move)
                 (char-equal #\[ next-position-char))
        (do-move (right-position next-position) move map))
      (when (and (vertical-move-p move)
                 (char-equal #\] next-position-char))
        (do-move (left-position next-position) move map))
      )
    (setf (aref map (row next-position) (col next-position)) char)
    (setf (aref map (row position) (col position)) #\.)
    next-position))

(defun next-position (position move)
  (cons (+ (row position) (row move))
        (+ (col position) (col move))))

(defun right-position (position)
  (cons (row position)
        (1+ (col position))))

(defun left-position (position)
  (cons (row position)
        (1- (col position))))

(defun horizontal-move-p (move)
  (zerop (row move)))

(defun vertical-move-p (move)
  (zerop (col move)))

(defun left-move-p (move)
  (and (zerop (row move))
       (minusp (col move))))

(defun right-move-p (move)
  (and (zerop (row move))
       (plusp (col move))))

(defun transform-map (map)
  (loop with row-count = (array-dimension map 0)
        with col-count = (array-dimension map 1)
        with new-map = (make-array (list row-count (* 2 col-count)))
        for row from 0 below row-count
        do (loop for col from 0 below col-count
                 for new-map-col = (* 2 col)
                 for val = (aref map row col)
                 for left-val = (case val
                                  (#\O #\[)
                                  (t val))
                 for right-val = (case val
                                   (#\O #\])
                                   (#\@ #\.)
                                   (t val))
                 do (setf (aref new-map row new-map-col) left-val
                          (aref new-map row (1+ new-map-col)) right-val))
        finally (return new-map)))
