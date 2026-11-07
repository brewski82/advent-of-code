(defpackage #:advent-2026
  (:use #:common-lisp #:advent-utils)
  (:export alexandria:compose
           alexandria:curry
           alexandria:lastcar
           alexandria:rcurry
           metabang-bind:bind
           serapeum:~>>
           serapeum:concat
           #:downloaded-pathname))

(in-package #:advent-2026)

(export-all-symbols :advent-utils)

(defun downloaded-pathname (file-name)
  (asdf:system-relative-pathname "advent-2026"
                                 (concat "downloads/" file-name)))
