(in-package #:cl-user)
(defpackage #:acronym
  (:use #:common-lisp)
  (:export #:acronym))

(in-package #:acronym)

(defun acronym (s)
  "converts a string to an acronym (first letter of each word, all capilatilzed)"
  (split-sequence:SPLIT-SEQUENCE #\Space s)
  )