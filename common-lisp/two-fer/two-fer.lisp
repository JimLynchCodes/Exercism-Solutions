(in-package #:cl-user)
(defpackage #:two-fer
  (:use #:cl)
  (:export #:twofer))
(in-package #:two-fer)

(defun twofer (&optional name)
  "returns the twofer string with the given name, otherwise \"you\""
  (format nil "One for -a, one for me." (or name "you")))