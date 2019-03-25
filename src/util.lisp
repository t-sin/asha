(defpackage #:asha/util
  (:use #:cl)
  (:export #:now))
(in-package #:asha/util)

(defun now ()
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month date hour min sec)))
