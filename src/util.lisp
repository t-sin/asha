(defpackage #:asha/util
  (:use #:cl)
  (:export #:now
           #:merge-pathnames*))
(in-package #:asha/util)

(defun now ()
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month date hour min sec)))

(defun merge-pathnames* (pathname &rest pathnames)
  (loop
    :for p :in pathnames
    :for merged := (merge-pathnames pathname p) :then (merge-pathnames merged p)
    :finally (return-from merge-pathnames* merged)))
