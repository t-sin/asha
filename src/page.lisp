(defpackage #:asha/page
  (:use #:cl)
  (:export #:page
           #:page-title
           #:page-created-at
           #:page-tags
           #:body
           #:make-page*
           #:list-shtml-files))
(in-package #:asha/page)

(defstruct page
  title created-at tags body)

(defun created-at (plist)
  (let ((created-at (getf plist :created-at)))
    (if created-at  ;; We assumes that `created-at` is valid ISO 8601 string
        created-at
        (multiple-value-bind (sec min hour date month year)
            (get-decoded-time)
          (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
                  year month date hour min sec)))))

(defun make-page* (stream)
  (let ((plist (read stream)))
    (make-page :title (getf plist :title)
               :tags (getf plist :tags)
               :body (getf plist :body)
               :created-at (created-at plist))))

(defun list-sthml-files (dirname)
  (uiop:directory-files (make-pathname :directory dirname)
                        (make-pathname :directory nil :name :wild :type "shtml")))
