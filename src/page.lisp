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

(defun list-sthml-files (dirpath)
  (uiop:directory-files dirpath
                        (make-pathname :directory nil :name :wild :type "shtml")))

(defun load-page-groups (rootpath dirname)
  (let* ((dirpath (merge-pathnames (make-pathname :directory (list :relative dirname))
                                   rootpath))
         (state-file (merge-pathnames (make-pathname :name ".state") dirpath))
         (shtmls (list-sthml-files dirpath)))
    (unless (probe-file state-file)
      (error (format nil "state file ~s does not exist." state-file)))
    (values (loop
              :for shtml :in shtmls
              :nconc (list (pathname-name shtml)
                           (with-open-file (in shtml :direction :input)
                             (make-page* in))))
            (with-open-file (in state-file :direction :input)
              (read in)))))