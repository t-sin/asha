(defpackage :asha.util
  (:use :cl
        :asha.website)
  (:export :list-directories
           :path-type
           :determine-output-path
           :read-to-string
           :make-index
           :now))
(in-package :asha.util)

(defun list-directories (path)
  (directory (merge-pathnames (make-pathname :directory (list :relative :wild))
                              path)))

(defun path-type (path)
  (intern (string-upcase (pathname-type path)) :keyword))

(defun determine-output-path (path)
  (case (path-type path)
    (:md (make-pathname :directory (pathname-directory path)
                        :name (pathname-name path)
                        :type "html"))
    (t path)))

(defun read-to-string (path)
  (with-output-to-string (out)
    (with-open-file (in path :direction :input)
      (loop
        :for line := (read-line in nil :eof)
        :until (eq line :eof)
        :do (write-line line out)))))

(defun make-index (htmlstr)
  (labels ((find-body (html)
             (if (eq (first html) :body)
                 html
                 (find-if #'find-body (rest html))))
           (make-index (elements)
             (loop
               :with headers := (loop
                                  :for n :from 0 :upto 9
                                  :collect (intern (format nil "H~d" n) :keyword))
               :for e :in elements
               :when (and (consp e) (member (first e) headers))
               :collect e)))
    (let ((html (chtml:parse htmlstr (chtml:make-lhtml-builder))))
      (make-index (rest (find-body html))))))

(defun now ()
  (local-time:format-timestring nil (local-time:now)))
