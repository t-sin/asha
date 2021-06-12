(defpackage :asha.render
  (:use :cl
        :asha.website
        :asha.util)
  (:export :document-newer-p
           :read-content
           :find-document
           :render-content))
(in-package :asha.render)

(defun document-newer-p (content document)
  (let ((content-updated-at (content-updated-at content))
        (document-updated-at (getf document :updated-at)))
    (if (null content-updated-at)
        (if (null document-updated-at)
            nil
            document-updated-at)
        (if (null document-updated-at)
            nil
            (local-time:timestamp< (local-time:parse-timestring content-updated-at)
                                   (local-time:parse-timestring document-updated-at))))))

(defun read-content (str)
  (let ((document (with-input-from-string (in str)
                    (rosa:peruse-as-plist in #'string-upcase))))
    (setf (getf document :title) (elt (getf document :title) 0))
    (setf (getf document :description) (elt (getf document :description) 0))
    (setf (getf document :content)
          (with-output-to-string (out)
            (loop
              :for s :across (getf document :content)
              :do (format out "~a" s))))
    document))

(defun find-document (name lis)
  (find name lis
        :key #'document-name
        :test #'string=))

(defun render-content (stream content website &optional params)
  (let ((template (find-document (content-template-name content)
                                 (website-templates website))))
    (if (null template)
        (with-open-file (in (merge-pathnames (content-pathstr content)
                                             (website-rootpath website))
                            :direction :input)
          (loop
            :for line := (read-line in nil :eof)
            :until (eq line :eof)
            :do (write-line line stream)))
        (let ((path (merge-pathnames (content-pathstr content)
                                     (website-rootpath website)))
              (temp-path (merge-pathnames (template-pathstr template)
                                          (website-rootpath website)))
              (website-metadata (metadata-plist (website-metadata website)))
              (tag-info-list (loop
                               :for tag :in (content-tags content)
                               :collect (list :name tag :link (format nil "~a.html" tag)))))
          (case (path-type path)
            (:html (apply #'djula:render-template* `(,path ,stream ,@website-metadata ,@params)))
            (:md (let* ((document (read-content (read-to-string path)))
                        (html (clcm:cm->html (getf document :content)))
                        (index (make-index html))
                        (metadata (list :created-at (content-created-at content)
                                        :updated-at (if (string= (content-updated-at content) "")
                                                        "-"
                                                        (content-updated-at content)))))
                   (setf (getf document :content) html)
                   (apply #'djula:render-template*
                          `(,temp-path ,stream
                                       ,@website-metadata ,@metadata ,@document ,@params
                                       :tag-info-list ,tag-info-list :index ,index)))))))))
