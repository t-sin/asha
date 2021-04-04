(defpackage :asha
  (:use :cl)
  (:export))
(in-package :asha)

;;; website
(defstruct website-metadata
  title author description date-from)

(defstruct website
  (rootpath #P"" :type pathname)
  (metadata (make-website-metadata)
            :type website-metadata)
  contents templates article-sets)

(defstruct document
  (name "" :type string))

(defstruct (template (:include document))
  (pathstr "" :type string))

(defstruct (content (:include document))
  (template-name nil :type (or string null))
  (created-at "" :type string)
  (updated-at "" :type string)
  (pathstr "" :type string))

(defstruct (article-set (:include document))
  (template-name "" :type string)
  articles)

(defun metadata-plist (metadata)
  (list :website-title (website-metadata-title metadata)
        :website-author (website-metadata-author metadata)
        :website-description (website-metadata-description metadata)
        :website-date-from (website-metadata-date-from metadata)))

;;; utilities

(defun determine-rootpath (path)
  path)

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

(defun read-content (str)
  (let ((document (with-input-from-string (in str)
                    (rosa:peruse-as-plist in #'string-upcase))))
    (setf (getf document :title) (elt (getf document :title) 0))
    (setf (getf document :created-at)
          (let ((s (elt (getf document :created-at) 0)))
            (unless (string= s "") s)))
    (setf (getf document :updated-at)
                    (let ((s (elt (getf document :updated-at) 0)))
            (unless (string= s "") s)))
    (setf (getf document :description) (elt (getf document :description) 0))
    (setf (getf document :content) (elt (getf document :content) 0))
    document))

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

;;; primitive operations

(defun find-document (name lis)
  (find name lis
        :key #'document-name
        :test #'string=))

(defgeneric render-document (stream document website))

(defmethod render-document (stream (content content) (website website))
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
              (metadata (metadata-plist (website-metadata website))))
          (case (path-type path)
            (:html (apply #'djula:render-template* `(,path ,stream ,@metadata)))
            (:md (let* ((document (read-content (read-to-string path)))
                        (html (with-output-to-string (out)
                                (markdown:markdown (getf document :content) :stream out)))
                        (index (make-index html)))
                   (setf (getf document :content) html)
                   (if (document-newer-p content document)
                       (error "This document is updated. Please maintain website state.")
                       (progn
                         (when (null (getf document :updated-at))
                           (setf (getf document :updated-at) "-"))
                         (apply #'djula:render-template*
                              `(,temp-path ,stream ,@metadata ,@document :index ,index)))))))))))

(deftype filetype ()
  '(member :text :binary))

(defstruct file
  (path nil :type pathname)
  (type :text :type filetype)
  content)

;;; user operations

(defparameter *asha-dir* (make-pathname :directory '(:relative  ".asha")))
(defparameter *asha-file* (make-pathname :name "website" :type "lisp"))
(defparameter *contents-file* (make-pathname :name "contents" :type "lisp"))
(defparameter *templates-file* (make-pathname :name "templates" :type "lisp"))
(defparameter *article-sets-file* (make-pathname :name "articles" :type "lisp"))

(defun load-article-set (article-set-dir)
  (let ((article-set-file (merge-pathnames (make-pathname :name "articles" :type "lisp")
                                           article-set-dir)))
    (with-open-file (in article-set-file :direction :input)
      (let ((obj (read in)))
        (if (typep obj 'article-set)
            obj
            (error "this file is not article-set"))))))

(defun create-website (rootpath)
  (make-website :rootpath (truename rootpath)))

(defun load-website (rootpath)
  (unless (probe-file rootpath)
    (error "no such directory: ~s" rootpath))
  (let* ((website (make-website))
         (asha-dir (merge-pathnames *asha-dir* rootpath))
         (asha-file (merge-pathnames *asha-file* asha-dir))
         (content-list-file (merge-pathnames *contents-file* asha-dir))
         (template-list-file (merge-pathnames *templates-file* asha-dir)))
    (setf (website-rootpath website) (pathname rootpath))
    (with-open-file (in asha-file :direction :input)
      (let ((obj (read in)))
        (unless (typep obj 'website-metadata)
          (error "this is not a website-metadata: ~s" obj))
        (setf (website-metadata website) obj)))
    (with-open-file (in content-list-file :direction :input)
      (let ((obj (read in)))
        (unless (typep obj 'list)
          (error "this is not a content-list: ~s" obj))
        (setf (website-contents website) obj)))
    (with-open-file (in template-list-file :direction :input)
      (let ((obj (read in)))
        (unless (typep obj 'list)
          (error "this is not a template-list: ~s" obj))
        (setf (website-templates website) obj)))
    (setf (website-article-sets website)
          (loop
            :for article-set :in (list-directories asha-dir)
            :collect (load-article-set article-set)))
    website))

(defun save-website (website)
  (let* ((asha-dir (merge-pathnames *asha-dir* (website-rootpath website)))
         (asha-file (merge-pathnames *asha-file* asha-dir))
         (content-list-file (merge-pathnames *contents-file* asha-dir))
         (template-list-file (merge-pathnames *templates-file* asha-dir)))
    (ensure-directories-exist asha-dir)
    (with-open-file (out asha-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (print (website-metadata website) out)))
    (with-open-file (out content-list-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (print (website-contents website) out)))
    (with-open-file (out template-list-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (print (website-templates website) out)))
    (loop
      :for article-set :in (website-article-sets website)
      :for name := (document-name article-set)
      :for article-set-path := (merge-pathnames (make-pathname :directory `(:relative ,name)) asha-dir)
      :do (ensure-directories-exist article-set-path)
      :do (with-open-file (out (merge-pathnames *article-sets-file* article-set-path)
                               :direction :output :if-exists :supersede)
            (let ((*print-pretty* t))
              (print article-set out))))))

(defun publish-website (website directory)
  (uiop:delete-directory-tree directory :validate t)
  (loop
    :for content :in (website-contents website)
    :for path := (merge-pathnames (content-pathstr content) directory)
    :for output-path := (determine-output-path path)
    :do (ensure-directories-exist path)
    :do (with-open-file (out output-path :direction :output :if-exists :supersede)
          (render-document out content website))))

(defun add-template (path website)
  (let ((template-path (merge-pathnames path (website-rootpath website))))
    (unless (probe-file template-path)
      (error "no such directory: ~s" template-path))
    (let ((template (make-template
                     :name (pathname-name template-path)
                     :pathstr (enough-namestring template-path (website-rootpath website)))))
      (push template (website-templates website))
      template-path)))

(defun add-content (path template-name website)
  (let ((content-path (merge-pathnames path (website-rootpath website))))
    (unless (probe-file content-path)
      (error "no such directory: ~s" content-path))
    (let ((content (make-content
                    :name (pathname-name content-path)
                    :template-name template-name
                    :pathstr (enough-namestring content-path (website-rootpath website)))))
      (push content (website-contents website))
      content-path)))

(defun add-article-set (name template-name website)
  (let ((article-set-path (merge-pathnames (make-pathname :directory `(:relative ,name))
                                           (merge-pathnames *asha-dir* (website-rootpath website)))))
    (when (probe-file article-set-path)
      (error "article set ~s already exists" name))
    (let ((article-set (make-article-set :name name
                                         :template-name template-name)))
      (push article-set (website-article-sets website))
      name)))
