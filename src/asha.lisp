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
  (pathstr "" :type string)
  (tags nil :type list))

(defstruct (article-set (:include document))
  (title "" :type string)
  (template-name "" :type string)
  (tag-template-name "" :type string)
  (index-template-name "" :type string)
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
    (setf (getf document :description) (elt (getf document :description) 0))
    (setf (getf document :content)
          (with-output-to-string (out)
            (loop
              :for s :across (getf document :content)
              :do (format out "~a" s))))
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

(defun now* ()
  (local-time:format-timestring nil (local-time:now)))

;;; primitive operations

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
              (pprint article-set out))))))

(defun collect-page-links (articles website)
  (flet ((read-content (article)
           (let* ((path (merge-pathnames (content-pathstr article)
                                         (website-rootpath website)))
                  (content (read-content (read-to-string path))))
             (list :name (content-name article)
                   :title (getf content :title)))))
    (let ((link-table (make-hash-table :test 'equal)))
      (loop
        :with prev := nil
        :for (a next) :on articles :by #'cdr
        :do (setf (gethash (document-name a) link-table)
                  (cons (when prev (read-content prev))
                        (when next (read-content next))))
        :do (setf prev a))
      link-table)))

(defun collect-article-info (articles website)
  (let ((tag-table (make-hash-table :test 'equal))
        (article-info-list))
    (loop
      :for article :in articles
      :for article-path := (merge-pathnames (content-pathstr article)
                                            (website-rootpath website))
      :for document := (read-content (read-to-string article-path))
      :for article-info := (list :title (getf document :title)
                                 :description (getf document :description)
                                 :created-at (content-created-at article)
                                 :tags (content-tags article)
                                 :link (format nil "~a.html" (document-name article)))
      :do (push article-info article-info-list)
      :do (loop
            :for tag :in (content-tags article)
            :do (push article-info (gethash tag tag-table)))
      :finally (setf article-info-list (nreverse article-info-list)))
    (values article-info-list tag-table)))

(defun publish-tag-pages (articles article-set website directory)
  (multiple-value-bind (article-info-list tag-table)
      (collect-article-info articles website)
    (let* ((index-template (find-document (article-set-index-template-name article-set)
                                          (website-templates website)))
           (template-path (merge-pathnames (template-pathstr index-template)
                                           (website-rootpath website)))
           (outpath (merge-pathnames "index.html" directory)))
      (with-open-file (out outpath :direction :output :if-exists :supersede)
        (let* ((metadata (metadata-plist (website-metadata website)))
               (args `(,@metadata :article-set-title ,(article-set-title article-set)
                                  :article-info-list ,article-info-list
                                  :tags ,(alexandria:hash-table-keys tag-table))))
          (apply #'djula:render-template* `(,template-path ,out ,@args)))))
    (let* ((tag-template (find-document (article-set-tag-template-name article-set)
                                        (website-templates website)))
           (template-path (merge-pathnames (template-pathstr tag-template)
                                           (website-rootpath website))))
      (loop
        :for tag :being :each :hash-key :of tag-table :using (hash-value tagged-article-info-list)
        :for filename := (format nil "~a.html" tag)
        :for outpath := (merge-pathnames filename directory)
        :do (with-open-file (out outpath :direction :output :if-exists :supersede)
              (let* ((metadata (metadata-plist (website-metadata website)))
                     (args `(,@metadata :article-set-title ,(article-set-title article-set)
                                        :article-info-list ,tagged-article-info-list
                                        :tag ,tag)))
                (apply #'djula:render-template* `(,template-path ,out ,@args))))))
    article-info-list))

(defun publish-article-set (article-set website directory)
  (let* ((name (document-name article-set))
         (path (merge-pathnames (make-pathname :directory `(:relative ,name))
                                directory)))
    (ensure-directories-exist path)
    (let* ((articles (copy-list (article-set-articles article-set)))
           (sorted (sort articles (lambda (a b)
                                    (let ((a (local-time:parse-timestring (content-created-at a)))
                                          (b (local-time:parse-timestring (content-created-at b))))
                                      (local-time:timestamp< a b)))))
           (article-link-table (collect-page-links sorted website)))
      (loop
        :for content :in sorted
        :for filename := (make-pathname :name (pathname-name (content-pathstr content))
                                        :type (pathname-type (content-pathstr content)))
        :for output-path := (determine-output-path (merge-pathnames filename path))
        :for (prev . next) := (gethash (document-name content) article-link-table)
        :do (setf content (copy-content content))
        :do (setf (content-template-name content) (article-set-template-name article-set))
        :do (with-open-file (out output-path :direction :output :if-exists :supersede)
              (render-content out content website
                              `(:article-set-title ,(article-set-title article-set)
                                :prev-article ,prev :next-article ,next))))
      (publish-tag-pages sorted article-set website path))))

(defun publish-website (website directory)
  (when (probe-file directory)
    (uiop:delete-directory-tree directory :validate t))
  (let ((article-sets))
    (loop
      :for article-set :in (website-article-sets website)
      :for article-set-info := (publish-article-set article-set website directory)
      :do (setf (getf article-sets (intern (string-upcase (article-set-name article-set)) :keyword))
                (list :article-info article-set-info)))
    (loop
      :with params := `(:article-set-info-list ,article-sets)
      :for content :in (website-contents website)
      :for path := (merge-pathnames (content-pathstr content) directory)
      :for output-path := (determine-output-path path)
      :do (ensure-directories-exist path)
      :do (with-open-file (out output-path :direction :output :if-exists :supersede)
            (render-content out content website params)))
    t))

(defun add-template (name path website)
  (let ((template-path (merge-pathnames path (website-rootpath website))))
    (unless (probe-file template-path)
      (error "no such directory: ~s" template-path))
    (let ((template (make-template
                     :name name
                     :pathstr (enough-namestring template-path (website-rootpath website)))))
      (push template (website-templates website))
      template-path)))

(defun add-content (path template-name website)
  (let ((content-path (merge-pathnames path (website-rootpath website))))
    (unless (probe-file content-path)
      (error "no such file: ~s" content-path))
    (when (and (not (null template-name))
               (null (find-document template-name (website-templates website))))
      (error "there is no template: ~s" template-name))
    (let ((content (find-document path
                                  (website-contents website))))
      (if content
          (progn
            (setf (content-updated-at content) (now*))
            content)
          (let* ((pathstr (enough-namestring path (website-rootpath website)))
                 (content (make-content
                           :name pathstr
                           :template-name template-name
                           :created-at (now*)
                           :pathstr pathstr)))
            (push content (website-contents website))
            content)))))

(defun add-article-set (name title website &key index-template content-template tag-template)
  (let ((article-set-path (merge-pathnames (make-pathname :directory `(:relative ,name))
                                           (merge-pathnames *asha-dir* (website-rootpath website)))))
    (when (probe-file article-set-path)
      (error "article set ~s already exists" name))
    (unless (find-document content-template (website-templates website))
      (error "there is no template: ~s" content-template))
    (let ((article-set (make-article-set :name name
                                         :title title
                                         :index-template-name index-template
                                         :template-name content-template
                                         :tag-template-name tag-template)))
      (push article-set (website-article-sets website))
      name)))

(defun add-article (path article-set-name website)
  (let* ((article-path (merge-pathnames path (website-rootpath website)))
         (article-set (find article-set-name (website-article-sets website)
                            :key #'document-name :test #'string=)))
    (unless (probe-file article-path)
      (error "no such file: ~s" article-path))
    (flet ((read-document (path)
             (read-content (read-to-string path))))
      (let ((content (find-document (pathname-name article-path)
                                    (article-set-articles article-set))))
        (if content
            (let* ((document (read-document (merge-pathnames (content-pathstr content)
                                                             (website-rootpath website))))
                   (tags (coerce (getf document :tags) 'list)))
              (setf (content-updated-at content) (now*)
                    (content-tags content) tags)
              content)
            (let* ((tags (coerce (getf (read-document article-path) :tags) 'list))
                   (article (make-content :name (pathname-name article-path)
                                          :created-at (now*)
                                          :tags tags
                                          :pathstr (enough-namestring article-path (website-rootpath website)))))
              (push article (article-set-articles article-set))
              article))))))
