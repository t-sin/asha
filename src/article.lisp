(defpackage #:asha/article
  (:use #:cl)
  (:import-from #:asha/util
                #:now)
  (:export #:*project-root-pathname*
           #:make-article
           #:article-name
           #:article-created-at
           #:article-tags
           #:article-title
           #:article-body
           #:article->plist
           #:article-set-meta
           #:article-set-template
           #:article-set-pages
           #:article-set-articles
           #:article-set->plist
           #:init-article-set
           #:load-article-set
           #:save-article-set
           #:add-article))
(in-package #:asha/article)

(defparameter *project-root-pathname* (truename "."))

(defstruct article
  name created-at tags title body)

(defun article->plist (a)
  (list :name (article-name a)
        :created-at (article-created-at a)
        :tags (article-tags a)
        :title (article-title a)
        :body (article-body a)))

(defmethod print-object ((o article) stream)
  (format stream "~s" (article->plist o)))


(defstruct article-set
  meta template pages articles)

(defun article-set-plist (as)
  (list :meta (article-set-meta as)
        :template (article-set-template as)
        :pages (article-set-pages as)
        :articles (article-set-articles as)))

(defmethod print-object ((o article-set) stream)
  (format stream "~s" (article-set->plist o)))

(defun init-article-set (set-name)
  (make-article-set :meta (list :name set-name)
                    :template "template"
                    :pages '((:name "index"))
                    :articles nil))

(defun load-article-set-state (set-name)
  (let* ((basepath (merge-pathnames (make-pathname :directory `(:relative ,set-name))
                                    *project-root-pathname*))
         (state-file (merge-pathnames (make-pathname :name ".articles") basepath)))
    (if (not (probe-file state-file))
        (error (format nil "state file ~s does not exist." state-file))
        (with-open-file (in state-file :direction :input)
          (let ((aset (read in)))
            (make-article-set :meta (getf aset :meta)
                              :template (getf aset :template)
                              :pages (getf aset :pages)
                              :articles (getf aset :articles)))))))

(defun save-article-set (aset)
  (let* ((name (getf (article-set-meta aset) :name))
         (basepath (merge-pathnames (make-pathname :directory (list :relative name))
                                    *project-root-pathname*))
         (state-file (merge-pathnames (make-pathname :name ".articles") basepath)))
    (ensure-directories-exist basepath)
    (with-open-file (out state-file :direction :output
                         :if-exists :supersede)
      (format out "~s" (list :meta (article-set-meta aset)
                             :template (article-set-template aset)
                             :pages (article-set-pages aset)
                             :articles (mapcar (lambda (a)
                                                 (list :name (article-name a)
                                                       :created-at (article-created-at a)))
                                               (article-set-articles aset))))
      t)))

(defun load-article-set (set-name)
  (let ((state (load-article-set-state set-name)))
    (loop
      :for article :in (article-set-articles state)
      :with articles := nil
      :do (let* ((name (getf article :name))
                 (path (merge-pathnames (make-pathname :name name :type "shtml")
                                        (merge-pathnames (make-pathname :directory `(:relative ,set-name))
                                                         *project-root-pathname*))))
            (if (not (probe-file path))
                (error "file '~s' not found. .articles file may be broken." path)
                (with-open-file (in path :direction :input)
                  (let ((a (read in)))
                    (push (make-article :name name
                                        :created-at (getf article :created-at)
                                        :tags (getf a :tags)
                                        :title (getf a :title)
                                        :body (getf a :body))
                          articles)))))
      :finally (setf (article-set-articles state) (nreverse articles)))
    state))

(defun add-article (article aset &optional (update-p nil))
  (when update-p
    (setf (article-created-at article) (now)))
  (let ((pos (position article (article-set-articles aset)
                       :test (lambda (a1 a2) (string= (article-name a1) (article-name a2))))))
    (if pos
        (setf (nth pos (article-set-articles aset)) article)
        (push article (article-set-articles aset)))
    (setf (article-set-articles aset)
          (sort (article-set-articles aset)
                (lambda (a1 a2) (string< (article-created-at a1)
                                         (article-created-at a2))))))
  aset)
