(defpackage #:asha
  (:use #:cl)
  (:import-from #:asha/util
                #:now)
  (:import-from #:asha/shtml
                #:make-element*
                #:render-element)
  (:import-from #:asha/article
                #:*project-root-pathname*
                #:make-article
                #:article-name
                #:article-set-meta
                #:article-set-template
                #:article-set-pages
                #:article-set-articles
                #:article-created-at
                #:init-article-set
                #:load-article-set
                #:save-article-set
                #:add-article)
  (:export #:create-blog
           #:new-post))
(in-package #:asha)

(defun create-blog (rootpath name)
  (let ((*project-root-pathname* rootpath))
    (save-article-set (init-article-set name))))

(defun new-post% (article aset &optional (update-p nil))
  (let* ((aname (getf article :name))
         (asname (getf (article-set-meta aset) :name))
         (apath (merge-pathnames (make-pathname :name aname
                                                :type "shtml")
                                 (merge-pathnames (make-pathname :directory `(:relative ,asname))
                                                  *project-root-pathname*))))
    (with-open-file (out apath :direction :output :if-exists :supersede)
      (print article out))
    (add-article (make-article :name aname
                               :created-at (let ((prev-a (find aname (article-set-articles aset)
                                                               :key #'article-name
                                                               :test #'string=)))
                                             (if prev-a (article-created-at prev-a) (now)))
                               :tags (getf article :tags)
                               :title (getf article :title)
                               :body (getf article :body))
                 aset update-p)
    (save-article-set aset)))

(defun new-post (rootpath article aset &optional (update-p nil))
  (let ((*project-root-pathname* rootpath))
    (typecase article
      (list (new-post% article aset update-p))
      (pathname (progn
                  (unless (probe-file (getf article :name))
                    (error "file '~s' does not exist." (getf article :name)))
                  (with-open-file (in article :direction :input)
                    (new-post% (read in) aset update-p))))
      (t (error "article should be one of plist or pathname.")))))
