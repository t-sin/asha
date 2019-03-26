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
                #:article->plist
                #:article-set-meta
                #:article-set-template
                #:article-set-pages
                #:article-set-articles
                #:article-set->plist
                #:article-created-at
                #:init-article-set
                #:load-article-set
                #:save-article-set
                #:add-article)
  (:export #:create-blog
           #:new-post
           #:render-blog))
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

(defun render-blog (aset rootpath outpath)
  (ensure-directories-exist outpath)
  (let* ((meta (article-set-meta aset))
         (asetpath (make-pathname :directory `(:relative ,(getf meta :name))))
         (template% (article-set-template aset))
         (template (with-open-file (in (merge-pathnames (make-pathname :name template%
                                                                       :type "shtml")
                                                        rootpath)
                                       :direction :input)
                     (eval (read in))))
         (pages (article-set-pages aset))
         (articles (mapcar #'article->plist (article-set-articles aset))))
    (print template)
    (loop
      :for a :in articles
      :for path := (merge-pathnames (make-pathname :name (getf a :name)
                                                   :type "html")
                                    (merge-pathnames asetpath outpath))
      :for params := `(:aset-meta ,meta
                       :path ,(pathname-directory path)
                      ,@a)
      :do (ensure-directories-exist path)
      :do (with-open-file (out path :direction :output)
            (let ((*standard-output* out))
              (render-element template params))))
    (loop
      :for p :in pages
      :for page := (with-open-file (in (merge-pathnames (make-pathname :name (getf p :name)
                                                                       :type "shtml")
                                                        rootpath)
                                       :direction :input)
                     (eval (read in)))
      :for path := (merge-pathnames (make-pathname :name (getf p :name)
                                                   :type "html")
                                    outpath)
      :for params := `(:aset-meta ,meta
                       :articles ,articles
                       ,@p)
      :do (with-open-file (out path :direction :output)
            (let ((*standard-output* out))
              (render-element page params))))))
