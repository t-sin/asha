(defpackage #:asha
  (:use #:cl)
  (:import-from #:asha/util
                #:now
                #:merge-pathnames*)
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
           #:load-blog
           #:save-blog
           #:new-post
           #:render-blog))
(in-package #:asha)

(defvar +blog-aset-name+ "posts")

(defun create-blog (rootpath)
  (let ((*project-root-pathname* rootpath))
    (save-article-set (init-article-set +blog-aset-name+))))

(defun load-blog (rootpath)
  (let ((*project-root-pathname* rootpath))
    (load-article-set +blog-aset-name+)))

(defun save-blog (rootpath aset)
  (let ((*project-root-pathname* rootpath))
    (save-article-set aset)))

(defun new-post% (article aset &optional (update-p nil))
  (let* ((aname (getf article :name))
         (asname (getf (article-set-meta aset) :name))
         (apath (merge-pathnames* (make-pathname :name aname
                                                :type "shtml")
                                  (make-pathname :directory `(:relative ,asname))
                                  *project-root-pathname*)))
    (with-open-file (out apath :direction :output :if-exists :supersede)
      (print article out))
    (let ((prev-a (find aname (article-set-articles aset)
                        :key #'article-name
                        :test #'string=)))
      (add-article (make-article :name aname
                                 :created-at (if prev-a (article-created-at prev-a) (now))
                                 :tags (getf article :tags)
                                 :title (getf article :title)
                                 :body (getf article :body))
                   aset update-p))
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

(defun render-articles% (articles template meta asetpath outpath)
  (loop
    :for a :in articles
    :for path := (merge-pathnames* (make-pathname :name (getf a :name)
                                                  :type "html")
                                   asetpath outpath)
    :for params := `(:aset-meta ,meta
                     :path ,(pathname-directory path)
                     ,@a)
    :do (ensure-directories-exist path)
    :do (with-open-file (out path :direction :output)
          (let ((*standard-output* out))
            (render-element template params)))))

(defun render-pages% (pages meta articles rootpath outpath)
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
              (render-element page params)))))

(defun render-blog (aset rootpath outpath)
  (uiop:delete-directory-tree outpath :validate t :if-does-not-exist :ignore)
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
    (render-articles% articles template meta asetpath outpath)
    (render-pages% pages meta articles rootpath outpath)))
