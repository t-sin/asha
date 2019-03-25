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
           #:init-article-set
           #:load-article-set
           #:save-article-set
           #:add-article*))
(in-package #:asha/article)

(defparameter *project-root-pathname* (truename "."))

(defstruct article
  name created-at tags title body)

(defmethod print-object ((o article) stream)
  (format stream "~s"
          (list :name (article-name o)
                :created-at (article-created-at o)
                :tags (article-tags o)
                :title (article-title o)
                :body (article-body o))))

(defstruct article-set
  meta pages articles)

(defmethod print-object ((o article-set) stream)
  (format stream "~s"
          (list :meta (article-set-meta o)
                :pages (article-set-pages o)
                :articles (article-set-articles o))))

(defun init-article-set (set-name)
  (make-article-set :meta (list :name set-name)
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
                              :pages (getf aset :pages)
                              :articles (getf aset :articles)))))))

(defun save-article-set (aset)
  (let* ((name (getf (article-set-meta aset) :name))
         (basepath (merge-pathnames (make-pathname :directory (list :relative name))
                                    *project-root-pathname*))
         (state-file (merge-pathnames (make-pathname :name ".articles") basepath)))
    (with-open-file (out state-file :direction :output
                         :if-exists :supersede)
      (format out "~s" (list :meta (article-set-meta aset)
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

(defun add-article* (article-path aset &optional (update-p nil))
  (if (not (probe-file article-path))
      (error "file '~s' does not exist." article-path)
      (let* ((asname (getf (article-set-meta aset) :name))
             (apath (merge-pathnames (make-pathname :name (pathname-name article-path)
                                                    :type "shtml")
                                     (merge-pathnames (make-pathname :directory `(:relative ,asname))
                                                      *project-root-pathname*))))
        (with-open-file (in article-path :direction :input)
          (let ((a (read in)))
            (with-open-file (out apath :direction :output :if-exists :supersede)
              (print a out))
            (add-article (make-article :name (pathname-name article-path)
                                       :created-at (let ((prev-a (find (pathname-name article-path)
                                                                       (article-set-articles aset)
                                                                       :key #'article-created-at
                                                                       :test #'string=)))
                                                     (if prev-a (article-created-at prev-a) (now)))
                                       :tags (getf a :tags)
                                       :title (getf a :title)
                                       :body (getf a :body))
                         aset update-p)
            (save-article-set aset))))))
