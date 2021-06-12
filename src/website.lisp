(defpackage :asha.website
  (:use :cl)
  (:export :website-metadata
           :website-metadata-title
           :website-metadata-author
           :website-metadata-description
           :website-metadata-date-from

           :website
           :make-website
           :website-rootpath
           :website-metadata
           :website-contents
           :website-templates
           :website-article-sets

           :template
           :template-pathstr

           :content
           :content-template-name
           :content-created-at
           :content-updated-at
           :content-pathstr
           :content-tags

           :article-set
           :article-set-title
           :article-set-template-name
           :article-set-tag-template-name
           :article-set-index-template-name
           :article-set-articles

           :metadata-plist))
(in-package :asha.website)

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
