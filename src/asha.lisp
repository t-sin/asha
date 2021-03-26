(defpackage :asha
  (:use :cl)
  (:export))
(in-package :asha)

(defclass element ()

(defclass page (element))

(defclass article-set (element))

(defclass website (element)
  ((rootpath)
   (metadata)
   (pages)
   (article-sets)))

(defgeneric add (type website obj))
(defgeneric render (obj))

(defun create-website (rootpath)
(defun load-website (rootpath))
(defun save-website (website))

(defmethod add ((type (eql :template)) (website website) path))
(defmethod add ((type (eql :page)) (website website) path))
(defmethod add ((type (eql :article-set)) (website website) obj))

(defmethod render ((obj page)))
(defmethod render ((obj article-set)))
(defmethod render ((obj website)))
