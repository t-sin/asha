(defpackage :asha
  (:use :cl)
  (:export))
(in-package :asha)

(defstruct website
  rootpath metadata pages templates article-sets)

(defclass document ()
  ((name :type string
         :initarg :name
         :accessor document-name)))

(defgeneric add-document (document website))

(deftype filetype ()
  '(member :text :binary))

(defstruct file
  (path nil :type pathname)
  (type :text :type filetype)
  content)

(defgeneric render-document (document website))

(defclass template (document)
  ((params :type symbol
           :initarg :params
           :accessor template-params)
   (path :type list
         :initarg :path
         :accessor template-path)))

(defclass content (document)
  ((type :type symbol
         :initarg :type
         :accessor content-type)
   (body :initarg :body
         :accessor content-body)))

(defun create-website (rootpath))
(defun load-website (rootpath))
(defun save-website (website))
