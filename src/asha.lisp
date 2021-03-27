(defpackage :asha
  (:use :cl)
  (:export))
(in-package :asha)

(defclass document ()
  ((name)))

(defclass template (document)
  ())

(defclass page (document)
  ())

(defstruct website
  rootpath metadata pages templates article-sets)

(defgeneric add-document (document website))

(deftype filetype ()
  '(member :text :binary))

(defstruct file
  (path nil :type pathname)
  (type :text :type filetype)
  content)

(defgeneric render-document (document website))

(defun create-website (rootpath))
(defun load-website (rootpath))
(defun save-website (website))
