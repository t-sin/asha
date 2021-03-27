(defpackage :asha
  (:use :cl)
  (:export))
(in-package :asha)

;;; website
(defstruct website
  rootpath metadata contents templates article-sets)

(defclass document ()
  ((name :type string
         :initarg :name
         :accessor document-name)))

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

;;; primitive operations

(defgeneric add-document (document website))

(defgeneric dump-document (document))

(deftype filetype ()
  '(member :text :binary))

(defstruct file
  (path nil :type pathname)
  (type :text :type filetype)
  content)

(defgeneric render-document (document website))

;;; utilities

(defun determine-rootpath (path))

;;; user operations

(defun create-website (rootpath)
  (make-website :rootpath (truename rootpath)))

(defun load-website (rootpath))

(defun save-website (website)
  (let* ((asha-dir (merge-pathnames (make-pathname :directory '(:relative  ".asha"))
                                    (website-rootpath website)))
         (asha-file (merge-pathnames (make-pathname :name "website" :type "lisp") asha-dir))
         (content-list-file (merge-pathnames (make-pathname :name "contents" :type "lisp") asha-dir))
         (template-list-file (merge-pathnames (make-pathname :name "templates" :type "lisp") asha-dir)))
    (ensure-directories-exist asha-dir)
    (with-open-file (out asha-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (print website out)))
    (with-open-file (out content-list-file :direction :output :if-exists :supersede)
      (print (website-contents website) out))
    (with-open-file (out template-list-file :direction :output :if-exists :supersede)
      (print (website-templates website) out))))

(defun add-template (path website))
(defun add-content (path website))
