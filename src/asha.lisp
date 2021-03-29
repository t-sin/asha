(defpackage :asha
  (:use :cl)
  (:export))
(in-package :asha)

;;; website
(defstruct website-metadata
  title author description date-from
  article-set-names)

(defstruct website
  (rootpath #P"" :type pathname)
  (metadata (make-website-metadata)
            :type website-metadata)
  contents templates article-sets)

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

(defun load-website (rootpath)
  (unless (probe-file rootpath)
    (error "no such directory: ~s" rootpath))
  (let* ((website (make-website))
         (asha-dir (merge-pathnames (make-pathname :directory '(:relative  ".asha")) rootpath))
         (asha-file (merge-pathnames (make-pathname :name "website" :type "lisp") asha-dir))
         (content-list-file (merge-pathnames (make-pathname :name "contents" :type "lisp") asha-dir))
         (template-list-file (merge-pathnames (make-pathname :name "templates" :type "lisp") asha-dir)))
    (with-open-file (in asha-file :direction :input)
      (let ((obj (read in)))
        (unless (typep obj 'website-metadata)
          (error "this is not a website-metadata: ~s" obj))
        (setf (website-metadata website) obj)))
    (with-open-file (in content-list-file :direction :input)
      (let ((obj (read in)))
        (unless (typep obj 'list)
          (error "this is not a content-list: ~s" obj))
        (setf (website-contents website) obj)))
    (with-open-file (in template-list-file :direction :input)
      (let ((obj (read in)))
        (unless (typep obj 'list)
          (error "this is not a template-list: ~s" obj))
        (setf (website-templates website) obj)))
    website))

(defun save-website (website)
  (let* ((asha-dir (merge-pathnames (make-pathname :directory '(:relative  ".asha"))
                                    (website-rootpath website)))
         (asha-file (merge-pathnames (make-pathname :name "website" :type "lisp") asha-dir))
         (content-list-file (merge-pathnames (make-pathname :name "contents" :type "lisp") asha-dir))
         (template-list-file (merge-pathnames (make-pathname :name "templates" :type "lisp") asha-dir)))
    (ensure-directories-exist asha-dir)
    (with-open-file (out asha-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (print (website-metadata website) out)))
    (with-open-file (out content-list-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (print (website-contents website) out)))
    (with-open-file (out template-list-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (print (website-templates website) out)))))

(defun add-template (path website))
(defun add-content (path website))
