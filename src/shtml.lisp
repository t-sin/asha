(defpackage #:asha/shtml
  (:use #:cl)
  (:export #:element
           #:element-name
           #:element-props
           #:element-children
           #:make-element*
           #:render-element))
(in-package #:asha/shtml)

(defstruct element
  name props children)

(defun get-name (shtml)
  (cond ((null shtml) (error "shtml must have its name."))
        ((not (keywordp (first shtml))) (error "first element of shtml must be a keyword."))
        (t (first shtml))))

(defun get-props (shtml-rest)
  (let ((props))
    (loop
      :for (k v . rest) :on shtml-rest :by #'cddr
      :do (cond ((and (keywordp k) (stringp v))
                 (progn
                   (push v props)
                   (push k props)))
                ((keywordp k)
                 (error (format nil "value ~s should be a string." v)))
                (t (return-from get-props
                     (values props (if (null v)
                                       (list k)
                                       (cons k (cons v rest))))))))
    (values props nil)))

(defun make-element* (shtml)
  (let ((name (get-name shtml)))
    (multiple-value-bind (props children)
        (get-props (rest shtml))
      (make-element :name name
                    :props props
                    :children (loop
                                :for c :in children
                                :collect (if (listp c) (make-element* c) c))))))

;; https://developer.mozilla.org/en-US/docs/Glossary/Empty_element
(defconstant +empty-elements+
  '(:area :base :br :col :embed :hr :img :input :link :meta :param :source :track :wbr))

(defun render-element (e &optional (indent 0))
  (format *standard-output* "~vt<~(~a~)~{~^ ~(~a~)=\"~a\"~}>~%" indent
          (element-name e) (element-props e))
  (loop
    :for c :in (element-children e)
    :do (if (element-p c)
            (render-element c (+ indent 2))
            (format *standard-output* "~vt~a~%" (+ indent 2) c)))
  (unless (member (element-name e) +empty-elements+)
    (format *standard-output* "~vt</~(~a~)>~%" indent (element-name e))))
