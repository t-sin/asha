(defpackage #:asha/shtml
  (:use #:cl))
(in-package #:asha/shtml)

;; https://developer.mozilla.org/en-US/docs/Glossary/Empty_element
(defconstant +empty-elements+
  '(:area :base :br :col :embed :hr :img :input :link :meta :param :source :track :wbr))
