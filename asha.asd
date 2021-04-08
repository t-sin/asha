(defsystem :asha
  :description "A static site generating system"
  :author "TANAKA Shinichi"
  :license "MIT"
  :depends-on ("uiop"
               "alexandria"
               "local-time"
               "cl-markdown"
               "djula"
               "closure-html"
               "rosa")
  :components ((:module "src"
                :serial t
                :components ((:file "asha")))))
