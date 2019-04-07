(defsystem :asha
  :description "A static site generating system"
  :author "TANAKA Shinichi"
  :license "MIT"
  :components ((:file "asha" :depends-on ("src"))
               (:module "src"
                 :components ((:file "util")
                              (:file "shtml")
                              (:file "article"))
                 :serial t)))
