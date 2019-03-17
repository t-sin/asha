(defsystem :asha
  :description "A static site generator"
  :author "TANAKA Shinichi"
  :components ((:file "asha" :depends-on ("src"))
               (:module "src"
                 :components ((:file "shtml"))
                 :serial t)))
