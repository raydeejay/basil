;;;; basic.asd

(asdf:defsystem #:basic
  :description "A BASIC interpreter"
  :author "Sergi Reyner <sergi.reyner@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "input-stream")
               (:file "basic")))
