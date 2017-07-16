;;;; basic.asd

(asdf:defsystem #:basic
  :description "A BASIC interpreter"
  :author "Sergi Reyner <sergi.reyner@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:file "macros")
               (:file "reader")
               (:file "commands")
               (:file "basic")))
