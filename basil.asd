;;;; basil.asd

(asdf:defsystem #:basil
  :description "A BASIC interpreter"
  :author "Sergi Reyner <sergi.reyner@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:split-sequence #:infix)
  :components ((:file "package")
               (:file "macros")
               (:file "reader")
               (:file "compiler")
               (:file "interpreter")
               (:file "commands")
               (:file "basil")))
