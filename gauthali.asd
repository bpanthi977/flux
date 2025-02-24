;;;; gauthali.asd

(asdf:defsystem #:gauthali
  :description "Play music in sync"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl3)
  :components ((:file "package")
	       (:file "utils")
	       (:file "widgets")
               (:file "gauthali")))
