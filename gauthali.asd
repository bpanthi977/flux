;;;; gauthali.asd

(asdf:defsystem #:gauthali
  :description "Play music in sync"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:sdl3 #:alexandria #:trivial-garbage #:anaphora)
  :components ((:file "package")
	       (:file "utils")
	       (:file "layout")
	       (:file "element")
	       (:file "draw")
	       (:file "cache")
               (:file "gauthali")))
