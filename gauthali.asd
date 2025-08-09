;;;; gauthali.asd

(asdf:defsystem #:gauthali
  :description "Play music in sync"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "2.0.0"
  :serial t
  :depends-on (#:sdl3 #:alexandria #:trivial-garbage #:trivial-macroexpand-all #:anaphora)
  :components ((:file "package")
	       (:file "utils")
	       (:file "layout")
	       (:file "widget")
	       (:module "widgets"
			:serial t
			:components ((:file "core")
				     (:file "text")
				     (:file "button")
				     (:file "text-entry")))
	       (:file "screens")
               (:file "gauthali")))

(asdf:defsystem #:gauthali/tests
  :description "Tests for gauthali"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "2.0.0"
  :serial t
  :depends-on (#:gauthali #:fiveam)
  :pathname "t/"
  :components ((:file "package")
	       (:file "utils")
	       (:file "tests")))
