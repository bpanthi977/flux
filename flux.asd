;;;; gauthali.asd

(asdf:defsystem #:flux
  :description "UI Framework for Common Lisp"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl3 #:alexandria #:trivial-garbage #:trivial-macroexpand-all #:anaphora #:closer-mop)
  :components ((:file "package")
	       (:file "utils")
	       (:file "layout")
	       (:file "widget")
	       (:file "font")
	       (:module "widgets"
			:serial t
			:components ((:file "core")
				     (:file "text")
				     (:file "button")
				     (:file "hoverable")
				     (:file "slider")
				     (:file "text-entry")
				     (:file "tabs")
				     (:file "scrollable")
				     (:file "select-file")))
	       (:module "screens"
			:serial t
			:components ((:file "home-screen")
				     (:file "leap-year-screen")
				     (:file "debugger-screen")))
               (:file "flux")))

(asdf:defsystem #:flux/tests
  :description "Tests for Flux"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:flux #:fiveam)
  :pathname "t/"
  :components ((:file "package")
	       (:file "utils")
	       (:file "benchmark")
	       (:file "layout")))
