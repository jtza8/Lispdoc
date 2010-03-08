(defsystem "lispdoc"
  :description "A simple lisp documentation system."
  :author "Jens Thiede"
  :licence "BSD-Style Licence"
  :version "0.1"
  :components ((:file "package")
               (:file "doc_tree" :depends-on ("package"))
	       (:file "generation" :depends-on ("doc_tree"))))