(defsystem "lispdoc"
  :description "A simple lisp documentation system."
  :author "Jens Thiede"
  :licence "BSD-Style Licence"
  :version "0.1"
;  :depends-on ""
  :components ((:file "package")
               (:file "mining" :depends-on ("package"))))