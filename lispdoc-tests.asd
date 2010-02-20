(defsystem "lispdoc-tests"
  :description "The test suite for lispdoc."
  :author "Jens Thiede"
  :licence "BSD-Style Licence"
  :depends-on ("lispdoc" "xlunit")
  :components ((:module "tests"
                :components ((:file "test_package")
                             (:file "mining_test"
                              :depends-on ("test_package"))))))