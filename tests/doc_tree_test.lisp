; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defclass miner-test (test-case)
  ())

(def-test-method test-get-sexp ((test miner-test))
  (assert-equal '(:foo :bar)
		(get-sexp '(:a :b (:foo :bar) :d) :foo))
  (assert-equal '((:foo :bar) :a :b)
		(setf (get-sexp '((:foo :baz) :a :b) :foo) '(:foo :bar))))

(def-test-method mine-package-test ((test miner-test))
  (let ((doc-tree (make-instance 'doc-tree)))
    (mine-package doc-tree 
                  '(defpackage :foo 
		    (:documentation "Reads minds.")
		    (:use :common-lisp)
		    (:shadow 'in-package)
		    (:size 30)))
    (assert-equal '(:foo
		    (:documentation "Reads minds.")
		    (:use :common-lisp)
		    (:shadow 'in-package)
		    (:size 30))
                  (get-sexp (packages doc-tree) :foo))))

(def-test-method mine-class-or-condition-test ((test miner-test))
  (let ((doc-tree (make-instance 'doc-tree)))
    (mine-class-or-condition doc-tree
                             '(defclass bar ()
			       ((variable-a :initarg "foo")
				variable-b)
			       (:default-init-args . whatever)
			       (:documentation "Reads minds.")))
    (assert-equal '(:bar (:package :common-lisp)
                         (:super-types)
		         (:default-init-args . whatever)
                         (:documentation "Reads minds."))
                  (get-sexp (classes doc-tree) :bar))
     (mine-class-or-condition doc-tree
                              '(define-condition condition (error)
 			       ((variable-a :initarg "foo")
 				variable-b)
 			       (:documentation "Reads minds.")
 			       (:report (stuff))))
     (assert-equal '(:condition (:package :common-lisp)
                                (:super-types error)
                                (:documentation "Reads minds.")
 		               (:report (stuff)))
                   (get-sexp (conditions doc-tree) :condition))))

(def-test-method mine-macro-or-function-test ((test miner-test))
  (let ((doc-tree (make-instance 'doc-tree)))
    (mine-macro-or-function doc-tree 
                            '(defun function-a (var-1 var-2)
			      "Reads minds"
			      (not (null t))))
    (assert-equal '(:function-a (:package :common-lisp)
		                (:arguments var-1 var-2)
                                (:documentation "Reads minds"))
                  (get-sexp (functions doc-tree) :function-a))
    (mine-macro-or-function doc-tree
                            '(defun function-b (var-1 var-2)
			      (not (null t))))
    (assert-equal '(:function-b (:package :common-lisp)
                                (:arguments var-1 var-2))
                  (get-sexp (functions doc-tree) :function-b))
    (mine-macro-or-function doc-tree
                            '(defmacro macro-a (var-1 var-2)
			      "Reads minds"
			      (not (null t))))
    (assert-equal '(:macro-a (:package :common-lisp)
                             (:arguments var-1 var-2)
                             (:documentation "Reads minds"))
                  (get-sexp (macros doc-tree) :macro-a))))

(def-test-method mine-generic-test ((test miner-test))
  (let ((doc-tree (make-instance 'doc-tree)))
    (mine-generic doc-tree
                  '(defgeneric generic (var-1 var-2)
                    (:documentation "Reads minds")))
    (assert-equal '(:generic (:package :common-lisp)
                             (:arguments var-1 var-2)
                             (:documentation "Reads minds"))
                  (get-sexp (generic-functions doc-tree) :generic))))

(def-test-method mine-method-test ((test miner-test))
  (let ((doc-tree (make-instance 'doc-tree)))
    (mine-method doc-tree
                 '(defmethod method-a (foo bar)
                   "Reads minds"
                   (not (null t))))
    (assert-equal '(:method-a (:package :common-lisp)
                              (:arguments foo bar)
                              (:setf-method nil)
                              (:documentation "Reads minds"))
                  (get-sexp (methods doc-tree) :method-a))
    (mine-method doc-tree
                 '(defmethod method-b :after (foo bar)
                   "Reads minds"
                   (not (null t))))
    (assert-equal '(:method-b (:package :common-lisp)
                              (:method-qualifiers :after)
                              (:arguments foo bar)
                              (:setf-method nil)
                              (:documentation "Reads minds"))
                  (get-sexp (methods doc-tree) :method-b))
    (mine-method doc-tree
                 '(defmethod (setf method-c) :after (foo bar)
                   "Reads minds"
                   (not (null t))))
    (assert-equal '(:method-c (:package :common-lisp)
                              (:method-qualifiers :after)
                              (:arguments foo bar)
                              (:setf-method t)
                              (:documentation "Reads minds"))
                  (get-sexp (methods doc-tree) :method-c))))

(def-test-method test-parse-doc-tree ((test miner-test))
  (parse-doc-tree (list (asdf:system-relative-pathname
			 :lispdoc
			 #P"tests/subjects/subject_01.lisp"))))