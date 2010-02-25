; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defclass miner-test (test-case)
  ())

(def-test-method convert-options-test ((test miner-test))
  (assert-equal '(:documentation "string"
		  :use (:foo :bar)
		  :other (:foo :bar :baz))
		(convert-options '((:documentation "string")
				   (:use :foo :bar)
				   (:blah "something")
				   (:other :foo :bar :baz))
				 :atomic-keys '(:documentation)
				 :ignore '(:blah))))

(def-test-method mine-package-test ((test miner-test))
  (let ((*doc-tree* (copy-seq *doc-tree*)))
    (mine-package '(defpackage :foo 
		    (:documentation "Reads minds.")
		    (:use :common-lisp)
		    (:shadow 'in-package)
		    (:size 30)))
    (assert-equal '(:documentation "Reads minds."
		    :use (:common-lisp)
		    :shadow ('in-package)
		    :size 30)
                  (getf (getf *doc-tree* :packages) :foo))))

(def-test-method mine-macro-or-function-test ((test miner-test))
  (let ((*doc-tree* (copy-seq *doc-tree*)))
    (mine-macro-or-function '(defun function-a (var-1 var-2)
			      "Reads minds"
			      (not (null t))))
    (assert-equal '(:package :cl-user
                    :arguments (var-1 var-2)
                    :documentation "Reads minds")
                  (getf (getf *doc-tree* :functions) :function-a))
    (mine-macro-or-function '(defun function-b (var-1 var-2)
			      (not (null t))))
    (assert-equal '(:package :cl-user
                    :arguments (var-1 var-2)
                    :documentation nil)
                  (getf (getf *doc-tree* :functions) :function-b))
    (mine-macro-or-function '(defmacro macro-a (var-1 var-2)
			      "Reads minds"
			      (not (null t))))
    (assert-equal '(:package :cl-user
                    :arguments (var-1 var-2)
                    :documentation "Reads minds")
                  (getf (getf *doc-tree* :macros) :macro-a))))

(def-test-method mine-class-or-condition-test ((test miner-test))
  (let ((*doc-tree* (copy-seq *doc-tree*)))
    (mine-class-or-condition '(defclass bar ()
			       ((variable-a :initarg "foo")
				variable-b)
			       (:default-init-args . whatever)
			       (:documentation "Reads minds.")))
    (assert-equal '(:package :cl-user
                    :super-types ()
		    :default-init-args whatever
                    :documentation "Reads minds.")
                  (getf (getf *doc-tree* :classes) :bar))
    (mine-class-or-condition '(define-condition condition (error)
			       ((variable-a :initarg "foo")
				variable-b)
			       (:documentation "Reads minds.")
			       (:report (stuff))))
    (assert-equal '(:package :cl-user
                    :super-types (error)
                    :documentation "Reads minds."
		    :report (stuff))
                  (getf (getf *doc-tree* :conditions) :condition))))

(def-test-method mine-method-test ((test miner-test))
  (let ((*doc-tree* (copy-seq *doc-tree*)))
    (mine-method '(defmethod method-a (foo bar)
                   "Reads minds"
                   (not (null t))))
    (assert-equal '(:package :cl-user
                    :method-qualifiers ()
                    :arguments (foo bar)
                    :setf-method nil
                    :documentation "Reads minds")
                  (getf (getf *doc-tree* :methods) :method-a))
    (mine-method '(defmethod method-b :after (foo bar)
                   "Reads minds"
                   (not (null t))))
    (assert-equal '(:package :cl-user
                    :method-qualifiers (:after)
                    :arguments (foo bar)
                    :setf-method nil
                    :documentation "Reads minds")
                  (getf (getf *doc-tree* :methods) :method-b))
    (mine-method '(defmethod (setf method-c) :after (foo bar)
                   "Reads minds"
                   (not (null t))))
    (assert-equal '(:package :cl-user
                    :method-qualifiers (:after)
                    :arguments (foo bar)
                    :setf-method t
                    :documentation "Reads minds")
                  (getf (getf *doc-tree* :methods) :method-c))))

(def-test-method mine-generic-test ((test miner-test))
  (let ((*doc-tree* (copy-seq *doc-tree*)))
    (mine-generic '(defgeneric generic (var-1 var-2)
                    (:documentation "Reads minds")))
    (assert-equal '(:package :cl-user
                    :arguments (var-1 var-2)
                    :documentation "Reads minds")
                  (getf (getf *doc-tree* :generic-functions) :generic))))

(def-test-method test-parse-doc-tree ((test miner-test))
  (parse-doc-tree (list (asdf:system-relative-pathname
			 :lispdoc
			 #P"tests/subjects/subject_01.lisp"))))