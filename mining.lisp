; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defparameter *doc-tree* '(:packages ()
                           :conditions ()
                           :classes ()
                           :functions ()
                           :generic-functions ()
                           :methods ()))
(defparameter *current-package* :cl-user)

(defun keywordise (symbol)
  (intern (symbol-name symbol) :keyword))

(defun mine-package (sexp &optional
                     (tree *doc-tree*)
                     (package *current-package*))
  (assert (eq (car sexp) 'defpackage))
  (push (getf sexp :documentation) (getf tree :packages))
  (push (keywordise (cadr sexp)) (getf tree :packages)))

(defun mine-class (sexp &optional
                   (tree *doc-tree*)
                   (package *current-package*))
  (let ((type-key (ecase (car sexp)
                    ((defclass) :classes)
                    ((define-condition) :conditions))))
    (push (list :package package
                :super-types (third sexp)
                :documentation (second (find-if 
                                        (lambda (item)
                                          (eq (first item) :documentation))
                                        (cddddr sexp))))
          (getf tree type-key))
  (push (keywordise (second sexp)) (getf tree type-key))))

(defun mine-function (sexp &optional
                      (tree *doc-tree*)
                      (package *current-package*))
  (assert (eq (car sexp) 'defun))
  (push (list :package *current-package*
              :arguments (third sexp)
              :documentation (if (stringp (fourth sexp))
                                 (fourth sexp)
                                 nil))
        (getf tree :functions))
  (push (keywordise (second sexp)) (getf tree :functions)))

(defun mine-generic (sexp &optional 
                     (tree *doc-tree*)
                     (package *current-package*))
  (assert (eq (car sexp) 'defgeneric))
  (push (list :package *current-package*
              :arguments (third sexp)
              :documentation (getf (cdddr sexp) :documentation))
        (getf tree :generic-functions))
  (push (keywordise (second sexp)) (getf tree :generic-functions)))

(defun mine-method (sexp &optional 
                    (tree *doc-tree*) 
                    (package *current-package*))
  (assert (eq (car sexp) 'defmethod))
  (let ((pointer (cddr sexp))
        (method-name (second sexp)))
    (push (list :package *current-package*
                :method-qualifiers (loop until (consp (car pointer))
                                      collect (car pointer) do
                                        (setf pointer (cdr pointer)))
                :arguments (pop pointer)
                :setf-method (and (consp method-name) 
                                  (eq (first method-name) 'setf))
                :documentation (if (stringp (car pointer))
                                 (pop pointer)
                                 nil))
        (getf tree :methods))
  (push (keywordise (if (consp method-name)
                        (second method-name)
                        method-name))
        (getf tree :methods))))

(defun parse-sexp (sexp)
  (case (car sexp)
    ((defpackage) (mine-package sexp))
    ((defclass define-condition) (mine-class sexp))
    ((defun) (mine-function sexp))
    ((defgeneric) (mine-generic sexp))
    ((defmethod) (mine-method sexp))
    ((in-package) (setf *current-package* (keywordise (second sexp))))))

(defun parse-doc-tree (file-list)
  (let ((*doc-tree* (copy-seq *doc-tree*))
        (*current-package* *current-package*))
    (dolist (file-name file-list *doc-tree*)
      (handler-case (with-open-file (input file-name :if-does-not-exist :error)
                      (loop (parse-sexp (read input))))
        (end-of-file () ())))))
