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

(defun mine-condition (sexp &optional
                       (tree *doc-tree*)
                       (package *current-package*))
  (assert (eq (car sexp) 'define-condition))
  (push (list :package *current-package*
              :super-types (third sexp)
              :documentation (getf (cddddr sexp) :documentation))
        (getf tree :conditions))
  (push (keywordise (second sexp)) (getf tree :conditions)))

(defun mine-class (sexp &optional
                   (tree *doc-tree*)
                   (package *current-package*))
  (assert (eq (car sexp) 'defclass))
  (push (list :package *current-package*
              :super-types (third sexp)
              :documentation (getf (cddddr sexp) :documentation))
        (getf tree :classes))
  (push (keywordise (second sexp)) (getf tree :classes)))

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
  (let ((pointer (cddr sexp)))
    (push (list :package *current-package*
                :method-qualifiers (loop until (consp (car pointer))
                                      collect (car pointer) do
                                        (setf pointer (cdr pointer)))
                :arguments (pop pointer)
                :documentation (if (stringp (car pointer))
                                 (pop pointer)
                                 nil))
        (getf tree :methods))
  (push (keywordise (second sexp)) (getf tree :methods))))

(defun parse-sexp (sexp)
  (case (car sexp)
    ((defpackage) (mine-package sexp))
    ((define-condition) (mine-condition sexp))
    ((defclass) (mine-class sexp))
    ((defun) (mine-function sexp))
    ((defgeneric) (mine-generic sexp))
    ((defmethod) (mine-method sexp))
    ((in-package) (setf *current-package* (keywordise (second sexp))))))

(defun parse-doc-tree (file-list)
  (let ((*doc-tree* (copy-seq *doc-tree*))
        (*current-package* *current-package*))
    (dolist (file-name file-list *doc-tree*)
      (handler-case (with-open-file (input file-name :if-does-not-exist :error)
                      (loop (parse-sexp (print (read input)))))
        (end-of-file () ())))))
