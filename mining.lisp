; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defparameter *doc-tree* '((:packages)
                           (:conditions)
                           (:classes)
			   (:macros)
                           (:functions)
                           (:generic-functions)
                           (:methods)))
(defparameter *current-package* :cl-user)

(defun keywordise (symbol)
  (intern (symbol-name symbol) :keyword))

(defun get-sexp (first-symbol sexp)
  (find-if (lambda (x)
	     (when (consp x)
	       (eq (first x) first-symbol)))
	   sexp))

(defun (setf get-sexp) (new-value symbol parent)
  (loop for partial on parent
     when (and (consp (car partial)) (eq (caar partial) symbol)) do
       (setf (car partial) new-value) and return parent))

(defun mine-package (sexp &optional
                     (tree *doc-tree*))
  (assert (eq (car sexp) 'defpackage))
  (push (cons (cadr sexp) (cddr sexp))
	(cdr (get-sexp :packages tree))))

(defun mine-class-or-condition (sexp &optional
				(tree *doc-tree*)
				(package *current-package*))
  (let* ((type-key (ecase (car sexp)
		     ((defclass) :classes)
		     ((define-condition) :conditions)))
	 (options (cddddr sexp)))
    (push (append (list (keywordise (second sexp))
			(list :package package)
			(cons :super-types (third sexp)))
		  options)
	  (cdr (get-sexp type-key tree)))))

(defun mine-macro-or-function (sexp &optional
			       (tree *doc-tree*)
			       (package *current-package*))
  (ecase (car sexp) ((defun defmacro) t))
  (let ((type-key (ecase (car sexp) 
		    ((defun) :functions)
		    ((defmacro) :macros))))
    (push (append (list (keywordise (second sexp))
			(list :package package)
			(cons :arguments (third sexp)))
		  (when (stringp (fourth sexp))
		    (list (list :documentation (fourth sexp)))))
	  (cdr (get-sexp type-key tree)))))

(defun mine-generic (sexp &optional 
                     (tree *doc-tree*)
                     (package *current-package*))
  (assert (eq (car sexp) 'defgeneric))
  (let ((options (cdddr sexp)))
    (push (append (list (keywordise (second sexp))
			(list :package package)
			(cons :arguments (third sexp)))
		  options)
	  (cdr (get-sexp :generic-functions tree)))))

(defun mine-method (sexp &optional 
                    (tree *doc-tree*) 
                    (package *current-package*))
  (assert (eq (car sexp) 'defmethod))
  (let* ((pointer (cddr sexp))
	 (qualifiers (loop until (consp (car pointer)) collect (car pointer) do
			  (setf pointer (cdr pointer))))
	 (arguments (pop pointer))
	 (documentation (pop pointer))
	 (method-name (second sexp)))
    (push (append (list (keywordise (if (consp method-name)
					(second method-name)
					method-name))
			(list :package package))
		  (unless (null qualifiers) 
		    (list (cons :method-qualifiers qualifiers)))
		  (unless (null arguments)
		    (list (cons :arguments arguments)))
		  (list (list :setf-method (and (consp method-name) 
						(eq (first method-name) 'setf))))
		  (when (stringp documentation)
		    (list (list :documentation documentation))))
	  (cdr (get-sexp :methods tree)))))

(defun parse-sexp (sexp)
  (case (car sexp)
    ((defpackage) (mine-package sexp))
    ((defclass define-condition) (mine-class-or-condition sexp))
    ((defun defmacro) (mine-macro-or-function sexp))
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
