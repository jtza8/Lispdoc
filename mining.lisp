; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defparameter *doc-tree* '(:packages ()
                           :conditions ()
                           :classes ()
			   :macros ()
                           :functions ()
                           :generic-functions ()
                           :methods ()))
(defparameter *current-package* :cl-user)

(defun keywordise (symbol)
  (intern (symbol-name symbol) :keyword))

(defun convert-options (options &key (atomic-keys nil) (ignore nil))
  (loop for option in options
     unless (find (first option) ignore)
       collect (first option) and
       unless (find (first option) atomic-keys) collect (rest option)
       else collect (second option)))

(defun mine-package (sexp &optional
                     (tree *doc-tree*))
  (assert (eq (car sexp) 'defpackage))
  (push (convert-options (cddr sexp)
			 :atomic-keys '(:documentation :size))
	(getf tree :packages))
  (push (keywordise (cadr sexp)) (getf tree :packages)))

(defun mine-class-or-condition (sexp &optional
				(tree *doc-tree*)
				(package *current-package*))
  (let* ((type-key (ecase (car sexp)
		     ((defclass) :classes)
		     ((define-condition) :conditions)))
	 (options (cddddr sexp))
	 (default-init-args (cdr (find-if (lambda (x) (eq (first x) :default-init-args))
					  options))))
    (push (append (list :package package
			:super-types (third sexp))
		  (when default-init-args
		    (list :default-init-args default-init-args))
		  (convert-options options 
				   :atomic-keys '(:documentation
						  :metaclass
						  :report)
				   :ignore '(:default-init-args)))
	  (getf tree type-key))
    (push (keywordise (second sexp)) (getf tree type-key))))

(defun mine-macro-or-function (sexp &optional
			       (tree *doc-tree*)
			       (package *current-package*))
  (ecase (car sexp) ((defun defmacro) t))
  (let ((type-key (ecase (car sexp) 
		    ((defun) :functions)
		    ((defmacro) :macros))))
    (push (list :package package
		:arguments (third sexp)
		:documentation (if (stringp (fourth sexp))
				   (fourth sexp)
				   nil))
	  (getf tree type-key))
    (push (keywordise (second sexp)) (getf tree type-key))))

(defun mine-generic (sexp &optional 
                     (tree *doc-tree*)
                     (package *current-package*))
  (assert (eq (car sexp) 'defgeneric))
  (let ((options (cdddr sexp)))
    (push (append (list :package package
			:arguments (third sexp))
		  (convert-options options :atomic-keys '(:documentation)))
	  (getf tree :generic-functions))
    (push (keywordise (second sexp)) (getf tree :generic-functions))))

(defun mine-method (sexp &optional 
                    (tree *doc-tree*) 
                    (package *current-package*))
  (assert (eq (car sexp) 'defmethod))
  (let ((pointer (cddr sexp))
        (method-name (second sexp)))
    (push (list :package package
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
