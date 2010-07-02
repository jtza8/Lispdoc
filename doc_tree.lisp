; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defclass doc-tree ()
  ((current-package :initform :common-lisp
                    :accessor current-package)
   (packages :initform '()
             :reader packages)
   (classes :initform '()
            :reader classes)
   (conditions :initform '()
               :reader conditions)
   (macros :initform '()
           :reader macros)
   (functions :initform '()
              :reader functions)
   (generic-functions :initform '()
                      :reader generic-functions)
   (methods :initform '()
            :reader methods)))

(defun keywordise (symbol)
  (intern (symbol-name symbol) :keyword))

(defun get-sexp (sexp first-symbol)
  (find-if (lambda (x)
	     (when (consp x)
	       (eq (first x) first-symbol)))
	   sexp))

(defun (setf get-sexp) (new-value parent symbol)
  (loop for partial on parent
     when (and (consp (car partial)) (eq (caar partial) symbol)) do
       (setf (car partial) new-value) and return parent))

(defmethod mine-package ((tree doc-tree) sexp)
  (assert (eq (car sexp) 'defpackage))
  (with-slots (packages) tree
    (push (cons (cadr sexp) (cddr sexp)) packages)))

(defmethod mine-class-or-condition ((tree doc-tree) sexp)
  (with-slots (classes conditions current-package) tree
    (let ((destination (ecase (car sexp)
                         ((defclass) 'classes)
                         ((define-condition) 'conditions)))
          (options (cddddr sexp)))
      (push (append (list (keywordise (second sexp))
                          (list :package current-package)
                          (cons :super-types (third sexp)))
                    options)
            (slot-value tree destination)))))

(defmethod mine-macro-or-function ((tree doc-tree) sexp)
  (assert (find (car sexp) '(defun defmacro)))
  (with-slots (current-package) tree
    (push (append (list (keywordise (second sexp))
                        (list :package current-package)
                        (cons :arguments (third sexp)))
                  (when (stringp (fourth sexp))
                    (list (list :documentation (fourth sexp)))))
          (slot-value tree (ecase (car sexp) 
                             ((defmacro) 'macros)
                             ((defun) 'functions))))))

(defmethod mine-generic ((tree doc-tree) sexp)
  (assert (eq (car sexp) 'defgeneric))
  (with-slots (generic-functions current-package) tree
    (let ((options (cdddr sexp)))
      (push (append (list (keywordise (second sexp))
                          (list :package current-package)
                          (cons :arguments (third sexp)))
                    options)
            generic-functions))))

(defmethod mine-method ((tree doc-tree) sexp) 
  (assert (eq (car sexp) 'defmethod))
  (with-slots (methods current-package) tree
    (let* ((pointer (cddr sexp))
           (qualifiers (loop until (consp (car pointer)) 
                          collect (car pointer) do
                            (setf pointer (cdr pointer))))
           (arguments (pop pointer))
           (documentation (pop pointer))
           (method-name (second sexp)))
      (push (append (list (keywordise (if (consp method-name)
                                          (second method-name)
                                          method-name))
                          (list :package current-package))
                    (unless (null qualifiers) 
                      (list (cons :method-qualifiers qualifiers)))
                    (unless (null arguments)
                      (list (cons :arguments arguments)))
                    (list (list :setf-method 
                                (and (consp method-name) 
                                     (eq (first method-name) 'setf))))
                    (when (stringp documentation)
                      (list (list :documentation documentation))))
            methods))))

(defmethod parse-sexp ((tree doc-tree) sexp)
  (case (car sexp)
    ((defpackage) (mine-package tree sexp))
    ((defclass define-condition) (mine-class-or-condition tree sexp))
    ((defun defmacro) (mine-macro-or-function tree sexp))
    ((defgeneric) (mine-generic tree sexp))
    ((defmethod) (mine-method tree sexp))
    ((in-package) (setf (current-package tree)
                        (keywordise (second sexp))))))

(defun parse-doc-tree (file-list)
  (let ((doc-tree (make-instance 'doc-tree)))
    (dolist (file-name file-list doc-tree)
      (handler-case (with-open-file (input file-name :if-does-not-exist :error)
                      (loop (parse-sexp doc-tree (read input))))
        (end-of-file () ())))))
