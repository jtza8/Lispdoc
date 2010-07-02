; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defparameter *template-path* nil)
(defparameter *output-path* nil)
(defparameter *doc-tree* nil)
(defparameter *source-file-postfixes* '("lisp"))
(defparameter *links* '())

(defun configure-paths (template-path output-path)
  (setf *template-path* template-path
        *output-path* output-path)
  
(defun scan-sources (&rest source-paths)
  (setf *doc-tree*
        (parse-doc-tree 
         (remove-if-not (lambda (path)
                          (find-if (lambda (string)
                                     (string= string (pathname-type path)))
                                   *source-file-postfixes*))
                        (apply #'append
                               (mapcar #'cl-fad:list-directory
                                       source-paths))))))