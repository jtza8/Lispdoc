; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defclass generator ()
  ((doc-tree :initform (error "No doc-tree specified.")
             :initarg :doc-tree)
   (templates-path :initform (error "No templates-path specified."))
   (output-path :initform (error "No output-path specified.")
                :initarg :output-path)
;;    (source-paths :initform (error "No source-paths specified.")
;;                  :initarg :source-paths)
   ))