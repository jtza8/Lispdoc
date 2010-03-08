; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

;; (defun generate-package (package-name info)
;;   (format nil "# ~a (~[~a ~])

;; Uses: ~[~a~]
;; Exports: ~[~a~]
;; Shadows: ~[~a~]"
;; 	  package-name
;; 	  (getf info :use)
;; 	  (getf info :export)
;; 	  (getf info :shadow)))