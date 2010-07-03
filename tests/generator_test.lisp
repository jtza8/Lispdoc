; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :lispdoc)

(defclass generation-test (test-case)
  ())

(def-test-method test-build-doc ((test generation-test))
  ())