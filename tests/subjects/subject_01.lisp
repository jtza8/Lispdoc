; Some comment

(defpackage :some-package)

(in-package :some-package)

(define-condition tag-error (error)
  ((fault :initarg :fault
          :initform :fault)
   (tag :initarg :tag
        :initform nil)
   (widget :initarg :widget
           :initform nil))
  (:report (lambda (condition stream)
             (with-slots (fault tag widget) condition
               (case fault
                 (:tag (format stream "Tag ~s for widget ~s must be unique"
                               tag widget))
                 (:widget (format stream "Widget ~s already has tag ~s"
                                  widget tag))
                 (:invalid-tag (format stream "Couldn't find tag ~s" tag))
                 (otherwise (format stream "Unknown fault: ~s" fault))))))
   (:documentation "Documentation here."))

(defclass window (widget)
  ((visible :initarg :visible
            :initform t)
   (listenable-events :initform '(:mouse-move :mouse-down :mouse-up))
   (widgets :initform '()
            :reader widgets)
   (tags :initform '()))
   (:documentation "Documentation."))

(defun mine-package (sexp &optional
                     (tree *doc-tree*)
                     (package *current-package*))
  "A function."
  (assert (eq (car sexp) 'defpackage))
  (push (getf sexp :documentation) (getf tree :packages))
  (push (keywordise (cadr sexp)) (getf tree :packages)))


(defgeneric blah (one two) (:documentation "Generic Function."))

(defmethod event-title-bar-drag ((window window) event)
  "Method documentation."
  (with-slots (x y) window
    (with-event-keys (x-offset y-offset) event
      (setf x x-offset
            y y-offset))))