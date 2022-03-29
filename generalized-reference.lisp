;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See generalized-reference/LICENSE for more information.

(in-package :cl-user)

(defpackage generalized-reference
  (:nicknames gen-ref genref gref)
  (:use c2cl series split-sequence)
  (:shadowing-import-from series
           #:series)
  (:import-from serapeum
           #:vref)
  (:import-from alexandria
           #:ensure-function
           #:ensure-symbol
           #:make-keyword
           #:string-designator)
  (:import-from trivial-types
           #:proper-list-p
           #:proper-list
           #:property-list-p
           #:property-list
           #:association-list-p
           #:association-list
           #:non-nil
           #:type-specifier-p
           #:type-expand)
  (:export #:ref
           #:$
           #:%ref
           #:series-p))

(in-package :generalized-reference)

(defun series-p (obj)
  (typecase obj
    (series t)
    (otherwise nil)))

(defclass pseudosymbol ()
  ((name :type string-designator :initarg :name :accessor pseudosymbol-name)
   (pkg-name :type (or string-designator nil) :initarg :package-name :accessor pseudosymbol-package-name)
   (value-slot :initarg :value :accessor pseudosymbol-value)
   (function-slot :initarg :function :accessor pseudosymbol-function)
   (plist :type property-list :initarg :plist :accessor pseudosymbol-plist)))

(defgeneric %ref (alice rabbit-hole)
  (:documentation "The generic function for reducing pairwise over arbitrary structures of data by place identifier."))

(defmethod %ref ((pkg package) (sym symbol))
  (let* ((symbolname (symbol-name sym))
         (the-sym (find-symbol symbolname (package-name pkg))))
    (make-instance 'pseudosymbol
      :name (symbol-name the-sym)
      :package (package-name (symbol-package the-sym))
      :value (symbol-value the-sym)
      :function (symbol-function the-sym)
      :plist (symbol-plist the-sym))))

(defmethod %ref ((psym pseudosymbol) (sym symbol))
  (case (ensure-symbol sym :keyword)
    (:name (pseudosymbol-name psym))
    (:package (find-package (pseudosymbol-package-name psym)))
    (:package-name (pseudosymbol-package-name psym))
    (:value (pseudosymbol-value psym))
    (:function (pseudosymbol-function psym))
    (otherwise (getf (pseudosymbol-plist psym) sym))))

(defmethod %ref ((psym pseudosymbol) (sym-name string))
  (%ref psym (make-keyword sym-name)))

;; Disable pathname and file reference, it still needs some work

;; (defmethod %ref ((store pathname) (fragment string))
;;   (let ((rabbit-hole (mapcar #'make-keyword (split-sequence "/" fragment))))
;;     (with-open-file (f store :direction :input)
;;       (reduce #'getf (%ref (read f) (car rabbit-hole)) (cdr rabbit-hole)))))

(defmethod %ref ((store standard-object) (slot-name symbol))
  (if (slot-boundp store slot-name)
      (or (funcall (ensure-function slot-name) store)
          (slot-value store slot-name))
      :not-bound))

(defmethod %ref ((store standard-object) (slot-name string))
  (%ref store (ensure-symbol slot-name)))

(defmethod %ref ((store hash-table) (hash-key t))
  (gethash hash-key store :not-found))

(defmethod %ref ((store list) (key symbol))
  (typecase store
    (property-list (or (getf store (ensure-symbol key :keyword))
                       (getf store key)))
    (association-list (cdr (assoc key store)))
    (otherwise (nthcdr (position key store :test #'equalp) store))))

(defmethod %ref ((store list) (key string))
  (typecase store
    (property-list (getf store (make-keyword key)))
    (association-list (cdr (assoc (ensure-symbol key) store)))
    (otherwise (nthcdr (position key store :test #'string-equal) store))))

(defmethod %ref ((store vector) (index integer))
  (vref store index))

(defmethod %ref ((store array) (indices sequence))
  (apply #'aref store (coerce indices 'list)))

(defmethod %ref ((store symbol) (prop symbol))
  (get store (ensure-symbol prop :keyword)))

(defmethod %ref ((store structure-object) (selection structure-object))
  (if (series-p store)
      (choose (mask selection) store)
      :not-series-object))

(defmethod %ref ((store structure-object) (index integer))
  (if (series-p store)
      (choose (mask (scan index)) store)
      :not-series-element))

(defmethod %ref ((store structure-object) (indices sequence))
  (if (series-p store)
      (choose (mask (scan indices)) store)
      :not-series-elements))

(defmethod %ref ((store structure-object) (key function))
  (if (series-p store)
      (choose-if key store)
      :not-series-choice))

(defun ref (&rest references)
  "The main interface for generalized reference by pairwise reduction over a
   parameter list of place identifiers."
  (reduce #'%ref references))

(defmacro $ (&rest references)
  "Convenience macro for generalized reference."
  `(ref ,@references))
