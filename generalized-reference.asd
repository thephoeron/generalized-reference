;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See generalized-reference/LICENSE for more information.

(in-package :cl-user)

(defpackage generalized-reference/asdf
  (:use cl asdf uiop))

(in-package :generalized-reference/asdf)

(defsystem generalized-reference
  :name "GENERALIZED-REFERENCE"
  :description "Generalized reference over structured data by pairwise reduction of arbitrary place identifiers for Common Lisp."
  :author "\"the Phoeron\" Colin J.E. Lupton"
  :source-control "https://github.com/thephoeron/generalized-reference/"
  :bug-tracker "https://github.com/thephoeron/generalized-reference/issues/"
  :mailto "thephoeron@protonmail.com"
  :version (:read-file-form "VERSION")
  :license "MIT"
  :depends-on (closer-mop
               alexandria
               serapeum
               split-sequence
               trivial-types
               series)
  :serial nil
  :components ((:file "generalized-reference")))
