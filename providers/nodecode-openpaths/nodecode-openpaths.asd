;;;; nodecode-openpaths.asd --- OpenPaths as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openpaths' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of lee101's hermes-openpaths (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-openpaths"
  :description "OpenPaths: auto task-tier routing and hosted models, as a Nodecode provider"
  :license "MIT"
  :author "lee101 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "openpaths" :name "OpenPaths" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openpaths/test"))))

(defsystem "nodecode-openpaths/test"
  :description "OpenPaths tests. Registered into the shared nodecode.test registry; RUN-OPENPATHS-TESTS filters by the OPENPATHS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-openpaths" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openpaths-tests)))
