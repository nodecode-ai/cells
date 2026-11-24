;;;; nodecode-opper.asd --- Opper, the EU-hosted AI gateway, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `opper' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from opper-ai's hermes-opper (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-opper"
  :description "Opper: an EU-hosted AI gateway, 700+ models behind one OpenAI-compatible API"
  :author "opper-ai (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "opper" :name "Opper" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-opper/test"))))

(defsystem "nodecode-opper/test"
  :description "Opper tests. Registered into the shared nodecode.test registry; RUN-OPPER-TESTS filters by the OPPER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-opper" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-opper-tests)))