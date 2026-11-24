;;;; nodecode-telnyx.asd --- Telnyx inference, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `telnyx' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of team-telnyx's hermes-inference-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-telnyx"
  :description "Telnyx: OpenAI-compatible inference, hosted open models and proxied frontier routes"
  :license "MIT"
  :author "team-telnyx (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "telnyx" :name "Telnyx" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-telnyx/test"))))

(defsystem "nodecode-telnyx/test"
  :description "Telnyx tests. Registered into the shared nodecode.test registry; RUN-TELNYX-TESTS filters by the TELNYX-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-telnyx" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-telnyx-tests)))
