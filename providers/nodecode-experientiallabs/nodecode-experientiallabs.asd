;;;; nodecode-experientiallabs.asd --- Experiential Labs, a multi-model gateway, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `experientiallabs' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of kyoto-aircon's hermes-experientiallabs (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-experientiallabs"
  :description "Experiential Labs: its OpenAI-compatible multi-model gateway as a Nodecode provider"
  :license "MIT"
  :author "kyoto-aircon (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "experientiallabs" :name "Experiential Labs" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "usage")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-experientiallabs/test"))))

(defsystem "nodecode-experientiallabs/test"
  :description "Experiential Labs tests. Registered into the shared nodecode.test registry; RUN-EXPERIENTIALLABS-TESTS filters by the EXPERIENTIALLABS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-experientiallabs" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-experientiallabs-tests)))
