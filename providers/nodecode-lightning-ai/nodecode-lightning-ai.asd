;;;; nodecode-lightning-ai.asd --- Lightning AI as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `lightning-ai' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; A rewrite of dinosauria123's hermes-lightningai-provider (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-lightning-ai"
  :description "Lightning AI: its OpenAI-compatible gateway as a Nodecode provider, with the model list it publishes"
  :license "MIT"
  :author "dinosauria123 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "lightningai" :name "Lightning AI" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lightning-ai/test"))))

(defsystem "nodecode-lightning-ai/test"
  :description "Lightning AI tests. Registered into the shared nodecode.test registry; RUN-LIGHTNING-AI-TESTS filters by the LIGHTNING-AI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-lightning-ai" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lightning-ai-tests)))
