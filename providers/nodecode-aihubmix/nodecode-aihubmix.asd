;;;; nodecode-aihubmix.asd --- AIHubMix, an OpenAI-compatible house of 400+ models, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `aihubmix' section and installs the hook cell.lisp declares.
;;;;
;;;; A rewrite of AIHubMix's Hermes plugin (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-aihubmix"
  :description "AIHubMix: 400+ OpenAI-compatible models as a provider, filtered to tool-capable routes"
  :license "MIT"
  :author "AIhubmix (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "aihubmix" :name "AIHubMix" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-aihubmix/test"))))

(defsystem "nodecode-aihubmix/test"
  :description "AIHubMix tests. Registered into the shared nodecode.test registry; RUN-AIHUBMIX-TESTS filters by the AIHUBMIX-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-aihubmix" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-aihubmix-tests)))
