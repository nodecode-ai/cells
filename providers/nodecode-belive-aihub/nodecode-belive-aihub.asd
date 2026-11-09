;;;; nodecode-belive-aihub.asd --- BeLive AI Hub, an OpenAI-compatible model
;;;; gateway, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `belive-aihub' section and installs the hook cell.lisp declares.
;;;;
;;;; A rewrite of BeLive Technology's Hermes plugin (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-belive-aihub"
  :description "BeLive AI Hub: an OpenAI-compatible model gateway as a provider, its picker filled from the live catalog"
  :license "MIT"
  :author "belivetech (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "belive-aihub" :name "BeLive AI Hub" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-belive-aihub/test"))))

(defsystem "nodecode-belive-aihub/test"
  :description "BeLive AI Hub tests. Registered into the shared nodecode.test registry; RUN-BELIVE-AIHUB-TESTS filters by the BELIVE-AIHUB-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-belive-aihub" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-belive-aihub-tests)))
