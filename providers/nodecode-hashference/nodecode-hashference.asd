;;;; nodecode-hashference.asd --- Hashference, an OpenAI-compatible house of models, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hashference' section and installs the hook cell.lisp declares.
;;;;
;;;; A rewrite of 0xalank's Hermes plugin (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-hashference"
  :description "Hashference: its models as a provider, each sized by the input and output limits its catalog states"
  :license "MIT"
  :author "0xalank (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "hashference" :name "Hashference" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hashference/test"))))

(defsystem "nodecode-hashference/test"
  :description "Hashference tests. Registered into the shared nodecode.test registry; RUN-HASHFERENCE-TESTS filters by the HASHFERENCE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-hashference" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hashference-tests)))
