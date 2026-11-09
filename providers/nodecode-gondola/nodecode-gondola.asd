;;;; nodecode-gondola.asd --- Gondola, a USDC-settled marketplace for Venice AI inference, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gondola' section and installs the hook cell.lisp declares.
;;;;
;;;; A rewrite of Gondola's Hermes plugin (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-gondola"
  :description "Gondola: a USDC-settled marketplace for Venice AI inference, as a provider, with its mixed catalog filtered to chat models"
  :license "MIT"
  :author "Gondola-Market (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "gondola" :name "Gondola" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gondola/test"))))

(defsystem "nodecode-gondola/test"
  :description "Gondola tests. Registered into the shared nodecode.test registry; RUN-GONDOLA-TESTS filters by the GONDOLA-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-gondola" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gondola-tests)))
