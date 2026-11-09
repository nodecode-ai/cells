;;;; nodecode-chutes.asd --- Chutes, its gateway and its tool-capable catalog, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `chutes' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; Rewritten from TheStreamCode's hermes-chutes-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-chutes"
  :description "Chutes: its gateway and its tool-capable model catalog, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "TheStreamCode (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "chutes" :name "Chutes" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-chutes/test"))))

(defsystem "nodecode-chutes/test"
  :description "Chutes tests. Registered into the shared nodecode.test registry; RUN-CHUTES-TESTS filters by the CHUTES-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-chutes" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chutes-tests)))
