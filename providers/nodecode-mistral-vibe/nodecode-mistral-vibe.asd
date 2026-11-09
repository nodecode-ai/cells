;;;; nodecode-mistral-vibe.asd --- Mistral Vibe as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mistral-vibe' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of nebneo's hermes-mistral-vibe (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-mistral-vibe"
  :description "Mistral Vibe: the browser sign-in and the model lane of an unofficial Mistral Vibe client"
  :license "MIT"
  :version "0.1.0"
  :author "nebneo (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "mistral-vibe" :name "Mistral Vibe" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mistral-vibe/test"))))

(defsystem "nodecode-mistral-vibe/test"
  :description "Mistral Vibe tests. Registered into the shared nodecode.test registry; RUN-MISTRAL-VIBE-TESTS filters by the MISTRAL-VIBE-CELL- name prefix."
  :license "MIT"
  :author "nebneo (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mistral-vibe" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mistral-vibe-tests)))
