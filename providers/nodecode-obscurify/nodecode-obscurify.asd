;;;; nodecode-obscurify.asd --- Obscurify.ai as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `obscurify' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of Obscurify-ai's hermes-obscurify-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-obscurify"
  :description "Obscurify.ai: a privacy-focused OpenAI-compatible API, listing only the models a turn can run"
  :license "MIT"
  :author "Obscurify-ai (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "obscurify" :name "Obscurify.ai" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-obscurify/test"))))

(defsystem "nodecode-obscurify/test"
  :description "Obscurify.ai tests. Registered into the shared nodecode.test registry; RUN-OBSCURIFY-TESTS filters by the OBSCURIFY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-obscurify" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-obscurify-tests)))
