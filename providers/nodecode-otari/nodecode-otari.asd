;;;; nodecode-otari.asd --- Otari, the open-source LLM gateway, as a provider.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `otari' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of mozilla-ai's hermes-provider-otari (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-otari"
  :description "Otari: the open-source LLM gateway as a Nodecode provider"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "mozilla-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "otari" :name "Otari" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-otari/test"))))

(defsystem "nodecode-otari/test"
  :description "Otari tests. Registered into the shared nodecode.test registry; RUN-OTARI-TESTS filters by the OTARI-CELL- name prefix."
  :license "Apache-2.0"
  :author "mozilla-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-otari" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-otari-tests)))
