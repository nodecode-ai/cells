;;;; nodecode-modular.asd --- Modular Cloud as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `modular' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of tommyjtl's hermes-modular-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-modular"
  :description "Modular Cloud: MiniMax, DeepSeek and Gemma models, each sent the reasoning effort its API takes"
  :license "Apache-2.0"
  :author "tommyjtl (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "modular" :name "Modular" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-modular/test"))))

(defsystem "nodecode-modular/test"
  :description "Modular tests. Registered into the shared nodecode.test registry; RUN-MODULAR-TESTS filters by the MODULAR-CELL- name prefix."
  :license "Apache-2.0"
  :depends-on ("nodecode-modular" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-modular-tests)))
