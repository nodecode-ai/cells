;;;; nodecode-apinex.asd --- APInex, a gateway's two wires, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `apinex' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from APInex-AI's Hermes provider plugin (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-apinex"
  :description "APInex: Claude over the Messages wire, and the OpenAI-compatible vendors and free tier, on one key"
  :license "MIT"
  :author "Apinex-AI (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "apinex" :name "APInex" :sign-in "key")
                        (:provider "apinex-openai" :name "APInex (OpenAI-compatible)" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-apinex/test"))))

(defsystem "nodecode-apinex/test"
  :description "APInex tests. Registered into the shared nodecode.test registry; RUN-APINEX-TESTS filters by the APINEX-CELL- name prefix."
  :license "MIT"
  :author "Apinex-AI (original), Nodecode (rewrite)"
  :depends-on ("nodecode-apinex" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-apinex-tests)))
