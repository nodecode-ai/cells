;;;; nodecode-crowbot.asd --- Crowbot, crowbot.sh's OpenAI-compatible relay, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `crowbot' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from AgileIndustrialComplex's hermes-crowbot (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-crowbot"
  :description "Crowbot: crowbot.sh's OpenAI-compatible relay as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "AgileIndustrialComplex (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "crowbot" :name "Crowbot" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-crowbot/test"))))

(defsystem "nodecode-crowbot/test"
  :description "Crowbot tests. Registered into the shared nodecode.test registry; RUN-CROWBOT-TESTS filters by the CROWBOT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-crowbot" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-crowbot-tests)))
