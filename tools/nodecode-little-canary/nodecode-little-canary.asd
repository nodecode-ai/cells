;;;; nodecode-little-canary.asd --- prompt-injection screening as a tool.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `little-canary' section, registers one tool, one slash command
;;;; and one :tool hook, and puts the primer on (help :little-canary).
;;;;
;;;; Rewritten from hermes-labs-ai's Hermes Agent plugin of the same name;
;;;; see NOTICE. A screen is structural checks first (cl-ppcre), then one
;;;; call to a small local Ollama model over NLK:HTTP; nothing else is
;;;; reached and no key is needed.

(defsystem "nodecode-little-canary"
  :description "Screens untrusted text for prompt injection with a local canary model, and withholds a session's tools on a BLOCK"
  :author "hermes-labs-ai (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.4.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "filter")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-little-canary/test"))))

(defsystem "nodecode-little-canary/test"
  :description "Little Canary tests. Registered into the shared nodecode.test registry; RUN-LITTLE-CANARY-TESTS filters by the LITTLE-CANARY-CELL- name prefix."
  :author "hermes-labs-ai (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-little-canary" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-little-canary-tests)))
