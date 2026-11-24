;;;; nodecode-codex-truly.asd --- the real Codex context window, measured and written.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `codex-truly' section, registers the codex_truly tool, the
;;;; /codex-truly command and one manual topic.
;;;;
;;;; Rewritten from kyssta-exe's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to the ChatGPT
;;;; backend, cl-base64 reads the account id out of the sign-in's token.

(defsystem "nodecode-codex-truly"
  :description "Codex Truly: the real context window of a Codex subscription, measured and written"
  :author "kyssta-exe (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-codex-truly/test"))))

(defsystem "nodecode-codex-truly/test"
  :description "Codex Truly tests. Registered into the shared nodecode.test registry; RUN-CODEX-TRULY-TESTS filters by the CODEX-TRULY-CELL- name prefix."
  :author "kyssta-exe (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-codex-truly" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-codex-truly-tests)))
