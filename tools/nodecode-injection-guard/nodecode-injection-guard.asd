;;;; nodecode-injection-guard.asd --- prompt-injection screening of tool output.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `injection-guard' section, registers two tools, one slash
;;;; command, one :TOOL hook and the manual topic, and puts the policy on
;;;; (setting :mode).
;;;;
;;;; Rewritten from moneya's Hermes Agent plugin of the same name; see NOTICE.
;;;; The detection is cl-ppcre and character inspection over one text, and
;;;; nothing else is reached: no network, no key, no model in the decision
;;;; path.

(defsystem "nodecode-injection-guard"
  :description "Screens web, browser, mail and file tool output for injected instructions, redacts them, and labels the rest untrusted"
  :author "moneya (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "detectors")
               (:file "sanitizer")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-injection-guard/test"))))

(defsystem "nodecode-injection-guard/test"
  :description "Injection Guard tests. Registered into the shared nodecode.test registry; RUN-INJECTION-GUARD-TESTS filters by the INJECTION-GUARD-CELL- name prefix."
  :author "moneya (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-injection-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-injection-guard-tests)))
