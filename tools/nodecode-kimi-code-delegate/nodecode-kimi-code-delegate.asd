;;;; nodecode-kimi-code-delegate.asd --- a read-only delegate to the Kimi
;;;; Code CLI over ACP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of s-a-s-k-i-a/hermes-kimi-code. Optional cell, not part of the
;;;; organism core: the folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/, and the gateway calls START-CELL after
;;;; recovery, which registers one tool, one slash command and one manual
;;;; topic. See NOTICE.

(defsystem "nodecode-kimi-code-delegate"
  :description "Ask the locally signed-in Kimi Code CLI for read-only advice"
  :license "MIT"
  :author "s-a-s-k-i-a (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "acp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kimi-code-delegate/test"))))

(defsystem "nodecode-kimi-code-delegate/test"
  :description "Kimi delegate tests: the lifecycle, the ACP dialogue against a scripted agent, the tool, the command, the refusals. Registered into the shared nodecode.test registry; RUN-KIMI-CODE-DELEGATE-TESTS filters by the KIMI-CODE-DELEGATE-CELL- name prefix."
  :license "MIT"
  :author "s-a-s-k-i-a (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kimi-code-delegate" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kimi-code-delegate-tests)))
