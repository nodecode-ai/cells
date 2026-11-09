;;;; nodecode-warm.asd --- keep an OpenAI-compatible prompt cache warm.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of mojomast/hermes-session-warmer for Hermes Agent -- see NOTICE.

(defsystem "nodecode-warm"
  :description "Keep a provider's prompt cache warm: an idle session's own request, resent one token at a time"
  :license "MIT"
  :author "mojomast (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-warm/test"))))

(defsystem "nodecode-warm/test"
  :description "Tests for the warm cell; RUN-WARM-TESTS filters the shared registry by the WARM-CELL- prefix."
  :license "MIT"
  :author "mojomast (original), Nodecode (rewrite)"
  :depends-on ("nodecode-warm" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-warm-tests)))
