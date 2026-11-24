;;;; nodecode-cjk-sanitizer.asd --- strip stray Chinese and Cyrillic from the model's answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://cjk-sanitizer' is accepted. The gateway calls
;;;; START-CELL after recovery; it reads the `cjk_sanitizer' config section
;;;; and advises NLE::EMIT-STREAM-PART, the one funnel every provider lane's
;;;; stream part goes through, so a leaked character is gone before any
;;;; shell, channel, transcript or history line reads it.

(defsystem "nodecode-cjk-sanitizer"
  :description "Strips stray Chinese and Cyrillic characters from the model's streamed answers"
  :license "MIT"
  :author "evanyudis (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cjk-sanitizer/test"))))

(defsystem "nodecode-cjk-sanitizer/test"
  :description "Tests for the cjk-sanitizer cell; RUN-CJK-SANITIZER-TESTS filters the shared registry by the CJK-SANITIZER- prefix."
  :license "MIT"
  :depends-on ("nodecode-cjk-sanitizer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "strip-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cjk-sanitizer-tests)))
