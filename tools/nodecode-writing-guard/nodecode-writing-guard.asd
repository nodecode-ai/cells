;;;; nodecode-writing-guard.asd --- dashes, AI phrases and du/Sie, checked as the answer streams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://writing-guard' is accepted. START-CELL reads the
;;;; `writing-guard' section, advises NLE::EMIT-STREAM-PART (so a fragment is
;;;; fixed where it is made and the line rides the answer itself) and
;;;; NLE::READ-HARNESS-SECTIONS (the standing primer), and registers the
;;;; writing_guard_check tool, /writing-guard and (help :writing-guard).

(defsystem "nodecode-writing-guard"
  :description "Dashes, stock AI phrases and du/Sie, reported under the answer or fixed as it streams"
  :license "MIT"
  :author "MadeByBrain (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "guard")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-writing-guard/test"))))

(defsystem "nodecode-writing-guard/test"
  :description "writing-guard tests; RUN-WRITING-GUARD-TESTS filters the shared registry by the WRITING-GUARD prefix."
  :license "MIT"
  :author "MadeByBrain (original), Nodecode (rewrite)"
  :depends-on ("nodecode-writing-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "guard-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-writing-guard-tests)))
