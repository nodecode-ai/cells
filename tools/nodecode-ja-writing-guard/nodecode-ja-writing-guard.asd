;;;; nodecode-ja-writing-guard.asd --- Japanese AI-style phrasing, scored with line and hint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://ja-writing-guard' is accepted. START-CELL reads the
;;;; `ja-writing-guard' section, advises NLE::EMIT-STREAM-PART (so one italic
;;;; line rides every Japanese answer), and registers the ja_writing_check
;;;; tool, /ja-writing-guard and (help :ja-writing-guard).

(defsystem "nodecode-ja-writing-guard"
  :description "Japanese AI-style phrasing: a 0-100 score, line, column and a fix hint"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "check")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ja-writing-guard/test"))))

(defsystem "nodecode-ja-writing-guard/test"
  :description "ja-writing-guard tests; RUN-JA-WRITING-GUARD-TESTS filters the shared registry by its prefix."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ja-writing-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "check-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ja-writing-guard-tests)))
