;;;; nodecode-gogogoogle.asd --- hand bulk work to the Antigravity CLI, then
;;;; have a fresh model check it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of josefacero's gogogoogle plugin for Hermes Agent: the
;;;; antigravity_delegate, antigravity_job and antigravity_stats tools over
;;;; the `agy` CLI, and /gogogoogle. See NOTICE.

(defsystem "nodecode-gogogoogle"
  :description "Antigravity delegation: bulk work to the agy CLI, verified by a fresh model"
  :license "MIT"
  :author "josefacero (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gogogoogle/test"))))

(defsystem "nodecode-gogogoogle/test"
  :description "gogogoogle tests: the lifecycle, the tools over a stubbed CLI and git, the refusals."
  :license "MIT"
  :author "josefacero (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gogogoogle" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gogogoogle-tests)))
