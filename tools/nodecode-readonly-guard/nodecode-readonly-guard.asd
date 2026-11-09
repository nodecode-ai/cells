;;;; nodecode-readonly-guard.asd --- per-session read-only, plan and writable modes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of mciocchi/hermes-readonly-guard: one :TOOL hook
;;;; that reads an EVAL call's form and refuses the ones that write, run or
;;;; send something while the session is in read-only or plan mode, plus one
;;;; /readonly command and one standing note. See README.md beside this file
;;;; and NOTICE for what came from where.

(defsystem "nodecode-readonly-guard"
  :description "Read-only and plan modes per session, with a writable lease"
  :license "MIT"
  :author "mciocchi (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-readonly-guard/test"))))

(defsystem "nodecode-readonly-guard/test"
  :description "Read-only guard tests. Registered into the shared nodecode.test registry; RUN-READONLY-GUARD-TESTS filters by the READONLY-GUARD- name prefix."
  :license "MIT"
  :author "mciocchi (original), Nodecode (rewrite)"
  :depends-on ("nodecode-readonly-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-readonly-guard-tests)))
