;;;; nodecode-tenuo.asd --- a warrant checked before every tool call runs.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A peripheral cell: one NLE:DEFINE-CELL in cell.lisp declares the
;;;; `tenuo' section, the :TOOL hook that enforces the warrant, two tools and
;;;; one command. The .asd sits inside its folder; presence under
;;;; ~/.nodecode/cells/ is enabled.

(defsystem "nodecode-tenuo"
  :description "A warrant checked before every tool call, with a local audit log of each decision"
  :author "tenuo-ai (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tenuo/test"))))

(defsystem "nodecode-tenuo/test"
  :description "tenuo tests. Registered into the shared nodecode.test registry; RUN-TENUO-TESTS filters by the TENUO- name prefix."
  :author "tenuo-ai (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-tenuo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tenuo-tests)))
