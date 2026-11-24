;;;; nodecode-linear-digest.asd --- the Linear issues assigned to you, in one morning list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `linear-digest' section and registers the linear_digest tool,
;;;; the /linear-digest command and the manual topic (help :linear-digest).
;;;;
;;;; Rewritten from the Linear Digest Hermes plugin; see NOTICE. The original
;;;; was a tool, a skill and a scheduled no-agent script over one GraphQL
;;;; query; this cell is the same query and the same list, read by the model
;;;; or printed by the operator.

(defsystem "nodecode-linear-digest"
  :description "Linear: the open issues assigned to you, grouped by overdue, due today and gone quiet, read with no model call"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-linear-digest/test"))))

(defsystem "nodecode-linear-digest/test"
  :description "Linear Digest tests. Registered into the shared nodecode.test registry; RUN-LINEAR-DIGEST-TESTS filters by the LINEAR-DIGEST-CELL- name prefix."
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-linear-digest" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-linear-digest-tests)))
