;;;; nodecode-xmemo.asd --- XMemo cloud memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `xmemo' section, registers the six xmemo_* tools and /xmemo,
;;;; and hooks the :frame point for the opt-in timeline.
;;;;
;;;; A Nodecode rewrite of yonro/hermes-xmemo-plugin (see NOTICE).

(defsystem "nodecode-xmemo"
  :description "XMemo cloud memory: semantic recall, durable facts and working state in a user's own account"
  :license "MIT"
  :version "0.1.0"
  :author "yonro (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-xmemo/test"))))

(defsystem "nodecode-xmemo/test"
  :description "XMemo tests. Registered into the shared nodecode.test registry; RUN-XMEMO-TESTS filters by the XMEMO-CELL- name prefix."
  :license "MIT"
  :author "yonro (original), Nodecode (rewrite)"
  :depends-on ("nodecode-xmemo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-xmemo-tests)))
