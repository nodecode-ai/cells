;;;; nodecode-memos.asd --- mem-os memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `memos' section, registers the six memos_* tools, puts
;;;; (help :memos) on the manual, registers /memos and, when the section asks,
;;;; adds the newest facts to every request.
;;;;
;;;; A Nodecode rewrite of Markgatcha/memos's Hermes plugin (see NOTICE).

(defsystem "nodecode-memos"
  :description "MemOS memory: durable facts in one local file, six tools, found by their words"
  :license "MIT"
  :version "0.1.0"
  :author "Markgatcha (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memos/test"))))

(defsystem "nodecode-memos/test"
  :description "MemOS tests. Registered into the shared nodecode.test registry; RUN-MEMOS-TESTS filters by the MEMOS-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Markgatcha (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memos" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memos-tests)))
