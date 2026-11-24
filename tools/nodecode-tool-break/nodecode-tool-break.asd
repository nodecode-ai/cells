;;;; nodecode-tool-break.asd --- break a stalled tool call without cancelling
;;;; the turn: /break, /again and /break-status over one :TOOL hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in the
;;;; release names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which hooks the :TOOL point and registers the three
;;;; commands.

(defsystem "nodecode-tool-break"
  :description "Break a stalled tool call with /break, steer it with /break {msg}, or reissue it once with /again"
  :license "MIT"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tool-break/test"))))

(defsystem "nodecode-tool-break/test"
  :description "Tool break tests. Registered into the shared nodecode.test registry; RUN-TOOL-BREAK-TESTS filters by the TOOL-BREAK-CELL- name prefix."
  :license "MIT"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tool-break" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tool-break-tests)))
