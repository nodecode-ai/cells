;;;; nodecode-kanban-stream.asd --- a session's progress, streamed to Telegram.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and
;;;; the gateway calls START-CELL after recovery. See README.md beside this
;;;; file, and src/CELLS.md for the contract.

(defsystem "nodecode-kanban-stream"
  :description "One Telegram message per session, edited in place as its turn works: tools, elapsed, tokens a second"
  :license "MIT"
  :author "mbgulden (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kanban-stream/test"))))

(defsystem "nodecode-kanban-stream/test"
  :description "Kanban-stream tests. Registered into the shared nodecode.test registry; RUN-KANBAN-STREAM-TESTS filters by the KANBAN-STREAM-CELL- name prefix."
  :license "MIT"
  :author "mbgulden (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kanban-stream" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kanban-stream-tests)))
