;;;; nodecode-composio.asd --- Composio's connected accounts as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `composio' section, registers composio_accounts,
;;;; composio_services, composio_connect, composio_disconnect and /composio,
;;;; and puts (help :composio) on the manual.
;;;;
;;;; A Nodecode rewrite of luisced/hermes-composio-connections (see NOTICE).

(defsystem "nodecode-composio"
  :description "Composio connected accounts: list, connect and disconnect them, with the toolkit catalog"
  :license "MIT"
  :version "0.1.0"
  :author "luisced (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-composio/test"))))

(defsystem "nodecode-composio/test"
  :description "Composio cell tests. Registered into the shared nodecode.test registry; RUN-COMPOSIO-TESTS filters by the COMPOSIO-CELL- name prefix."
  :license "MIT"
  :author "luisced (original), Nodecode (rewrite)"
  :depends-on ("nodecode-composio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-composio-tests)))
