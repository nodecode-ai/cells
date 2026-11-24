;;;; nodecode-recued.asd --- the user's own Recued server, as mcp tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in the
;;;; release names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; with the parsed config, which reads the `recued' section, writes
;;;; mcp.servers.<server_name> when it is not there, puts (help :recued) on
;;;; the manual and /recued in the catalog. Nodecode's own mcp cell does the
;;;; talking: this folder is the entry plus the travel directions.

(defsystem "nodecode-recued"
  :description "This computer's own Recued server as mcp tools: notes, tasks and the things the user owns, every change waiting on their approval"
  :license "MIT"
  :author "recued (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-recued/test"))))

(defsystem "nodecode-recued/test"
  :description "Recued tests: the config entry it writes, /recued's answers, and the refusals. Registered into the shared nodecode.test registry; RUN-RECUED-TESTS filters by the RECUED-CELL- name prefix."
  :license "MIT"
  :author "recued (original), Nodecode (rewrite)"
  :depends-on ("nodecode-recued" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-recued-tests)))
