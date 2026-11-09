;;;; nodecode-mermail.asd --- Mermail's mailboxes and mail as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `mermail' section and registers the tools
;;;; cell.lisp declares. The server is Mermail's own, hosted: this cell never
;;;; installs or starts anything, and every byte it sends goes to the address
;;;; the section names.
;;;;
;;;; A rewrite of Nudgen-Marketing/mermail-skills (see NOTICE).

(defsystem "nodecode-mermail"
  :description "Mermail: agent mailboxes and email over the hosted MCP server, as tools"
  :license "MIT"
  :author "Nudgen-Marketing (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mermail/test"))))

(defsystem "nodecode-mermail/test"
  :description "Mermail tests. Registered into the shared nodecode.test registry; RUN-MERMAIL-TESTS filters by the MERMAIL-CELL- name prefix."
  :license "MIT"
  :author "Nudgen-Marketing (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mermail" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mermail-tests)))
