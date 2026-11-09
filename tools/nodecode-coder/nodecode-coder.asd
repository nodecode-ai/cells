;;;; nodecode-coder.asd --- Coder workspaces as three tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `coder' section, registers the three tools and puts the primer
;;;; on (help :coder).
;;;;
;;;; Rewritten from perqin's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http for Coder's REST API, the
;;;; websocket-driver nodecode already loads for the workspace PTY, ppcre for
;;;; the exit marker and quri for the query string.

(defsystem "nodecode-coder"
  :description "Coder: run a command in your own remote workspace, and list or read those workspaces"
  :author "perqin (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-coder/test"))))

(defsystem "nodecode-coder/test"
  :description "Coder tests. Registered into the shared nodecode.test registry; RUN-CODER-TESTS filters by the CODER-CELL- name prefix."
  :author "perqin (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-coder" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-coder-tests)))
