;;;; nodecode-screenrig.asd --- screens, playlists and media on a screenRIG account.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `screenrig' section, puts (help :screenrig) on the manual,
;;;; registers /screenrig and the five tools cell.lisp lists.
;;;;
;;;; Rewritten from screenRIG's Agent Plugins package; see NOTICE. The
;;;; original was a skill, two MCP declarations and a bundled Node CLI; this
;;;; cell speaks the same control API itself, through nlk:http, so no Node is
;;;; needed.

(defsystem "nodecode-screenrig"
  :description "screenRIG: your screens, playlists and media, and what each screen plays"
  :author "screenrig (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-screenrig/test"))))

(defsystem "nodecode-screenrig/test"
  :description "screenRIG cell tests. Registered into the shared nodecode.test registry; RUN-SCREENRIG-TESTS filters by the SCREENRIG-CELL- name prefix."
  :author "screenrig (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-screenrig" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-screenrig-tests)))
