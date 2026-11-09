;;;; nodecode-rex.asd --- the Rex terminal multiplexer, in a Nodecode session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the Nodecode core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `rex' section, registers four tools, /rex and
;;;; (help :rex).
;;;;
;;;; A rewrite of kshitijk4poor's hermes-rex-plugin (see NOTICE). Every
;;;; dependency rides with Nodecode; the one outside thing it runs is the rex
;;;; CLI the user already has.

(defsystem "nodecode-rex"
  :description "Rex panes for a session: long commands in a visible pane, read back, with the window named and marked"
  :license "MIT"
  :version "0.1.0"
  :author "kshitijk4poor (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-rex/test"))))

(defsystem "nodecode-rex/test"
  :description "Rex cell tests. Registered into the shared nodecode.test registry; RUN-REX-TESTS filters by the REX-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "kshitijk4poor (original), Nodecode (rewrite)"
  :depends-on ("nodecode-rex" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-rex-tests)))
