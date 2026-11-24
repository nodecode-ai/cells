;;;; nodecode-bubblewrap.asd --- commands run inside their own bwrap sandbox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in src/ names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `bubblewrap' section, registers the two tools and the
;;;; /bubblewrap command, and puts the sandbox's boundary on the manual as
;;;; (help :bubblewrap).
;;;;
;;;; A rewrite of NousResearch's hermes-official-plugins bubblewrap plugin;
;;;; see NOTICE. The original was a terminal backend: every terminal and
;;;; execute_code command of the harness ran inside bwrap. This cell gives the
;;;; model the same box as a tool it calls, building the same argv.

(defsystem "nodecode-bubblewrap"
  :description "Bubblewrap: run a command in its own bwrap sandbox, read-only host, secrets hidden"
  :author "NousResearch (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bubblewrap/test"))))

(defsystem "nodecode-bubblewrap/test"
  :description "Bubblewrap cell tests. Registered into the shared nodecode.test registry; RUN-BUBBLEWRAP-TESTS filters by the BUBBLEWRAP-CELL- name prefix."
  :author "NousResearch (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-bubblewrap" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bubblewrap-tests)))
