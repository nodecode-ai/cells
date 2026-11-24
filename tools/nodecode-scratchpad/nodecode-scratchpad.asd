;;;; nodecode-scratchpad.asd --- Scratchpad: per-session draft buffers, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `scratchpad' section, registers the scratchpad tool and
;;;; /scratchpad, hooks the :ROUND point for the per-turn rule, and puts
;;;; (help :scratchpad) on the manual.
;;;;
;;;; A Nodecode rewrite of Zek-Takai/hermes-devkit (see NOTICE).

(defsystem "nodecode-scratchpad"
  :description "Scratchpad: per-session draft buffers the model edits in place, removed with the session"
  :license "MIT"
  :version "0.1.0"
  :author "Zek-Takai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-scratchpad/test"))))

(defsystem "nodecode-scratchpad/test"
  :description "Scratchpad tests. Registered into the shared nodecode.test registry; RUN-SCRATCHPAD-TESTS filters by the SCRATCHPAD-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Zek-Takai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-scratchpad" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-scratchpad-tests)))
