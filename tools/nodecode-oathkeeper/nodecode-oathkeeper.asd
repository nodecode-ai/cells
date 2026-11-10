;;;; nodecode-oathkeeper.asd --- an owner's oath, enforced at the tool boundary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `oathkeeper' section, hooks the :TOOL point, and registers the
;;;; oath_check and oath_status tools, /oathkeeper and (help :oathkeeper).
;;;;
;;;; A rewrite of Whydiriki700/oathkeeper (see NOTICE): the original hooked a
;;;; Hermes agent's pre_tool_call, post_tool_call and pre_llm_call. This cell is
;;;; the one :TOOL around Nodecode offers, plus a standing notice for the brief.

(defsystem "nodecode-oathkeeper"
  :description "An owner's oath of never/require rules, enforced deterministically on every tool call, with fingerprint snapshots of the sworn surfaces"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "oath")
               (:file "actions")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-oathkeeper/test"))))

(defsystem "nodecode-oathkeeper/test"
  :description "Oathkeeper cell tests. Registered into the shared nodecode.test registry; RUN-OATHKEEPER-TESTS filters by the OATHKEEPER- name prefix."
  :license "MIT"
  :depends-on ("nodecode-oathkeeper" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-oathkeeper-tests)))
