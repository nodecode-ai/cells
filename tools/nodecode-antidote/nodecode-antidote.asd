;;;; nodecode-antidote.asd --- runtime guardrails for tool calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of GroveMinting/llmAntidote's runtime half: one
;;;; :TOOL hook that judges every call before it runs, one /antidote
;;;; command, one manual topic. NOTHING in the organism names this system;
;;;; the folder loader loads it when the directory sits under
;;;; ~/.nodecode/cells/ and the gateway calls START-CELL after recovery.
;;;;
;;;; The .asd sits INSIDE its folder: a cell is a directory carrying its
;;;; .asd, and it is installed by putting — or symlinking — that directory
;;;; under ~/.nodecode/cells/. Presence is enabled.

(defsystem "nodecode-antidote"
  :description "Guardrails on what a tool call reads and sends"
  :license "MIT"
  :version "0.1.0"
  :author "GroveMinting (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-antidote/test"))))

(defsystem "nodecode-antidote/test"
  :description "Antidote tests, registered into the shared nodecode.test registry; RUN-ANTIDOTE-TESTS filters by the ANTIDOTE- name prefix."
  :license "MIT"
  :depends-on ("nodecode-antidote" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-antidote-tests)))
