;;;; nodecode-kumiho-memory.asd --- Kumiho Cloud memory as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `kumiho' section, registers kumiho_recall, kumiho_latest,
;;;; kumiho_remember, kumiho_forget, kumiho_spaces, kumiho_one_memory,
;;;; kumiho_summary and /kumiho, and puts (help :kumiho) on
;;;; the manual.
;;;;
;;;; A Nodecode rewrite of the Hermes plugin in KumihoIO/kumiho-plugins (see
;;;; NOTICE).

(defsystem "nodecode-kumiho-memory"
  :description "Kumiho Cloud memory: recall, save and retire decisions, preferences and facts across sessions"
  :license "MIT"
  :version "0.1.0"
  :author "KumihoIO (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kumiho-memory/test"))))

(defsystem "nodecode-kumiho-memory/test"
  :description "Kumiho memory tests. Registered into the shared nodecode.test registry; RUN-KUMIHO-MEMORY-TESTS filters by the KUMIHO-MEMORY-CELL- name prefix."
  :license "MIT"
  :author "KumihoIO (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kumiho-memory" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kumiho-memory-tests)))
