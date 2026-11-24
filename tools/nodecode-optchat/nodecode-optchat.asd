;;;; nodecode-optchat.asd --- endless local memory: a verbatim log and a summary tree.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. A rewrite of aska-digital/optchat-memory's Hermes plugin
;;;; (see NOTICE). The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `optchat' config section, observes the :FRAME point for
;;;; the messages of every turn, advises the harness-section read so the view
;;;; of the log rides every request, starts one summary worker and registers
;;;; the optchat_zoom and optchat_date tools and /optchat.

(defsystem "nodecode-optchat"
  :description "Endless local memory: an append-only log of every turn and a binary summary tree to recall it"
  :license "MIT"
  :author "aska-digital (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "tree")
               (:file "view")
               (:file "compact")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-optchat/test"))))

(defsystem "nodecode-optchat/test"
  :description "OptChat tests. Registered into the shared nodecode.test registry; RUN-OPTCHAT-TESTS filters by the OPTCHAT-CELL- name prefix."
  :license "MIT"
  :author "aska-digital (original), Nodecode (rewrite)"
  :depends-on ("nodecode-optchat" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-optchat-tests)))
