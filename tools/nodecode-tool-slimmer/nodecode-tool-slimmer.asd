;;;; nodecode-tool-slimmer.asd --- the tool schemas one request needs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tool-slimmer' section, advises two functions, registers the
;;;; three tools cell.lisp lists and puts the topic on the manual as
;;;; (help :tool-slimmer).
;;;;
;;;; Ported from tool-slimmer (see NOTICE and README.md).

(defsystem "nodecode-tool-slimmer"
  :description "Trim each request's tool schemas to the tools its words ask for, and report the tokens saved"
  :author "aliasocracy (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tool-slimmer/test"))))

(defsystem "nodecode-tool-slimmer/test"
  :description "Tool slimmer cell tests. Registered into the shared nodecode.test registry; RUN-TOOL-SLIMMER-TESTS filters by the TOOL-SLIMMER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-tool-slimmer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tool-slimmer-tests)))
