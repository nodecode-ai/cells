;;;; nodecode-unbrowse.asd --- the hosted Unbrowse tools as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `unbrowse' section, puts (help :unbrowse) on the manual and
;;;; registers the /unbrowse command cell.lisp lists.
;;;;
;;;; Ported from unbrowse-ai/unbrowse's Hermes plugin, plugins/hermes (see
;;;; NOTICE). Every dependency rides with nodecode: nlk:http is the one HTTP
;;;; seam, nlk:decode-json the one reader.

(defsystem "nodecode-unbrowse"
  :description "Unbrowse: read pages, run website tasks and drive a cloud browser through the hosted Unbrowse MCP"
  :license "MIT"
  :author "unbrowse-ai (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-unbrowse/test"))))

(defsystem "nodecode-unbrowse/test"
  :description "Unbrowse tests. Registered into the shared nodecode.test registry; RUN-UNBROWSE-TESTS filters by the UNBROWSE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-unbrowse" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-unbrowse-tests)))
