;;;; nodecode-adwave.asd --- Adwave's MCP server as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `adwave' section, registers the two tools and the /adwave
;;;; command, and puts the skill on the manual as (help :adwave).
;;;;
;;;; Rewritten from Adwave's Agent Plugins package; see NOTICE. The original
;;;; was an MCP server declaration and one skill; this cell speaks the same
;;;; server's streamable HTTP itself, through nlk:http.

(defsystem "nodecode-adwave"
  :description "Adwave: make ads from a website and run them on TV, Meta, Google, YouTube and Reddit from one prepaid daily budget"
  :author "AdwaveInc (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-adwave/test"))))

(defsystem "nodecode-adwave/test"
  :description "Adwave cell tests. Registered into the shared nodecode.test registry; RUN-ADWAVE-TESTS filters by the ADWAVE-CELL- name prefix."
  :author "AdwaveInc (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-adwave" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-adwave-tests)))
