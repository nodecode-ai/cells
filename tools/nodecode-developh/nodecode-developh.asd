;;;; nodecode-developh.asd --- mobile website audits from Developh's MCP server, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `developh' section, registers the four tools cell.lisp lists and
;;;; puts the plugin's skill on the manual as (help :developh).
;;;;
;;;; A rewrite of Developh's Developh/skills (see NOTICE): the original
;;;; shipped the studio's hosted MCP server through a portable manifest and one
;;;; skill. The skill is the help topic; the server's four tools are four
;;;; JSON-RPC calls this cell makes itself, with no MCP transport in between.

(defsystem "nodecode-developh"
  :description "Developh website audit: PageSpeed Insights audits with a ranked fix list, plus the studio's profile and case studies"
  :author "Developh (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-developh/test"))))

(defsystem "nodecode-developh/test"
  :description "Developh cell tests. Registered into the shared nodecode.test registry; RUN-DEVELOPH-TESTS filters by the DEVELOPH-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-developh" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-developh-tests)))
