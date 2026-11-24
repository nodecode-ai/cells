;;;; nodecode-gurusup-brain.asd --- a company-memory MCP server as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gurusup-brain' section and registers the three
;;;; gurusup_brain_* tools, the /gurusup-brain command and the
;;;; (help :gurusup-brain) topic.
;;;;
;;;; A Nodecode rewrite of gurusup/gurusup-brain-plugin (see NOTICE).

(defsystem "nodecode-gurusup-brain"
  :description "GuruSup Brain: your company's knowledge from Slack, HubSpot and Notion, over its MCP server"
  :license "MIT"
  :version "0.1.0"
  :author "gurusup (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gurusup-brain/test"))))

(defsystem "nodecode-gurusup-brain/test"
  :description "GuruSup Brain tests. Registered into the shared nodecode.test registry; RUN-GURUSUP-BRAIN-TESTS filters by the GURUSUP-BRAIN-CELL- name prefix."
  :license "MIT"
  :author "gurusup (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gurusup-brain" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gurusup-brain-tests)))
