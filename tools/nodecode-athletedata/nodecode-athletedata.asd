;;;; nodecode-athletedata.asd --- the athlete's own training and health data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of athletedata-health/mcp's Agent Plugins v1 package (see
;;;; NOTICE). A cell, not part of the organism core: the folder loader loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/, and the
;;;; gateway calls START-CELL after recovery, which reads the `athletedata'
;;;; section, refuses without a key, and puts the two tools, /athletedata and
;;;; the manual topic on.
;;;;
;;;; Nothing of the original runs here: it is manifests only, pointing at a
;;;; hosted server. The cell speaks that server's own protocol (MCP over
;;;; Streamable HTTP) with NLK:HTTP, so no Node, no bridge and no dependency
;;;; beyond what nodecode already carries.

(defsystem "nodecode-athletedata"
  :description "The athlete's own training and health data from Garmin, Oura, WHOOP, Hevy and 35 more apps, through the athletedata MCP server"
  :license "MIT"
  :author "athletedata-health (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-athletedata/test"))))

(defsystem "nodecode-athletedata/test"
  :description "athletedata tests. Registered into the shared nodecode.test registry; RUN-ATHLETEDATA-TESTS filters by the ATHLETEDATA-CELL- name prefix."
  :license "MIT"
  :author "athletedata-health (original), Nodecode (rewrite)"
  :depends-on ("nodecode-athletedata" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-athletedata-tests)))
