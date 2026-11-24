;;;; nodecode-whoop.asd --- WHOOP recovery, sleep and strain as five tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `whoop' section, registers the five tools and puts the primer
;;;; on (help :whoop).
;;;;
;;;; Rewritten from davidmosiah's whoop-mcp (an MCP server); see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to WHOOP's API and
;;;; its OAuth token endpoint.

(defsystem "nodecode-whoop"
  :description "WHOOP: recovery, sleep, strain, workouts and profile as five tools"
  :author "davidmosiah (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-whoop/test"))))

(defsystem "nodecode-whoop/test"
  :description "WHOOP tests. Registered into the shared nodecode.test registry; RUN-WHOOP-TESTS filters by the WHOOP-CELL- name prefix."
  :author "davidmosiah (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-whoop" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-whoop-tests)))
