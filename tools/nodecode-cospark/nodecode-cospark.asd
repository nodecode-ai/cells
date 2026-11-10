;;;; nodecode-cospark.asd --- Cospark's MCP server as tools the model calls, with its skill as
;;;; the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cospark' section, registers the three tools and the /cospark
;;;; command, and puts the skill on the manual as (help :cospark).
;;;;
;;;; Rewritten from Cospark's Agent Plugins package; see NOTICE. The original
;;;; was two manifests, one skill and an MCP server declaration; this cell
;;;; speaks that server's streamable HTTP itself, through nlk:http.

(defsystem "nodecode-cospark"
  :description "Cospark: research ad references and make images, video ads and voiceovers through its hosted MCP server"
  :author "corift (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cospark/test"))))

(defsystem "nodecode-cospark/test"
  :description "Cospark cell tests. Registered into the shared nodecode.test registry; RUN-COSPARK-TESTS filters by the COSPARK-CELL- name prefix."
  :author "corift (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-cospark" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cospark-tests)))
