;;;; nodecode-api-indonesia.asd --- Indonesian public data through API Indonesia.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of khmuhtadin/api-indonesia-examples' Agent Plugins v1 package
;;;; (see NOTICE). A cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and the
;;;; gateway calls START-CELL after recovery, which reads the `api-indonesia'
;;;; section, refuses without a key, and puts the three tools, /api-indonesia
;;;; and the manual topic on.
;;;;
;;;; The original is manifests and example clients for one hosted server, so
;;;; nothing of it runs here: the cell speaks the server's own protocols (MCP
;;;; over Streamable HTTP, and the REST API the skill documents) with NLK:HTTP,
;;;; and needs no Node, no pip and no dependency beyond what nodecode carries.

(defsystem "nodecode-api-indonesia"
  :description "Indonesian public data through API Indonesia's hosted MCP server: regions, postal codes, holidays, prayer times, BMKG"
  :license "MIT"
  :author "khmuhtadin (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-api-indonesia/test"))))

(defsystem "nodecode-api-indonesia/test"
  :description "api-indonesia tests. Registered into the shared nodecode.test registry; RUN-API-INDONESIA-TESTS filters by the API-INDONESIA-CELL- name prefix."
  :license "MIT"
  :author "khmuhtadin (original), Nodecode (rewrite)"
  :depends-on ("nodecode-api-indonesia" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-api-indonesia-tests)))
