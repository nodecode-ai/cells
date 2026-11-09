;;;; nodecode-pethost.asd --- Pethost's MCP server, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `pethost' section, registers the pethost_machine, pethost_call
;;;; and pethost_tools tools and the /pethost command, and puts the playbook
;;;; on (help :pethost).
;;;;
;;;; Rewritten from pethost-dev's Hermes Agent plugin; see NOTICE. Every
;;;; dependency rides with nodecode: nlk:http speaks JSON-RPC to the server.

(defsystem "nodecode-pethost"
  :description "Pethost: deploy and look after Docker Compose projects on your own machine"
  :author "pethost-dev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pethost/test"))))

(defsystem "nodecode-pethost/test"
  :description "Pethost tests. Registered into the shared nodecode.test registry; RUN-PETHOST-TESTS filters by the PETHOST-CELL- name prefix."
  :author "pethost-dev (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-pethost" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pethost-tests)))
