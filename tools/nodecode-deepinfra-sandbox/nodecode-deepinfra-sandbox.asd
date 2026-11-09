;;;; nodecode-deepinfra-sandbox.asd --- a DeepInfra cloud sandbox as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `deepinfra_sandbox' section and registers the four tools and
;;;; /deepinfra command cell.lisp lists.
;;;;
;;;; Rewritten from DeepInfra's terminal.backend plugin for hermes-agent (see
;;;; NOTICE): the same service, on the same account key, spoken to with
;;;; nlk:http instead of the Python SDK.

(defsystem "nodecode-deepinfra-sandbox"
  :description "DeepInfra sandboxes: run shell commands and keep files in an isolated cloud microVM"
  :license "MIT"
  :author "deepinfra (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deepinfra-sandbox/test"))))

(defsystem "nodecode-deepinfra-sandbox/test"
  :description "DeepInfra sandbox tests. Registered into the shared nodecode.test registry; RUN-DEEPINFRA-SANDBOX-TESTS filters by the DEEPINFRA-CELL- name prefix."
  :license "MIT"
  :author "deepinfra (original), Nodecode (rewrite)"
  :depends-on ("nodecode-deepinfra-sandbox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deepinfra-sandbox-tests)))
