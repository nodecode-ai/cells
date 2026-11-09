;;;; nodecode-t3.asd --- T3 Code's threads, as tools a Nodecode model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `t3' section and registers six tools and /t3 over T3 Code's
;;;; local orchestration API.
;;;;
;;;; A Nodecode rewrite of thetasigmaio/hermes-t3-control (see NOTICE).

(defsystem "nodecode-t3"
  :description "T3 Code threads as tools: list, read, create, send, interrupt, stop, over T3's loopback orchestration API"
  :license "MIT"
  :version "0.1.0"
  :author "thetasigmaio (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-t3/test"))))

(defsystem "nodecode-t3/test"
  :description "T3 cell tests. Registered into the shared nodecode.test registry; RUN-T3-TESTS filters by the T3-CELL- name prefix."
  :license "MIT"
  :author "thetasigmaio (original), Nodecode (rewrite)"
  :depends-on ("nodecode-t3" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-t3-tests)))
