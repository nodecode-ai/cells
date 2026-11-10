;;;; nodecode-coding-route.asd --- heavy/light model routing for coding work, as a tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `coding-route' section, registers the two tools cell.lisp lists
;;;; and puts the manual on (help :coding-route).
;;;;
;;;; A rewrite of metamindedu's hermes-model-router (see NOTICE): the original
;;;; was a Hermes settings page plus a routed-job runner -- two lanes, six
;;;; purposes, background agent processes and a job reader, wrapped around
;;;; Hermes' own profile config and plugin manager. This cell keeps the lanes,
;;;; the purposes and the job, and takes the session, the model pick and the
;;;; answer from the organism instead.

(defsystem "nodecode-coding-route"
  :description "Heavy/light model routing: send a coding task to a background agent on the model its purpose calls for"
  :author "metamindedu (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-coding-route/test"))))

(defsystem "nodecode-coding-route/test"
  :description "Coding route tests. Registered into the shared nodecode.test registry; RUN-CODING-ROUTE-TESTS filters by the CODING-ROUTE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-coding-route" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-coding-route-tests)))
