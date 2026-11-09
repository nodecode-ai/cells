;;;; nodecode-rtk-backends.asd --- shell commands rewritten through the rtk binary.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of petertaras/hermes-rtk-rewrite-with-docker-support
;;;; (Apache-2.0), whose Hermes plugin bridges every terminal tool call to
;;;; `rtk rewrite' before the command runs, and adds the backend gate. That
;;;; plugin is itself a fork of kerrz2020/hermes-rtk-rewrite, which is derived
;;;; from the RTK project's own Hermes hook; both are credited in NOTICE. Here
;;;; the same bridge is the core's :TOOL point: the command of a shell call,
;;;; or of an eval snippet that is one, is rewritten on its way to the shell,
;;;; when the command's backend is one the config opts in to.

(defsystem "nodecode-rtk-backends"
  :description "Rewrite shell commands through the rtk binary before they run, for the backends you opt in to"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "petertaras (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-rtk-backends/test"))))

(defsystem "nodecode-rtk-backends/test"
  :description "RTK-backends cell tests. Registered into the shared nodecode.test registry; RUN-RTK-BACKENDS-TESTS filters by the RTK-BACKENDS-CELL name prefix so the core suite is never re-run."
  :license "Apache-2.0"
  :author "petertaras (original), Nodecode (rewrite)"
  :depends-on ("nodecode-rtk-backends" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-rtk-backends-tests)))
