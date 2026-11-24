;;;; nodecode-delx-recovery.asd --- Delx Protocol recovery as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `delx' section, registers the five delx_* tools, /delx and
;;;; (help :delx), and refuses an endpoint that is not an https:// address.
;;;;
;;;; A Nodecode rewrite of davidmosiah/delx-plugins' delx-recovery plugin
;;;; (see NOTICE): the same MCP endpoint, the same tools and capsule.

(defsystem "nodecode-delx-recovery"
  :description "Delx Protocol recovery: resume a session, seal a continuity capsule, close with feedback"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "davidmosiah (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-delx-recovery/test"))))

(defsystem "nodecode-delx-recovery/test"
  :description "Delx recovery tests. Registered into the shared nodecode.test registry; RUN-DELX-RECOVERY-TESTS filters by the DELX- name prefix."
  :license "Apache-2.0"
  :author "davidmosiah (original), Nodecode (rewrite)"
  :depends-on ("nodecode-delx-recovery" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-delx-recovery-tests)))
