;;;; nodecode-permission-canvas.asd --- element-level permissions for desktop work.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in src/ names this system.
;;;; The folder loader loads it at boot when this directory sits under
;;;; ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://permission-canvas' is accepted. No key, no network, no
;;;; shell: the policy, the approvals and the audit log are files in one folder.
;;;;
;;;; A Nodecode rewrite of TANGBUDU/permission-canvas. See NOTICE.

(defsystem "nodecode-permission-canvas"
  :description "Element-level desktop permissions: allow, read only, ask once, or hide - with rules, one exact approval and an audit line per decision"
  :license "MIT"
  :author "TANGBUDU (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "policy")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-permission-canvas/test"))))

(defsystem "nodecode-permission-canvas/test"
  :description "permission-canvas tests. Registered into the shared nodecode.test registry; RUN-PERMISSION-CANVAS-TESTS filters by the PERMISSION-CANVAS- name prefix."
  :license "MIT"
  :depends-on ("nodecode-permission-canvas" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-permission-canvas-tests)))
