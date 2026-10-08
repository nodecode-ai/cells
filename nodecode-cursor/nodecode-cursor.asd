;;;; nodecode-cursor.asd --- Cursor (Claude, GPT, etc.) as a provider lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cursor' section, registers the cursor lane, installs the hooks
;;;; cell.lisp lists and registers /cursor.
;;;;
;;;; Ported from oh-my-pi's cursor provider (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-cursor"
  :description "Cursor: a browser sign-in, and Cursor's Agent protocol (Connect over HTTP, protobuf) as a lane of its own"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "proto")
               (:file "provider")
               (:file "signin")
               (:file "agent")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cursor/test"))))

(defsystem "nodecode-cursor/test"
  :description "Cursor tests. Registered into the shared nodecode.test registry; RUN-CURSOR-TESTS filters by the CURSOR-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cursor" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cursor-tests)))
