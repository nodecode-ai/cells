;;;; nodecode-devin.asd --- Devin (Codeium's Cascade) as a provider lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `devin' section, registers the devin lane, installs the hooks
;;;; cell.lisp lists and registers /devin.
;;;;
;;;; Ported from oh-my-pi's devin provider (see NOTICE). Every dependency
;;;; rides with nodecode (chipz, which inflates a gzipped Connect frame, comes
;;;; in with dexador).

(defsystem "nodecode-devin"
  :description "Devin: a browser sign-in, and Codeium's Cascade wire (Connect over protobuf) as a lane of its own"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "proto")
               (:file "provider")
               (:file "signin")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-devin/test"))))

(defsystem "nodecode-devin/test"
  :description "Devin tests. Registered into the shared nodecode.test registry; RUN-DEVIN-TESTS filters by the DEVIN-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-devin" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-devin-tests)))
