;;;; nodecode-protonpass.asd --- Proton Pass secrets, through the pass-cli you have.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `protonpass' section, registers the four tools cell.lisp
;;;; declares, puts the workflow on the manual as (help :protonpass) and
;;;; registers /protonpass.
;;;;
;;;; Ported from lucas-nicolas's hermes-protonpass (see NOTICE). The original
;;;; was a Hermes secret source: it resolved pass:// references into Hermes'
;;;; environment at startup. Nodecode has no such seam, so the same reads are
;;;; tools, and the CLI itself is the operator's, like nodecode-snyk's.

(defsystem "nodecode-protonpass"
  :description "Proton Pass secrets for a session: read pass:// references through your own pass-cli"
  :author "lucas-nicolas (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cli")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-protonpass/test"))))

(defsystem "nodecode-protonpass/test"
  :description "Proton Pass cell tests. Registered into the shared nodecode.test registry; RUN-PROTONPASS-TESTS filters by the PROTONPASS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-protonpass" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-protonpass-tests)))
