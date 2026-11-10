;;;; nodecode-devsentry.asd --- read-only project inventory, traceback leads, git diffs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `devsentry' section, registers the three tools, puts /devsentry
;;;; in the one slash catalog and the manual on (help :devsentry).
;;;;
;;;; Ported from Archangel20's devsentry (see NOTICE). The original was a
;;;; Hermes plugin of three Python tools; this cell reads the same things in
;;;; Lisp, with nothing bundled and no dependency beyond the image.

(defsystem "nodecode-devsentry"
  :description "Read-only project inventory, traceback leads and tracked git diffs"
  :author "Archangel20 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "files")
               (:file "trace")
               (:file "git")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-devsentry/test"))))

(defsystem "nodecode-devsentry/test"
  :description "Devsentry tests. Registered into the shared nodecode.test registry; RUN-DEVSENTRY-TESTS filters by the DEVSENTRY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-devsentry" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-devsentry-tests)))
