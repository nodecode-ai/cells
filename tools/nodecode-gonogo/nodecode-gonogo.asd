;;;; nodecode-gonogo.asd --- an eval score turned into a ship / don't-ship call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gonogo' section, puts (help :gonogo) on the manual and
;;;; registers /gonogo.
;;;;
;;;; A Nodecode rewrite of keppy's hermes-plugin-gonogo (see NOTICE). The
;;;; statistics and the verdict rules are gonogo's; this folder carries them
;;;; in Common Lisp, so no Python and no dependency are needed.

(defsystem "nodecode-gonogo"
  :description "Turn a pilot-scale eval score into a ship / don't-ship verdict"
  :license "MIT"
  :author "keppy (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "stats")
               (:file "rows")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gonogo/test"))))

(defsystem "nodecode-gonogo/test"
  :description "Go/No-Go tests. Registered into the shared nodecode.test registry; RUN-GONOGO-TESTS filters by the GONOGO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-gonogo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gonogo-tests)))
