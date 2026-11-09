;;;; nodecode-adaptive-effort.asd --- a scorer model picks this session's reasoning effort.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `adaptive-effort' section, puts (help :adaptive-effort) on the
;;;; manual, registers /adaptive and hooks the :round point, so a session set
;;;; to "once" or "always" has its effort chosen for it.
;;;;
;;;; Ported from the Hermes plugin hermes-adaptive-effort (see NOTICE).

(defsystem "nodecode-adaptive-effort"
  :description "Adaptive Effort: a scorer model judges the task and picks this session's reasoning effort"
  :license "MIT"
  :author "atostivint (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "score")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-adaptive-effort/test"))))

(defsystem "nodecode-adaptive-effort/test"
  :description "Adaptive Effort tests. Registered into the shared nodecode.test registry; RUN-ADAPTIVE-EFFORT-TESTS filters by the ADAPTIVE-EFFORT-CELL- name prefix."
  :license "MIT"
  :author "atostivint (original), Nodecode (rewrite)"
  :depends-on ("nodecode-adaptive-effort" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-adaptive-effort-tests)))
