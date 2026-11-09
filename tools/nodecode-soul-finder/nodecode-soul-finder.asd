;;;; nodecode-soul-finder.asd --- draft or refine the agent's SOUL.md.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell: /soul, three tools and one first-run offer, ported
;;;; from the Hermes plugin soul-finder (see NOTICE).

(defsystem "nodecode-soul-finder"
  :description "Soul Finder: draft or refine the agent's SOUL.md persona file through a short guided interview"
  :license "MIT"
  :author "RegeneratusLabs (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-soul-finder/test"))))

(defsystem "nodecode-soul-finder/test"
  :description "Soul Finder's test slice: its lifecycle, the new-versus-refine decision, /soul, and the bundled workflow."
  :license "MIT"
  :author "RegeneratusLabs (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode-soul-finder" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-soul-finder-tests)))
