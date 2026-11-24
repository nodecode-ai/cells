;;;; nodecode-showreel.asd --- a session's own history, cut into a demo video.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Duffey234's hermes-showreel (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL, which reads
;;;; the `showreel' section, registers the showreel_render tool and /showreel,
;;;; and puts the manual on (help :showreel). Everything it reads is already
;;;; in the event log and on the user's own ffmpeg; nothing is downloaded.

(defsystem "nodecode-showreel"
  :description "A session's own history, cut into a captioned demo video"
  :license "MIT"
  :author "Duffey234 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-showreel/test"))))

(defsystem "nodecode-showreel/test"
  :description "Showreel tests. Registered into the shared nodecode.test registry; RUN-SHOWREEL-TESTS filters by the SHOWREEL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-showreel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-showreel-tests)))
