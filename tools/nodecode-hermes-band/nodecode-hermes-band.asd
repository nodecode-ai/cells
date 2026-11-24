;;;; nodecode-hermes-band.asd --- compose and render music on this machine, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of RegeneratusLabs/hermes-band for Hermes Agent -- see NOTICE.

(defsystem "nodecode-hermes-band"
  :description "Compose and render original music locally: a score in, an exact-length wav and mp3 out"
  :license "MIT"
  :author "RegeneratusLabs (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "theory")
               (:file "score")
               (:file "compose")
               (:file "midi")
               (:file "render")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hermes-band/test"))))

(defsystem "nodecode-hermes-band/test"
  :description "Tests for the band cell; RUN-BAND-TESTS filters the shared registry by the BAND- prefix."
  :license "MIT"
  :author "RegeneratusLabs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hermes-band" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hermes-band-tests)))
