;;;; nodecode-podcast.asd --- a podcast episode from a script, as Nodecode tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: the folder loader loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/, and the
;;;; gateway calls START-CELL after recovery, which reads the `podcast'
;;;; section, registers podcast_produce, podcast_voices and podcast_episodes
;;;; and /podcast, and puts the workflow on (help :podcast).
;;;;
;;;; Dependencies are in the serving image: websocket-driver for the voice
;;;; service's socket, ironclad for its request token.

(defsystem "nodecode-podcast"
  :description "Turn a two-host script into a mastered MP3 episode, with the voices spoken line by line"
  :license "MIT"
  :author "suhas-svg (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "websocket-driver" "ironclad")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-podcast/test"))))

(defsystem "nodecode-podcast/test"
  :description "Podcast cell tests; RUN-PODCAST-TESTS filters by the PODCAST-CELL- name prefix."
  :license "MIT"
  :author "suhas-svg (original), Nodecode (rewrite)"
  :depends-on ("nodecode-podcast" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-podcast-tests)))
