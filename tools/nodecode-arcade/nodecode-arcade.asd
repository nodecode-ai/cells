;;;; nodecode-arcade.asd --- the arcade cabinet as a cell: games and scoreboards.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when `nodecode
;;;; add nc://arcade' is accepted. It reads the `arcade' config section and
;;;; contributes four tools (arcade_games, arcade_play, arcade_top_scores,
;;;; arcade_player_stats), one command (/arcade) and one manual topic (help
;;;; :arcade); nothing starts a thread, dials a host or reads a credential.

(defsystem "nodecode-arcade"
  :description "Snake, Breakout and Pong in the conversation, with leaderboards the model can read"
  :license "MIT"
  :author "leoresendedop-sys (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "games")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-arcade/test"))))

(defsystem "nodecode-arcade/test"
  :description "Tests for the arcade cell; RUN-ARCADE-TESTS filters the shared registry by the ARCADE- name prefix."
  :license "MIT"
  :depends-on ("nodecode-arcade" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "games-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-arcade-tests)))
