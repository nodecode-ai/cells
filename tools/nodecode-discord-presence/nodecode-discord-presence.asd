;;;; nodecode-discord-presence.asd --- the agent's work on a Discord profile.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `discord-presence' section, registers the three tools and
;;;; /presence, and starts the one thread that keeps the card current on the
;;;; Discord desktop client of this machine.
;;;;
;;;; Rewritten from DarRahman's Hermes Agent plugin of the same purpose; see
;;;; NOTICE. Every dependency rides with nodecode: sb-bsd-sockets is the SBCL
;;;; contribution for the client's local socket, and nlk's JSON is the wire.

(defsystem "nodecode-discord-presence"
  :description "Discord Rich Presence for the agent's work: the session, the tool it runs, the model and the turn's tokens"
  :author "DarRahman (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ipc")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-discord-presence/test"))))

(defsystem "nodecode-discord-presence/test"
  :description "Discord presence tests. Registered into the shared nodecode.test registry; RUN-DISCORD-PRESENCE-TESTS filters by the DISCORD-PRESENCE-CELL- name prefix."
  :author "DarRahman (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-discord-presence" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-discord-presence-tests)))
