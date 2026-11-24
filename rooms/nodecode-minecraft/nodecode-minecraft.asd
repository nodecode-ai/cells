;;;; nodecode-minecraft.asd --- the agent in a Minecraft server's chat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of kyssta-exe/hermes-minecraft-gateway (see NOTICE). An
;;;; optional ASDF cell: the folder loader loads it at boot when the
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL after recovery, which reads the `channels.minecraft'
;;;; section, joins the server and registers /minecraft.
;;;;
;;;; The original reached the game through a Node.js Mineflayer bridge it
;;;; spawned and polled over HTTP. This cell speaks the game's own protocol
;;;; (wire.lisp, protocol.lisp): the dependencies below are the ones the
;;;; framing needs, and no process is started.

(defsystem "nodecode-minecraft"
  :description "Minecraft: talk to the agent in a server's in-game chat"
  :license "MIT"
  :version "0.1.0"
  :author "kyssta-exe (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "usocket" "chipz" "salza2" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "protocol")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-minecraft/test"))))

(defsystem "nodecode-minecraft/test"
  :description "Minecraft cell tests. Registered into the shared nodecode.test registry; RUN-MINECRAFT-TESTS filters by the MINECRAFT-CELL- name prefix."
  :license "MIT"
  :author "kyssta-exe (original), Nodecode (rewrite)"
  :depends-on ("nodecode-minecraft" "nodecode/test" "flexi-streams")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-minecraft-tests)))
