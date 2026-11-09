;;;; nodecode-nvidia-app.asd --- NVIDIA App, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `nvidia-app' section, registers the twelve
;;;; model-facing tools cell.lisp declares, puts (help :nvidia-app) on the
;;;; manual with the rules the original plugin shipped as a skill, and
;;;; registers /nvidia. Every byte it sends goes to NVIDIA App's own MCP
;;;; server on loopback, at the address and token the app publishes.
;;;;
;;;; A rewrite of the nvidia-app plugin of NousResearch/hermes-nvidia (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-nvidia-app"
  :description "NVIDIA App: driver status, game optimisation and In-Game Overlay recording through the app's own local MCP server"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nvidia-app/test"))))

(defsystem "nodecode-nvidia-app/test"
  :description "NVIDIA App tests. Registered into the shared nodecode.test registry; RUN-NVIDIA-APP-TESTS filters by the NVIDIA-APP-CELL- name prefix."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-nvidia-app" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nvidia-app-tests)))
