;;;; nodecode-nvidia-broadcast.asd --- NVIDIA Broadcast's effects and devices as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `nvidia-broadcast' section, registers the ten tools and puts
;;;; the primer on (help :nvidia-broadcast).
;;;;
;;;; Rewritten from NousResearch's Hermes Agent plugin of the same name; see
;;;; NOTICE. Everything it needs rides with nodecode: nlk:http speaks the
;;;; MCP gateway's streamable HTTP on loopback, nlk:read-events and
;;;; nlk:decode-json read its answers.

(defsystem "nodecode-nvidia-broadcast"
  :description "NVIDIA Broadcast: camera, microphone and speaker effects, devices and file processing over its local MCP gateway"
  :author "NousResearch (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nvidia-broadcast/test"))))

(defsystem "nodecode-nvidia-broadcast/test"
  :description "NVIDIA Broadcast tests. Registered into the shared nodecode.test registry; RUN-NVIDIA-BROADCAST-TESTS filters by the NVIDIA-BROADCAST-CELL- name prefix."
  :author "NousResearch (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-nvidia-broadcast" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nvidia-broadcast-tests)))
