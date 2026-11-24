;;;; nodecode-lfg-alarm.asd --- LFG Alarm's tools, over its MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `lfg-alarm' section, connects to the server and registers every
;;;; tool it lists, and puts the primer on (help :lfg-alarm).
;;;;
;;;; Rewritten from lfg-gm's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks the MCP streamable
;;;; HTTP transport and nlk's JSON readers read its bodies.

(defsystem "nodecode-lfg-alarm"
  :description "LFG Alarm: a real alarm on a phone, through the LFG Alarm MCP server"
  :author "lfg-gm (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lfg-alarm/test"))))

(defsystem "nodecode-lfg-alarm/test"
  :description "LFG Alarm tests. Registered into the shared nodecode.test registry; RUN-LFG-ALARM-TESTS filters by the LFG-ALARM-CELL- name prefix."
  :author "lfg-gm (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-lfg-alarm" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lfg-alarm-tests)))
