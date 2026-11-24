;;;; nodecode-upstash-box.asd --- Upstash Box cloud containers as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `upstash-box' section, registers box_exec, box_read, box_write,
;;;; box_list and /box, and pauses the Box it holds on the way out.
;;;;
;;;; A Nodecode rewrite of upstash/box's Hermes terminal backend (see NOTICE).

(defsystem "nodecode-upstash-box"
  :description "Upstash Box: run a shell command, read and write files in a cloud Linux container"
  :license "MIT"
  :version "0.1.0"
  :author "upstash (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-upstash-box/test"))))

(defsystem "nodecode-upstash-box/test"
  :description "Upstash Box tests. Registered into the shared nodecode.test registry; RUN-UPSTASH-BOX-TESTS filters by the UPSTASH-BOX-CELL- name prefix."
  :license "MIT"
  :author "upstash (original), Nodecode (rewrite)"
  :depends-on ("nodecode-upstash-box" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-upstash-box-tests)))
