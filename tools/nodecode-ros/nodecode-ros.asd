;;;; nodecode-ros.asd --- drive and watch a ROS or ROS 2 robot over rosbridge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ros' section, registers the nine tools, the /ros command and
;;;; the manual on (help :ros).
;;;;
;;;; The plugin's own safety interlock is kept, though a cell owns its tools
;;;; and needs no pre-call hook for it: every write is checked inside the tool
;;;; that makes it. The mock robot is kept too, so the cell runs with no ROS.
;;;; See NOTICE: this folder is a rewrite of arcabotai/hermes-ros.

(defsystem "nodecode-ros"
  :description "ROS robots: list, echo, drive, publish, call, pose, snapshot, stop, status"
  :license "MIT"
  :version "0.1.0"
  :author "arcabotai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "robot")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ros/test"))))

(defsystem "nodecode-ros/test"
  :description "ros tests. Registered into the shared nodecode.test registry; RUN-ROS-TESTS filters by the ROS-CELL- name prefix."
  :license "MIT"
  :author "arcabotai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ros" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ros-tests)))
