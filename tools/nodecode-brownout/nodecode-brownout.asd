;;;; nodecode-brownout.asd --- a power-loss guard for the host Nodecode runs on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Osiyomeoh/brownout. Optional quicklisp/ASDF cell, not part
;;;; of the organism core: the folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls START-CELL
;;;; after recovery. That reads the `brownout' config section, polls the host's
;;;; power on one worker thread, hooks the :TOOL point so a parked host holds
;;;; new tool calls, and registers /brownout, the brownout tool and the
;;;; :brownout manual topic.

(defsystem "nodecode-brownout"
  :description "Notices a host that lost mains power, measures battery runway, and parks tool calls"
  :license "MIT"
  :author "Osiyomeoh (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "power")
               (:file "engine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-brownout/test"))))

(defsystem "nodecode-brownout/test"
  :description "Brownout tests. Registered into the shared nodecode.test registry; RUN-BROWNOUT-TESTS filters by the BROWNOUT-CELL- name prefix."
  :license "MIT"
  :author "Osiyomeoh (original), Nodecode (rewrite)"
  :depends-on ("nodecode-brownout" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-brownout-tests)))
