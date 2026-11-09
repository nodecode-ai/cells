;;;; nodecode-fmsg.asd --- fmsg as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fmsg' section and registers the three fmsg_* tools, the
;;;; /fmsg command and the (help :fmsg) topic.
;;;;
;;;; A Nodecode rewrite of markmnl/hermes-fmsg (see NOTICE).

(defsystem "nodecode-fmsg"
  :description "fmsg: an fmsg address of the agent's own, with threaded send, inbox and thread reads"
  :license "MIT"
  :version "0.1.0"
  :author "markmnl (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fmsg/test"))))

(defsystem "nodecode-fmsg/test"
  :description "fmsg tests. Registered into the shared nodecode.test registry; RUN-FMSG-TESTS filters by the FMSG-CELL- name prefix."
  :license "MIT"
  :author "markmnl (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fmsg" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fmsg-tests)))
