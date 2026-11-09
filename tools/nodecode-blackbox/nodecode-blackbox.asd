;;;; nodecode-blackbox.asd --- the local half of Agent Blackbox, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `blackbox' section, registers the two tools and
;;;; /blackbox, and puts the manual on (help :blackbox).
;;;;
;;;; Nothing outside this folder is needed and nothing is downloaded: the
;;;; rules are data in cell.lisp, ported from the plugin's own heuristic and
;;;; secret tables. The graph, the DKG node and the community share are the
;;;; half of the original this cell leaves out -- see NOTICE.

(defsystem "nodecode-blackbox"
  :description "Agent Blackbox, local half: scan untrusted text and shell commands for threats"
  :license "MIT"
  :version "0.1.0"
  :author "umanitek (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-blackbox/test"))))

(defsystem "nodecode-blackbox/test"
  :description "Blackbox tests. Registered into the shared nodecode.test registry; RUN-BLACKBOX-TESTS filters by the BLACKBOX-CELL- name prefix."
  :license "MIT"
  :author "umanitek (original), Nodecode (rewrite)"
  :depends-on ("nodecode-blackbox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-blackbox-tests)))
