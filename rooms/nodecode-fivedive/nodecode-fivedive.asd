;;;; nodecode-fivedive.asd --- the 5dive app chat, through the box's local spool.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of 5dive-ai/5dive-plugins' `fivedive' platform adapter (see
;;;; NOTICE). Optional ASDF cell, not part of the organism core: the folder
;;;; loader loads it at boot when its directory sits under ~/.nodecode/cells/;
;;;; the gateway calls START-CELL after recovery, which reads the
;;;; `channels.fivedive' section, starts one spool lap, and registers
;;;; /fivedive and the fivedive_send tool.

(defsystem "nodecode-fivedive"
  :description "5dive app chat: the web and mobile chat of a 5dive box, through its local spool"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "5dive-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fivedive/test"))))

(defsystem "nodecode-fivedive/test"
  :description "5dive spool tests, registered under the FIVEDIVE-CELL- prefix."
  :license "Apache-2.0"
  :author "5dive-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fivedive" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fivedive-tests)))
