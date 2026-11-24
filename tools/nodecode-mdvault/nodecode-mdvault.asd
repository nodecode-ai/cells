;;;; nodecode-mdvault.asd --- a folder of Markdown notes as the organism's memory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mdvault' section, registers the four mdvault_ tools and the
;;;; /mdvault command, and recalls notes into the prompt before each turn.
;;;;
;;;; A Nodecode rewrite of kailashrajh/hermes-mdvault (see NOTICE).

(defsystem "nodecode-mdvault"
  :description "Markdown vault memory: recall from and write to a folder of Markdown notes"
  :license "MIT"
  :version "0.1.0"
  :author "kailashrajh (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mdvault/test"))))

(defsystem "nodecode-mdvault/test"
  :description "Markdown vault tests, registered into the shared nodecode.test registry; RUN-MDVAULT-TESTS filters by the MDVAULT-CELL- prefix."
  :license "MIT"
  :version "0.1.0"
  :author "kailashrajh (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mdvault" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mdvault-tests)))
