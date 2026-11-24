;;;; nodecode-wincred.asd --- the Windows Credential Manager as a secret source.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder loader loads it at boot
;;;; and the gateway calls START-CELL, which reads the `wincred' section and
;;;; puts the three tools, the /wincred command and the (help :wincred) topic
;;;; on the organism that cell.lisp lists.
;;;;
;;;; Ported from the Hermes plugin of Yicheng-Mark/hermes-windows-plugins (see
;;;; NOTICE). The original was a secret source: at startup it read the mapped
;;;; generic credentials out of the current user's Windows Credential Manager
;;;; and handed the values to Hermes's orchestrator as environment variables.
;;;; Nodecode has no such seam, so the same read is a tool, an explicit apply
;;;; and the cell's own start.

(defsystem "nodecode-wincred"
  :description "Windows Credential Manager: resolve environment variables from the current user's generic credentials, read-only"
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "vault")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-wincred/test"))))

(defsystem "nodecode-wincred/test"
  :description "wincred cell tests: the vault stubbed, decoding on real bytes, no Windows needed."
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode-wincred" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-wincred-tests)))
