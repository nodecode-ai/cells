;;;; nodecode-secret-guard.asd --- API keys kept out of the conversation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: the folder loader loads this
;;;; system at boot when its directory sits under ~/.nodecode/cells/, and the
;;;; gateway calls START-CELL after recovery with the whole config. The cell
;;;; reads its `secret-guard' section, keeps the secrets in one private file,
;;;; advises the :TOOL point so every tool result is scrubbed and every
;;;; credential read is refused, and registers three tools, /secret and
;;;; (help :secret-guard).
;;;;
;;;; Dependencies beyond nodecode: cl-ppcre and ironclad's sha256, both of
;;;; which the serving image already carries. Nothing here reaches the
;;;; network, spawns a process or reads a file the store does not name.

(defsystem "nodecode-secret-guard"
  :description "Keeps API keys out of the conversation: register a key by name, redact its value from every tool result, refuse credential reads"
  :license "MIT"
  :author "ChasClogston (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre" "ironclad/digest/sha256")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "guard")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-secret-guard/test"))))

(defsystem "nodecode-secret-guard/test"
  :description "Secret-guard tests. Registered into the shared nodecode.test registry; RUN-SECRET-GUARD-TESTS filters by the SECRET-GUARD-CELL- name prefix."
  :license "MIT"
  :author "ChasClogston (original), Nodecode (rewrite)"
  :depends-on ("nodecode-secret-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-secret-guard-tests)))
