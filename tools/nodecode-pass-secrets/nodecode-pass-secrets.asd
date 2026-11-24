;;;; nodecode-pass-secrets.asd --- a local pass (password-store) as secrets a
;;;; session can read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder loader loads it at boot
;;;; and the gateway calls START-CELL, which reads the `pass-secrets' section
;;;; and puts the four tools, the /pass command and the (help :pass-secrets)
;;;; topic on the organism that cell.lisp lists.
;;;;
;;;; Ported from the Hermes plugin of dyiapanis/pass-secrets (see NOTICE). The
;;;; original was a secret source: it read the store at startup and injected
;;;; every entry as an environment variable. Nodecode has no such seam, so the
;;;; same reads are tools, and the pass CLI is the operator's own binary on
;;;; PATH: nothing here vendors it, downloads it or talks to a network.

(defsystem "nodecode-pass-secrets"
  :description "pass (password-store) secrets: read a local GPG-encrypted store through the pass CLI"
  :license "MIT"
  :author "dyiapanis (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pass-secrets/test"))))

(defsystem "nodecode-pass-secrets/test"
  :description "pass-secrets cell tests: the store walked on disk, every pass call stubbed, no process started."
  :license "MIT"
  :author "dyiapanis (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pass-secrets" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pass-secrets-tests)))
