;;;; nodecode-dpapi-secrets.asd --- a Windows DPAPI secret store, read as
;;;; named secrets a session can use.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder loader loads it at boot
;;;; and the gateway calls START-CELL, which reads the `dpapi-secrets' section
;;;; and puts the six tools, the /dpapi-secrets command and the
;;;; (help :dpapi-secrets) topic on the organism that cell.lisp lists.
;;;;
;;;; Ported from the Hermes plugin of Yicheng-Mark/hermes-windows-plugins (see
;;;; NOTICE). The original was a secret source: at startup it decrypted one
;;;; DPAPI-secret file and handed the mapped values to the orchestrator, which
;;;; made them environment variables. Nodecode has no such seam, so the same
;;;; store is read through tools, and the file format is the original's, so a
;;;; store the plugin wrote is read here unchanged.

(defsystem "nodecode-dpapi-secrets"
  :description "Windows DPAPI secrets: one encrypted store of named keys, resolved by name"
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "dpapi")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dpapi-secrets/test"))))

(defsystem "nodecode-dpapi-secrets/test"
  :description "dpapi-secrets cell tests: a scratch store on disk, the platform's own protection, no network."
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode-dpapi-secrets" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dpapi-secrets-tests)))
