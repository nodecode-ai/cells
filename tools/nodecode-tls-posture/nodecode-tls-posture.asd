;;;; nodecode-tls-posture.asd --- a read-only audit of the TLS this host serves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tls-posture' section, registers the tls_posture tool, puts
;;;; /tls in the catalog and the manual on (help :tls).
;;;;
;;;; A Nodecode rewrite of Pacific-Cloud-Solutions's pcs-security-tls-posture
;;;; Hermes Agent plugin; see NOTICE. No dependency beyond nodecode itself:
;;;; the certificate decoder below is Common Lisp over the DER the file holds.

(defsystem "nodecode-tls-posture"
  :description "Read-only TLS posture audit: certificate validity, key strength, deprecated protocols and the certificate each server serves"
  :author "Pacific-Cloud-Solutions (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "x509")
               (:file "audit")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tls-posture/test"))))

(defsystem "nodecode-tls-posture/test"
  :description "TLS posture tests. Registered into the shared nodecode.test registry; RUN-TLS-POSTURE-TESTS filters by the TLS-POSTURE-CELL- name prefix."
  :author "Pacific-Cloud-Solutions (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-tls-posture" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tls-posture-tests)))
