;;;; nodecode-roam.asd --- an encrypted bundle of this home, on two machines.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `roam' section, registers roam_status, roam_push and
;;;; roam_pull, puts /roam and (help :roam) on their catalogs, and makes
;;;; the small folder it remembers itself in.
;;;;
;;;; A Nodecode rewrite of vcruz305/hermes-roam (see NOTICE). The bundle
;;;; is AES-256-GCM with an HMAC-signed head, from ironclad: the two
;;;; subsystems the shipped Discord voice cell already asks for.

(defsystem "nodecode-roam"
  :description "An encrypted bundle of this home in storage you own, on two machines"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "vcruz305 (original), Nodecode (rewrite)"
  :depends-on ("nodecode"
               "ironclad/cipher/aes" "ironclad/aead/gcm"
               "ironclad/digest/sha256" "ironclad/mac/hmac")
  :serial t
  :components ((:file "package")
               (:file "archive")
               (:file "scope")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-roam/test"))))

(defsystem "nodecode-roam/test"
  :description "Roam tests. Registered into the shared nodecode.test registry; RUN-ROAM-TESTS filters by the ROAM-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "vcruz305 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-roam" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-roam-tests)))
