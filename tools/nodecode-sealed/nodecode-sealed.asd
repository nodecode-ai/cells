;;;; nodecode-sealed.asd --- an encrypted store of environment variables, in
;;;; the original's format, loaded into the process at start.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `sealed' section, puts the store's variables into this
;;;; process's environment, and registers sealed_status, sealed_list,
;;;; sealed_set, sealed_unset, /sealed and (help :sealed).
;;;;
;;;; The cipher is ironclad's, already in the serving image; the store's
;;;; format, its dotenv body and its key custody are this folder's own.

(defsystem "nodecode-sealed"
  :description "An encrypted store of environment variables, in the original's AES-256-GCM format, loaded into the process at start"
  :license "MIT"
  :author "kailashrajh (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad" "ironclad/aead/gcm" "ironclad/cipher/aes")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sealed/test"))))

(defsystem "nodecode-sealed/test"
  :description "Sealed cell tests. Registered into the shared nodecode.test registry; RUN-SEALED-TESTS filters by the SEALED-CELL- name prefix."
  :license "MIT"
  :author "kailashrajh (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sealed" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sealed-tests)))
