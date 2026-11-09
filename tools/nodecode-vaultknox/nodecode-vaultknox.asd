;;;; nodecode-vaultknox.asd --- secret values redacted from the text that leaves.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `vaultknox' section, hooks the wire fold so secret values
;;;; are redacted from every message on its way to the provider, and registers
;;;; the scan_secrets tool, /vaultknox and (help :vaultknox).
;;;;
;;;; Every dependency is already in the serving image (cl-ppcre rides with
;;;; nodecode). The .asd sits INSIDE its folder: install by putting -- or
;;;; symlinking -- the directory under ~/.nodecode/cells/.

(defsystem "nodecode-vaultknox"
  :description "API keys, tokens and passwords redacted from chat in both directions"
  :license "Apache-2.0"
  :author "Ufonik88 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "detect")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-vaultknox/test"))))

(defsystem "nodecode-vaultknox/test"
  :description "VaultKnox tests. Registered into the shared nodecode.test registry; RUN-VAULTKNOX-TESTS filters by the VAULTKNOX-CELL- name prefix."
  :license "Apache-2.0"
  :author "Ufonik88 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-vaultknox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-vaultknox-tests)))
