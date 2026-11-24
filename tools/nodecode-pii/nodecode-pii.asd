;;;; nodecode-pii.asd --- personal data masked in the text that leaves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `pii' section, hooks the wire fold so every user message
;;;; is scrubbed on its way to the provider, and registers the redact_text
;;;; tool, /pii and (help :pii).
;;;;
;;;; Every dependency is already in the serving image. The .asd sits INSIDE
;;;; its folder: install by putting -- or symlinking -- the directory under
;;;; ~/.nodecode/cells/.

(defsystem "nodecode-pii"
  :description "Masks emails, phones, cards, IBANs, IPs and names in the text you send"
  :license "MIT"
  :author "lukefx (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "detect")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pii/test"))))

(defsystem "nodecode-pii/test"
  :description "PII tests. Registered into the shared nodecode.test registry; RUN-PII-TESTS filters by the PII-CELL- name prefix."
  :license "MIT"
  :author "lukefx (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pii" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pii-tests)))
