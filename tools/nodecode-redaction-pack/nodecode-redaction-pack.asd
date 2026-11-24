;;;; nodecode-redaction-pack.asd --- 71 more vendor credential formats masked in the text you send.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `redaction-pack' section, hooks the wire fold so the user
;;;; and tool text of every request is masked on its way to the provider, and
;;;; registers the redact_credentials tool, /redaction-pack and
;;;; (help :redaction-pack).
;;;;
;;;; Every dependency is already in the serving image (cl-ppcre rides with
;;;; nodecode). The .asd sits INSIDE its folder: install by putting -- or
;;;; symlinking -- the directory under ~/.nodecode/cells/.

(defsystem "nodecode-redaction-pack"
  :description "Masks 71 more vendor credential formats in the text you send and in tool output"
  :license "MIT"
  :author "pierrenode (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "formats")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-redaction-pack/test"))))

(defsystem "nodecode-redaction-pack/test"
  :description "Redaction pack tests. Registered into the shared nodecode.test registry; RUN-REDACTION-PACK-TESTS filters by the REDACTION-PACK-CELL- name prefix."
  :license "MIT"
  :author "pierrenode (original), Nodecode (rewrite)"
  :depends-on ("nodecode-redaction-pack" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-redaction-pack-tests)))
