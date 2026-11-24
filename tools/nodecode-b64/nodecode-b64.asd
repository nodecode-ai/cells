;;;; nodecode-b64.asd --- decode base64, hex and percent-encoded text for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://b64' is accepted. It reads the `b64' config section and
;;;; contributes one tool (b64_decode), one command (/b64) and one manual
;;;; topic (help :b64); nothing starts a thread, dials a host or reads a file.

(defsystem "nodecode-b64"
  :description "Decode base64, base64url, hex and percent-encoded text, and say what it holds"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "decode")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-b64/test"))))

(defsystem "nodecode-b64/test"
  :description "Tests for the b64 cell; RUN-B64-TESTS filters the shared registry by the B64- name prefix."
  :license "MIT"
  :depends-on ("nodecode-b64" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "decode-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-b64-tests)))
