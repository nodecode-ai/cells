;;;; nodecode-openai-codex-device.asd --- a ChatGPT Plus/Pro subscription, signed in with a device code.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openai-codex-device' section, registers the Codex lane and
;;;; installs the hooks and the /openai-codex-device command cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's openai-codex-device sign-in and the openai-codex
;;;; provider it signs in to (see NOTICE). Every dependency rides with
;;;; nodecode.

(defsystem "nodecode-openai-codex-device"
  :description "ChatGPT Plus/Pro (Codex, headless/device): sign in with a device code, and serve the Codex models"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openai-codex-device/test"))))

(defsystem "nodecode-openai-codex-device/test"
  :description "openai-codex-device tests. Registered into the shared nodecode.test registry; RUN-OPENAI-CODEX-DEVICE-TESTS filters by the OPENAI-CODEX-DEVICE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-openai-codex-device" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openai-codex-device-tests)))
