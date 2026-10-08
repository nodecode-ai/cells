;;;; nodecode-openai-codex.asd --- a ChatGPT Plus/Pro subscription as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openai-codex' section, registers the Codex lane and installs
;;;; the hooks and the /openai-codex command cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's openai-codex provider and its ChatGPT sign-in (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-openai-codex"
  :description "ChatGPT Plus/Pro (Codex subscription): sign in, and serve the Codex models"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openai-codex/test"))))

(defsystem "nodecode-openai-codex/test"
  :description "openai-codex tests. Registered into the shared nodecode.test registry; RUN-OPENAI-CODEX-TESTS filters by the OPENAI-CODEX-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-openai-codex" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openai-codex-tests)))
