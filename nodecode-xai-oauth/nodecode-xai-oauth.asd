;;;; nodecode-xai-oauth.asd --- xAI Grok on a SuperGrok or X Premium+ sign-in, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `xai-oauth' section, installs the hooks cell.lisp lists and
;;;; registers /xai-oauth, the sign-in.
;;;;
;;;; Ported from oh-my-pi's xai-oauth provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-xai-oauth"
  :description "xAI Grok through a SuperGrok or X Premium+ sign-in, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-xai-oauth/test"))))

(defsystem "nodecode-xai-oauth/test"
  :description "xAI OAuth tests. Registered into the shared nodecode.test registry; RUN-XAI-OAUTH-TESTS filters by the XAI-OAUTH-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-xai-oauth" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-xai-oauth-tests)))
