;;;; nodecode-google-antigravity.asd --- Antigravity (Gemini 3, Claude, GPT-OSS) as a provider lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `google-antigravity' section, registers the google-antigravity
;;;; lane, installs the hooks cell.lisp lists and registers /google-antigravity.
;;;;
;;;; Ported from oh-my-pi's google-antigravity provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-google-antigravity"
  :description "Antigravity: a Google sign-in, and Antigravity's Cloud Code Assist wire as a lane of its own"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-google-antigravity/test"))))

(defsystem "nodecode-google-antigravity/test"
  :description "Antigravity tests. Registered into the shared nodecode.test registry; RUN-GOOGLE-ANTIGRAVITY-TESTS filters by the GOOGLE-ANTIGRAVITY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-google-antigravity" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-google-antigravity-tests)))
