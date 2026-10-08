;;;; nodecode-google-gemini-cli.asd --- Google Cloud Code Assist (the Gemini CLI's backend) as a provider lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `google-gemini-cli' section, registers the google-gemini-cli
;;;; lane, installs the hooks cell.lisp lists and registers /google-gemini-cli.
;;;;
;;;; Ported from oh-my-pi's google-gemini-cli provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-google-gemini-cli"
  :description "Google Cloud Code Assist: a Google sign-in, and the Gemini CLI's wire as a lane of its own"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-google-gemini-cli/test"))))

(defsystem "nodecode-google-gemini-cli/test"
  :description "Google Cloud Code Assist (Gemini CLI) tests. Registered into the shared nodecode.test registry; RUN-GOOGLE-GEMINI-CLI-TESTS filters by the GOOGLE-GEMINI-CLI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-google-gemini-cli" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-google-gemini-cli-tests)))
