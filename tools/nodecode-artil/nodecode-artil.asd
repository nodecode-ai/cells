;;;; nodecode-artil.asd --- an agent's own inbox, by the artil CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `artil' section, registers six tools and the /artil command,
;;;; and puts the manual on (help :artil).
;;;;
;;;; The one thing outside this folder is the artil program itself, a Node
;;;; CLI the user installs with `npm i -g artil-ai' (https://www.npmjs.com/
;;;; package/artil-ai), which holds the sign-in and speaks to the account.
;;;; See NOTICE: this folder is a rewrite of artilai/plugins.

(defsystem "nodecode-artil"
  :description "Artil: an agent's own email address, phone number and Telegram, through the artil CLI"
  :license "MIT"
  :version "0.1.0"
  :author "artilai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-artil/test"))))

(defsystem "nodecode-artil/test"
  :description "Artil tests. Registered into the shared nodecode.test registry; RUN-ARTIL-TESTS filters by the ARTIL-CELL- name prefix."
  :license "MIT"
  :author "artilai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-artil" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-artil-tests)))
