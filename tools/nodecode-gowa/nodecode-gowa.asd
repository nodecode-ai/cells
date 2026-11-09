;;;; nodecode-gowa.asd --- GOWA's device slots: list them, pair one, remove one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of anpicasso/hermes-go-whatsapp (see NOTICE): the original's
;;;; three Hermes tools over one GOWA server's REST API, reimplemented in
;;;; Common Lisp against NLK:HTTP, with a section it reads instead of a
;;;; plugin's config, and /gowa where the plugin had a setup command.

(defsystem "nodecode-gowa"
  :description "GOWA device slots: list them, pair one, remove one"
  :license "MIT"
  :version "0.1.0"
  :author "anpicasso (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gowa/test"))))

(defsystem "nodecode-gowa/test"
  :description "The device tools against a stubbed GOWA; no network, no server."
  :license "MIT"
  :depends-on ("nodecode-gowa" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gowa-tests)))
