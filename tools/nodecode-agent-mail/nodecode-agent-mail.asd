;;;; nodecode-agent-mail.asd --- one mailbox, through Agent Mail Gateway's REST API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. Fourteen tools over the gateway's
;;;; /v1 routes, the manual topic :agent-mail, and the /mail command. A
;;;; rewrite of dominikamann's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; gateway, quri encodes the query string.

(defsystem "nodecode-agent-mail"
  :description "Agent Mail Gateway: read, send and answer mail, fetch attachments, and manage calendar events"
  :author "dominikamann (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agent-mail/test"))))

(defsystem "nodecode-agent-mail/test"
  :description "Agent Mail Gateway tests. Registered into the shared nodecode.test registry; RUN-AGENT-MAIL-TESTS filters by the AGENT-MAIL- name prefix."
  :author "dominikamann (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-agent-mail" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agent-mail-tests)))
