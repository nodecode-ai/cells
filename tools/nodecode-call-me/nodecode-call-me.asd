;;;; nodecode-call-me.asd --- the /call-me phone as two tools and a command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `call_me' section, registers the callme_text and callme_call
;;;; tools, the /callme command and one manual topic.
;;;;
;;;; Rewritten from radres's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to the service and
;;;; quri escapes the little that rides a query string.

(defsystem "nodecode-call-me"
  :description "/call-me: ring a phone, speak a sentence, read the spoken answer back as text"
  :author "radres (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "client")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-call-me/test"))))

(defsystem "nodecode-call-me/test"
  :description "Call-me tests. Registered into the shared nodecode.test registry; RUN-CALL-ME-TESTS filters by the CALL-ME-CELL- name prefix."
  :author "radres (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-call-me" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-call-me-tests)))
