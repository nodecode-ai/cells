;;;; nodecode-telegram-business.asd --- Telegram Business mode as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. It teaches the model three verbs
;;;; that talk to the Telegram Bot API on a business connection: read the
;;;; customers' messages, draft a reply, and deliver a reply once the owner
;;;; has approved it. Ported from NousResearch's Hermes Agent plugin (NOTICE).

(defsystem "nodecode-telegram-business"
  :description "Draft replies to your Telegram Business customers and send one only after the owner approves it"
  :license "MIT"
  :version "0.1.0"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "base")
               (:file "state")
               (:file "bot")
               (:file "updates")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-telegram-business/test"))))

(defsystem "nodecode-telegram-business/test"
  :description "Telegram Business tests. Registered into the shared nodecode.test registry; RUN-TELEGRAM-BUSINESS-TESTS filters by the TELEGRAM-BUSINESS-CELL- name prefix."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-telegram-business" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-telegram-business-tests)))
