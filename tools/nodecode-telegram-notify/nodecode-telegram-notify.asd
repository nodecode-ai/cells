;;;; nodecode-telegram-notify.asd --- Telegram notifications as a tool, a command and a hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `telegram-notify' section, registers the telegram_notify tool,
;;;; the /telegram-notify command and one :frame hook, and puts the primer on
;;;; (help :telegram-notify).
;;;;
;;;; Rewritten from madpower2000's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the Bot
;;;; API and the JSON reader and writer in nlk build and read its bodies.

(defsystem "nodecode-telegram-notify"
  :description "Telegram notify: a message when a turn ends, fails or needs an answer, from a bot of your own"
  :author "madpower2000 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-telegram-notify/test"))))

(defsystem "nodecode-telegram-notify/test"
  :description "Telegram notify tests. Registered into the shared nodecode.test registry; RUN-TELEGRAM-NOTIFY-TESTS filters by the TELEGRAM-NOTIFY-CELL- name prefix."
  :author "madpower2000 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-telegram-notify" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-telegram-notify-tests)))
