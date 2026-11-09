;;;; nodecode-telegram-stickers.asd --- Telegram stickers as three tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `telegramstickers' section, registers the three tools and the
;;;; /stickers command, and puts the primer on (help :telegram-stickers).
;;;;
;;;; Rewritten from churnast's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to the Bot API and
;;;; ppcre reads the ids and the words.

(defsystem "nodecode-telegram-stickers"
  :description "Telegram stickers: find a sticker in your own packs, send one, mute or pace a chat"
  :author "churnast (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-telegram-stickers/test"))))

(defsystem "nodecode-telegram-stickers/test"
  :description "Telegram stickers tests. Registered into the shared nodecode.test registry; RUN-TELEGRAM-STICKERS-TESTS filters by the TELEGRAM-STICKERS-CELL- name prefix."
  :author "churnast (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-telegram-stickers" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-telegram-stickers-tests)))
