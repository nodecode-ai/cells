;;;; nodecode-yandex-mail.asd --- Yandex Mail over IMAP and SMTP, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `yandex-mail' section and registers the eight tools.
;;;;
;;;; Rewritten from akinfold's Hermes Agent plugin; see NOTICE. IMAP and SMTP
;;;; are spoken here, over the sockets nodecode already loads (usocket,
;;;; cl+ssl): the mailbox is the user's own, and no service sits between.

(defsystem "nodecode-yandex-mail"
  :description "Yandex Mail: search, read, flag, move and delete mail over IMAP, save attachments, send over SMTP"
  :author "akinfold (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "usocket" "cl+ssl" "cl-base64")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "mime")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-yandex-mail/test"))))

(defsystem "nodecode-yandex-mail/test"
  :description "Yandex Mail tests, against a scripted mailbox; nothing leaves the machine."
  :author "akinfold (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-yandex-mail" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-yandex-mail-tests)))
