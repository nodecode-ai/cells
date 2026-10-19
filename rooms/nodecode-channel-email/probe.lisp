;;;; probe.lisp --- what the credentials reach, and the section declaration.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-channel-email)

(defun probe-channel (section &key executor (timeout 15))
  "Whether SECTION's credentials log in to IMAP and SMTP, as lines: the
mailbox and how many messages it holds, and the SMTP server's acceptance.
Read only: nothing is fetched and nothing is sent."
  (declare (ignore executor))
  (let* ((address (string-downcase (or (config-string section "address" nil) "")))
         (domain (domain-of address))
         (username (config-string section "username" (base-login address)))
         (password (ignore-errors (resolve-channel-secret section "password")))
         (lines '()))
    (flet ((say (control &rest args) (push (apply #'format nil control args) lines)))
      (cond
        ((string= address "") (say "address: unset"))
        ((null password) (say "password: not found -- set password_env or password_file"))
        (t
         (let ((imap-host (config-string section "imap_host" (format nil "imap.~a" domain)))
               (mailbox (config-string section "mailbox" "INBOX"))
               (wire nil))
           (handler-case
               (progn
                 (setf wire (imap-open imap-host (config-integer section "imap_port" 993 :min 1)
                                       username password :timeout timeout))
                 (multiple-value-bind (validity next) (imap-examine wire mailbox)
                   (declare (ignore validity))
                   (say "imap: ok -- ~a as ~a, ~a holds ~:[?~;~:*~d~] message~:p so far"
                        imap-host username mailbox (and next (1- next)))))
             (error (condition) (say "imap: ~a" condition)))
           (imap-close wire))
         (let* ((smtp-host (config-string section "smtp_host" (format nil "smtp.~a" domain)))
                (port (config-integer section "smtp_port" 465 :min 1))
                (wire nil))
           (handler-case
               (progn
                 (setf wire (wire-connect smtp-host port :tls (= port 465) :timeout timeout))
                 (smtp-expect wire '(220) "greeting")
                 (wire-write wire (format nil "EHLO nodecode.local~a" +crlf+))
                 (smtp-expect wire '(250) "EHLO")
                 (unless (= port 465)
                   (wire-write wire (format nil "STARTTLS~a" +crlf+))
                   (smtp-expect wire '(220) "STARTTLS")
                   (wire-starttls wire)
                   (wire-write wire (format nil "EHLO nodecode.local~a" +crlf+))
                   (smtp-expect wire '(250) "EHLO"))
                 (wire-write wire (format nil "AUTH PLAIN ~a~a"
                                          (cl-base64:usb8-array-to-base64-string
                                           (utf8 (format nil "~c~a~c~a" (code-char 0) username (code-char 0) password)))
                                          +crlf+))
                 (smtp-expect wire '(235) "AUTH")
                 (say "smtp: ok -- ~a:~d accepts ~a" smtp-host port username)
                 (ignore-errors (wire-write wire (format nil "QUIT~a" +crlf+))))
             (error (condition) (say "smtp: ~a" condition)))
           (wire-close wire)))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(nlk:define-section ("channels" "email")
  (:guide "Give the bot an address: a mailbox of its own, or a plus address of one you have (you+bot@example.com -- only mail sent To or Cc that exact address is read, and the mailbox is opened read-only, so a shared inbox keeps every flag). Create an app password for the mailbox and keep it in a file or an environment variable, never in the config. IMAP and SMTP default to imap.<domain>:993 and smtp.<domain>:465; set them when the provider names others. Only allowed_senders are answered, and only when the receiving server vouched for their domain (DMARC, DKIM or SPF in Authentication-Results).")
  (:check #'probe-channel)
  (:one-of "password_env" "password_file")
  ("address" :string :doc "the bot's address: answers come from it, and only mail To or Cc it is read (a plus address works)")
  ("from_name" :string :default "Nodecode" :doc "the name answers are signed with in From")
  ("username" :string :doc "the login, when it is not the address without its +tag")
  ("password_env" :env :doc "environment variable holding the mailbox's app password")
  ("password_file" :path :doc "file holding the mailbox's app password")
  ("imap_host" :string :doc "IMAP server; imap.<domain> when unset")
  ("imap_port" :integer :default 993 :doc "993 is TLS; 143 is STARTTLS")
  ("mailbox" :string :default "INBOX" :doc "the folder read for new mail")
  ("smtp_host" :string :doc "SMTP server; smtp.<domain> when unset")
  ("smtp_port" :integer :default 465 :doc "465 is TLS; 587 is STARTTLS")
  ("allowed_senders" :list :doc "addresses answered, or @domain for every address at that domain")
  ("owner" :list :doc "the operator's address, whose word is standing policy in the channel")
  ("require_auth" :boolean :default t
                  :doc "answer only mail whose From domain the receiving server vouched for (DMARC, DKIM or SPF); false trusts the From line, which anyone can type")
  ("auth_server" :string :doc "the receiving server's authserv-id; when set, only its Authentication-Results count")
  ("poll_seconds" :integer :default 30 :doc "how often the mailbox is read")
  ("catch_up_minutes" :integer :default 60 :doc "after a restart, mail that arrived within this many minutes is still answered; older is left")
  ("max_megabytes" :integer :default 25 :doc "a larger message is not read")
  ("state_file" :path :doc "where the thread book is kept; channels/email-state.sexp under the home when unset")
  ("turn_budget_minutes" :integer :default 0
                         :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona"))
