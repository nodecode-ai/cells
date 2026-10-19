;;;; package.lisp --- NODECODE-CHANNEL-EMAIL package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The email channel adapter: one message read (mime.lisp), whether it may
;;;; be answered and as what candidate (gate.lisp), one reply written
;;;; (compose.lisp), IMAP and SMTP over TLS (wire.lisp), the platform, the
;;;; executor and the polling lap the kit's host runs over (adapter.lisp), and
;;;; what the credentials reach (probe.lisp). The room/lane topology, the gate
;;;; and the digest delivery are the kit's.

(defpackage #:nodecode-channel-email
  (:documentation "Email channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:nce)
  (:export
   ;; mime.lisp --- pure
   #:mail #:mail-p #:mail-headers #:mail-text #:mail-attachments
   #:parse-message #:header #:header-values #:header-text #:decode-words
   #:parse-address #:parse-address-list #:message-ids #:strip-quoted-reply #:html-text
   ;; gate.lisp --- pure
   #:from-address #:automated-p #:authenticated-p #:sender-allowed-p #:addressed-to-p
   #:thread-root #:mail-id #:thread-channel
   ;; compose.lisp --- pure
   #:compose-message #:new-message-id #:reply-subject
   ;; wire.lisp
   #:imap-open #:imap-close #:imap-examine #:imap-search-new #:imap-fetch #:smtp-send
   ;; adapter.lisp --- the channel entry (kit host convention), the
   ;; model-facing send, the raw-message seam, and the platform
   #:start-channel #:send-mail #:email-handle-message #:email-platform
   #:make-mail-book #:mail-candidate
   ;; probe.lisp --- what the credentials reach; the section declaration
   ;; beside it is read through the kernel
   #:probe-channel))
