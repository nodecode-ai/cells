;;;; package.lisp --- NODECODE-CHANNEL-WHATSAPP package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The WhatsApp channel adapter. WhatsApp is reached as a linked device --
;;;; the account's phone stays the primary, this machine one of its Linked
;;;; devices -- through a small Go program over whatsmeow (driver/), started
;;;; and spoken to by this cell over stdin and stdout (driver.lisp). The cell
;;;; holds every platform decision: Markdown as WhatsApp spells it
;;;; (markdown.lisp), the driver's events as the kit's candidates
;;;; (events.lisp), the plans a post, an edit, a reaction, a poll is
;;;; (plans.lisp), what the device can see and how it is linked (probe.lisp),
;;;; and the platform plus the lap the kit's host runs over (adapter.lisp).
;;;; Rooms, lanes, the gate and the digest are the kit's.

(defpackage #:nodecode-channel-whatsapp
  (:documentation "WhatsApp channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:ncw)
  (:export
   ;; adapter.lisp --- the channel entry, the model's door, the raw-event seam,
   ;; linking, the platform
   #:start-channel #:request #:whatsapp-handle-event #:whatsapp-platform
   #:whatsapp-where-text #:link #:unlink
   ;; probe.lisp --- what the device can see; the section declaration beside it
   #:probe-channel #:probe-chat-choices #:probe-user-choices #:qr-png
   ;; markdown.lisp --- pure
   #:whatsapp-text
   ;; events.lisp --- pure, exported for table-driven tests
   #:normalize-phone #:user-jid #:chat-jid #:whatsapp-mention-p #:whatsapp-strip-mention
   #:whatsapp-message-candidate #:whatsapp-reaction-candidate #:whatsapp-vote-press
   #:whatsapp-inbound-policy
   ;; plans.lisp
   #:make-whatsapp-book #:send-message-plan #:edit-message-plan #:revoke-plan #:typing-plan
   #:reaction-plans #:file-plan #:read-plan #:op-plan #:whatsapp-message-id #:whatsapp-address
   #:voice-waveform
   ;; driver.lisp
   #:make-driver #:driver-call #:driver-accept-line #:driver-next-event #:driver-alive-p
   #:make-driver-executor #:start-driver-process #:stop-driver #:ensure-driver-binary))
