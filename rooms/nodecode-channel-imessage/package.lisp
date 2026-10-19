;;;; package.lisp --- NODECODE-CHANNEL-IMESSAGE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The iMessage channel adapter over a BlueBubbles server: Markdown spelled as
;;;; the plain text Messages shows (text.lisp), messages and tapbacks as the
;;;; kit's candidates (events.lisp), REST plans, the chat book and the executor
;;;; (rest.lisp), what the server can see (probe.lisp), and the platform plus
;;;; the poll lap the kit's host runs over (adapter.lisp). The room/lane
;;;; topology, the gate and the digest delivery are the kit's.

(defpackage #:nodecode-channel-imessage
  (:documentation "iMessage channel adapter for the Lisp organism, over a BlueBubbles server.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:nci)
  (:export
   ;; adapter.lisp --- the channel entry (kit host convention), the
   ;; model-facing REST call, the raw-message seam, the platform
   #:start-channel #:request #:imessage-handle-message #:imessage-platform #:imessage-where-text
   ;; probe.lisp --- what the server can see; the section declaration beside it
   #:probe-channel #:probe-chat-choices #:probe-user-choices
   ;; text.lisp --- pure
   #:markdown-plain
   ;; events.lisp --- pure, exported for table-driven tests
   #:normalize-handle #:direct-chat-p #:message-chat-guid #:imessage-text
   #:imessage-message-candidate #:imessage-tapback #:imessage-reaction-candidate
   #:emoji-tapback #:tapback-emoji #:imessage-mention-p #:imessage-strip-mention
   #:imessage-inbound-policy #:choice-items #:choice-picked
   ;; rest.lisp
   #:make-imessage-book #:message-plan #:edit-plan #:delete-plan #:typing-plan #:reaction-plans
   #:file-plan #:query-plan #:newest-plan #:imessage-message-id #:card-id-p
   #:wrap-imessage-executor #:make-imessage-executor))
