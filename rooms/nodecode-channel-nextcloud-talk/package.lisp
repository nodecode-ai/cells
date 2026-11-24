;;;; package.lisp --- NODECODE-CHANNEL-NEXTCLOUD-TALK package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Nextcloud Talk adapter over Talk's OCS REST API: the room list and
;;;; the chat wire as the kit's candidates, the request plans that speak a
;;;; room, what an app password can see, and the platform plus the poll lap
;;;; the kit's host runs over. The room/lane topology, the gate, the digest
;;;; delivery and the command catalog are the kit's.

(defpackage #:nodecode-channel-nextcloud-talk
  (:documentation "Nextcloud Talk channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:channel-nextcloud-talk)
  (:export
   #:start-channel #:probe-channel #:talk-platform #:request
   #:message-plan #:edit-plan #:delete-plan #:typing-plan #:reaction-plans
   #:message-id-of #:talk-candidate #:talk-mentioned-p #:talk-strip-mention
   #:talk-inbound-policy #:talk-poll-lap #:talk-room-type))
