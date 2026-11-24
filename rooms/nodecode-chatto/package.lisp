;;;; package.lisp --- NODECODE-CHATTO package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Chatto channel lane: the ConnectRPC calls that read rooms and post
;;;; messages, the candidate a Chatto message becomes, the platform the kit's
;;;; host draws its answers with, and the poll lap that turns room events
;;;; into asks. Rooms, lanes, the gate and delivery are the kit's.

(defpackage #:nodecode-chatto
  (:documentation "Chatto (self-hosted team chat) as a Nodecode channel.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:chatto)
  (:export
   ;; the adapter convention
   #:start-channel #:probe-channel
   ;; the platform, the lap, the seam
   #:chatto-platform #:chatto-lap #:chatto-learn-rooms
   ;; pure: what a message becomes
   #:chatto-message-candidate #:chatto-mention-p #:chatto-strip-mention
   #:chatto-message-id #:chatto-watched-rooms
   ;; plans
   #:chatto-login-plan #:chatto-rooms-plan #:chatto-events-plan
   #:chatto-message-plan #:chatto-delete-plan #:chatto-typing-plan
   #:chatto-policy))
