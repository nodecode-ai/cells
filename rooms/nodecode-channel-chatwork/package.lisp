;;;; package.lisp --- NODECODE-CHANNEL-CHATWORK package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Chatwork channel lane: the API v2 calls that read rooms and post
;;;; messages, Chatwork's own message notation in and out, the candidate a
;;;; room message becomes, the platform the kit's host draws its answers with,
;;;; and the poll lap. Rooms, lanes, the gate and delivery are the kit's.
;;;; A rewrite of TakeshiTGAL's Hermes Agent plugin (see NOTICE).

(defpackage #:nodecode-channel-chatwork
  (:documentation "Chatwork (Japanese business chat) as a Nodecode channel.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:channel-chatwork)
  (:export
   ;; the adapter convention
   #:start-channel #:probe-channel
   ;; the platform, the lap, the state
   #:chatwork-platform #:chatwork-lap #:chatwork-policy #:chatwork-learn-rooms
   ;; pure: what a message says, and what an answer is written as
   #:chatwork-parse-inbound #:chatwork-render-outbound #:chatwork-defuse-tags
   #:chatwork-reply-header #:chatwork-message-candidate #:chatwork-watched-rooms
   ;; plans
   #:chatwork-me-plan #:chatwork-rooms-plan #:chatwork-messages-plan
   #:chatwork-message-plan #:chatwork-edit-plan #:chatwork-message-id
   ;; state
   #:chatwork-state #:chatwork-state-cursors #:chatwork-state-load #:chatwork-state-save
   #:chatwork-cursor #:chatwork-remember-sent-p))
