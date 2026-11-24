;;;; package.lisp --- NODECODE-CHANNEL-MAX package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The MAX messenger adapter over MAX's bot API (platform-api2.max.ru):
;;;; updates read by long polling as the kit's candidates, the request plans
;;;; that speak a chat, what a token can see, and the platform plus the poll
;;;; lap the kit's host runs over. The room/lane topology, the gate, the
;;;; digest delivery and the command catalog are the kit's.

(defpackage #:nodecode-channel-max
  (:documentation "MAX messenger channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:channel-max)
  (:export
   #:start-channel #:probe-channel #:max-platform
   #:message-plan #:edit-plan #:delete-plan #:typing-plan #:message-id-of
   #:max-candidate #:max-mention-p #:max-inbound-policy #:max-poll-lap))
