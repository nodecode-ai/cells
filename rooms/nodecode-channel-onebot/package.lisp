;;;; package.lisp --- NODECODE-CHANNEL-ONEBOT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The OneBot 11 lane: the bridge's HTTP API (history reads and sends),
;;;; QQ's message segments and CQ codes in and out, the candidate an event
;;;; becomes, the platform the kit's host draws its answers with, and the
;;;; poll lap. Groups, lanes, the gate and delivery are the kit's.
;;;; A rewrite of mario841859784's Hermes Agent plugin (see NOTICE).

(defpackage #:nodecode-channel-onebot
  (:documentation "QQ through a self-hosted OneBot 11 bridge, as a Nodecode channel.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:channel-onebot)
  (:export
   ;; the adapter convention
   #:start-channel #:probe-channel
   ;; the platform, the lap, the policy
   #:onebot-platform #:onebot-lap #:onebot-inbound-policy #:onebot-learn-groups
   ;; pure: what a message says, and what an answer is written as
   #:onebot-parse-message #:onebot-render-outbound #:onebot-defuse
   #:onebot-message-candidate #:onebot-event-id
   ;; plans
   #:onebot-login-plan #:onebot-groups-plan #:onebot-history-plan
   #:onebot-send-plan #:onebot-delete-plan #:onebot-message-id-of
   #:onebot-ok-p #:onebot-sent-by-bot-p #:onebot-remember-sent))
