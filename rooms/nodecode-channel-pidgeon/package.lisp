;;;; package.lisp --- NODECODE-CHANNEL-PIDGEON package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Pidgeon channel lane: the REST calls that read an agent's Pidgeon
;;;; inbox and reply in a thread, the gate that decides whether a piece of
;;;; mail may become a turn, the candidate an email becomes, the platform the
;;;; kit's host draws its answers with, and the poll lap. Sessions, lanes,
;;;; delivery and the allowlist are the kit's.

(defpackage #:nodecode-channel-pidgeon
  (:documentation "Pidgeon (email for agents) as a Nodecode channel.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:npi)
  (:export
   ;; the adapter convention
   #:start-channel #:probe-channel
   ;; the platform, the lap, the seam
   #:pidgeon-platform #:pidgeon-lap #:pidgeon-take-events #:pidgeon-admit
   #:pidgeon-policy #:make-pidgeon-adapter
   ;; pure: what a message becomes, and whether it may
   #:pidgeon-decide #:pidgeon-authenticated-p #:pidgeon-message-candidate
   #:pidgeon-body-for-agent #:pidgeon-plain-text #:pidgeon-iso-now
   ;; plans
   #:pidgeon-me-plan #:pidgeon-events-plan #:pidgeon-message-get-plan
   #:pidgeon-reply-plan))
