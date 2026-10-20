;;;; package.lisp --- NODECODE-CHANNEL-X package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The X channel adapter over a logged-in X web session (tweetkit-x, MIT):
;;;; mentions and replies as the kit's candidates (events.lisp), the bridge
;;;; child that speaks to X, its plans, the conversation book and the executor
;;;; (rest.lisp), what the session can see (probe.lisp), and the platform plus
;;;; the poll lap the kit's host runs over (adapter.lisp). The room/lane
;;;; topology, the gate and the digest delivery are the kit's.

(defpackage #:nodecode-channel-x
  (:documentation "X (Twitter) channel adapter for the Lisp organism, over a logged-in web session.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:ncx)
  (:export
   ;; adapter.lisp --- the channel entry (kit host convention), the model-facing
   ;; request, the seam over every mention the lap reads, the platform
   #:start-channel #:request #:x-handle-mention #:x-platform #:x-where-text
   ;; probe.lisp --- what the session can see; the section declaration beside it
   #:probe-channel
   ;; events.lisp --- pure, exported for table-driven tests
   #:markdown-plain #:x-unescape #:x-strip-reply-prefix #:x-mention-p
   #:x-mention-candidate #:x-inbound-policy #:normalize-x-handle
   ;; rest.lisp
   #:make-x-book #:message-plan #:delete-plan #:bridge-plan #:x-message-id
   #:wrap-x-executor #:start-bridge #:stop-bridge #:bridge-executor))
