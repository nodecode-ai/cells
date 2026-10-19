;;;; package.lisp --- NODECODE-CHANNEL-MATRIX package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Matrix channel adapter: Markdown as Matrix's HTML (markdown.lisp),
;;;; room events as the kit's candidates (events.lisp), Client-Server API
;;;; plans, the matrix book and the executor (rest.lisp), what the token can see
;;;; (probe.lisp), and the platform plus the /sync lap the kit's host runs
;;;; over (adapter.lisp). The room/lane topology, the gate and the digest
;;;; delivery are the kit's.

(defpackage #:nodecode-channel-matrix
  (:documentation "Matrix channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:ncm)
  (:export
   ;; adapter.lisp --- the channel entry (kit host convention), the
   ;; model-facing Client-Server API call, the raw-event seam, the platform
   #:start-channel #:request #:matrix-handle-event #:matrix-platform #:matrix-where-text
   ;; probe.lisp --- what the token can see; the section declaration beside it
   #:probe-channel #:probe-channel-choices #:probe-user-choices
   ;; markdown.lisp --- pure
   #:matrix-html
   ;; events.lisp --- pure, exported for table-driven tests
   #:matrix-plain-text #:matrix-mention-p #:matrix-strip-mention
   #:matrix-message-candidate #:matrix-control-press #:matrix-inbound-policy
   #:invite-sender #:control-glyph
   ;; rest.lisp
   #:make-matrix-book #:note-room-event #:bot-thread-p #:reply-thread
   #:message-plan #:edit-plan #:delete-plan #:typing-plan #:reaction-plans #:file-plan
   #:sync-plan #:join-plan #:matrix-event-id #:matrix-address #:matrix-retry-after-ms
   #:wrap-matrix-executor #:make-matrix-executor))
