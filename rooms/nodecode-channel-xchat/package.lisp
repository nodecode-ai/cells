;;;; package.lisp --- NODECODE-CHANNEL-XCHAT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The X Chat lane: X's end-to-end encrypted direct messages, read and
;;;; answered as the connected account. The kit's host, rooms, gate and
;;;; delivery are the kit's; this folder brings the wire (rest.lisp), the
;;;; sealing the ciphertext is made of (crypto.lisp) and the lap that turns
;;;; new events into candidates (adapter.lisp).

(defpackage #:nodecode-channel-xchat
  (:documentation "X Chat (X's encrypted DMs) as a Nodecode channel.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:ncxc)
  (:import-from #:nodecode.kernel #:config-error)
  (:export
   ;; adapter.lisp --- the channel entry, the platform, the lap, the section
   #:start-channel #:xchat-platform #:xchat-poll-lap #:xchat-request
   #:xchat-conversations-plan #:xchat-events-plan #:xchat-message-plan
   #:xchat-read-plan #:+xchat-text-limit+ #:*xchat-adapter*
   ;; crypto.lisp --- pure, exported for tests
   #:seal-text #:open-text #:conversation-key #:hkdf-sha256
   #:hex-encode #:hex-decode #:digest256 #:hmac256
   ;; rest.lisp
   #:make-xchat-book #:xchat-book-token #:xchat-cursor #:keep-cursors))
