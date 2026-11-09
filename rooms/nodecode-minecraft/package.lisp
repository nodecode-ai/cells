;;;; package.lisp --- NODECODE-MINECRAFT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell is the Minecraft Java protocol client (wire.lisp, protocol.lisp)
;;;; and the chat surface over it (cell.lisp): a bot joins the server, every
;;;; player who talks to it gets a session of their own, and its answer is
;;;; posted back into the game's chat.

(defpackage #:nodecode-minecraft
  (:documentation "A Minecraft bot that relays in-game chat to the agent.")
  (:use #:cl)
  (:export
   ;; cell.lisp --- the entry, the chat shaping the original had, /minecraft
   #:start-cell #:minecraft-error #:minecraft-error-detail
   #:mc-format #:chunk-text #:session-of #:allowed-p #:strip-prefix
   ;; protocol.lisp --- packets, so a test proves them without a server
   #:flatten-component #:player-chat-said #:system-chat-said
   #:player-info-names #:sync-position-of #:disconnect-reason
   #:chat-message-packet #:keep-alive-packet #:confirm-teleport-packet
   #:set-rotation-packet #:handshake-packet #:login-start-packet
   #:packet-id #:packet-version
   ;; wire.lisp --- the framing
   #:make-buf #:buf-octets #:buf-byte #:buf-us #:buf-long #:buf-string
   #:buf-varint #:buf-bytes #:read-frame #:write-frame #:make-cur #:cur
   #:cur-u8 #:cur-varint #:cur-string #:cur-long #:cur-bool #:cur-skip
   #:cur-uuid #:cur-double #:cur-float #:cur-exhausted-p))
