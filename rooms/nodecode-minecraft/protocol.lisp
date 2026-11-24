;;;; protocol.lisp --- the packets this bot speaks, for Java protocol 763.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The ids are data, in one table, because they are the one part of the
;;;; protocol that moves between game versions: 763 is Minecraft 1.20.1, the
;;;; version the original's bridge was built against (mineflayer 4). A server
;;;; of another version answers the handshake with its own number, and the
;;;; cell refuses rather than guess an id.

(in-package #:nodecode-minecraft)

(defparameter +packet-ids+
  '((:version . 763)
    (:login . ((:disconnect . #x00) (:success . #x02) (:compression . #x03)))
    (:login-out . ((:start . #x00) (:handshake . #x00)))
    (:play . ((:login . #x27) (:keep-alive . #x22) (:player-chat . #x34)
              (:disguised-chat . #x1B) (:system-chat . #x65) (:player-info . #x39)
              (:sync-position . #x3B) (:disconnect . #x1A)))
    (:play-out . ((:confirm-teleport . #x00) (:chat . #x05) (:keep-alive . #x12)
                  (:position . #x14) (:rotation . #x16)))))

(defun packet-version () (cdr (assoc :version +packet-ids+)))

(defun packet-id (name &optional (state :play) (direction :in))
  (let* ((table (cdr (assoc (if (eq direction :out) (intern (format nil "~a-OUT" (string state)) :keyword) state)
                             +packet-ids+)))
         (entry (and table (assoc name table))))
    (unless entry (fail "minecraft: no ~a packet named ~a" state name))
    (cdr entry)))

;;; --- what the bot sends ---------------------------------------------------------

(defun handshake-packet (host port)
  "The first packet: which protocol this is, where it is going, to log in."
  (let ((body (make-buf)))
    (buf-varint body (packet-version))
    (buf-string body host)
    (buf-us body port)
    (buf-varint body 2)
    (values (packet-id :handshake :login :out) (buf-octets body))))

(defun login-start-packet (username)
  "Login Start, offline mode: the name, and nothing signed."
  (let ((body (make-buf)))
    (buf-string body username)
    (values (packet-id :start :login :out) (buf-octets body))))

(defun chat-message-packet (text &key (timestamp 0) (salt 0))
  "Chat Message, unsigned: the server still shows it, it just cannot be
reported as a signed one could."
  (let ((body (make-buf)))
    (buf-string body text)
    (buf-long body timestamp)
    (buf-long body salt)
    (buf-bool body nil)                 ; no signature
    (buf-varint body 0)                 ; this is the first unsigned message
    (buf-bytes body (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
    (values (packet-id :chat :play :out) (buf-octets body))))

(defun keep-alive-packet (id)
  (let ((body (make-buf)))
    (buf-long body id)
    (values (packet-id :keep-alive :play :out) (buf-octets body))))

(defun confirm-teleport-packet (teleport-id)
  (let ((body (make-buf)))
    (buf-varint body teleport-id)
    (values (packet-id :confirm-teleport :play :out) (buf-octets body))))

(defun set-position-packet (x y z &key (on-ground t))
  (let ((body (make-buf)))
    (buf-double body x)
    (buf-double body y)
    (buf-double body z)
    (buf-bool body on-ground)
    (values (packet-id :position :play :out) (buf-octets body))))

(defun set-rotation-packet (yaw pitch &key (on-ground t))
  (let ((body (make-buf)))
    (buf-float body yaw)
    (buf-float body pitch)
    (buf-bool body on-ground)
    (values (packet-id :rotation :play :out) (buf-octets body))))

;;; --- what the server says -------------------------------------------------------

(defun component-text (value)
  "A chat component -- a string, or an object with text, translate and
extra -- as the one line the game would draw."
  (cond
    ((stringp value) value)
    ((hash-table-p value)
     (concatenate 'string
                  (or (component-text (gethash "text" value)) "")
                  (let ((translate (gethash "translate" value))
                        (with (gethash "with" value)))
                    (if (and translate (vectorp with))
                        (format nil "~{~a~^ ~}" (loop for part across with collect (component-text part)))
                        (or (and translate (string translate)) "")))
                  (let ((extra (gethash "extra" value)))
                    (if (vectorp extra)
                        (format nil "~{~a~}" (loop for part across extra collect (component-text part)))
                        ""))))
    ((vectorp value) (format nil "~{~a~}" (loop for part across value collect (component-text part))))
    ((null value) "")
    (t (princ-to-string value))))

(defun flatten-component (json)
  "JSON -- a decoded chat component -- as plain text."
  (component-text json))

(defun player-chat-said (body)
  "A Player Chat Message => (VALUES SENDER-UUID TEXT). The sender's signature
and the rest of the packet are read past."
  (let ((cur (make-cur body)))
    (let ((uuid (cur-uuid cur)))
      (cur-varint cur)                                     ; message index
      (when (cur-bool cur) (cur-skip cur 256))             ; a signature, when present
      (values uuid (cur-string cur)))))

(defun system-chat-said (body)
  "A System Chat Message => (VALUES TEXT OVERLAY-P)."
  (let ((cur (make-cur body)))
    (values (flatten-component (nlk:decode-json (cur-string cur)))
            (cur-bool cur))))

(defun disconnect-reason (body)
  "A Disconnect packet => the reason the server gives, as one line."
  (flatten-component (nlk:decode-json (cur-string (make-cur body)))))

(defun sync-position-of (body)
  "Synchronize Player Position => (VALUES X Y Z TELEPORT-ID), the spot the
server put the bot at and the id it must be confirmed with."
  (let ((cur (make-cur body)))
    (values (cur-double cur) (cur-double cur) (cur-double cur)
            (progn (cur-float cur) (cur-float cur) (cur-u8 cur) (cur-varint cur)))))

(defun player-info-names (body)
  "Player Info Update => an alist (UUID . NAME) of the entries whose add-player
action is in it, or NIL when the ids of this version's actions do not fit."
  (handler-case
      (let ((cur (make-cur body))
            (names '()))
        (let ((actions (cur-u8 cur))
              (entries (cur-varint cur)))
          (dotimes (i entries names)
            (let ((uuid (cur-uuid cur)))
              (when (logbitp 0 actions)                     ; ADD_PLAYER: the name
                (let ((name (cur-string cur))
                      (properties (cur-varint cur)))
                  (dotimes (j properties)
                    (cur-string cur) (cur-string cur)
                    (when (cur-bool cur) (cur-string cur)))
                  (push (cons uuid name) names)))
              (when (logbitp 1 actions)                     ; INITIALIZE_CHAT
                (when (cur-bool cur)
                  (cur-uuid cur) (cur-long cur)
                  (cur-skip cur (cur-varint cur))           ; public key
                  (cur-skip cur (cur-varint cur))))        ; key signature
              (when (logbitp 2 actions) (cur-varint cur))  ; game mode
              (when (logbitp 3 actions) (cur-bool cur))     ; listed
              (when (logbitp 4 actions) (cur-varint cur))  ; latency
              (when (logbitp 5 actions)                     ; display name
                (when (cur-bool cur) (cur-string cur)))
              (when (logbitp 6 actions) (cur-varint cur))  ; list order
              (when (logbitp 7 actions) (cur-bool cur)))))) ; hat
    (error () nil)))
