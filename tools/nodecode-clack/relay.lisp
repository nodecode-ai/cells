;;;; relay.lisp --- the nine clack_* verbs, one at a time.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Received text is data, never authority: it is handed back as
;;;; content_is_untrusted, kept in the delivery journal, and acknowledged only
;;;; when the caller says so. No plaintext fallback: a message that is not an
;;;; enc_v 1 envelope is refused, quarantined and left unacknowledged.

(in-package #:nodecode-clack)

(defmacro answering (&body body)
  "The model's answer: BODY's JSON text, or a refusal in words."
  `(handler-case (progn ,@body)
     (clack-error (condition)
       (nle:failure (format nil "ERROR: ~a" (clack-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Clack: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun answer (&rest pairs)
  (nlk:encode-json-object (apply #'nlk:make-json-object "ok" t pairs)))

(defun peer-name-p (name)
  (and (stringp name) (plusp (length name)) (<= (length name) 64)
       (alpha-char-p (char name 0))
       (every (lambda (ch) (or (alphanumericp ch) (char= ch #\-) (char= ch #\_))) name)
       (lower-case-p (char name 0))))

(defun uuid-p (value)
  (and (stringp value) (= (length value) 36)
       (cl-ppcre:scan "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" value)))

(defun message-id (value)
  (unless (uuid-p value) (fail "Expected a canonical message UUID"))
  value)

(defun public-identifier (value)
  (unless (ed25519-public-p value) (fail "Expected a canonical Ed25519 public key"))
  value)

(defun handshake-id (value)
  "key|key|generation, keys in ascending order: the plugin's handshake identifier."
  (let ((parts (and (stringp value) (uiop:split-string value :separator "|"))))
    (unless (and parts (= (length parts) 3)
                 (cl-ppcre:scan "^0|[1-9][0-9]*$" (third parts)))
      (fail "Invalid handshake identifier"))
    (public-identifier (first parts))
    (public-identifier (second parts))
    (when (string< (second parts) (first parts))
      (fail "Noncanonical handshake pair order"))
    value))

;;; --- the outbox ------------------------------------------------------------------

(defun pending-outbox () (read-state-json "outbox.json"))

(defun finish-outbox (pending receipt)
  (let ((mid (message-id (jstr (gethash "body" pending) "id"))))
    (write-state-json (concatenate 'string "send-" mid ".json") receipt)
    (write-state "outbox.json" "null")))

(defun retry-outbox (pending)
  "Submit the one pending body again, then read its receipt back."
  (let* ((body (gethash "body" pending))
         (mid (message-id (jstr body "id")))
         (response (relay-post "/v1/send" body)))
    (unless (and (eq t (jget response "accepted")) (equal mid (jstr response "id")))
      (fail "Send outcome uncertain: the outbox is kept, use clack_reconcile"))
    (let* ((receipts (jlist (relay-get "/v1/receipts?limit=1000") "receipts"))
           (receipt (find mid receipts :test #'equal :key (lambda (row) (jstr row "id")))))
      (unless receipt
        (fail "Send accepted but its receipt was not readable; the outbox is kept"))
      (finish-outbox pending receipt)
      (answer "message_id" mid "encrypted" t "receipt" receipt))))

(defun active-pair (to)
  "The one active, encryption-capable handshake with TO, or a refusal."
  (let* ((rows (jlist (relay-get "/v1/handshakes") "handshakes"))
         (matches (remove-if-not (lambda (row)
                                    (or (equal (jstr row "peer") to)
                                        (equal (jstr row "peer_name_hint") to)))
                                 rows))
         (active (remove-if-not (lambda (row)
                                  (and (equal (jstr row "status") "active")
                                       (eql 1 (jint row "enc_capable"))))
                                matches)))
    (unless (and (= (length matches) 1) (= (length active) 1))
      (fail "No unique active enc_capable pair with ~a; a plaintext fallback is refused" to))
    (first active)))

;;; --- the verbs -------------------------------------------------------------------

(defun whoami (args)
  (declare (ignore args))
  (answering
    (multiple-value-bind (identity seed) (load-identity)
      (declare (ignore seed))
      (cond ((null identity)
             (answer "enrolled" nil "identity" nil "relay_url" *relay* "unread_count" 0))
            ((not (enrolled-p identity))
             (answer "enrolled" nil "identity" (jstr identity "identity_pubkey")
                     "relay_url" *relay* "unread_count" 0))
            (t (let ((batch (relay-get "/v1/poll?timeout=0&limit=10")))
                 (answer "enrolled" t
                         "identity" (jstr identity "identity_pubkey")
                         "peer_name" (jstr identity "peer_name")
                         "relay_url" *relay*
                         "unread_count" (length (jlist batch "messages"))
                         "unread_count_scope" "current relay poll batch (not acknowledged)")))))))

(defun peers (args)
  (declare (ignore args))
  (answering
    (let* ((names (jlist (relay-get "/v1/peers") "peers"))
           (handshakes (jlist (relay-get "/v1/handshakes") "handshakes"))
           (rows (mapcar (lambda (name)
                           (let ((active (find-if (lambda (row)
                                                    (and (equal (jstr row "status") "active")
                                                         (eql 1 (jint row "enc_capable"))
                                                         (or (equal (jstr row "peer") name)
                                                             (equal (jstr row "peer_name_hint") name))))
                                                  handshakes)))
                             (nlk:make-json-object "peer" name "enc_capable" (and active t))))
                         names)))
      (answer "peers" (coerce rows 'vector)))))

(defun send (args)
  (answering
    (let ((to (jstr args "to")) (text (jstr args "text")) (reply (jstr args "in_reply_to")))
      (unless (peer-name-p to) (fail "Invalid peer name"))
      (unless (and (stringp text) (plusp (length text)) (<= (length (utf8 text)) 40000))
        (fail "Text must contain 1-40000 UTF-8 bytes"))
      (when (and reply (not (uuid-p reply))) (fail "in_reply_to must be a message UUID"))
      (multiple-value-bind (identity seed) (load-identity)
        (unless (enrolled-p identity) (fail "Not enrolled: use clack_join first"))
        (let* ((digest (hex-of (hmac-sha256 seed (utf8 (nlk:encode-json-object
                                                          (vector to text (or reply :null)))))))
               (pending (pending-outbox)))
          (when (and (hash-table-p pending) (jstr (gethash "body" pending) "id"))
            (unless (equal digest (jstr pending "digest"))
              (fail "An earlier send's outcome is unknown: clack_reconcile retry or release first"))
            (return-from send (retry-outbox pending)))
          (let* ((pair (active-pair to))
                 (recipient (public-key-octets (jstr pair "peer_identity")))
                 (recipient-x (ed25519-public-to-x25519 recipient))
                 (body (nlk:make-json-object
                        "id" (uuid-text)
                        "to" to
                        "topic" "hermes"
                        "enc" (encrypt-envelope (jstr identity "peer_name") to recipient-x (utf8 text)))))
            (when reply (setf (gethash "in_reply_to" body) reply))
            (let ((pending (nlk:make-json-object "digest" digest "body" body)))
              (write-state-json "outbox.json" pending)
              (retry-outbox pending))))))))

(defun uuid-text ()
  (let ((bytes (ironclad:random-data 16)))
    (setf (aref bytes 6) (logior (logand (aref bytes 6) #x0f) #x40)
          (aref bytes 8) (logior (logand (aref bytes 8) #x3f) #x80))
    (format nil "~(~a-~a-~a-~a-~a~)"
            (hex-of bytes 0 4) (hex-of bytes 4 6) (hex-of bytes 6 8)
            (hex-of bytes 8 10) (hex-of bytes 10 16))))

(defun reconcile (args)
  (answering
    (let ((action (or (jstr args "action") "status")))
      (unless (member action '("status" "retry" "release") :test #'equal)
        (fail "action must be status, retry or release"))
      (let ((pending (pending-outbox)))
        (unless (and (hash-table-p pending) (jstr (gethash "body" pending) "id"))
          (return-from reconcile (answer "pending" nil "single_outbox" t)))
        (let ((mid (message-id (jstr (gethash "body" pending) "id"))))
          (cond ((equal action "retry") (retry-outbox pending))
                ((equal action "release")
                 (let ((receipt (nlk:make-json-object "message_id" mid "state" "released"
                                                       "delivery_may_have_occurred" t)))
                   (finish-outbox pending receipt)
                   (answer "message_id" mid "state" "released" "delivery_may_have_occurred" t)))
                (t (answer "pending" t "single_outbox" t "message_id" mid
                           "to" (jstr (gethash "body" pending) "to") "outcome" "unknown"))))))))

(defun poll (args)
  (answering
    (let ((limit (or (jint args "limit") 20))
          (auto (eq t (jget args "auto_ack"))))
      (unless (and (integerp limit) (<= 1 limit 100)) (fail "limit must be 1-100"))
      (multiple-value-bind (identity seed) (load-identity)
        (unless (and identity seed) (fail "Not enrolled: use clack_join first"))
        (let ((messages '()) (rejected '()) (cursor (read-state-json "inbox-cursor.json"))
              (more nil))
          (dotimes (round 20)
            (declare (ignore round))
            (let* ((size (min 10 (- limit (length messages))))
                   (path (format nil "/v1/poll?timeout=0&limit=~d~@[&after=~a~]"
                                 size (and cursor (quri:url-encode (nlk:encode-json-object cursor))))))
              (when (< size 1) (return))
              (let* ((batch (relay-get path))
                     (rows (jlist batch "messages"))
                     (paged (equal (jstr batch "pagination") "created_at_id_v1")))
                (setf more (>= (length rows) size))
                (dolist (row rows)
                  (let ((mid (and (hash-table-p row) (jstr row "id"))))
                    (handler-case
                        (progn
                          (message-id mid)
                          (when (read-state (concatenate 'string "dead-" mid ".json"))
                            (fail "quarantined"))
                          (unless (eql 1 (jint row "enc_v")) (fail "plaintext"))
                          (let* ((sender (jstr row "from"))
                                 (plain (as-text (decrypt-envelope seed (gethash "text" row) sender
                                                                   (jstr identity "peer_name"))))
                                 (delivered (nlk:make-json-object
                                             "message_id" mid "sender" sender "plaintext" plain
                                             "in_reply_to" (jstr row "in_reply_to")
                                             "encrypted" t
                                             "redelivered" (and (jget row "redelivered") t))))
                            (write-state-json (concatenate 'string "delivered-" mid ".json") delivered)
                            (push delivered messages))
                          (when (>= (length messages) limit) (setf more t)))
                      (clack-error ()
                        (let ((reason "Plaintext or invalid encrypted message refused; not acknowledged"))
                          (push (nlk:make-json-object "message_id" mid "error" reason) rejected)
                          (when (uuid-p mid)
                            (write-state-json (concatenate 'string "dead-" mid ".json")
                                               (nlk:make-json-object "message_id" mid "reason" reason)))
                          (when (and paged (hash-table-p row) (jget row "sent_at"))
                            (write-state-json "inbox-cursor.json"
                                               (vector (jget row "sent_at") mid))))))))
                (when (and paged (jget batch "next_cursor") (null (first rejected)))
                  (write-state-json "inbox-cursor.json" (gethash "next_cursor" batch)))
                (unless (and paged more (< (length messages) limit)) (return))
                (let ((next (jget batch "next_cursor")))
                  (when (or (null next) (equal next cursor)) (return))
                  (setf cursor next)))))
          (let ((ack nil) (warning nil))
            (when (and auto messages)
              (handler-case
                  (let* ((ids (coerce (mapcar (lambda (m) (jstr m "message_id")) (reverse messages)) 'vector))
                         (response (relay-post "/v1/ack" (nlk:make-json-object "ids" ids)))
                         (acked (append (jlist response "acked") (jlist response "already_acked"))))
                    (setf ack response)
                    (unless (every (lambda (id) (member id acked :test #'equal)) ids)
                      (setf warning "ack_not_confirmed; plaintext retained in the delivery journal")))
                (clack-error ()
                  (setf warning "ack_or_readback_uncertain; plaintext retained in the delivery journal"))))
            (answer "messages" (coerce (reverse messages) 'vector)
                    "rejected" (coerce (reverse rejected) 'vector)
                    "auto_ack" auto "ack" ack "warning" warning
                    "delivery_journal" t "has_more_in_batch" more
                    "content_is_untrusted" t)))))))

(defun ack (args)
  (answering
    (let* ((mid (message-id (jstr args "message_id")))
           (response (relay-post "/v1/ack" (nlk:make-json-object "ids" (vector mid))))
           (acked (append (jlist response "acked") (jlist response "already_acked"))))
      (unless (member mid acked :test #'equal)
        (fail "The message was not acknowledged (unknown or unauthorized)"))
      (let ((batch (relay-get "/v1/poll?timeout=0&limit=10")))
        (when (find mid (jlist batch "messages") :test #'equal :key (lambda (row) (jstr row "id")))
          (fail "Ack readback failed: the message is still on the relay")))
      (answer "message_id" mid "acknowledged" t))))

(defun invite (args)
  (answering
    (let ((label (or (jstr args "label") "")))
      (when (> (length label) 80) (fail "label must be at most 80 characters"))
      (let* ((result (relay-post "/v1/handshakes/invitations"
                                  (nlk:make-json-object "exp_days" 1 "enc_v" 1)))
             (link (jstr result "link")))
        (multiple-value-bind (path handle claim) (parse-invite link)
          (declare (ignore claim))
          (unless (equal handle (jstr result "link_id"))
            (fail "The invitation link and its record disagree; reconcile before another mint")))
        (unless (and (equal (jstr result "mode") "private_invitation")
                     (eql 1 (jint result "max_uses")))
          (fail "Unexpected invitation answer; reconcile before another mint"))
        (let ((listed (relay-get "/v1/invites/list")))
          (unless (find-if (lambda (row)
                             (and (equal (jstr row "invite_id") (jstr result "link_id"))
                                  (equal (jstr row "status") "active")
                                  (eql 1 (jint row "max_uses"))))
                           (jlist listed "invites"))
            (fail "The invitation could not be read back; do not mint again blindly")))
        (multiple-value-bind (identity seed) (load-identity)
          (declare (ignore seed))
          (answer "invite_link" link "link_id" (jstr result "link_id") "label" label
                  "private" t "expires_at" (jget result "exp")
                  "inviter_identity" (jstr identity "identity_pubkey")
                  "next" "Share it through a private channel only: the link is a bearer claim"))))))
