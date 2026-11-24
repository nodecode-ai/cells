;;;; join.lisp --- a private invitation, a pending handshake, a separate consent.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Joining is two calls on purpose, as in the plugin: clack_join redeems a
;;;; trusted private invitation into a PENDING pair and never accepts it;
;;;; clack_accept accepts exactly the pair this machine reviewed, and only
;;;; then is the peer enc_capable.

(in-package #:nodecode-clack)

(defvar *expected-inviter* nil "expected_inviter from the section: the trust anchor.")
(defvar *peer-name* nil "peer_name from the section: the name to enroll under.")

(defun parse-invite (link)
  "LINK as (values PATH HANDLE-UUID CLAIM), refusing anything else.
The plugin's shape: <relay>/i/<22 base64url chars>#<32-byte canonical key>."
  (unless (and (stringp link) (<= (length link) 512) (= 1 (count #\# link)))
    (fail "Expected a private /i/ invitation with a fragment"))
  (let* ((hash (position #\# link))
         (head (subseq link 0 hash))
         (claim (subseq link (1+ hash)))
         (uri (quri:uri head))
         (path (quri:uri-path uri)))
    (unless (and (equal (normalize-origin (format nil "~a://~a" (quri:uri-scheme uri) (quri:uri-host uri)))
                        *relay*)
                 (null (quri:uri-query uri))
                 (stringp path)
                 (cl-ppcre:scan "^/i/[A-Za-z0-9_-]{22}$" path))
      (fail "An invitation must match the configured relay and the private /i/ form"))
    (let ((raw (b64u-decode (subseq path 3))))
      (unless (and (= (length raw) 16) (equal (subseq path 3) (b64u-encode raw)))
        (fail "Noncanonical invitation handle"))
      (public-identifier claim)
      (values path (uuid-of-bytes raw) claim))))

(defun uuid-of-bytes (bytes)
  (format nil "~(~a-~a-~a-~a-~a~)"
          (hex-of bytes 0 4) (hex-of bytes 4 6) (hex-of bytes 6 8)
          (hex-of bytes 8 10) (hex-of bytes 10 16)))

(defun join (args)
  (answering
    (let ((link (jstr args "invite_link")))
      (multiple-value-bind (path handle claim) (parse-invite link)
        (let ((expected (or (and *expected-inviter* (public-identifier *expected-inviter*))
                            (fail "Configure expected_inviter with the inviter's independently verified key first"))))
          (let ((metadata (relay-get path :authenticated nil)))
            (unless (and (equal (jstr metadata "mode") "private_invitation")
                         (equal (jstr metadata "inviter_identity") expected))
              (fail "The invitation is unavailable, or its inviter is not the configured one")))
          (multiple-value-bind (identity seed) (load-identity)
            (when (and identity (jstr identity "pending_handshake"))
              (return-from join
                (answer "status" "pending" "handshake_id" (jstr identity "pending_handshake")
                        "inviter_identity" (jstr identity "pending_inviter")
                        "next" "Review, then call clack_accept; nothing is accepted automatically")))
            (let ((fresh nil))
              (unless identity
                (let ((name *peer-name*))
                  (unless (and (stringp name) (plusp (length name)) (<= (length name) 31)
                               (cl-ppcre:scan "^[a-z0-9][a-z0-9_-]{0,30}$" name)
                               (not (uiop:string-prefix-p "guest-" name)))
                    (fail "Configure a valid lowercase peer_name before enrolling"))
                  (multiple-value-setq (identity seed) (create-identity *relay*))
                  (setf (gethash "requested_name" identity) name
                        fresh t)
                  (save-identity identity)))
              (let* ((enrolled (enrolled-p identity))
                     (body (if enrolled
                               (nlk:make-json-object "h" handle "k" claim)
                               (let* ((pub (jstr identity "identity_pubkey"))
                                      (challenge (relay-post "/v1/enroll/challenge"
                                                             (nlk:make-json-object
                                                              "invite_id" handle "k" claim
                                                              "identity_pubkey" pub)
                                                             :authenticated nil)))
                                 (unless (equal (jstr challenge "gate") "invite")
                                   (fail "This relay does not gate enrollment by invitation"))
                                 (nlk:make-json-object
                                  "h" handle "k" claim
                                  "identity_pubkey" pub
                                  "requested_name" (jstr identity "requested_name")
                                  "proof" (nlk:make-json-object
                                           "nonce" (jstr challenge "nonce")
                                           "signature" (b64u-encode
                                                        (ed25519-sign seed (join-octets
                                                                            (b64u-decode (jstr challenge "nonce"))
                                                                            (utf8 handle)
                                                                            (public-key-octets pub))))))))))
                (declare (ignore fresh))
                (let ((result (relay-post "/v1/handshakes/redeem" body :authenticated enrolled)))
                  (unless (and (equal (jstr result "status") "pending")
                               (equal (jstr result "minter_identity") expected))
                    (fail "The redeem answer was not the expected pending pair; reconcile before retrying"))
                  (let ((hid (handshake-id (jstr result "handshake_id"))))
                    (unless enrolled
                      (unless (and (equal (jstr result "peer_name") (jstr identity "requested_name"))
                                   (jstr result "service_token"))
                        (fail "Enrollment name or token mismatch; keep this state and reconcile"))
                      (setf (gethash "peer_name" identity) (jstr result "peer_name")
                            (gethash "service_token" identity) (jstr result "service_token")))
                    (setf (gethash "pending_handshake" identity) hid
                          (gethash "pending_inviter" identity) expected)
                    (save-identity identity)
                    (let ((rows (jlist (relay-get "/v1/handshakes") "handshakes")))
                      (unless (find-if (lambda (row)
                                         (and (equal (or (jstr row "handshake_id") (jstr row "id")) hid)
                                              (equal (jstr row "status") "pending")))
                                       rows)
                        (fail "The join was kept but the pending pair could not be read back")))
                    (answer "status" "pending" "peer_name" (jstr identity "peer_name")
                            "handshake_id" hid "inviter_identity" expected
                            "pending_expires_at" (jget result "pending_expires_at")
                            "next" "Review the inviter and deadline, then call clack_accept")))))))))))

(defun accept (args)
  (answering
    (let ((hid (handshake-id (jstr args "handshake_id"))))
      (multiple-value-bind (identity seed) (load-identity)
        (declare (ignore seed))
        (unless (equal hid (jstr identity "pending_handshake"))
          (fail "Only the locally reviewed pending join may be accepted"))
        (relay-post "/v1/handshakes/accept" (nlk:make-json-object "handshake_id" hid "enc_v" 1))
        (let ((row (find-if (lambda (candidate)
                               (equal (or (jstr candidate "handshake_id") (jstr candidate "id")) hid))
                             (jlist (relay-get "/v1/handshakes") "handshakes"))))
          (unless (and row (equal (jstr row "status") "active")
                       (eql 1 (jint row "enc_capable"))
                       (equal (jstr row "peer_identity") (jstr identity "pending_inviter")))
            (fail "The acceptance was not verified active and encrypted; the pending state is kept")))
        (remhash "pending_handshake" identity)
        (remhash "pending_inviter" identity)
        (save-identity identity)
        (answer "handshake_id" hid "status" "active" "enc_capable" t)))))
