;;;; envelope.lisp --- the two curves, the message envelope, and the relay pin.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; An identity is one 32-byte Ed25519 seed. The same seed signs every relay
;;;; request and, through the Ed25519 -> X25519 conversion RFC 7748 describes,
;;;; opens the bodies peers send: exactly the plugin's
;;;; crypto_sign_ed25519_sk_to_curve25519 posture, no second key to distribute.

(in-package #:nodecode-clack)

(defconstant +p25519+ (- (ash 1 255) 19)
  "The field the Montgomery and Edwards forms of Curve25519 share.")

(defparameter +x25519-base+ (join-octets (make-octets 1 :fill 9) (make-octets 31))
  "X25519's base point, u = 9.")

;;; --- keys ------------------------------------------------------------------------

(defun x25519 (scalar point)
  "X25519(SCALAR, POINT), both 32 bytes: ironclad's ladder, clamped as RFC 7748 says."
  (unless (and (= (length scalar) 32) (= (length point) 32))
    (fail "X25519 takes two 32-byte values"))
  (coerce (ironclad:diffie-hellman
           (ironclad:make-private-key :curve25519 :x (coerce scalar 'octets))
           (ironclad:make-public-key :curve25519 :y (coerce point 'octets)))
          'octets))

(defun x25519-public (scalar)
  (x25519 scalar +x25519-base+))

(defun ed25519-public (seed)
  "The 32-byte public key of a 32-byte SEED."
  (let ((key (ironclad:make-private-key :ed25519 :x (coerce seed 'octets))))
    (coerce (getf (ironclad:destructure-private-key key) :y) 'octets)))

(defun ed25519-sign (seed message)
  "The 64-byte signature of MESSAGE under SEED."
  (coerce (ironclad:sign-message (ironclad:make-private-key :ed25519 :x (coerce seed 'octets))
                                 (coerce message 'octets))
          'octets))

(defun ed25519-public-p (value)
  "Whether VALUE is a canonical 32-byte Ed25519 public key in base64url."
  (and (stringp value)
       (= (length value) 43)
       (handler-case (= 32 (length (b64u-decode value))) (error () nil))
       (equal value (b64u-encode (b64u-decode value)))))

(defun public-key-octets (value)
  (unless (ed25519-public-p value)
    (fail "Expected a canonical Ed25519 public key"))
  (b64u-decode value))

(defun x25519-private (seed)
  "The X25519 scalar a 32-byte Ed25519 SEED converts to: SHA-512(seed)[0,32], clamped."
  (let* ((hash (sha512 seed))
         (scalar (subseq hash 0 32)))
    (setf (aref scalar 0) (logand (aref scalar 0) #xf8)
          (aref scalar 31) (logior (logand (aref scalar 31) #x7f) #x40))
    scalar))

(defun ed25519-public-to-x25519 (public)
  "u = (1 + y) / (1 - y), y the Edwards coordinate PUBLIC (32 bytes) encodes."
  (let* ((y (logand (le-int public) (- (ash 1 255) 1))))
    (when (= (mod y +p25519+) 1)
      (fail "An Ed25519 public key with y = 1 has no X25519 counterpart"))
    (int-le (mod (* (mod (1+ y) +p25519+)
                    (mod-expt (mod (- 1 y) +p25519+) (- +p25519+ 2) +p25519+))
                 +p25519+)
            32)))

(defun key-id (x25519-public)
  "The 8-byte kid an envelope names: SHA256 of the recipient's X25519 public key."
  (subseq (sha256 x25519-public) 0 8))

;;; --- the envelope -----------------------------------------------------------------

(defun envelope-info (sender recipient)
  (join-octets (utf8 +hkdf-info-prefix+) (utf8 sender) (utf8 recipient)))

(defun encrypt-envelope (sender recipient recipient-x25519-public plaintext)
  "A v1 envelope object for PLAINTEXT (octets), fresh ephemeral per message."
  (unless (= (length recipient-x25519-public) 32)
    (fail "Recipient X25519 public key must be 32 bytes"))
  (let* ((ephemeral (ironclad:random-data 32))
         (ephemeral-public (x25519-public ephemeral))
         (shared (x25519 ephemeral recipient-x25519-public)))
    (when (every #'zerop shared)
      (fail "Refusing to encrypt: X25519 shared secret is all zero"))
    (let* ((key (hkdf-sha256 shared (envelope-info sender recipient)))
           (nonce (ironclad:random-data 12)))
      (multiple-value-bind (ciphertext tag) (aead-encrypt key nonce plaintext)
        (nlk:make-json-object
         "v" +enc-version+
         "alg" +enc-alg+
         "eph" (b64u-encode ephemeral-public)
         "nonce" (b64u-encode nonce)
         "kid" (b64u-encode (key-id recipient-x25519-public))
         "ct" (b64u-encode (join-octets ciphertext tag)))))))

(defun envelope-problem (envelope)
  "What is wrong with ENVELOPE's shape, or NIL: the relay's own check."
  (cond ((not (hash-table-p envelope)) "envelope must be an object")
        ((not (eql 1 (gethash "v" envelope))) "unsupported envelope version")
        ((not (equal +enc-alg+ (gethash "alg" envelope))) "unsupported algorithm")
        (t (handler-case
               (let ((ephemeral (b64u-decode (gethash "eph" envelope)))
                     (nonce (b64u-decode (gethash "nonce" envelope)))
                     (kid (b64u-decode (gethash "kid" envelope)))
                     (body (b64u-decode (gethash "ct" envelope))))
                 (cond ((/= (length ephemeral) 32) "bad ephemeral key length")
                       ((/= (length nonce) 12) "bad nonce length")
                       ((/= (length kid) 8) "bad key id length")
                       ((< (length body) 16) "bad ciphertext length")
                       (t nil)))
             (error () "envelope fields must be base64url strings")))))

(defun decrypt-envelope (seed envelope sender recipient)
  "PLAINTEXT octets of ENVELOPE, opened with the recipient's 32-byte SEED."
  (when (hash-table-p envelope)
    (let ((problem (envelope-problem envelope)))
      (when problem (fail "~a" problem))))
  (let* ((private (x25519-private seed))
         (public (x25519-public private)))
    (unless (equal (b64u-encode (key-id public)) (gethash "kid" envelope))
      (fail "Envelope was built for another key"))
    (let* ((ephemeral (b64u-decode (gethash "eph" envelope)))
           (shared (x25519 private ephemeral)))
      (when (every #'zerop shared)
        (fail "Refusing to decrypt: X25519 shared secret is all zero"))
      (let* ((key (hkdf-sha256 shared (envelope-info sender recipient)))
             (nonce (b64u-decode (gethash "nonce" envelope)))
             (body (b64u-decode (gethash "ct" envelope)))
             (split (- (length body) 16)))
        (aead-decrypt key nonce (subseq body 0 split) (subseq body split))))))

;;; --- the relay's own key ----------------------------------------------------------

(defun relay-key (value)
  "A relay public key as (values N E) integers from hex n and e, or a refusal.
Exactly what the plugin's rsa_public_key accepts: 2048 bits, odd, e = 65537."
  (let* ((n (and (stringp (getf value :n)) (ignore-errors (parse-integer (getf value :n) :radix 16))))
         (e (and (stringp (getf value :e)) (ignore-errors (parse-integer (getf value :e) :radix 16)))))
    (unless (and n e (= (integer-length n) 2048) (oddp n) (= e 65537))
      (fail "Invalid RSA-2048 relay public key"))
    (values n e)))

(defparameter +sha256-digest-info+ "3031300d060960864801650304020105000420"
  "The DER prefix of a SHA-256 DigestInfo, as the plugin spells it out.")

(defun rsa-pkcs1-em (message &optional (size 256))
  "The encoded message RSASSA-PKCS1-v1_5 SHA-256 signs: 00 01 FF.. 00 DigestInfo(sha256)."
  (let ((digest (octets-of-hex (concatenate 'string +sha256-digest-info+
                                             (hex-of (sha256 message))))))
    (join-octets (make-octets 1 :fill 0) (make-octets 1 :fill 1)
                 (make-octets (- size (length digest) 3) :fill #xff)
                 (make-octets 1) digest)))

(defun rsa-verify (n e signature message)
  "Whether SIGNATURE (256 bytes) is N,E's RSASSA-PKCS1-v1_5 SHA-256 signature of MESSAGE."
  (and (= (length signature) 256)
       (< (le-int-be signature) n)
       (equalp (rsa-pkcs1-em message)
               (int-be (mod-expt (le-int-be signature) e n) 256))))

(defun le-int-be (octets)
  "OCTETS read as a big-endian integer (the relay's signature is big-endian)."
  (let ((value 0))
    (loop for byte across octets do (setf value (logior (ash value 8) byte)))
    value))

(defun int-be (value length)
  (let ((out (make-octets length))
        (value (mod value (ash 1 (* 8 length)))))
    (loop for i from 0 below length
          do (setf (aref out (- length 1 i)) (ldb (byte 8 (* 8 i)) value)))
    out))

(defun b64-standard-decode (string)
  "Standard, padded base64 — how the relay signs its identity proof."
  (b64u-decode (substitute #\- #\+ (substitute #\_ #\/ string))))
