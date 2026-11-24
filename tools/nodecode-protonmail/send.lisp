;;;; send.lisp --- encrypting and signing a message the way Proton's send API wants it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Proton does not take a finished PGP message per recipient. It takes the
;;;; body once, encrypted under a session key (the *data packet*), plus that
;;;; session key wrapped for every recipient inside Proton (a *key packet*)
;;;; and, for recipients outside Proton, the session key in the clear so
;;;; Proton's servers can deliver ordinary mail. So the body is built once,
;;;; encrypted once by gpg to this account and to each inside recipient, and
;;;; the resulting packet stream is taken apart -- which is what
;;;; SPLIT-PACKETS is for. The same shape as the original's send.py, without
;;;; a PGP library: gpg does the cryptography, this file does the framing.

(in-package #:nodecode-protonmail)

(defparameter +tag-pkesk+ 1)
(defparameter +tag-seipd+ 18)
(defparameter +scheme-internal+ 1)
(defparameter +scheme-clear+ 4)
(defparameter +session-key-algorithm+ "aes256")

;;; --- octets ---------------------------------------------------------------------

(defun octets (&rest parts)
  "PARTS, each a string or an octet vector, as one octet vector."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (part parts)
      (let ((bytes (if (stringp part)
                       (map '(vector (unsigned-byte 8)) #'char-code part)
                       part)))
        (loop for byte across bytes do (vector-push-extend byte out))))
    (coerce out '(vector (unsigned-byte 8)))))

(defun u8 (value) (octets (vector (ldb (byte 8 0) value))))

(defun u16 (value)
  (octets (vector (ldb (byte 8 8) value) (ldb (byte 8 0) value))))

(defun u32 (value)
  (octets (vector (ldb (byte 8 24) value) (ldb (byte 8 16) value)
                  (ldb (byte 8 8) value) (ldb (byte 8 0) value))))

(defun packet (tag body)
  "BODY as a new-format OpenPGP packet of TAG, with a two- or five-octet length."
  (let ((length (length body))
        (first (logior #xC0 tag)))
    (cond ((< length 192) (octets (vector first) (u8 length) body))
          ((< length 8384)
           (let ((rest (- length 192)))
             (octets (vector first (+ 192 (ash rest -8)) (logand rest 255)) body)))
          (t (octets (vector first 255) (u32 length) body)))))

(defun split-packets (blob)
  "BLOB's OpenPGP packets as a list of (TAG . WHOLE-PACKET-OCTETS)."
  (let ((packets '())
        (offset 0)
        (length (length blob)))
    (loop while (< offset length)
          do (let* ((start offset)
                    (header (aref blob offset))
                    (tag 0)
                    (size 0))
               (unless (logtest header #x80) (fail "this is not an OpenPGP packet stream"))
               (incf offset)
               (if (logtest header #x40)
                   (let ((first (aref blob offset)))
                     (setf tag (logand header #x3F))
                     (incf offset)
                     (cond ((< first 192) (setf size first))
                           ((< first 224) (setf size (+ (* (- first 192) 256)
                                                        (aref blob offset) 192))
                                          (incf offset))
                           ((= first 255) (setf size (+ (* (aref blob offset) 16777216)
                                                        (* (aref blob (+ offset 1)) 65536)
                                                        (* (aref blob (+ offset 2)) 256)
                                                        (aref blob (+ offset 3))))
                                          (incf offset 4))
                           (t (fail "a partial-length OpenPGP packet is not supported"))))
                   (let ((size-type (logand header #x03)))
                     (setf tag (logand (ash header -2) #x0F))
                     (case size-type
                       (0 (setf size (aref blob offset)) (incf offset))
                       (1 (setf size (+ (* (aref blob offset) 256) (aref blob (+ offset 1))))
                          (incf offset 2))
                       (2 (setf size (+ (* (aref blob offset) 16777216)
                                        (* (aref blob (+ offset 1)) 65536)
                                        (* (aref blob (+ offset 2)) 256)
                                        (aref blob (+ offset 3))))
                          (incf offset 4))
                       (t (fail "an indeterminate-length OpenPGP packet is not supported")))))
               (incf offset size)
               (when (> offset length) (fail "a truncated OpenPGP packet"))
               (push (cons tag (subseq blob start offset)) packets)))
    (nreverse packets)))

(defun packet-with-tag (blob tag)
  "The first packet of BLOB carrying TAG, whole."
  (let ((found (find tag (split-packets blob) :key #'car)))
    (unless found (fail "the encrypted message has no packet of tag ~a" tag))
    (cdr found)))

(defun armored (blob)
  "BLOB as an ASCII-armoured PGP message, the way Proton stores a body."
  (let ((text (cl-base64:usb8-array-to-base64-string blob)))
    (with-output-to-string (out)
      (format out "-----BEGIN PGP MESSAGE-----~%~%")
      (loop for start from 0 below (length text) by 64
            do (format out "~a~%" (subseq text start (min (length text) (+ start 64)))))
      (format out "=~a~%-----END PGP MESSAGE-----~%"
              (subseq (cl-base64:usb8-array-to-base64-string
                       (ironclad:digest-sequence
                        :sha1 (map '(vector (unsigned-byte 8)) #'char-code
                                    (remove #\Newline text))))
                      0 4)))))

(defun literal-packet (text)
  "TEXT as an OpenPGP literal data packet of UTF-8 format."
  (packet 11 (octets (vector (char-code #\u)) (u8 0) (u32 0) text)))

;;; --- gpg, for the two things it is better at -----------------------------------

(defun armored-keys-named (armored-key)
  "Import ARMORED-KEY into the throwaway keyring: gpg can only encrypt to a key it holds."
  (run-gpg (list "--import") :input (map '(vector (unsigned-byte 8)) #'char-code armored-key)))

(defun detach-sign (text)
  "TEXT signed by the account's own key, as one signature packet."
  (multiple-value-bind (out status)
      (run-gpg (list "--detach-sign" "--no-armor" "--digest-algo" "SHA256")
               :input (map '(vector (unsigned-byte 8)) #'char-code text)
               :password (session-passphrase (ensure-session)))
    (unless (and (integerp status) (zerop status))
      (fail "gpg could not sign the message: ~a" (nlk:one-line (or out ""))))
    out))

(defun encrypt-packets (inner recipients)
  "INNER (a packet stream) encrypted under one session key to RECIPIENTS, in order.
=> the packet stream gpg wrote: one key packet per recipient, then the data packet."
  (multiple-value-bind (out status)
      (run-gpg (append (list "--encrypt" "--no-literal" "--no-armor" "--trust-model" "always")
                       (loop for recipient in recipients append (list "-r" recipient)))
               :input inner)
    (unless (and (integerp status) (zerop status))
      (fail "gpg could not encrypt the message: ~a" (nlk:one-line (or out ""))))
    (map '(vector (unsigned-byte 8)) #'char-code out)))

(defun session-key-of (armored-message)
  "The session key ARMORED-MESSAGE was encrypted under, as octets.
gpg prints it when it decrypts the copy addressed to this account; Proton
needs it in the clear for the recipients outside Proton."
  (multiple-value-bind (out status)
      (run-gpg (list "--show-session-key" "--decrypt")
               :input (map '(vector (unsigned-byte 8)) #'char-code armored-message)
               :password (session-passphrase (ensure-session)))
    (unless (and (integerp status) (zerop status))
      (fail "gpg could not read the session key back: ~a" (nlk:one-line (or out ""))))
    (let* ((head "session key: '")
           (at (search head out))
           (open (and at (+ at (length head))))
           (close (and open (position #\' out :start open))))
      (unless close (fail "gpg did not name the session key"))
      (let ((named (subseq out open close)))
        ;; gpg names it "ALGORITHM:HEX"; the octets are what Proton wants.
        (hex-octets (subseq named (1+ (position #\: named))))))))

(defun hex-octets (hex)
  "HEX as octets."
  (let ((out (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length out) out)
      (setf (aref out i)
            (parse-integer (subseq hex (* 2 i) (+ 2 (* 2 i))) :radix 16)))))

;;; --- the pieces Proton's send call is made of ------------------------------------

(defstruct (send-material (:constructor make-send-material
                                          (armored data-packet session-key key-packets)))
  "One encrypted body, ready to be addressed to anyone."
  armored                        ; what the draft stores, encrypted to this account
  data-packet                    ; the body alone, for the send packages
  (session-key nil)              ; octets, or NIL when nobody is outside Proton
  key-packets)                   ; (ADDRESS . PKESK-OCTETS), one per inside recipient

(defun build-material (text addresses)
  "TEXT carved into the pieces Proton's send call wants.

ADDRESSES are the recipients inside Proton: gpg wraps the one session key
for this account and for each of them, in that order, and the packets come
back in the same order, so the first key packet is this account's and the
rest line up with ADDRESSES one for one."
  (let* ((self (sending-address))
         (own (json self "Email"))
         (whole (encrypt-packets (octets (literal-packet text) (detach-sign text))
                                 (cons own addresses)))
         (kept (remove +tag-seipd+ (split-packets whole) :key #'car))
         (body (packet-with-tag whole +tag-seipd+)))
    (make-send-material (armored whole)
                        body
                        (when (plusp (length addresses)) (session-key-of (armored whole)))
                        (loop for address in addresses
                              for packet in (rest kept)
                              collect (cons address (cdr packet))))))

(defun recipient-inside-p (address)
  "True when ADDRESS is a Proton address, whose public key Proton hands out."
  (let* ((answer (request "GET" (format nil "/core/v4/keys?Email=~a" (url-encode address))
                          :authenticated-p t))
         (keys (jlist answer "Keys")))
    (when (and (eql 1 (json answer "RecipientType" :integer)) (plusp (length keys)))
      (json (aref keys 0) "PublicKey"))))
