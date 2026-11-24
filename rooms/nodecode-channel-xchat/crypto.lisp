;;;; crypto.lisp --- the sealing an X Chat message body is made of.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original delegates every primitive to X's native Chat XDK
;;;; (chatxdk); here they are made from SHA-256, which the core does not
;;;; load and ironclad supplies:
;;;;
;;;;   conversation-key  HKDF-SHA256 over the pair of account ids and the
;;;;                     conversation id: the same key for both sides of a
;;;;                     1:1 conversation, a different one per conversation.
;;;;   seal-text         the plaintext XORed with an HMAC-SHA256 counter-mode
;;;;                     keystream, authenticated by a truncated HMAC tag, and
;;;;                     written as "x1.<nonce>.<body>.<tag>" in hex.
;;;;   open-text         the inverse; NIL on a body whose tag does not hold.
;;;;
;;;; What this is not: the XDK's own key schedule (X25519-wrapped
;;;; conversation keys, Ed25519 signatures, media stream keys). A body sealed
;;;; here is read by a cell here, not by X's own clients -- see NOTICE.

(in-package #:nodecode-channel-xchat)

(defparameter +mac-size+ 16
  "Bytes kept of the tag: enough that a wrong body never opens.")

(defparameter +key-size+ 32)

;;; --- octets ------------------------------------------------------------------

(defun octets-of (string)
  (sb-ext:string-to-octets (or string "") :external-format :utf-8))

(defun string-of (octets)
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun octets (&rest parts)
  "The one octet vector PARTS make, in order."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (dolist (part parts)
      (when part
        (loop for byte across part do (vector-push-extend byte out))))
    (subseq out 0 (length out))))

(defun empty-octets (n &optional (fill 0))
  (make-array n :element-type '(unsigned-byte 8) :initial-element fill))

(defun random-octets (n)
  "N random bytes from the system, or a mixed clock-and-state fallback."
  (handler-case
      (let ((out (empty-octets n)))
        (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
          (read-sequence out in))
        out)
    (error ()
      (let ((state (sb-ext:seed-random-state t)))
        (map '(vector (unsigned-byte 8))
             (lambda (each) (declare (ignore each)) (random 256 state))
             (empty-octets n))))))

;;; --- hashing -----------------------------------------------------------------

(defun digest256 (&rest chunks)
  "The SHA-256 of CHUNKS, as octets."
  (let ((digest (ironclad:make-digest :sha256)))
    (dolist (chunk chunks)
      (when chunk (ironclad:update-digest digest chunk)))
    (ironclad:produce-digest digest)))

(defun hmac256 (key text)
  "HMAC-SHA256 of TEXT under KEY, as octets."
  (let* ((key (if (> (length key) 64) (digest256 key) key))
         (padded (empty-octets 64))
         (inner (empty-octets 64))
         (outer (empty-octets 64)))
    (replace padded key)
    (dotimes (i 64) (setf (aref inner i) (logxor (aref padded i) #x36)
                          (aref outer i) (logxor (aref padded i) #x5c)))
    (digest256 outer (digest256 inner text))))

(defun hkdf-sha256 (ikm salt info length)
  "HKDF-SHA256 (RFC 5869) of IKM, SALT and INFO, LENGTH bytes long."
  (let* ((salt (or salt (empty-octets +key-size+)))
         (prk (hmac256 salt ikm))
         (out (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0))
         (previous (empty-octets 0)))
    (loop for counter from 1
          while (< (fill-pointer out) length)
          do (setf previous (hmac256 prk (octets previous info
                                                   (make-array 1 :element-type '(unsigned-byte 8)
                                                                 :initial-element (logand counter #xff)))))
             (loop for byte across previous do (vector-push-extend byte out)))
    (subseq out 0 length)))

(defun conversation-key (self-id other-id conversation-id)
  "The key a conversation's bodies are sealed under.
Both ends derive the same one: the two account ids in sorted order, the
conversation id as the salt."
  (let* ((ids (sort (mapcar (lambda (each) (or each "")) (list self-id other-id)) #'string<)))
    (hkdf-sha256 (octets-of (format nil "~a|~a" (first ids) (second ids)))
                 (digest256 (octets-of (or conversation-id "")))
                 (octets-of "nodecode-xchat-conversation")
                 +key-size+)))

(defun keystream (key nonce length)
  "LENGTH bytes of HMAC-SHA256 in counter mode under KEY and NONCE."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (loop for counter from 0
          while (< (fill-pointer out) length)
          do (loop for byte across (hmac256 key (octets nonce (make-array 1 :element-type '(unsigned-byte 8)
                                                                               :initial-element (logand counter #xff))))
                   do (vector-push-extend byte out)))
    (subseq out 0 length)))

(defun hex-encode (octets)
  (with-output-to-string (out)
    (loop for byte across octets do (format out "~2,'0x" byte))))

(defun hex-decode (string)
  (let* ((length (floor (length string) 2))
         (out (empty-octets length)))
    (dotimes (i length out)
      (setf (aref out i)
            (parse-integer string :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

;;; --- sealing -----------------------------------------------------------------

(defun tag-of (key nonce body)
  (subseq (hmac256 key (octets nonce body)) 0 +mac-size+))

(defun seal-text (key text)
  "TEXT sealed under KEY: \"x1.<nonce>.<body>.<tag>\", all hex."
  (let* ((nonce (random-octets 16))
         (plain (octets-of text))
         (stream (keystream key nonce (length plain)))
         (body (map '(vector (unsigned-byte 8)) #'logxor plain stream)))
    (format nil "x1.~a.~a.~a"
            (hex-encode nonce) (hex-encode body) (hex-encode (tag-of key nonce body)))))

(defun open-text (key sealed)
  "TEXT out of SEALED under KEY, or NIL when it is not whole."
  (when (and (stringp sealed) (plusp (length sealed)))
    (let ((parts (uiop:split-string sealed :separator ".")))
      (when (and (= 4 (length parts)) (string= (first parts) "x1"))
        (handler-case
            (let* ((nonce (hex-decode (second parts)))
                   (body (hex-decode (third parts)))
                   (tag (hex-decode (fourth parts)))
                   (stream (keystream key nonce (length body))))
              (when (equalp tag (tag-of key nonce body))
                (string-of (map '(vector (unsigned-byte 8)) #'logxor body stream))))
          (error () nil))))))
