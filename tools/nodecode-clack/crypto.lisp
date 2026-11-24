;;;; crypto.lisp --- the cryptography the Clack envelope and the relay pin need.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every line of it is readable: no table, no blob, nothing decoded and run.
;;;; Ironclad carries the elliptic curves and the two SHA digests (X25519,
;;;; Ed25519, SHA-256, SHA-512); ChaCha20, Poly1305, HKDF-SHA256, HMAC-SHA256
;;;; and the RSASSA-PKCS1-v1_5 relay-pin check are here, as the original
;;;; plugin carried them in vendor/e2ee.py and with the same shapes:
;;;;   envelope {"v":1,"alg":"x25519-chacha20poly1305","eph":..,"nonce":..,
;;;;             "kid":..,"ct":<ciphertext||16-byte tag>}
;;;;   kid = first 8 bytes of SHA256(recipient X25519 public key)
;;;;   key = HKDF-SHA256(X25519(eph, recipient), "clack-enc-v1"|sender|recipient)

(in-package #:nodecode-clack)

;;; --- octets, hex, base64url -------------------------------------------------------

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun utf8 (string)
  (sb-ext:string-to-octets (or string "") :external-format :utf-8))

(defun as-text (octets)
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
    (error () (fail "Received bytes are not UTF-8"))))

(defun make-octets (length &key (fill 0))
  (make-array length :element-type '(unsigned-byte 8) :initial-element fill))

(defun join-octets (&rest parts)
  (let* ((total (reduce #'+ parts :key #'length :initial-value 0))
         (out (make-octets total))
         (at 0))
    (dolist (part parts out)
      (replace out part :start1 at)
      (incf at (length part)))))

(defun hex-of (octets &optional (start 0) (end (length octets)))
  (with-output-to-string (out)
    (loop for i from start below end
          do (format out "~2,'0x" (aref octets i)))))

(defun octets-of-hex (string)
  "The octets a hex STRING names; a bad one is one of the cell's own refusals."
  (unless (and (stringp string) (evenp (length string))
               (every (lambda (ch) (digit-char-p ch 16)) string))
    (fail "Expected hex text"))
  (let ((out (make-octets (/ (length string) 2))))
    (loop for i from 0 below (length out)
          do (setf (aref out i) (parse-integer string :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))
    out))

(defun le-int (octets &optional (start 0) (end (length octets)))
  "OCTETS[START,END) read as a little-endian integer."
  (let ((value 0))
    (loop for i from (1- end) downto start
          do (setf value (logior (ash value 8) (aref octets i))))
    value))

(defun int-le (value length)
  (let ((out (make-octets length))
        (value (mod value (ash 1 (* 8 length)))))
    (loop for i from 0 below length
          do (setf (aref out i) (ldb (byte 8 (* 8 i)) value)))
    out))

(defparameter +b64u+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "base64url, unpadded: the plugin's one wire encoding.")

(defun b64u-encode (octets)
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 3
          for rest = (- (length octets) i)
          do (let* ((b0 (aref octets i))
                    (b1 (if (> rest 1) (aref octets (1+ i)) 0))
                    (b2 (if (> rest 2) (aref octets (+ i 2)) 0))
                    (n (logior (ash b0 16) (ash b1 8) b2)))
               (write-char (char +b64u+ (ldb (byte 6 18) n)) out)
               (write-char (char +b64u+ (ldb (byte 6 12) n)) out)
               (when (> rest 1) (write-char (char +b64u+ (ldb (byte 6 6) n)) out))
               (when (> rest 2) (write-char (char +b64u+ (ldb (byte 6 0) n)) out))))))

(defun b64u-decode (string)
  (unless (stringp string)
    (fail "Expected base64url text"))
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (accumulator 0)
        (bits 0))
    (block walk
      (loop for ch across string
            do (when (char= ch #\=) (return-from walk))
               (let ((digit (position ch +b64u+)))
                 (unless digit (fail "Bad base64url character"))
                 (setf accumulator (logior (ash accumulator 6) digit)
                       bits (+ bits 6))
                 (when (>= bits 8)
                   (setf bits (- bits 8))
                   (vector-push-extend (ldb (byte 8 bits) accumulator) out)
                   (setf accumulator (ldb (byte bits 0) accumulator))))))
    (make-array (length out) :element-type '(unsigned-byte 8)
                              :initial-contents out)))

;;; --- modular arithmetic -------------------------------------------------------------

(defun mod-expt (base power modulus)
  "BASE to POWER modulo MODULUS, square and multiply: no library, no secret in a table."
  (let ((result 1)
        (base (mod base modulus))
        (power power))
    (loop while (plusp power)
          do (when (oddp power) (setf result (mod (* result base) modulus)))
             (setf base (mod (* base base) modulus)
                   power (ash power -1)))
    result))

(defun mod-inverse (value modulus)
  "The inverse of VALUE modulo MODULUS, by the extended Euclidean algorithm."
  (let ((old-r (mod value modulus)) (r modulus)
        (old-s 1) (s 0))
    (loop while (plusp r)
          do (let ((quotient (floor old-r r)))
               (multiple-value-setq (old-r r) (values r (- old-r (* quotient r))))
               (multiple-value-setq (old-s s) (values s (- old-s (* quotient s))))))
    (when (/= old-r 1) (fail "No modular inverse"))
    (mod old-s modulus)))

;;; --- hashes -----------------------------------------------------------------------

(defun sha256 (octets)
  (coerce (ironclad:digest-sequence :sha256 octets) 'octets))

(defun sha512 (octets)
  (coerce (ironclad:digest-sequence :sha512 octets) 'octets))

(defun hmac-sha256 (key message)
  (let* ((key (if (> (length key) 64) (sha256 key) key))
         (block (make-octets 64))
         (inner (make-octets 64 :fill #x36))
         (outer (make-octets 64 :fill #x5c)))
    (replace block key)
    (dotimes (i 64)
      (setf (aref inner i) (logxor (aref inner i) (aref block i))
            (aref outer i) (logxor (aref outer i) (aref block i))))
    (sha256 (join-octets outer (sha256 (join-octets inner message))))))

(defun hkdf-sha256 (ikm info &key (length 32) (salt (make-octets 32)))
  "RFC 5869, one block at a time: what the plugin's vendor/e2ee.py derives."
  (let* ((prk (hmac-sha256 salt ikm))
         (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (block (make-octets 0)))
    (loop for i from 1
          until (>= (length out) length)
          do (setf block (hmac-sha256 prk (join-octets block info (make-octets 1 :fill i))))
             (loop for byte across block do (vector-push-extend byte out)))
    (make-array length :element-type '(unsigned-byte 8)
                        :initial-contents (subseq out 0 length))))

;;; --- ChaCha20 (RFC 8439) ----------------------------------------------------------

(defun rotl32 (value shift)
  (logand #xffffffff (logior (ash value shift) (ash value (- shift 32)))))

(defun chacha20-block (key counter nonce)
  "One 64-byte ChaCha20 block: KEY 32, COUNTER a 32-bit number, NONCE 12."
  (let ((state (make-array 16 :element-type '(unsigned-byte 32)))
        (working (make-array 16 :element-type '(unsigned-byte 32))))
    (setf (aref state 0) #x61707865 (aref state 1) #x3320646e
          (aref state 2) #x79622d32 (aref state 3) #x6b206574)
    (dotimes (i 8)
      (setf (aref state (+ 4 i)) (le-int key (* 4 i) (+ 4 (* 4 i)))))
    (setf (aref state 12) (logand counter #xffffffff))
    (dotimes (i 3)
      (setf (aref state (+ 13 i)) (le-int nonce (* 4 i) (+ 4 (* 4 i)))))
    (replace working state)
    (flet ((quarter (a b c d)
             (setf (aref working a) (logand #xffffffff (+ (aref working a) (aref working b)))
                   (aref working d) (rotl32 (logxor (aref working d) (aref working a)) 16)
                   (aref working c) (logand #xffffffff (+ (aref working c) (aref working d)))
                   (aref working b) (rotl32 (logxor (aref working b) (aref working c)) 12)
                   (aref working a) (logand #xffffffff (+ (aref working a) (aref working b)))
                   (aref working d) (rotl32 (logxor (aref working d) (aref working a)) 8)
                   (aref working c) (logand #xffffffff (+ (aref working c) (aref working d)))
                   (aref working b) (rotl32 (logxor (aref working b) (aref working c)) 7))))
      (dotimes (round 10)
        (declare (ignore round))
        (quarter 0 4 8 12) (quarter 1 5 9 13) (quarter 2 6 10 14) (quarter 3 7 11 15)
        (quarter 0 5 10 15) (quarter 1 6 11 12) (quarter 2 7 8 13) (quarter 3 4 9 14)))
    (dotimes (i 16)
      (setf (aref working i) (logand #xffffffff (+ (aref working i) (aref state i)))))
    (let ((out (make-octets 64)))
      (dotimes (i 16)
        (let ((word (aref working i)))
          (loop for j from 0 below 4
                do (setf (aref out (+ (* 4 i) j)) (ldb (byte 8 (* 8 j)) word)))))
      out)))

(defun chacha20-xor (key counter nonce data)
  "DATA xor the ChaCha20 keystream from COUNTER on."
  (let ((out (make-octets (length data)))
        (at 0)
        (block-counter counter))
    (loop while (< at (length data))
          do (let ((block (chacha20-block key block-counter nonce)))
               (dotimes (i 64)
                 (let ((index (+ at i)))
                   (when (< index (length data))
                     (setf (aref out index) (logxor (aref data index) (aref block i)))))))
             (incf at 64)
             (incf block-counter))
    out))

;;; --- Poly1305 ---------------------------------------------------------------------

(defun poly1305 (message key)
  "The 16-byte tag of MESSAGE under the 32-byte one-time KEY (r||s)."
  (let ((r (logand (le-int key 0 16) #x0ffffffc0ffffffc0ffffffc0fffffff))
        (s (le-int key 16 32))
        (prime (- (ash 1 130) 5))
        (accumulator 0))
    (loop for start from 0 below (length message) by 16
          for end = (min (+ start 16) (length message))
          do (let ((chunk (+ (le-int message start end) (ash 1 (* 8 (- end start))))))
               (setf accumulator (mod (* (+ accumulator chunk) r) prime))))
    (int-le (mod (+ accumulator s) (ash 1 128)) 16)))

;;; --- the AEAD the envelope uses ----------------------------------------------------

(defun pad16 (octets)
  (let ((rest (mod (length octets) 16)))
    (if (zerop rest) octets (join-octets octets (make-octets (- 16 rest))))))

(defun aead-mac-data (aad ciphertext)
  (join-octets (pad16 aad) (pad16 ciphertext)
               (int-le (length aad) 8) (int-le (length ciphertext) 8)))

(defun aead-encrypt (key nonce plaintext &optional (aad (make-octets 0)))
  "=> (values CIPHERTEXT TAG), the plugin's construction exactly: the one-time
Poly1305 key is the first 32 bytes of block 0, the keystream starts at 1."
  (let* ((one-time (subseq (chacha20-block key 0 nonce) 0 32))
         (ciphertext (chacha20-xor key 1 nonce plaintext))
         (tag (poly1305 (aead-mac-data aad ciphertext) one-time)))
    (values ciphertext tag)))

(defun aead-decrypt (key nonce ciphertext tag &optional (aad (make-octets 0)))
  (let* ((one-time (subseq (chacha20-block key 0 nonce) 0 32))
         (expected (poly1305 (aead-mac-data aad ciphertext) one-time)))
    (unless (and (= (length tag) 16) (equalp expected tag))
      (fail "Message body failed its authentication tag"))
    (chacha20-xor key 1 nonce ciphertext)))
