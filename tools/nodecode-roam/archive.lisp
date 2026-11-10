;;;; archive.lisp --- the sealed bundle: one file, AES-256-GCM, a signed head.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original sealed a tar archive under AES-256-GCM with a random
;;;; nonce and signed its head with an HKDF subkey (roam/archive.py,
;;;; roam/head.py). This file is the same shape in a format of its own:
;;;;
;;;;   "NROAM1\n" | u32 head-length | head JSON | u32 meta-length | meta JSON | sealed
;;;;
;;;; HEAD is the part a reader can see without the passphrase -- version,
;;;; saved at, profile, file and byte counts, and one digest of the
;;;; contents -- and it is the GCM associated data, so it cannot be
;;;; edited without the tag failing. META holds what opens it: the salt,
;;;; the nonce, and the HMAC-SHA256 over the head octets, so a bundle
;;;; with a head nobody signed is refused before a byte is decrypted.
;;;; A wrong passphrase fails authentication, never yields garbage.

(in-package #:nodecode-roam)

(defconstant +format+ 1 "The bundle's own format, so a later one can tell.")
(defconstant +magic+ "NROAM1
" "Seven bytes at the front: what this file is.")
(defconstant +bundle-info+ "roam-bundle-v1" "HKDF info for the GCM key.")
(defconstant +head-info+ "roam-head-v1" "HKDF info for the head's MAC key, a subkey of its own.")
(defconstant +max-bundle+ (* 64 1024 1024) "A bundle larger than this is refused, not read into memory.")

;;; --- octets -----------------------------------------------------------------

(defun octets-of (thing)
  "THING as a simple octet vector: a vector, a list of bytes, or octets already."
  (coerce thing '(simple-array (unsigned-byte 8) (*))))

(defun text-octets (text)
  "TEXT as UTF-8 octets."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun octets-text (octets)
  "OCTETS as UTF-8 text."
  (sb-ext:octets-to-string (octets-of octets) :external-format :utf-8))

(defun read-octets (path)
  "The whole file at PATH as octets."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((out (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence out in)
      out)))

(defun hex (octets)
  "OCTETS as lowercase hex."
  (ironclad:byte-array-to-hex-string (octets-of octets)))

(defun unhex (text)
  "TEXT, lowercase or not hex, as octets."
  (ironclad:hex-string-to-byte-array text))

(defun be32 (number)
  "NUMBER as four big-endian octets."
  (let ((out (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref out 0) (ldb (byte 8 24) number)
          (aref out 1) (ldb (byte 8 16) number)
          (aref out 2) (ldb (byte 8 8) number)
          (aref out 3) (ldb (byte 8 0) number))
    out))

(defun be32-value (octets start)
  "The four big-endian octets at START as an integer."
  (logior (ash (aref octets start) 24)
          (ash (aref octets (+ start 1)) 16)
          (ash (aref octets (+ start 2)) 8)
          (aref octets (+ start 3))))

;;; --- base64 -----------------------------------------------------------------

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The standard alphabet, so a bundle's JSON reads in any tool.")

(defun base64 (octets)
  "OCTETS as standard base64 with padding."
  (let ((octets (octets-of octets)))
    (with-output-to-string (out)
      (loop for index from 0 below (length octets) by 3
            for left = (- (length octets) index)
            for a = (aref octets index)
            for b = (if (> left 1) (aref octets (+ index 1)) 0)
            for c = (if (> left 2) (aref octets (+ index 2)) 0)
            do (write-char (char +base64-alphabet+ (ash a -2)) out)
               (write-char (char +base64-alphabet+ (logior (ash (logand a 3) 4) (ash b -4))) out)
               (write-char (if (> left 1)
                               (char +base64-alphabet+ (logior (ash (logand b 15) 2) (ash c -6)))
                               #\=)
                           out)
               (write-char (if (> left 2) (char +base64-alphabet+ (logand c 63)) #\=) out)))))

(defun base64-value (character)
  "CHARACTER's six bits, or NIL when it is not in the alphabet."
  (position character +base64-alphabet+ :test #'char=))

(defun base64-decode (text)
  "TEXT, standard base64, as octets; a character outside the alphabet is refused."
  (let ((clean (remove-if (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return) :test #'char=)) text)))
    (unless (zerop (mod (length clean) 4))
      (fail "roam: a bundle's base64 is not a whole number of four-character groups"))
    (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
      (loop for index from 0 below (length clean) by 4
            for a = (base64-value (char clean index))
            for b = (base64-value (char clean (+ index 1)))
            for c = (char clean (+ index 2))
            for d = (char clean (+ index 3))
            do (unless (and a b) (fail "roam: a bundle's base64 carries a character it should not"))
               (vector-push-extend (logior (ash a 2) (ash b -4)) out)
               (unless (char= c #\=)
                 (let ((c (base64-value c)))
                   (unless c (fail "roam: a bundle's base64 carries a character it should not"))
                   (vector-push-extend (logior (ash (logand b 15) 4) (ash c -2)) out)
                   (unless (char= d #\=)
                     (let ((d (base64-value d)))
                       (unless d (fail "roam: a bundle's base64 carries a character it should not"))
                       (vector-push-extend (logior (ash (logand c 3) 6) d) out)))))
            finally (return (octets-of out))))))

;;; --- the key schedule --------------------------------------------------------

(defun sha256 (octets)
  "OCTETS' SHA-256."
  (ironclad:digest-sequence :sha256 (octets-of octets)))

(defun hmac (key octets)
  "HMAC-SHA256 of OCTETS under KEY."
  (let ((hmac (ironclad:make-hmac (octets-of key) :sha256)))
    (ironclad:update-hmac hmac (octets-of octets))
    (ironclad:hmac-digest hmac)))

(defun hkdf (secret salt info)
  "Thirty-two octets from SECRET: HKDF-SHA256 (extract, then one expand
block of INFO || 0x01), the original's HKDF by another name."
  (let ((prk (hmac (octets-of salt) (octets-of secret))))
    (hmac prk (concatenate '(vector (unsigned-byte 8))
                            (text-octets info)
                            (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1)))))

;;; --- AES-256-GCM -------------------------------------------------------------

(defun gcm-cipher (key nonce aad)
  "A GCM cipher under KEY (32 octets), NONCE (12) with AAD already taken in."
  (let ((cipher (ironclad:make-authenticated-encryption-mode
                 :gcm :cipher-name :aes :key (octets-of key)
                 :initialization-vector (octets-of nonce))))
    (ironclad:process-associated-data cipher (octets-of aad))
    cipher))

(defun seal (key nonce aad plaintext)
  "PLAINTEXT under KEY and NONCE with AAD: ciphertext then its sixteen-octet tag."
  (let ((cipher (gcm-cipher key nonce aad)))
    (concatenate '(vector (unsigned-byte 8))
                 (ironclad:encrypt-message cipher (octets-of plaintext))
                 (ironclad:produce-tag cipher))))

(defun unseal (key nonce aad sealed)
  "The plaintext under SEALED, or NIL when the tag does not verify."
  (when (>= (length sealed) 16)
    (let* ((size (length sealed))
           (body (subseq sealed 0 (- size 16)))
           (tag (subseq sealed (- size 16)))
           (cipher (gcm-cipher key nonce aad))
           (plain (ignore-errors (ironclad:decrypt-message cipher body))))
      (and plain (equalp tag (ironclad:produce-tag cipher)) plain))))

;;; --- the bundle ---------------------------------------------------------------

(defun seal-bundle (passphrase head-octets manifest-octets)
  "HEAD-OCTETS (a JSON object) and MANIFEST-OCTETS sealed under PASSPHRASE.
=> the file's octets."
  (let* ((salt (nlk:random-bytes 16))
         (nonce (nlk:random-bytes 12))
         (key (hkdf (text-octets passphrase) salt +bundle-info+))
         (mac (hmac (hkdf (text-octets passphrase) salt +head-info+) head-octets))
         (sealed (seal key nonce head-octets manifest-octets))
         (meta (text-octets
                (nlk:encode-json-object
                 (nlk:json-object "format" +format+
                                  "cipher" "aes-256-gcm"
                                  "kdf" "hkdf-sha256"
                                  "salt" (hex salt)
                                  "nonce" (hex nonce)
                                  "mac" (hex mac))))))
    (concatenate '(vector (unsigned-byte 8))
                 (text-octets +magic+) (be32 (length head-octets)) (octets-of head-octets)
                 (be32 (length meta)) meta sealed)))

(defun bundle-parts (octets)
  "OCTETS split into the head, the meta object and the sealed body.
=> (values HEAD-OCTETS META-OBJECT SEALED); anything malformed is a refusal."
  (let* ((octets (octets-of octets))
         (magic (length (text-octets +magic+))))
    (unless (and (> (length octets) (+ magic 8))
                 (equalp (subseq octets 0 magic) (text-octets +magic+)))
      (fail "roam: this is not a roam bundle: its first bytes are not the format's"))
    (let* ((head-length (be32-value octets magic))
           (head-start (+ magic 4))
           (meta-start (+ head-start head-length)))
      (unless (and (>= (length octets) (+ meta-start 4)) (<= head-length +max-bundle+))
        (fail "roam: this bundle's head length runs past the file"))
      (let* ((meta-length (be32-value octets meta-start))
             (meta-octets-start (+ meta-start 4))
             (sealed-start (+ meta-octets-start meta-length)))
        (unless (and (>= (length octets) sealed-start) (<= meta-length 65536))
          (fail "roam: this bundle's meta length runs past the file"))
        (values (subseq octets head-start meta-start)
                (nlk:decode-json (subseq octets meta-octets-start sealed-start))
                (subseq octets sealed-start))))))

(defun bundle-head (octets)
  "The head of OCTETS as a JSON object -- readable without the passphrase."
  (multiple-value-bind (head-octets) (bundle-parts octets)
    (nlk:decode-json (octets-text head-octets))))

(defun open-bundle (passphrase octets)
  "OCTETS opened under PASSPHRASE. => (values MANIFEST HEAD); a wrong
passphrase or a damaged file is one refusal, before anything is written."
  (multiple-value-bind (head-octets meta sealed) (bundle-parts octets)
    (let* ((salt (unhex (or (nlk:json-value meta :string "salt")
                            (fail "roam: this bundle carries no salt"))))
           (nonce (unhex (or (nlk:json-value meta :string "nonce")
                             (fail "roam: this bundle carries no nonce"))))
           (head (nlk:decode-json (octets-text head-octets))))
      (unless (equal (hex (hmac (hkdf (text-octets passphrase) salt +head-info+) head-octets))
                     (nlk:json-value meta :string "mac"))
        (fail "roam: this bundle's head does not authenticate: the passphrase is wrong or the file was changed"))
      (let ((plain (unseal (hkdf (text-octets passphrase) salt +bundle-info+) nonce head-octets sealed)))
        (unless plain
          (fail "roam: this bundle did not open: the passphrase is wrong or the file was changed"))
        (values (nlk:decode-json plain) head)))))
