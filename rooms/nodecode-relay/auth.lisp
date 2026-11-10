;;;; auth.lisp --- who gets in: a password, pairing codes, device tokens.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here talks to a socket: every verb takes the store and the clock
;;;; as arguments, so the whole gate is reachable from a test. A secret is
;;;; never kept, only its salted digest; a pairing code lives five minutes
;;;; and dies on first use; a device holds a token the store keeps as a
;;;; digest, and a session cookie is an HMAC under a key that signing out
;;;; everywhere replaces.

(in-package #:nodecode-relay)

(defparameter *iterations* 60000
  "Rounds of SHA-256 over a password. Tests bind it low; a person's password
takes a fraction of a second and a guess costs the same.")

(defparameter +code-seconds+ 300
  "How long a pairing code is good for.")

(defparameter +code-digits+ 8)

(defparameter +session-cookie+ "__Host-relay")
(defparameter +failures-allowed+ 5)
(defparameter +failure-window+ 600)
(defparameter +lock-seconds+ 300)

(defun utf8 (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun hex (bytes)
  (format nil "~(~{~2,'0x~}~)" (coerce bytes 'list)))

(defun digest-hex (bytes)
  "BYTES' SHA-256, in hex, from the image's own OpenSSL digest."
  (subseq (nlk:sha256-text bytes) 7))

(defun hex-bytes (hex)
  (let ((bytes (make-array (/ (length hex) 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length bytes)
          do (setf (aref bytes i)
                   (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))
    bytes))

(defun sha256 (string)
  "STRING's SHA-256, in hex."
  (digest-hex (utf8 string)))

(defun random-hex (octets)
  "OCTETS random bytes from the kernel, in hex."
  (let ((v (make-array octets :element-type '(unsigned-byte 8))))
    (with-open-file (in #p"/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence v in))
    (hex v)))

(defun stretch (secret salt &optional (iterations *iterations*))
  "SECRET salted with SALT, hashed ITERATIONS times, in hex."
  (let ((h (concatenate '(vector (unsigned-byte 8)) (utf8 salt) (utf8 secret))))
    (dotimes (i iterations (digest-hex h))
      (declare (ignore i))
      (setf h (hex-bytes (digest-hex h))))))

(defun plist (thing key &optional default)
  (let ((cell (getf thing key :absent)))
    (if (eq cell :absent) default cell)))

;;; --- the store --------------------------------------------------------------
;;; One plist, printed to a 0600 file: password, devices, codes, signing key
;;; and the addresses that guessed wrong. Everything in it is readable, so a
;;; read with *READ-EVAL* off cannot run anything.

(defstruct (store (:constructor make-store))
  (password nil)
  (devices nil)
  (codes nil)
  (key (random-hex 32))
  (attempts nil))

(defun new-store ()
  (make-store :key (random-hex 32)))

(defun store-text (store)
  (let ((*print-pretty* nil) (*print-circle* nil) (*print-readably* nil))
    (prin1-to-string
     (list :password (store-password store)
           :devices (store-devices store)
           :codes (store-codes store)
           :key (store-key store)
           :attempts (store-attempts store)))))

(defun text-store (text)
  "STORE from its printed form, or a fresh one."
  (or (ignore-errors
        (let ((*read-eval* nil) (*package* (find-package '#:keyword)))
          (apply #'make-store (read-from-string text))))
      (new-store)))

(defun load-store (path)
  (if (probe-file path)
      (text-store (uiop:read-file-string path))
      (new-store)))

(defun save-store (store path)
  (nlk:write-file-atomically path (store-text store)
                             :mode #o600 :directory-mode #o700)
  store)

;;; --- the password -----------------------------------------------------------

(defun set-password (store secret)
  "SECRET becomes the password: a fresh salt, and the digest of both."
  (when (zerop (length (or secret "")))
    (fail "a password cannot be empty"))
  (let ((salt (random-hex 16)))
    (setf (store-password store)
          (list :salt salt :iterations *iterations*
                :hash (stretch secret salt))))
  ;; A password set from the machine is the machine's, so nobody at the
  ;; network can claim the relay first.
  store)

(defun password-ok-p (store secret)
  (let ((p (store-password store)))
    (and p (stringp secret)
         (equal (getf p :hash)
                (stretch secret (getf p :salt) (getf p :iterations))))))

(defun password-set-p (store)
  (and (store-password store) t))

;;; --- pairing codes and devices ----------------------------------------------

(defun prune-codes (store now)
  (setf (store-codes store)
        (remove-if (lambda (code) (<= (plist code :expires 0) now))
                   (store-codes store))))

(defun mint-code (store &key (now (get-universal-time)))
  "A fresh 8-digit pairing code, kept only as a digest, good for five minutes."
  (prune-codes store now)
  (let ((code (format nil "~8,'0d" (random 100000000))))
    (push (list :hash (sha256 code) :expires (+ now +code-seconds+))
          (store-codes store))
    code))

(defun redeem-code (store code name &key (now (get-universal-time)))
  "CODE spent for a device called NAME => (VALUES DEVICE TOKEN), or NIL. A
code is single-use: redeeming it takes it out of the store."
  (let ((digest (sha256 (or code ""))))
    (when (find digest (store-codes store) :key (lambda (c) (plist c :hash)) :test #'equal)
      (setf (store-codes store)
            (remove digest (store-codes store) :key (lambda (c) (plist c :hash)) :test #'equal))
      (mint-device store (or name "")))))

(defun mint-device (store name &key (now (get-universal-time)))
  "A device with its own token => (VALUES DEVICE TOKEN). The store keeps the
token's digest and nothing that could be replayed."
  (let* ((token (random-hex 24))
         (device (list :id (random-hex 4)
                       :name (if (plusp (length (or name ""))) name "a device")
                       :hash (sha256 token)
                       :added now)))
    (push device (store-devices store))
    (values device token)))

(defun device-for-token (store token)
  (and (stringp token)
       (let ((digest (sha256 token)))
         (find digest (store-devices store)
               :key (lambda (device) (plist device :hash)) :test #'equal))))

(defun revoke-device (store id)
  "DEVICE with ID taken out => the name it had, or NIL."
  (let ((device (find id (store-devices store) :key (lambda (d) (plist d :id)) :test #'equal)))
    (when device
      (setf (store-devices store) (remove device (store-devices store)))
      (plist device :name))))

(defun sign-out-everywhere (store)
  "Every session and every device ends: a new signing key, no devices."
  (setf (store-key store) (random-hex 32)
        (store-devices store) nil
        (store-attempts store) nil)
  store)

;;; --- sessions ---------------------------------------------------------------

(defun sign (store text)
  "TEXT's HMAC-SHA256 under the store's signing key, in hex."
  (let* ((key (utf8 (store-key store)))
         (key (if (> (length key) 64) (hex-bytes (digest-hex key)) key))
         (padded (concatenate '(vector (unsigned-byte 8)) key
                              (make-array (- 64 (length key)) :element-type '(unsigned-byte 8)
                                                              :initial-element 0)))
         (inner (hex-bytes
                 (digest-hex
                  (concatenate '(vector (unsigned-byte 8))
                               (map 'vector (lambda (byte) (logxor byte #x36)) padded)
                               (utf8 text))))))
    (digest-hex (concatenate '(vector (unsigned-byte 8))
                             (map 'vector (lambda (byte) (logxor byte #x5c)) padded)
                             inner))))

(defun session-cookie (store device)
  "The cookie a paired device holds: its id and an HMAC under the store's key."
  (let ((id (plist device :id))
        (stamp (plist device :added 0)))
    (format nil "~a.~a.~a" id stamp (sign store (format nil "~a.~a" id stamp)))))

(defun cookie-device (store cookie)
  "The device COOKIE names, or NIL."
  (when cookie
    (let ((parts (uiop:split-string cookie :separator ".")))
      (when (= 3 (length parts))
        (destructuring-bind (id stamp mac) parts
          (let ((device (find id (store-devices store)
                              :key (lambda (d) (plist d :id)) :test #'equal)))
            (when (and device
                       (equal (princ-to-string (plist device :added 0)) stamp)
                       (equal mac (sign store (format nil "~a.~a" id stamp))))
              device)))))))

(defun cookie-header-value (headers name)
  "The value the request HEADERS, an alist, carries for NAME."
  (cdr (assoc name headers :test #'string-equal)))

(defun cookie-value (header name)
  (loop for part in (uiop:split-string (or header "") :separator ";")
        for pair = (string-trim " " part)
        when (uiop:string-prefix-p (format nil "~a=" name) pair)
          return (subseq pair (1+ (length name)))))

(defun caller-device (store headers)
  "The device the request comes as: a session cookie or a bearer token, or NIL."
  (or (cookie-device store (cookie-value (cookie-header-value headers "cookie") +session-cookie+))
      (let ((auth (cookie-header-value headers "authorization")))
        (and auth
             (uiop:string-prefix-p "Bearer " auth)
             (device-for-token store (subseq auth 7))))))

;;; --- wrong guesses ----------------------------------------------------------

(defun locked-p (store address &key (now (get-universal-time)))
  "Whether ADDRESS has guessed wrong too often too recently."
  (let ((row (assoc address (store-attempts store) :test #'equal)))
    (and row (<= now (plist (cdr row) :until 0)))))

(defun note-failure (store address &key (now (get-universal-time)))
  "One wrong guess from ADDRESS; the one past the allowance locks it."
  (let* ((row (assoc address (store-attempts store) :test #'equal))
         (seen (if row (plist (cdr row) :failures 0) 0))
         (first (if row (plist (cdr row) :first now) now))
         (failures (if (and row (< (- now first) +failure-window+)) (1+ seen) 1))
         (until (if (> failures +failures-allowed+) (+ now +lock-seconds+) 0)))
    (setf (store-attempts store)
          (acons address (list :failures failures :first now :until until)
                 (remove address (store-attempts store) :key #'car :test #'equal)))
    (locked-p store address :now now)))

(defun note-success (store address)
  (setf (store-attempts store) (remove address (store-attempts store) :key #'car :test #'equal))
  store)
