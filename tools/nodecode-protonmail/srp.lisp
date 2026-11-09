;;;; srp.lisp --- Proton's SRP-6a login math and the bcrypt it derives from.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Proton's handshake is textbook SRP with three twists, all of them here:
;;;; every big integer travels little-endian, the hash is EXPAND-HASH (four
;;;; SHA-512 digests concatenated) rather than a bare SHA-512, and the
;;;; password enters as a bcrypt hash over the server's salt (Proton's own
;;;; bcrypt fork, which echoes the first 22 characters of the encoded salt).
;;;; The Blowfish constants are the standard ones; they are read from the
;;;; copy ironclad already has rather than copied into this file.

(in-package #:nodecode-protonmail)

(defparameter +generator+ 2)
(defparameter +bit-length+ 2048)

;;; --- bcrypt's base64 -----------------------------------------------------------

(defparameter +bcrypt-b64+ "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

(defun encode-bcrypt-b64 (octets)
  "OCTETS as bcrypt's unpadded base64: './' first, no padding."
  (let ((out (make-string-output-stream))
        (n (length octets)))
    (loop for i from 0 below n by 3
          for a = (aref octets i)
          for b = (if (< (1+ i) n) (aref octets (1+ i)) 0)
          for c = (if (< (+ i 2) n) (aref octets (+ i 2)) 0)
          for triple = (+ (ash a 16) (ash b 8) c)
          do (flet ((ch (shift) (char +bcrypt-b64+ (ldb (byte 6 shift) triple))))
               (write-char (ch 18) out)
               (write-char (ch 12) out)
               (when (< (+ i 1) n) (write-char (ch 6) out))
               (when (< (+ i 2) n) (write-char (ch 0) out))))
    (get-output-stream-string out)))

(defun decode-bcrypt-b64 (text)
  "TEXT as octets, the trailing bits of an odd length dropped, as bcrypt does."
  (let* ((bits (loop for char across text
                      for index = (position char +bcrypt-b64+)
                      when index append (loop for shift from 5 downto 0
                                              collect (ldb (byte 1 shift) index))))
         (count (floor (length bits) 8))
         (octets (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count)
      (setf (aref octets i)
            (loop for j from 0 below 8
                  for bit = (nth (+ (* i 8) j) bits)
                  for value = (if (= bit 1) (ash 1 (- 7 j)) 0)
                  sum value)))
    octets))

;;; --- Blowfish, with the salted key schedule bcrypt needs -----------------------

(declaim (inline bf-f bf-encrypt-block))

(defun bf-words (octets)
  "OCTETS as a vector of big-endian 32-bit words, a partial tail zero-padded."
  (let* ((count (ceiling (length octets) 4))
         (words (make-array count :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (i count)
      (setf (aref words i)
            (loop for j from 0 below 4
                  for index = (+ (* i 4) j)
                  sum (if (< index (length octets))
                          (ash (aref octets index) (- 24 (* 8 j)))
                          0))))
    words))

(defun bf-tables ()
  "Fresh copies of Blowfish's initial P-array and four S-boxes."
  (values (copy-seq (symbol-value (find-symbol "+P-ARRAY+" "CRYPTO")))
          (copy-seq (symbol-value (find-symbol "+S-BOX-0+" "CRYPTO")))
          (copy-seq (symbol-value (find-symbol "+S-BOX-1+" "CRYPTO")))
          (copy-seq (symbol-value (find-symbol "+S-BOX-2+" "CRYPTO")))
          (copy-seq (symbol-value (find-symbol "+S-BOX-3+" "CRYPTO")))))

(defmacro bf-f (s0 s1 s2 s3 x)
  `(let* ((word ,x)
          (a (aref ,s0 (ldb (byte 8 24) word)))
          (b (aref ,s1 (ldb (byte 8 16) word)))
          (c (aref ,s2 (ldb (byte 8 8) word)))
          (d (aref ,s3 (ldb (byte 8 0) word))))
     (logxor (ldb (byte 32 0) (+ a b)) (ldb (byte 32 0) (+ c d)))))

(defun bf-encrypt-block (p s0 s1 s2 s3 l r)
  "One Blowfish block, L and R the halves."
  (dotimes (i 16)
    (setf l (logxor l (aref p i)))
    (setf r (logxor r (bf-f s0 s1 s2 s3 l)))
    (rotatef l r))
  (rotatef l r)
  (setf r (logxor r (aref p 16)))
  (setf l (logxor l (aref p 17)))
  (values l r))

(defun bf-expand (p s0 s1 s2 s3 salt-words key-words)
  "bcrypt's ExpandKey: the key into P, then P and the S-boxes re-keyed from
data that is the running cipher state, xored with SALT-WORDS when there are any."
  (let ((key-length (length key-words))
        (salt-length (length salt-words))
        (key-index 0)
        (salt-index 0)
        (l 0)
        (r 0))
    (flet ((next-key ()
             (prog1 (aref key-words (mod key-index key-length))
               (incf key-index)))
           (next-salt ()
             (when (plusp salt-length)
               (prog1 (aref salt-words (mod salt-index salt-length))
                 (incf salt-index)))))
      (dotimes (i 18) (setf (aref p i) (logxor (aref p i) (next-key))))
      (labels ((bf-step (setter)
                 (when (plusp salt-length)
                   (setf l (logxor l (next-salt)))
                   (setf r (logxor r (next-salt))))
                 (multiple-value-setq (l r) (bf-encrypt-block p s0 s1 s2 s3 l r))
                 (funcall setter l r)))
        (loop for i from 0 below 18 by 2
              do (let ((index i))
                   (bf-step (lambda (a b) (setf (aref p index) a (aref p (1+ index)) b)))))
        (dolist (box (list s0 s1 s2 s3))
          (loop for i from 0 below 256 by 2
                do (let ((index i) (sbox box))
                     (bf-step (lambda (a b) (setf (aref sbox index) a (aref sbox (1+ index)) b))))))))
    (values p s0 s1 s2 s3)))

(defun bcrypt-raw (password salt cost)
  "The 23 bytes bcrypt hashes to: eksblowfish over 'OrpheanBeholderScryDoubt'."
  (let ((key (bf-words password))
        (salt-words (bf-words salt)))
    (multiple-value-bind (p s0 s1 s2 s3) (bf-tables)
      (multiple-value-setq (p s0 s1 s2 s3)
        (bf-expand p s0 s1 s2 s3 salt-words key))
      (dotimes (round (ash 1 cost))
        (declare (ignore round))
        (multiple-value-setq (p s0 s1 s2 s3)
          (bf-expand p s0 s1 s2 s3 #() key))
        (multiple-value-setq (p s0 s1 s2 s3)
          (bf-expand p s0 s1 s2 s3 #() salt-words)))
      (let ((chunk (map '(vector (unsigned-byte 8)) #'char-code "OrpheanBeholderScryDoubt"))
            (out (make-array 24 :element-type '(unsigned-byte 8))))
        (dotimes (block 3)
          (let ((words (bf-words (subseq chunk (* block 8) (+ (* block 8) 8)))))
            (let ((l (aref words 0)) (r (aref words 1)))
              (dotimes (i 64) (declare (ignore i))
                (multiple-value-setq (l r) (bf-encrypt-block p s0 s1 s2 s3 l r)))
              (loop for j from 0 below 4
                    do (setf (aref out (+ (* block 8) j)) (ldb (byte 8 (- 24 (* 8 j))) l)
                             (aref out (+ (* block 8) 4 j)) (ldb (byte 8 (- 24 (* 8 j))) r))))))
        (subseq out 0 23)))))

(defun bcrypt-hash (password salt)
  "PASSWORD (octets) under SALT (16 octets) as bcrypt's modular-crypt string.
Proton's fork reads the first 22 characters of the encoded salt and echoes
them back, so the string carries Proton's spelling, not a canonical one."
  (let* ((echoed (subseq (encode-bcrypt-b64 salt) 0 22))
         (canonical (decode-bcrypt-b64 echoed)))
    (format nil "$2y$10$~a~a" echoed
            (encode-bcrypt-b64 (bcrypt-raw (subseq password 0 (min 72 (length password)))
                                            canonical 10)))))

;;; --- the SRP handshake ---------------------------------------------------------

(defun expand-hash (octets)
  "SHA-512 of OCTETS with each one-byte suffix 0..3, concatenated: 256 bytes."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8)))
        (length (length octets)))
    (dotimes (i 4)
      (let* ((chunk (make-array (1+ length) :element-type '(unsigned-byte 8)))
             (digest (make-array 64 :element-type '(unsigned-byte 8))))
        (replace chunk octets)
        (setf (aref chunk length) i)
        (setf digest (ironclad:digest-sequence :sha512 chunk))
        (replace out digest :start1 (* i 64))))
    out))

(defun little-endian->integer (octets)
  (loop for i from (1- (length octets)) downto 0
        for value = (aref octets i)
        sum (ash value (* 8 i))))

(defun integer->little-endian (value &optional (length +bit-length+))
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length octets)
      (setf (aref octets i) (ldb (byte 8 (* 8 i)) value)))))

(defun salted-bcrypt (password salt)
  "Proton's salted bcrypt: the 16-byte salt spelled in bcrypt's own base64."
  (bcrypt-hash password (subseq salt 0 (min 16 (length salt)))))

(defun password-hash (password salt modulus-octets)
  "Auth version 3/4: bcrypt over salt++'proton', then expand-hash with the modulus."
  (let* ((salted (concatenate '(vector (unsigned-byte 8)) salt
                               (map '(vector (unsigned-byte 8)) #'char-code "proton")))
         (digest (map '(vector (unsigned-byte 8)) #'char-code (bcrypt-hash password salted))))
    (expand-hash (concatenate '(vector (unsigned-byte 8)) digest modulus-octets))))

(defun key-passphrase (password salt)
  "The passphrase that unlocks the account's private keys: bcrypt's last 31 octets."
  (let* ((hash (bcrypt-hash password (subseq salt 0 (min 16 (length salt)))))
         (octets (map '(vector (unsigned-byte 8)) #'char-code hash)))
    (subseq octets (max 0 (- (length octets) 31)))))

(defstruct (proofs (:constructor make-proofs (client-ephemeral client-proof server-proof)))
  client-ephemeral client-proof server-proof)

(defun srp-proofs (&key password salt server-ephemeral signed-modulus)
  "Run the client half of Proton's handshake.
PASSWORD and SALT and SERVER-EPHEMERAL and SIGNED-MODULUS are octets, the
modulus base64-decoded from the server's signed blob. => PROOFS."
  (let* ((modulus-octets signed-modulus)
         (modulus (little-endian->integer modulus-octets))
         (server (little-endian->integer server-ephemeral))
         (hashed (password-hash password salt modulus-octets))
         (x (little-endian->integer hashed))
         (multiplier (mod (little-endian->integer
                           (expand-hash (concatenate '(vector (unsigned-byte 8))
                                                     (integer->little-endian +generator+)
                                                     (integer->little-endian modulus))))
                          modulus))
         (secret 0) (client 0) (scrambler 0))
    (unless (= (integer-length modulus) +bit-length+)
      (fail "the server's SRP modulus is not 2048 bits"))
    (unless (and (< 1 server) (< server (1- modulus)))
      (fail "the server's SRP ephemeral is out of bounds"))
    (loop (setf secret (+ (random (- (ash 1 512) 1)) (ash 1 512)))
          (setf client (ironclad:expt-mod +generator+ secret modulus))
          (setf scrambler (little-endian->integer
                            (expand-hash (concatenate '(vector (unsigned-byte 8))
                                                      (integer->little-endian client)
                                                      server-ephemeral))))
          (when (plusp scrambler) (return)))
    (let* ((base (mod (- server (* multiplier (ironclad:expt-mod +generator+ x modulus)))
                      modulus))
           (exponent (mod (+ (* scrambler x) secret) (1- modulus)))
           (shared (integer->little-endian (ironclad:expt-mod base exponent modulus)))
           (client-octets (integer->little-endian client))
           (client-proof (expand-hash (concatenate '(vector (unsigned-byte 8))
                                                    client-octets server-ephemeral shared)))
           (server-proof (expand-hash (concatenate '(vector (unsigned-byte 8))
                                                    client-octets client-proof shared))))
      (make-proofs client-octets client-proof server-proof))))
