;;;; token.lisp --- the session a login mints: a stateless signed token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's sessions are signed tokens, not rows: verifying one on every
;;;; request never touches the directory, and there is nothing server-side to
;;;; revoke. The scheme here is the same idea with a simpler payload - the
;;;; fields this cell carries, name=value pairs with their separators escaped -
;;;; and the signature over the payload's own octets.

(in-package #:nodecode-ldap-auth)

(defparameter +base64-url+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "The URL-safe alphabet, so a token survives a URL, a header and a chat message.")

(defun base64-url-encode (octets)
  "OCTETS as base64url text, without the padding."
  (let ((out (make-string-output-stream)))
    (loop for at from 0 below (length octets) by 3
          do (let* ((a (aref octets at))
                    (b (if (< (+ at 1) (length octets)) (aref octets (+ at 1)) 0))
                    (c (if (< (+ at 2) (length octets)) (aref octets (+ at 2)) 0))
                    (n (logior (ash a 16) (ash b 8) c))
                    (chars (min 4 (+ 1 (min 3 (- (length octets) at))))))
               (dotimes (i chars)
                 (write-char (char +base64-url+ (logand (ash n (- (* 6 (- 3 i)))) #x3f)) out))))
    (get-output-stream-string out)))

(defun base64-url-value (char)
  (position char +base64-url+))

(defun base64-url-decode (text)
  "TEXT as octets, or NIL when it is not base64url."
  (when (stringp text)
    (let ((values (map 'list #'base64-url-value text)))
      (when (every #'identity values)
        (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
              (n 0)
              (bits 0))
          (dolist (value values)
            (setf n (logior (ash n 6) value))
            (incf bits 6)
            (when (>= bits 8)
              (decf bits 8)
              (vector-push-extend (logand (ash n (- bits)) #xff) out)))
          (coerce out '(simple-array (unsigned-byte 8) (*))))))))

(defun hmac-sha256 (key octets)
  "The HMAC of OCTETS under KEY, as 32 octets."
  (let ((hmac (ironclad:make-hmac key :sha256)))
    (ironclad:update-hmac hmac octets)
    (ironclad:hmac-digest hmac)))

(defun hex-string-p (text)
  (and (plusp (length text))
       (evenp (length text))
       (every (lambda (char) (digit-char-p char 16)) text)))

(defun hex-octets (text)
  (let ((out (make-array (/ (length text) 2) :element-type '(unsigned-byte 8))))
    (loop for at from 0 below (length text) by 2
          for i from 0
          do (setf (aref out i) (parse-integer text :start at :end (+ at 2) :radix 16)))
    out))

(defun token-key (secret)
  "The signing key the section names: written as hex, as base64, or raw text.
An empty setting answers a random key for this process, so sessions live until
the process restarts (which is what the original does when its secret is unset)."
  (let ((secret (string-trim " " (or secret ""))))
    (cond ((zerop (length secret))
           (let ((key (make-array 32 :element-type '(unsigned-byte 8))))
             (loop for i below 32 do (setf (aref key i) (random 256)))
             key))
          ((and (>= (length secret) 32) (hex-string-p secret)) (hex-octets secret))
          (t (let ((decoded (and (>= (length secret) 22) (base64-url-decode secret))))
               (if (and decoded (>= (length decoded) 16)) decoded (utf8 secret)))))))

;;; --- the payload ------------------------------------------------------------

(defparameter +payload-fields+ '("sub" "dn" "em" "nm" "kind" "exp"))

(defun escape-payload-value (text)
  "TEXT with the separators of the payload escaped, so a DN's own & and = are
never read back as structure."
  (with-output-to-string (out)
    (loop for char across (or text "")
          do (case char
               (#\% (write-string "%25" out))
               (#\& (write-string "%26" out))
               (#\= (write-string "%3d" out))
               (t (write-char char out))))))

(defun unescape-payload-value (text)
  (with-output-to-string (out)
    (loop with at = 0
          while (< at (length text))
          do (let ((char (char text at)))
               (if (and (char= char #\%) (< (+ at 2) (length text)))
                   (let ((code (ignore-errors (parse-integer text :start (1+ at) :end (+ at 3) :radix 16))))
                     (if code (write-char (code-char code) out) (write-char char out))
                     (incf at 3))
                   (progn (write-char char out) (incf at)))))))

(defun payload-text (payload)
  "PAYLOAD as the bytes the signature covers."
  (utf8 (with-output-to-string (out)
          (loop for (key . value) in payload
                for first = t then nil
                do (unless first (write-char #\& out))
                   (write-string key out)
                   (write-char #\= out)
                   (write-string (escape-payload-value (princ-to-string value)) out)))))

(defun read-payload (octets)
  "OCTETS read back as the payload alist of strings, or NIL."
  (handler-case
      (let ((text (text-of octets))
            (fields '()))
        (dolist (part (uiop:split-string text :separator "&"))
          (let ((equals (position #\= part)))
            (unless equals (return-from read-payload nil))
            (push (cons (subseq part 0 equals)
                        (unescape-payload-value (subseq part (1+ equals))))
                  fields)))
        (unless (= (length fields) (length +payload-fields+))
          (return-from read-payload nil))
        (nreverse fields))
    (error () nil)))

;;; --- sign, and check -------------------------------------------------------

(defun now-seconds ()
  "This second since the epoch, which is what a token's exp counts."
  (- (get-universal-time) 2208988800))

(defun expiry-of (payload)
  "PAYLOAD's exp as an integer, or NIL."
  (ignore-errors (parse-integer (or (cdr (assoc "exp" payload :test #'string=)) "")
                                 :junk-allowed t)))

(defun signature-holds-p (body signature key)
  "Whether SIGNATURE is KEY's HMAC over BODY."
  (and body signature
       (let ((expected (hmac-sha256 key body)))
         (and (= (length expected) (length signature))
              (ironclad:constant-time-equal expected signature)))))

(defun sign-token (payload key)
  "PAYLOAD signed under KEY => the token text. PAYLOAD is an alist of
(sub dn em nm kind exp); nothing of it is stored anywhere."
  (let* ((body (payload-text payload))
         (signature (hmac-sha256 key body)))
    (format nil "~a.~a" (base64-url-encode body) (base64-url-encode signature))))

(defun verify-token (token key &key now)
  "TOKEN's payload when its signature and its expiry hold, else NIL. NOW
defaults to this second; the directory is not asked."
  (when (stringp token)
    (let ((dot (position #\. token)))
      (when (and dot (> dot 0) (< dot (1- (length token))))
        (let* ((body (base64-url-decode (subseq token 0 dot)))
               (signature (base64-url-decode (subseq token (1+ dot))))
               (payload (and body signature (signature-holds-p body signature key)
                             (read-payload body)))
               (expiry (and payload (expiry-of payload))))
          (and expiry (> expiry (or now (now-seconds))) payload))))))
