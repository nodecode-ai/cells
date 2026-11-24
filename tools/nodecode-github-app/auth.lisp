;;;; auth.lisp --- the App's JWT, its PEM key, and the credential a call runs on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original resolved credentials bot-first (github_app/auth.py,
;;;; github_app/client.py): a GitHub App's App ID, installation ID and private
;;;; key sign an RS256 JWT, the JWT is exchanged for an installation token,
;;;; and every result is attributed to <slug>[bot]. A personal access token,
;;;; or an already-signed-in gh CLI, answer for a human instead. The RSA
;;;; signing the original delegated to PyJWT is done here with ironclad, which
;;;; the image already carries, so the cell needs no Python and no openssl.

(in-package #:nodecode-github-app)

;;; --- octets and base64 -------------------------------------------------------

(defun text-octets (text)
  "TEXT's bytes: the JWT's segments are ASCII by construction."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter +base64url-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(defun base64-decode (text)
  "TEXT, base64 or base64url, with or without line breaks and padding, as octets."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (bits 0)
        (count 0))
    (loop for char across (remove-if (lambda (char) (find char (list #\Space #\Tab #\Return #\Linefeed #\=) :test #'char=))
                                     (substitute #\+ #\- (substitute #\/ #\_ text)))
          for value = (position char +base64-alphabet+ :test #'char=)
          when value
            do (setf bits (+ (ash bits 6) value)
                     count (+ count 6))
               (when (>= count 8)
                 (decf count 8)
                 (vector-push-extend (ldb (byte 8 count) bits) out)
                 (setf bits (logand bits (1- (ash 1 count))))))
    (coerce out '(vector (unsigned-byte 8)))))

(defun base64url (octets)
  "OCTETS as a JWT segment: base64url, no padding."
  (let ((out (make-array (* 4 (ceiling (length octets) 3))
                          :element-type 'character :fill-pointer 0)))
    (loop for start from 0 below (length octets) by 3
          for left = (- (length octets) start)
          for word = (logior (ash (aref octets start) 16)
                             (ash (if (> left 1) (aref octets (1+ start)) 0) 8)
                             (if (> left 2) (aref octets (+ start 2)) 0))
          do (dotimes (index (if (> left 2) 4 (1+ left)))
               (vector-push (aref +base64url-alphabet+
                                   (ldb (byte 6 (* 6 (- 3 index))) word))
                            out)))
    (coerce out 'string)))

;;; --- the App's private key -----------------------------------------------------

(defun der-element (octets position)
  "The DER element at POSITION => (values TAG CONTENT-START CONTENT-END NEXT)."
  (let* ((tag (aref octets position))
         (index (1+ position))
         (first (aref octets index))
         (length 0))
    (incf index)
    (if (< first #x80)
        (setf length first)
        (let ((count (logand first #x7f)))
          (dotimes (offset count)
            (setf length (+ (ash length 8) (aref octets (+ index offset)))))
          (incf index count)))
    (values tag index (+ index length) (+ index length))))

(defun octets-integer (octets start end)
  "The unsigned integer OCTETS spells between START and END."
  (let ((value 0))
    (loop for index from start below end
          do (setf value (+ (ash value 8) (aref octets index))))
    value))

(defun rsa-numbers (der)
  "N and D (values) from a PKCS#1 RSAPrivateKey DER: version, n, e, d, ..."
  (multiple-value-bind (tag start end) (der-element der 0)
    (declare (ignore end))
    (unless (= tag #x30)
      (fail "the App's private key is not a DER SEQUENCE"))
    (let ((position start)
          (numbers '()))
      (loop repeat 4
            while (< position (length der))
            do (multiple-value-bind (inner from to next) (der-element der position)
                 (push (if (= inner #x02) (octets-integer der from to) 0) numbers)
                 (setf position next)))
      (destructuring-bind (version n exponent d) (nreverse numbers)
        (declare (ignore version exponent))
        (values n d)))))

(defun pem-numbers (pem)
  "N and D (values) of the RSA private key PEM carries: PKCS#1
(\"BEGIN RSA PRIVATE KEY\") or PKCS#8 (\"BEGIN PRIVATE KEY\"), as GitHub hands
the key out and as openssl writes it."
  (let ((der (base64-decode
              (with-output-to-string (out)
                (dolist (line (uiop:split-string (or pem "") :separator '(#\Newline)))
                  (let ((line (nlk:trimmed line)))
                    (unless (uiop:string-prefix-p "-----" line)
                      (write-string line out))))))))
    (unless (plusp (length der))
      (fail "the App's private key holds no PEM body"))
    (multiple-value-bind (tag start end) (der-element der 0)
      (declare (ignore end))
      (unless (= tag #x30)
        (fail "the App's private key is not a DER SEQUENCE"))
      (multiple-value-bind (inner from to next) (der-element der start)
        (declare (ignore next))
        (if (= inner #x02)
            (rsa-numbers der)
            ;; PKCS#8: version, an AlgorithmIdentifier SEQUENCE, then an OCTET
            ;; STRING holding the PKCS#1 key.
            (multiple-value-bind (tag key-start key-end) (der-element der to)
              (declare (ignore key-start))
              (unless (= tag #x04)
                (fail "the App's private key is PKCS#8 without an OCTET STRING key"))
              (rsa-numbers (subseq der key-end))))))))

;;; --- RS256 ------------------------------------------------------------------------

(defparameter +sha256-digest-info+
  #(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01
    #x05 #x00 #x04 #x20)
  "The DER DigestInfo header RSASSA-PKCS1-v1_5 puts before a SHA-256 digest.")

(defun rsa-sign (message n d)
  "MESSAGE's SHA-256, signed RSASSA-PKCS1-v1_5 as a JWT's RS256 signature is
built, with the key N and D: the original delegated exactly this to PyJWT."
  (let* ((size (ceiling (integer-length n) 8))
         (digest (ironclad:digest-sequence :sha256 message))
         (info (concatenate '(vector (unsigned-byte 8)) +sha256-digest-info+ digest))
         (fill (- size (length info) 3)))
    (when (minusp fill)
      (fail "the App's private key is too short to sign a SHA-256 digest"))
    (let ((block (concatenate '(vector (unsigned-byte 8))
                               #(#x00 #x01)
                               (make-array fill :element-type '(unsigned-byte 8)
                                               :initial-element #xff)
                               #(#x00)
                               info)))
      (ironclad:integer-to-octets
       (ironclad:expt-mod (octets-integer block 0 (length block)) d n)
       :n-bits (* 8 size)))))

(defun timestamp ()
  (get-universal-time))

(defun app-jwt (&key (now (timestamp)))
  "The GitHub App's RS256 JWT: issued a minute early and valid nine minutes,
as GitHub's own documentation bounds it."
  (let* ((issued (- now 60))
         (header (base64url (text-octets
                             (nlk:encode-json-object
                              (nlk:json-object "alg" "RS256" "typ" "JWT")))))
         (payload (base64url
                   (text-octets
                    (nlk:encode-json-object
                     (nlk:json-object "iat" issued
                                      "exp" (+ issued 540)
                                      "iss" (princ-to-string (setting :app-id)))))))
         (signing-input (format nil "~a.~a" header payload)))
    (multiple-value-bind (n d) (pem-numbers (setting :private-key))
      (format nil "~a.~a" signing-input
              (base64url (rsa-sign (text-octets signing-input) n d))))))

;;; --- the credential a call runs on ----------------------------------------------------

(defvar *auth* nil "The credential the cell resolved at start, or NIL.")
(defvar *installation* nil "The installation token, cached until it nearly expires.")
(defvar *slug* nil "The App's slug, read once.")

(defun gh-cli-token (api-url)
  "The token the operator's own gh CLI holds for API-URL's host, or NIL. The
original ran `gh auth token --hostname' the same way (github_app/auth.py)."
  (let* ((url (if (uiop:string-prefix-p "https://" (or api-url "")) api-url +default-api+))
         (rest (subseq url (length "https://")))
         (host (subseq rest 0 (or (position #\/ rest) (length rest))))
         (host (if (uiop:string-prefix-p "api." host) (subseq host 4) host)))
    (when (plusp (length host))
      (nonblank
       (ignore-errors
         (uiop:run-program (list "gh" "auth" "token" "--hostname" host)
                           :output :string :error-output :string
                           :ignore-error-status t))))))

(defun setting-or (name default)
  (let ((value (ignore-errors (setting name))))
    (if (null value) default value)))

(defun app-slug ()
  "The App's slug: the section's, else GitHub's own answer for the JWT."
  (or *slug*
      (setf *slug*
            (or (nonblank (setting-or :app-slug nil))
                (nlk:json-value (api-json :get "/app" :token (app-jwt)) :string "slug")
                "github-app"))))

(defun installation-token ()
  "An installation access token, minted once and kept until it nearly expires."
  (let ((token (getf *installation* :token))
        (expiry (getf *installation* :expires-at)))
    (if (and token expiry (> expiry (+ (timestamp) 120)))
        token
        (let* ((answer (api-json :post
                                  (format nil "/app/installations/~a/access_tokens"
                                          (nlk:trimmed (setting :installation-id)))
                                  :token (app-jwt)
                                  :body (nlk:json-object)))
               (token (nonblank (nlk:json-value answer :string "token"))))
          (unless token
            (fail "GitHub answered no installation token for installation ~a"
                  (setting :installation-id)))
          (setf *installation* (list :token token :expires-at (+ (timestamp) 3000)))
          token))))

(defun resolve-auth ()
  "The credential plist (:method, :token, :actor) every call and every answer
carries. A GitHub App signs a JWT and exchanges it; a token is used as it is."
  (case (setting :method)
    (:app (list :method "github-app"
                :token (installation-token)
                :actor (format nil "~a[bot]" (app-slug))))
    (t (list :method (if (eq (setting :method) :gh) "gh-cli" "pat")
             :token (setting :token)))))

(defun auth ()
  (or *auth* (setf *auth* (resolve-auth))))

(defun token ()
  (getf (auth) :token))

(defun actor ()
  "Who the call is attributed to: an App's <slug>[bot], or the login of the
token's own user, read once from GET /user."
  (or (getf (auth) :actor)
      (setf (getf *auth* :actor)
            (or (nlk:json-value (api-json :get "/user" :token (token)) :string "login")
                "unknown-user"))))

(defun attribution ()
  "The attribution every github_* answer carries, as the original carried it."
  (nlk:json-object "auth_method" (getf (auth) :method)
                    "actor" (actor)))
