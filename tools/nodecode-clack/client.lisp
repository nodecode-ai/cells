;;;; client.lisp --- the state the cell keeps and the relay it talks to.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What the original keeps in plugin-data/clack, this keeps in the cell's
;;;; home under clack/: identity.json and identity.key (the Ed25519 seed, the
;;;; one thing not to lose), relay-pin.json (the RSA key the relay proved on
;;;; first contact), outbox.json (the single send whose outcome is unknown),
;;;; delivered-<id>.json (the delivery journal), dead-<id>.json (a message
;;;; refused and never acknowledged) and inbox-cursor.json.
;;;;
;;;; One relay endpoint family, /v1/ and /i/, of one origin, and every request
;;;; signed: the canonical string is the plugin's,
;;;;   clack-ed25519-v1 \n METHOD \n PATH \n SHA256(body) \n NONCE.

(in-package #:nodecode-clack)

;;; --- what the cell reads from the section -----------------------------------------

(defvar *relay* nil "The relay origin the cell runs on, fixed when it starts.")
(defvar *relay-key* nil "The configured relay_pubkey, text or object.")
(defvar *relay-timeout* 10)
(defvar *data-dir* nil "Where this cell keeps its identity and journals.")

(defun data-dir ()
  (or *data-dir* (setf *data-dir* (nlk:home "clack/"))))

(defun state-path (name)
  (merge-pathnames name (pathname (data-dir))))

(defun ensure-data-dir ()
  (ensure-directories-exist (data-dir))
  (data-dir))

(defun read-state (name)
  (let ((path (state-path name)))
    (when (probe-file path) (nlk:read-text path))))

(defun write-state (name text)
  (ensure-data-dir)
  (nlk:write-file-atomically (state-path name) text :mode #o600 :directory-mode #o700)
  text)

(defun read-state-json (name)
  (let ((text (read-state name)))
    (and text (ignore-errors (nlk:decode-json text)))))

(defun write-state-json (name object)
  (write-state name (nlk:encode-json-object object)))

;;; --- reading the relay's answers --------------------------------------------------

(defun jget (object key) (and (hash-table-p object) (gethash key object)))
(defun jstr (object key) (let ((value (jget object key))) (and (stringp value) value)))
(defun jint (object key) (let ((value (jget object key))) (and (integerp value) value)))
(defun jlist (object key)
  (let ((value (jget object key)))
    (cond ((vectorp value) (coerce value 'list)) ((listp value) value) (t '()))))

;;; --- the identity ----------------------------------------------------------------

(defun seed-text (seed) (b64u-encode seed))

(defun kept-seed ()
  (let ((text (read-state "identity.key")))
    (and text (b64u-decode (string-trim '(#\Space #\Newline #\Return) text)))))

(defun load-identity ()
  "=> (values IDENTITY SEED), the kept identity and the Ed25519 seed behind it."
  (let* ((identity (read-state-json "identity.json"))
         (seed (kept-seed)))
    (when (and identity (not seed)) (fail "The identity key is missing: keep this state for recovery"))
    (when (and seed (not identity)) (fail "An orphan identity key: recover it explicitly, never replace it"))
    (when (and seed (/= (length seed) 32)) (fail "The identity key is not 32 bytes"))
    (when (and seed identity
               (not (equal (b64u-encode (ed25519-public seed)) (jstr identity "identity_pubkey"))))
      (fail "The identity and its key disagree: keep this state for recovery"))
    (when (and identity (jget identity "service_token_enc"))
      (let* ((sealed (gethash "service_token_enc" identity))
             (key (hkdf-sha256 seed (utf8 +token-info+)))
             (plain (ignore-errors
                      (aead-decrypt key (b64u-decode (jstr sealed "nonce"))
                                    (b64u-decode (jstr sealed "ct"))
                                    (b64u-decode (jstr sealed "tag"))
                                    (utf8 "clack-token")))))
        (unless plain (fail "The kept relay token does not open with this identity"))
        (remhash "service_token_enc" identity)
        (setf (gethash "service_token" identity) (as-text plain))))
    (values identity seed)))

(defun save-identity (identity)
  "Keep IDENTITY, its relay token sealed under the identity seed, never in the clear."
  (let* ((copy (nlk:decode-json (nlk:encode-json-object identity)))
         (token (jstr copy "service_token"))
         (seed (kept-seed)))
    (remhash "service_token" copy)
    (when token
      (unless (and seed (= (length seed) 32))
        (fail "No identity key to seal the relay token under"))
      (let* ((key (hkdf-sha256 seed (utf8 +token-info+)))
             (nonce (ironclad:random-data 12)))
        (multiple-value-bind (ciphertext tag) (aead-encrypt key nonce (utf8 token) (utf8 "clack-token"))
          (setf (gethash "service_token_enc" copy)
                (nlk:make-json-object "nonce" (b64u-encode nonce)
                                       "ct" (b64u-encode ciphertext)
                                       "tag" (b64u-encode tag))))))
    (write-state-json "identity.json" copy)))

(defun create-identity (relay)
  "A fresh Ed25519 identity for RELAY: the seed lands before the record that names it."
  (let ((seed (ironclad:random-data 32)))
    (write-state "identity.key" (seed-text seed))
    (let ((identity (nlk:make-json-object "relay_url" relay
                                           "identity_pubkey" (b64u-encode (ed25519-public seed)))))
      (save-identity identity)
      (values identity seed))))

(defun enrolled-p (identity) (and identity (jstr identity "service_token") t))

;;; --- the wire --------------------------------------------------------------------

(defun loopback-host-p (host)
  (or (equal host "localhost") (equal host "::1")
      (and (>= (length host) 4) (uiop:string-prefix-p "127." host))))

(defun normalize-origin (value)
  "VALUE as the clean origin the plugin insists on: HTTPS, or HTTP on loopback."
  (unless (and (stringp value) (plusp (length value))
               (every (lambda (ch) (> (char-code ch) 32)) value)
               (not (find-if (lambda (ch) (find ch "\\%?#")) value)))
    (fail "Invalid relay origin"))
  (let* ((uri (quri:uri value))
         (scheme (quri:uri-scheme uri))
         (host (quri:uri-host uri))
         (path (quri:uri-path uri)))
    (unless (and (stringp scheme) (member scheme '("http" "https") :test #'string=)
                 (stringp host) (plusp (length host))
                 (or (null path) (string= path "") (string= path "/"))
                 (null (quri:uri-query uri))
                 (null (quri:uri-fragment uri))
                 (not (find #\@ value)))
      (fail "Relay must be a clean HTTPS origin"))
    (when (and (string= scheme "http") (not (loopback-host-p (string-downcase host))))
      (fail "HTTP is permitted only on loopback"))
    (string-right-trim "/" value)))

(defun nonce-text ()
  (format nil "~d:~a" (get-universal-time) (hex-of (ironclad:random-data 16))))

(defun auth-headers (method path raw identity seed)
  (let* ((nonce (nonce-text))
         (canonical (format nil "clack-ed25519-v1~%~a~%~a~%~a~%~a"
                            method path
                            (hex-of (sha256 (or raw (make-octets 0))))
                            nonce)))
      (list (cons "authorization" (concatenate 'string "Bearer " (jstr identity "service_token")))
            (cons "x-clack-scheme" "1")
            (cons "x-clack-key" (jstr identity "peer_name"))
            (cons "x-clack-nonce" nonce)
            (cons "x-clack-sig" (hex-of (ed25519-sign seed (utf8 canonical)))))))

(defun exchange (method path &key body (authenticated t) (verify t))
  "One exchange with the relay => the decoded JSON object.
PATH is /v1/... or /i/...; BODY an object or NIL. Verification of the relay
key runs first unless VERIFY is NIL (the challenge itself)."
  (unless (or (uiop:string-prefix-p "/v1/" path) (uiop:string-prefix-p "/i/" path))
    (fail "Unsupported relay endpoint"))
  (unless *relay* (fail "The clack section has no relay_url"))
  (when verify (verify-relay))
  (let* ((raw (and body (utf8 (nlk:encode-json-object body))))
         (headers (append '(("accept" . "application/json")
                            ("content-type" . "application/json")
                            ("user-agent" . "Nodecode-Clack/0.1.0"))
                          (when authenticated
                            (multiple-value-bind (identity seed) (load-identity)
                              (unless (enrolled-p identity)
                                (fail "Not enrolled: use clack_join with a trusted private invitation"))
                              (auth-headers (if body "POST" "GET") path raw identity seed))))))
    (multiple-value-bind (text status)
        (nlk:http method (concatenate 'string *relay* path)
                  :headers headers
                  :content (and raw (as-text raw))
                  :timeout *relay-timeout* :connect-timeout *relay-timeout*)
      (let ((answer (and (stringp text) (ignore-errors (nlk:decode-json text)))))
        (unless (and (integerp status) (<= 200 status 299) (hash-table-p answer))
          (fail "Relay rejected the request (HTTP ~a)" (or status "no answer")))
        (when (> (length (or text "")) +max-response+) (fail "Relay answer exceeded the limit"))
        answer))))

(defun relay-get (path &key (authenticated t)) (exchange :get path :authenticated authenticated))
(defun relay-post (path body &key (authenticated t))
  (exchange :post path :body body :authenticated authenticated))

;;; --- the relay's own key ---------------------------------------------------------

(defun configured-relay-key ()
  "The RELAY-KEY as the relay's (values N E), or NIL when none is configured."
  (let ((value *relay-key*))
    (when (and (stringp value) (plusp (length (string-trim " " value))))
      (let ((object (or (ignore-errors (nlk:decode-json value)) value)))
        (unless (hash-table-p object) (fail "relay_pubkey must be a JSON RSA public key"))
        (relay-key-from (jstr object "n") (jstr object "e"))))))

(defun relay-key-from (n e)
  (let* ((n (and (stringp n) (ignore-errors (parse-integer n :radix 16))))
         (e (and (stringp e) (ignore-errors (parse-integer e :radix 16)))))
    (unless (and n e (= (integer-length n) 2048) (oddp n) (= e 65537))
      (fail "Invalid RSA-2048 relay public key"))
    (values n e)))

(defun verify-relay ()
  "A fresh challenge from the relay, checked against the pinned or configured
key before any token, proof or body leaves: what the plugin does on every call."
  (let* ((pin (read-state-json "relay-pin.json"))
         (pinned (and pin (relay-key-from (jstr (gethash "public_key" pin) "n")
                                          (jstr (gethash "public_key" pin) "e"))))
         (configured (configured-relay-key)))
    (when pin
      (unless (equal *relay* (jstr pin "relay_url"))
        (fail "The relay pin belongs to another origin"))
      (when (and configured (/= configured pinned))
        (fail "relay_pubkey differs from the durable pin")))
    (let ((expected (or pinned configured)))
      (unless expected
        (fail "Provision an independently verified relay_pubkey before first contact"))
      (let* ((nonce (hex-of (ironclad:random-data 32)))
             (proof (exchange :get (concatenate 'string "/v1/identity?nonce=" nonce)
                              :authenticated nil :verify nil)))
        (multiple-value-bind (n e) (relay-key-from (jstr (jget proof "public_key") "n")
                                                    (jstr (jget proof "public_key") "e"))
          (unless (and (= n expected) (= e 65537))
            (fail "The relay's key is not the pinned key"))
          (unless (and (equal (jstr proof "nonce") nonce)
                       (equal (jstr proof "algorithm") "rsassa-pkcs1-v1_5-sha256")
                       (rsa-verify n e (b64-standard-decode (or (jstr proof "signature") ""))
                                   (octets-of-hex nonce)))
            (fail "The relay's identity proof does not verify; nothing was sent"))
          (unless pin
            (write-state-json "relay-pin.json"
                               (nlk:make-json-object
                                "relay_url" *relay*
                                "public_key" (nlk:make-json-object
                                              "n" (format nil "~(~x~)" n)
                                              "e" "10001")))))))))
