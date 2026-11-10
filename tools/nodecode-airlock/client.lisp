;;;; client.lisp --- the three routes of an Airlock deployment, and the trust
;;;; material the section pins.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Airlock's public HTTP surface is small: GET /healthz, POST /api/arena
;;;; (the policy gate, and the sandbox when the answer is allow), and
;;;; GET /api/receipt/{id}. Nothing here invents a fourth. The base URL is the
;;;; operator's own -- there is no built-in host, no demo default, and no
;;;; fallback: with none set the cell refuses before dialling anything.

(in-package #:nodecode-airlock)

;;; --- the section, read ------------------------------------------------------

(defun trimmed (text)
  "TEXT without its edges, or NIL when it is not a string or is only space."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defparameter +languages+ '("bash" "python")
  "The arena languages the plugin names in its config_schema.")

(defun airlock-origin-p (url)
  "Whether URL is the clean https origin this client dials: a scheme, a host,
an optional port, and nothing else -- no userinfo, path, query or fragment."
  (and (or (ppcre:scan "^https://[A-Za-z0-9]([A-Za-z0-9.-]*[A-Za-z0-9])?(:[0-9]{1,5})?$" url)
           (ppcre:scan "^https://\\[[0-9A-Fa-f:]+\\](:[0-9]{1,5})?$" url))
       t))

(defun read-settings (values table)
  "The declared VALUES, with the endpoint stripped of a trailing slash, the
optional token resolved from its variable, and every fail-closed check the
plugin made before a call made here instead."
  (declare (ignore table))
  (let* ((raw (trimmed (getf values :base-url)))
         (base-url (and raw (string-right-trim "/" raw)))
         (key (let ((key (trimmed (getf values :public-key-hex))))
                (and key (string-downcase key))))
         ;; The section parser has already applied the declaration's default,
         ;; so NIL here is an explicit "verify_receipts": false and nothing else.
         (verify (getf values :verify-receipts))
         (language (or (trimmed (getf values :language)) "bash"))
         (timeout (or (getf values :timeout-s) 60))
         (token-env (trimmed (getf values :token-env)))
         (token (or (trimmed (getf values :token))
                    (and token-env (trimmed (uiop:getenv token-env))))))
    (when (and base-url (not (airlock-origin-p base-url)))
      (nlk:config-error "base_url ~s is not a clean https origin: no scheme other than https, no userinfo, path, query or fragment (put a token in token, not in the URL)" base-url))
    ;; A key, when one is set at all, is checked even with verification off:
    ;; a 63-character key is a typo, not a preference.
    (when (and key (not (hex-string-p key 64)))
      (nlk:config-error "public_key_hex is not exactly 64 lowercase hex characters (an Ed25519 raw public key)"))
    (when (and verify (not key))
      (nlk:config-error "verify_receipts is on and public_key_hex names no key: pin the Ed25519 key you obtained out-of-band from the operator of your Airlock, or set verify_receipts false to see ALLOWED instead of VERIFIED"))
    (unless (member language +languages+ :test #'equal)
      (nlk:config-error "language ~s is not one of ~{~a~^, ~}" language +languages+))
    (unless (and (integerp timeout) (<= 1 timeout 3600))
      (nlk:config-error "timeout_s ~s is not a whole number of seconds between 1 and 3600" timeout))
    (list :base-url base-url :public-key-hex key :verify-receipts verify
          :language language :timeout-s timeout :token-env token-env :token token)))

;;; --- one exchange -----------------------------------------------------------

(defun airlock-url (route)
  "ROUTE on the origin the section names."
  (let ((base (setting :base-url)))
    (unless base
      (fail "the airlock section names no base_url: set it to the https origin of your Airlock deployment (there is no built-in host)"))
    (concatenate 'string base route)))

(defun readable (condition)
  "CONDITION as one short line."
  (nlk:one-line (princ-to-string condition) :cap 200))

(defun exchange (method route &key body timeout)
  "METHOD ROUTE with BODY, an object, or NIL. => (values TEXT STATUS).
Airlock's bearer token, when the section names one, rides every call."
  (let* ((json (and body (nlk:encode-json-object body)))
         (token (setting :token))
         (headers (append (list (cons "Accept" "application/json"))
                          (when token
                            (list (cons "Authorization" (format nil "Bearer ~a" token))))
                          (when json
                            (list (cons "Content-Type" "application/json")))))
         (seconds (or timeout (setting :timeout-s) 60)))
    (handler-case
        (nlk:http method (airlock-url route) :headers headers :content json
                  :timeout seconds)
      (error (condition)
        (fail "Airlock could not be reached at ~a (~a); nothing ran on this machine"
              (airlock-url route) (readable condition))))))

(defun failure-detail (object)
  "What a failed Airlock answer says, as text."
  (let ((detail (and (hash-table-p object) (gethash "detail" object)))
        (error (and (hash-table-p object) (gethash "error" object))))
    (let ((said (or detail error)))
      (cond ((stringp said) said)
            ((and said (hash-table-p said)) (nlk:encode-json-object said))
            ((and said (vectorp said)) (nlk:encode-json-object said))
            (t "")))))

(defun call-json (method route &key body timeout)
  "METHOD ROUTE, answered as JSON. Refuses on a redirect, a non-JSON body, or
any status at or past 400 -- fail closed, as the plugin did."
  (multiple-value-bind (text status) (exchange method route :body body :timeout timeout)
    (when (<= 300 status 399)
      (fail "Airlock answered HTTP ~d for ~a and a redirect is refused, so the command body is never replayed onto another origin; dial the origin that answers directly"
            status route))
    (let ((object (handler-case (nlk:decode-json (or text "") :whole t)
                    (error () nil))))
      (unless object
        (fail "Airlock answered ~a with something that is not JSON (HTTP ~d)" route status))
      (when (>= status 400)
        (fail "Airlock answered HTTP ~d for ~a: ~a" status route (failure-detail object)))
      object)))

;;; --- the three routes -------------------------------------------------------

(defun healthz ()
  "Airlock's own health answer, which must say ok true."
  (let ((answer (call-json "GET" "/healthz" :timeout 20)))
    (unless (and (hash-table-p answer) (eq t (gethash "ok" answer)))
      (fail "Airlock /healthz did not report ok=true"))
    answer))

(defun arena (code &key input-text)
  "CODE, and INPUT-TEXT, posted to the arena: the gate decides, the sandbox
runs when it allows. => the arena step, an object."
  (let ((body (apply #'nlk:make-json-object
                     (append (list "lang" (setting :language) "code" code)
                             (when input-text (list "input_text" input-text))))))
    (multiple-value-bind (answer status)
        (exchange "POST" "/api/arena" :body body :timeout (setting :timeout-s))
      (when (<= 300 status 399)
        (fail "Airlock answered HTTP ~d to the arena and a redirect is refused, so the command body is never replayed onto another origin" status))
      (let ((object (handler-case (nlk:decode-json (or answer "") :whole t)
                      (error (condition)
                        (fail "Airlock's arena answer is not JSON (~a)" (readable condition))))))
        (unless (hash-table-p object)
          (fail "Airlock's arena answer is not a JSON object"))
        (when (>= status 400)
          (fail "Airlock refused the call with HTTP ~d: ~a" status (failure-detail object)))
        object))))

(defun receipt-text (id)
  "The receipt ID as Airlock serves it, unparsed."
  (unless (hex-string-p (string-downcase (or id "")) 64)
    (fail "receipt ~s is not a receipt id (64 hex characters)" id))
  (multiple-value-bind (text status)
      (exchange "GET" (format nil "/api/receipt/~a" (string-downcase id)) :timeout 20)
    (when (<= 300 status 399)
      (fail "Airlock answered HTTP ~d for the receipt and a redirect is refused" status))
    (when (>= status 400)
      (fail "Airlock answered HTTP ~d for receipt ~a" status id))
    text))
