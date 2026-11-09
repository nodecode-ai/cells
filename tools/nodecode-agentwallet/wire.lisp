;;;; wire.lisp --- the one network seam, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through CALL, so a test stubs the one
;;;; name and never touches the network. OpenClawCash's API answers a JSON
;;;; body and an HTTP status as a value; anything but a 2xx is
;;;; AGENTWALLET-ERROR carrying the service's own message, which is the text
;;;; the model repairs from.
;;;;
;;;; The agent key is the original's: an X-Agent-Key header, to
;;;; https://openclawcash.com or a subdomain of it and nowhere else. The
;;;; plugin read its base URL from the environment, and so the allowlist
;;;; exists here for the same reason -- a tampered base_url would otherwise
;;;; send the key to a host of the tamperer's choosing.

(in-package #:nodecode-agentwallet)

(defparameter +base-url+ "https://openclawcash.com"
  "OpenClawCash's API, where every tool goes unless the section names another host.")

(defparameter +user-agent+ "agentwalletapi-hermes"
  "The name the original sent, so the service sees the same client it always has.")

(defparameter +host+ "openclawcash.com"
  "The one domain the agent key may travel to.")

(defparameter +max-answer-chars+ 20000
  "How much of a one-value answer a tool result carries.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

;;; --- the section's values ------------------------------------------------------

(defun agent-key-variable ()
  "The variable the agent key is read from: the section's, else the plugin's own."
  (or (nonempty (setting :agent-key-env)) "OPENCLAWCASH_AGENT_KEY"))

(defun agent-key ()
  "The agent key, inline or from the variable the section names."
  (or (nonempty (setting :agent-key))
      (nonempty (uiop:getenv (agent-key-variable)))))

(defun passphrase-variable ()
  "The variable the export passphrase is read from, never a tool argument."
  (or (nonempty (setting :export-passphrase-env)) "OPENCLAWCASH_EXPORT_PASSPHRASE"))

(defun export-passphrase ()
  "The export passphrase wallet creation needs, as the operator exported it."
  (nonempty (uiop:getenv (passphrase-variable))))

;;; --- the allowlist ---------------------------------------------------------------

(defun label-char-p (char)
  "True for what a DNS label may carry: a letter, a digit or a hyphen."
  (or (alphanumericp char) (char= char #\-)))

(defun allowed-host-p (host)
  "HOST is openclawcash.com or one label of it: a.b.openclawcash.com is not."
  (or (string-equal host +host+)
      (let ((suffix (concatenate 'string "." +host+)))
        (and (> (length host) (length suffix))
             (uiop:string-suffix-p host suffix)
             (let ((label (subseq host 0 (- (length host) (length suffix)))))
               (and (every #'label-char-p label)
                    (not (char= #\- (char label 0)))
                    (not (char= #\- (char label (1- (length label)))))))))))

(defun configured-root ()
  "(values ROOT REASON): the https root the key may travel to, or NIL and why not."
  (let ((given (string-right-trim "/" (or (nonempty (setting :base-url)) +base-url+))))
    (cond ((not (uiop:string-prefix-p "https://" given))
           (values nil (format nil "~a is not https" given)))
          (t
           (let ((rest (subseq given (length "https://"))))
             (cond ((zerop (length rest))
                    (values nil (format nil "~a names no host" given)))
                   ((or (find #\@ rest) (find #\/ rest) (find #\: rest))
                    (values nil (format nil "~a must not carry a path, a port or credentials" given)))
                   ((not (allowed-host-p rest))
                    (values nil (format nil "~a is not ~a or a subdomain of it" given +host+)))
                   (t (values (concatenate 'string "https://" rest) nil))))))))

(defun root ()
  "The API root this cell talks to; signals when the section names a host the key may not visit."
  (multiple-value-bind (value reason) (configured-root)
    (or value (fail "agentwallet: ~a" reason))))

(defun check-config ()
  "Refuse a start that could not reach OpenClawCash, naming the member to set."
  (unless (agent-key)
    (error 'nlk:config-refusal
           :detail (format nil "agentwallet: this cell needs an agent key: set ~a in the shell that starts ~
Nodecode, or \"agent_key\" (or \"agent_key_env\") in the agentwallet section" (agent-key-variable))))
  (multiple-value-bind (value reason) (configured-root)
    (declare (ignore value))
    (when reason
      (error 'nlk:config-refusal
             :detail (format nil "agentwallet: base_url ~a; use ~a or a subdomain of it" reason +base-url+)))))

;;; --- one exchange ----------------------------------------------------------------

(defun url-encode (text)
  "TEXT escaped for a query string, byte by byte over its UTF-8 octets."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          do (let ((char (code-char octet)))
               (if (or (and char (alphanumericp char)) (find char "-._~"))
                   (write-char char out)
                   (format out "%~2,'0X" octet))))))

(defun query-string (query)
  "QUERY -- an alist of (NAME . VALUE) -- as ?a=b&c=d, or the empty string."
  (let ((kept (remove-if-not #'cdr query)))
    (if (null kept)
        ""
        (format nil "?~{~a~^&~}"
                (mapcar (lambda (pair)
                          (format nil "~a=~a" (car pair) (url-encode (cdr pair))))
                        kept)))))

(defun request-headers (with-body)
  "What every request carries: the key in the original's header, and a name Cloudflare lets through."
  (append (list (cons "accept" "application/json")
                (cons "user-agent" +user-agent+)
                (cons "x-agent-key" (or (agent-key) "")))
          (when with-body
            (list (cons "content-type" "application/json")))))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (nlk:trimmed answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value (nlk:json-value body :object "detail") :text "message")
        (nlk:json-value body :text "message")
        (nlk:json-value body :text "error")
        (nlk:json-value (nlk:json-value body :object "detail") :text "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun call (method path &key query body)
  "METHOD on PATH at the configured root => the decoded answer. A transport
failure or a refused status signals AGENTWALLET-ERROR."
  (let* ((url (concatenate 'string (root) path (query-string query)))
         (limit (setting :timeout-seconds))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (request-headers (and body t))
                    :content (and body (nlk:encode-json-object body))
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "agentwallet ~a: ~a" path (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300)) (decode-body answer))
          ((and (integerp status) (= status 401))
           (fail "agentwallet ~a: HTTP 401, the agent key was refused; check agent_key (or agent_key_env) ~
in the agentwallet section" path))
          ((and (integerp status) (= status 404))
           (fail "agentwallet ~a: HTTP 404, no such wallet on this agent key; list them with ~
agentwalletapi_wallets_list" path))
          (t (fail "agentwallet ~a: HTTP ~d~@[: ~a~]" path status (body-error-text answer))))))

;;; --- reading the answer ----------------------------------------------------------

(defun value-text (object &rest keys)
  "The first of KEYS carrying a scalar in OBJECT, as text; NIL when none does."
  (loop for key in keys
        thereis (let ((value (and (hash-table-p object) (gethash key object))))
                  (cond ((stringp value) (nonempty value))
                        ((numberp value) (princ-to-string value))
                        ((eq value t) "true")))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun json-text (value &optional (limit +max-answer-chars+))
  "VALUE as the JSON the service answered, cut to LIMIT characters."
  (clip (if (hash-table-p value)
            (nlk:encode-json-object value)
            (princ-to-string value))
        limit))
