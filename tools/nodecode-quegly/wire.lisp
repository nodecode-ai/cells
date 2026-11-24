;;;; wire.lisp --- the one network seam: the key, the request, what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through REQUEST, so a test stubs the one
;;;; name NLK:HTTP and never touches the network. Quegly answers JSON and
;;;; names its refusals in the body (code, message, field), so anything but a
;;;; 2xx becomes QUEGLY-ERROR carrying the service's own words, with the key
;;;; redacted out of every byte the model reads.

(in-package #:nodecode-quegly)

(defparameter +base-url+ "https://api.quegly.com/v1"
  "Quegly's public API, where every call goes unless the section names another.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, trimmed, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun api-key ()
  "The key the section carries, else QUEGLY_API_KEY."
  (or (nonempty (setting :api-key))
      (nonempty (uiop:getenv "QUEGLY_API_KEY"))))

(defun check-config ()
  "Refuse to start a section that could reach Quegly with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "quegly: this cell needs a key: paste it in \"api_key\" in the quegly "
                                 "section, or export QUEGLY_API_KEY; a key is made at "
                                 "quegly.com/settings/api and starts with qgl_"))))

(defun root ()
  "The endpoint this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +base-url+)))

(defun redact (text)
  "TEXT with the key replaced, so no answer ever carries it."
  (let ((key (api-key)))
    (if (and key (stringp text))
        (with-output-to-string (out)
          (loop with start = 0
                for found = (search key text :start2 start)
                do (if found
                       (progn (write-string text out :start start :end found)
                              (write-string "[redacted]" out)
                              (setf start (+ found (length key))))
                       (progn (write-string text out :start start)
                              (return)))))
        text)))

(defun workspace-id (given)
  "The workspace this call acts on: the call's own, else the section's."
  (or (nonempty given)
      (nonempty (setting :workspace-id))))

(defun query-string (pairs)
  "PAIRS -- (NAME . VALUE) -- as a query string, leaving out every NIL or empty
VALUE; NIL when none is left, so a path with no query keeps no question mark."
  (let ((kept (loop for (name . value) in pairs
                    when (and value (not (equal value "")))
                      collect (format nil "~a=~a"
                                      name
                                      (quri:url-encode (princ-to-string value) :encoding :utf-8)))))
    (and kept (format nil "~{~a~^&~}" kept))))

(defun decode-body (text what)
  "TEXT, Quegly's answer, decoded: always a JSON object."
  (cond ((null text) (nlk:make-json-object))
        ((not (stringp text))
         (fail "quegly ~a: Quegly answered something that is not text" what))
        ((zerop (length (nlk:trimmed text))) (nlk:make-json-object))
        (t (let ((body (handler-case (nlk:decode-json text :whole t)
                         (error () (fail "quegly ~a: Quegly answered malformed JSON" what)))))
             (if (hash-table-p body)
                 body
                 (nlk:make-json-object "value" body))))))

(defun error-part (body key)
  (nlk:json-value body :text "error" key))

(defun refusal (what status body)
  "What a refused call says, in the service's own words plus what to do about it."
  (let ((code (error-part body "code"))
        (message (error-part body "message"))
        (field (error-part body "field")))
    (format nil "quegly ~a: HTTP ~a~@[ ~a~]~@[: ~a~]~@[ - fix the field ~a~]~a"
            what status code message field
            (cond ((and (integerp status) (= status 401))
                   "; Quegly rejected the API key: check api_key in the quegly section")
                  ((and (integerp status) (= status 403))
                   "; the subscription may be inactive, or the key lacks that workspace's scope")
                  ((and (integerp status) (= status 429))
                   "; the key's rate limit is spent: wait a moment rather than retrying at once")
                  ((equal code "CHANNEL_PAUSED")
                   "; that account is paused past the plan's channels: leave it out of account_ids")
                  (t "")))))

(defun request (method path &key query body workspace idempotency (what path))
  "One exchange with Quegly. => the decoded body, a hash table. A transport
failure or a refused status signals QUEGLY-ERROR, the key redacted out."
  (let* ((url (format nil "~a~a~@[?~a~]" (root) path (query-string query)))
         (content (and body (nlk:encode-json-object body)))
         (headers (append (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
                                (cons "accept" "application/json"))
                          (when content (list (cons "content-type" "application/json")))
                          (let ((id (workspace-id workspace)))
                            (when id (list (cons "x-workspace-id" id))))
                          (when (nonempty idempotency)
                            (list (cons "idempotency-key" idempotency)))))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers headers
                    :content content
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "quegly ~a: could not reach Quegly - ~a"
              what (redact (nle:transport-failure-label condition (root))))))
    (let ((decoded (decode-body answer what)))
      (unless (and (integerp status) (< status 300))
        (fail "~a" (refusal what status decoded)))
      decoded)))
