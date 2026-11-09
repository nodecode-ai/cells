;;;; wire.lisp --- the one seam to Soniox: the key, the two roots, the request
;;;; that carries the key and never echoes it, and the poll an asynchronous
;;;; transcription is read through.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-soniox)

;;; --- the key, and the headers it rides in ----------------------------------------

(defun api-key ()
  "The key a call is billed to: the section's, else the variable api_key_env
names. A refusal here costs nothing, so it is raised before any request."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    (when (zerop (length key))
      (fail "no Soniox key: paste one into the soniox section's api_key, or set the ~
             variable ~a (create a key at https://console.soniox.com/). Nothing was sent ~
             and nothing was billed."
            (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the Soniox key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was billed."))
    key))

(defun bearer-headers (key &optional (content-type "application/json"))
  "The headers every Soniox call carries: the key as a bearer, and what the
body is."
  (append (list (cons "authorization" (format nil "Bearer ~a" key)))
          (when content-type (list (cons "content-type" content-type)))))

;;; --- the roots --------------------------------------------------------------------

(defun checked-url (text label)
  "TEXT as a root without its trailing slash, or a CONFIG-REFUSAL. Plain http
is allowed only for a loopback host: anywhere else it would send the key in
clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "the soniox section's ~a is empty" label))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest host (plusp (length host)))
        (nlk:config-error "the soniox section's ~a is ~s; it must be an https URL" label url))
      (when (and plain (not (member (string-downcase host) '("localhost" "127.0.0.1" "::1" "[::1]")
                                     :test #'string=)))
        (nlk:config-error "the soniox section's ~a is ~s; plain http would send the key in clear, so it is allowed only for a localhost stand-in"
                          label url))
      url)))

(defun base-url () (setting :api-base-url))
(defun tts-url () (setting :tts-url))

(defun transcriptions-url (&optional id rest)
  "The transcriptions endpoint: the collection, one transcription of ID, or
ID's REST under it."
  (format nil "~a/transcriptions~@[/~a~]~@[/~a~]" (base-url) id rest))

(defun files-url (&optional id)
  "The files endpoint: the collection, or one uploaded file of ID."
  (format nil "~a/files~@[/~a~]" (base-url) id))

;;; --- redaction, and the words a refusal carries ------------------------------------

(defun redact (text key)
  "TEXT with every occurrence of KEY written as ***: an endpoint that echoes
the header back must not put the key in an answer the model reads."
  (if (and key (plusp (length key)))
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search key text :start2 start)
              do (write-string text out :start start :end (or at (length text)))
              while at
              do (write-string "***" out)
                 (setf start (+ at (length key)))))
      text))

(defun error-note (status error-type)
  "The fix a failed STATUS, or Soniox's own error_type, points at, in
parentheses; NIL when neither says one."
  (or (cdr (assoc error-type '(("invalid_api_key" . " (that Soniox key is not valid)")
                                ("unauthorized" . " (check the key: create one at https://console.soniox.com/)")
                                ("insufficient_credits" . " (the Soniox account has no credit)")
                                ("payment_required" . " (add credit to the Soniox account)")
                                ("rate_limit_exceeded" . " (rate limited; try again later)")
                                ("file_not_found" . " (the uploaded file is gone; send the recording again)"))
                        :test #'string-equal))
      (cdr (assoc status '((400 . " (Soniox refused the request; its sentence says which part)")
                           (401 . " (check the key: create one at https://console.soniox.com/)")
                           (402 . " (add credit to the Soniox account)")
                           (403 . " (this key cannot use that endpoint)")
                           (404 . " (Soniox has no such resource)")
                           (429 . " (rate limited; try again later)"))))))

;;; --- one exchange -----------------------------------------------------------------

(defun request (op method url &key headers content binary timeout max-bytes)
  "One Soniox exchange, answering (values BODY STATUS HEADERS TRUNCATED).
A transport failure — DNS, TLS, a timeout — is a SONIOX-ERROR naming what
failed, with the key redacted out of its own words."
  (handler-case
      (nlk:http method url
                :headers headers :content content :binary binary
                :timeout timeout :max-bytes max-bytes)
    (error (condition)
      (fail "Soniox ~a failed (~a): ~a"
            op (type-of condition) (redact (princ-to-string condition) (or (ignore-errors (api-key)) ""))))))

(defun json-body (op body)
  "BODY decoded as the JSON object Soniox answered, or a SONIOX-ERROR: an
answer that is not one object is a malformed response, not a value."
  (let ((payload (ignore-errors (nlk:decode-json (octets-text body)))))
    (unless (hash-table-p payload)
      (fail "Soniox ~a returned a malformed response: ~a" op (compact-text body)))
    payload))

(defun answer-id (op payload)
  "The id PAYLOAD carries, or a SONIOX-ERROR: every create call answers one."
  (let ((id (nlk:json-value payload :string "id")))
    (when (or (null id) (zerop (length id)))
      (fail "Soniox ~a returned a malformed response (no id)" op))
    id))

(defun call-json (op method url key &key content timeout binary max-bytes)
  "(values PAYLOAD STATUS) of one Soniox call that answers JSON. A failed
status is a SONIOX-ERROR carrying Soniox's own sentence, redacted."
  (multiple-value-bind (body status headers truncated)
      (request op method url
               :headers (bearer-headers key (and content "application/json"))
               :content content :binary binary :timeout (or timeout 60) :max-bytes max-bytes)
    (declare (ignore headers))
    (when truncated
      (fail "Soniox ~a answered more than one answer's worth of bytes; ask for less at a time" op))
    (unless (= status 200)
      (let* ((text (octets-text body))
             (payload (ignore-errors (nlk:decode-json text)))
             (error-type (and (hash-table-p payload)
                              (or (nlk:json-value payload :string "error_type")
                                  (nlk:json-value payload :string "code"))))
             (message (or (and (hash-table-p payload)
                               (or (nlk:json-value payload :string "error_message")
                                   (nlk:json-value payload :string "message")))
                          (compact-text body))))
        (fail "Soniox returned HTTP ~d: ~a~a"
              status (redact message key) (or (error-note status error-type) ""))))
    (values (json-body op body) status)))

;;; --- reading a language ------------------------------------------------------------

(defun string-replace-all (text from to)
  "TEXT with every FROM written as TO."
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search from text :start2 start)
          do (write-string text out :start start :end (or at (length text)))
          while at
          do (write-char to out)
             (setf start (+ at (length from))))))

(defun normalize-language (text)
  "zh-CN / EN_us as zh / en: Soniox takes bare ISO codes."
  (let ((value (nlk:trimmed (string-replace-all (or text "") "_" "-"))))
    (when (plusp (length value))
      (let ((code (string-downcase (subseq value 0 (or (position #\- value) (length value))))))
        (and (plusp (length code)) code)))))

(defun language-hints (text)
  "TEXT as the ISO codes it names -- one code, or several comma separated --
de-duplicated and in order; an empty list when it names none, which lets
Soniox detect the language itself."
  (let ((hints '()))
    (dolist (item (uiop:split-string (or text "") :separator ","))
      (let ((code (normalize-language item)))
        (when (and code (not (member code hints :test #'string=)))
          (setf hints (append hints (list code))))))
    hints))

;;; --- the poll --------------------------------------------------------------------

(defparameter +poll-initial+ 1.0
  "The first wait between two status reads of a transcription.")

(defparameter +poll-max+ 5.0
  "The longest wait between two status reads: the interval grows by half each
time until it reaches this.")

(defun wait-for-transcription (id key)
  "Poll transcription ID until Soniox says it is complete. An error status is
refused with Soniox's own words; one that outlasts the section's
poll_timeout_seconds is refused as a timeout."
  (let ((start (funcall *monotonic*))
        (interval +poll-initial+)
        (limit (setting :poll-timeout-seconds)))
    (loop
      (multiple-value-bind (payload status)
          (call-json "transcription status" :get (transcriptions-url id) key :timeout 30)
        (declare (ignore status))
        (let ((state (nlk:json-value payload :string "status")))
          (cond
            ((equal state "completed") (return t))
            ((equal state "error")
             (let ((error-type (nlk:json-value payload :string "error_type"))
                   (message (nlk:json-value payload :string "error_message")))
               (fail "Soniox transcription failed~@[: ~a~]~@[ (~a)~]" message error-type)))
            (t
             ;; Queued, processing, or a word this cell does not know: still in
             ;; flight, and the poll's own deadline ends it either way.
             (let ((remaining (- limit (- (funcall *monotonic*) start))))
               (when (<= remaining 0)
                 (fail "Soniox transcription timed out after ~a s: the recording is long or Soniox is busy; try a shorter piece"
                       limit))
               (funcall *sleep* (min interval remaining))
               (setf interval (min (* interval 1.5) +poll-max+)))))))))
)