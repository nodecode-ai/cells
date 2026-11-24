;;;; cell.lisp --- payment-aware HTTP, as one tool and one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of tempoxyz/hermes-mpp (see NOTICE). The original makes every
;;;; HTTPX exchange in the host process payment-aware; a cell has no such seam,
;;;; so the same behaviour lives where the model actually reaches the network:
;;;; mpp_fetch sends the request, reads an MPP 402 challenge, checks it against
;;;; the section's origin allowlist and amount cap, asks the configured payer
;;;; command for the credential, and retries the request once.

(defpackage #:nodecode-mpp
  (:use #:cl)
  (:export #:mpp-error #:mpp-error-detail))

(in-package #:nodecode-mpp)

(nlk:define-peripheral mpp :not-running t)

(defparameter +max-headers+ 50 "The most response headers an answer repeats.")
(defparameter +max-header-chars+ 1000 "How much of one response header an answer repeats.")
(defparameter +max-credential-chars+ 8000 "The longest credential a payer command may print.")
(defparameter +sensitive-markers+ '("api-key" "authentication-info" "authorization" "cookie"
                                    "secret" "token")
  "A response header whose name carries one of these is not repeated: it may be a key.")

(defparameter +primer+
  "MPP (the Machine Payments Protocol) lets an HTTP API charge per request: it answers 402 with a
WWW-Authenticate: Payment challenge naming an amount, a currency and a recipient.
  mpp_fetch {url, method?, headers?, body?, json?}  one request, paid for when the answer asks
Use it for any HTTP API: a free answer passes through, a paid one is paid once and retried, and a
challenge the mpp section does not allow comes back as an ordinary answer carrying its price.
Payment spends real funds: the section's allowed_origins and max_amount bound what it may pay."
  "What (help :mpp) answers while the cell runs.")

;;; --- the section, read back ---------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, with the payer command checked and the allowlist trimmed."
  (declare (ignore table))
  (let ((command (nonblank (getf values :payer-command)))
        (plist (copy-list values)))
    (unless command
      (nlk:config-error "no payer: set payer_command to the program that signs an MPP charge for the challenge it reads on stdin and prints the Authorization value"))
    (setf (getf plist :payer-command) command
          (getf plist :allowed-origins)
          (remove-if-not #'nonblank (or (getf values :allowed-origins) '())))
    plist))

;;; --- base64url, and the WWW-Authenticate grammar -------------------------------

(defun base64-digit (char)
  "CHAR's six-bit base64 or base64url value, or NIL."
  (cond ((char<= #\A char #\Z) (- (char-code char) 65))
        ((char<= #\a char #\z) (+ 26 (- (char-code char) 97)))
        ((char<= #\0 char #\9) (+ 52 (- (char-code char) 48)))
        ((or (char= char #\+) (char= char #\-)) 62)
        ((or (char= char #\/) (char= char #\_)) 63)
        (t nil)))

(defun base64url-decode (text)
  "TEXT, base64 or base64url with or without padding => the UTF-8 text its bytes spell."
  (let ((bits 0) (count 0) (octets '()))
    (loop for char across text
          for digit = (base64-digit char)
          do (when digit
               (setf bits (logior (ash bits 6) digit)
                     count (+ count 6))
               (when (>= count 8)
                 (setf count (- count 8))
                 (push (ldb (byte 8 0) (ash bits (- count))) octets)
                 (setf bits (ldb (byte count 0) bits)))))
    (sb-ext:octets-to-string (coerce (nreverse octets) '(simple-array (unsigned-byte 8) (*)))
                             :external-format :utf-8)))

(defun auth-fields (text)
  "TEXT, an auth-param list, split at the commas outside quotes."
  (let ((fields '()) (start 0) (quoted nil) (escaped nil))
    (loop for index from 0 below (length text)
          for char = (char text index)
          do (cond (escaped (setf escaped nil))
                   ((and quoted (char= char #\\)) (setf escaped t))
                   ((char= char #\") (setf quoted (not quoted)))
                   ((and (char= char #\,) (not quoted))
                    (push (subseq text start index) fields)
                    (setf start (1+ index)))))
    (push (subseq text start) fields)
    (nreverse fields)))

(defun auth-params (text)
  "TEXT's auth-params as an alist of (lower-case name . value), quoted strings unquoted."
  (loop for field in (auth-fields text)
        for eq = (position #\= field)
        for name = (and eq (string-downcase (nlk:trimmed (subseq field 0 eq))))
        for raw = (and eq (nlk:trimmed (subseq field (1+ eq))))
        when (and name raw (plusp (length name)))
          collect (cons name
                        (if (and (> (length raw) 1) (char= (char raw 0) #\"))
                            (with-output-to-string (out)
                              (loop with inner = (subseq raw 1 (max 1 (1- (length raw))))
                                    for index from 0 below (length inner)
                                    for char = (char inner index)
                                    do (cond ((and (char= char #\\) (< (1+ index) (length inner)))
                                              (write-char (char inner (1+ index)) out)
                                              (incf index))
                                             ((char= char #\") nil)
                                             (t (write-char char out)))))
                            raw))))

(defun payment-challenge (header)
  "HEADER, one WWW-Authenticate value => the Payment challenge in it as a plist,
with its request decoded, or NIL when it is not a readable Payment challenge."
  (let ((header (and (stringp header) (nlk:trimmed header))))
    (when (and header
               (> (length header) 8)
               (string-equal "payment " (subseq header 0 8)))
      (let* ((params (auth-params (subseq header 8)))
             (get (lambda (name) (cdr (assoc name params :test #'string=))))
             (id (funcall get "id"))
             (method (funcall get "method"))
             (intent (funcall get "intent"))
             (request-b64 (funcall get "request")))
        (when (and (nonblank id) (nonblank method) (nonblank intent) (nonblank request-b64))
          (let ((request (ignore-errors (nlk:decode-json (base64url-decode request-b64)))))
            (list :id id
                  :realm (funcall get "realm")
                  :method (string-downcase method)
                  :intent (string-downcase intent)
                  :request-b64 request-b64
                  :request (and (hash-table-p request) request)
                  :expires (funcall get "expires")
                  :digest (funcall get "digest")
                  :opaque (funcall get "opaque")
                  :header (funcall get "header"))))))))

(defun header-values (headers name)
  "Every value of the header NAME in HEADERS, an alist of (name . value)."
  (loop for (key . value) in headers
        when (and (stringp key) (string-equal key name))
          collect value))

(defun challenges-of (headers)

  "The Payment challenges HEADERS carries, in the order the peer sent them."

  (loop for value in (header-values headers "www-authenticate")

        for challenge = (payment-challenge value)

        when challenge collect challenge))

(defun origin-of (url)
  "URL's origin, scheme://host[:port], lower case; NIL when URL is not an absolute http URL."
  (let* ((uri (ignore-errors (quri:uri url)))
         (scheme (and uri (string-downcase (or (quri:uri-scheme uri) "")))))
    (when (and uri (member scheme '("http" "https") :test #'string=) (quri:uri-host uri))
      (format nil "~a://~a~@[:~a~]" scheme
              (string-downcase (quri:uri-host uri))
              (let ((port (quri:uri-port uri)))
                (unless (or (and (string= scheme "http") (eql port 80))
                            (and (string= scheme "https") (eql port 443)))
                  port))))))

(defun private-host-p (url)
  "Whether URL's host is a loopback, private or link-local name or literal."
  (let* ((uri (ignore-errors (quri:uri url)))
         (host (and uri (string-downcase (string-trim "[]" (or (quri:uri-host uri) ""))))))
    (and host
         (or (string= host "localhost")
             (string= host "::1")
             (uiop:string-prefix-p "127." host)
             (uiop:string-prefix-p "10." host)
             (uiop:string-prefix-p "192.168." host)
             (uiop:string-prefix-p "169.254." host)
             (uiop:string-prefix-p "fd" host)
             (and (uiop:string-prefix-p "172." host)
                  (let ((second (ignore-errors (parse-integer host :start 4 :end 6 :junk-allowed t))))
                    (and second (<= 16 second 31))))))))

(defun origin-allowed-p (origin)
  "Whether the section's allowlist admits ORIGIN; an empty allowlist admits any."
  (let ((allowed (setting :allowed-origins)))
    (or (null allowed)
        (some (lambda (entry)
                (string= (string-right-trim "/" (string-downcase (nlk:trimmed entry))) origin))
              allowed))))

(defun challenge-amount (challenge)
  "CHALLENGE's requested amount as a number when it reads as one, else NIL."
  (let* ((request (getf challenge :request))
         (value (and request (nlk:json-value request :any "amount"))))
    (cond ((numberp value) value)
          ((stringp value)
           (let ((number (ignore-errors (read-from-string (nlk:trimmed value)))))
             (and (realp number) number)))
          (t nil))))

(defun challenge-field (challenge key)
  "CHALLENGE's request member KEY, or NIL."
  (let ((request (getf challenge :request)))
    (and request (nlk:json-value request :any key))))

(defun challenge-refusal (challenge origin)
  "Why the section does not pay CHALLENGE from ORIGIN, or NIL when it may."
  (let ((cap (setting :max-amount))
        (amount (challenge-amount challenge)))
    (cond ((not (origin-allowed-p origin))
           (format nil "~a is not in allowed_origins" origin))
          ((and cap (plusp cap) (numberp amount) (> amount cap))
           (format nil "the challenge asks ~a, over max_amount ~a" amount cap))
          ((and cap (plusp cap) (null amount))
           (format nil "the challenge names no amount to hold to max_amount ~a" cap))
          (t nil))))

;;; --- the payer -------------------------------------------------------------------

(defun payer-input (challenge url method)
  "What the payer command reads on stdin: the whole challenge, decoded."
  (nlk:json-object
   "url" url
   "http_method" method
   "challenge"
   (nlk:json-object "id" (getf challenge :id)
                     "realm" (or (getf challenge :realm) "")
                     "method" (getf challenge :method)
                     "intent" (getf challenge :intent)
                     "request" (or (getf challenge :request) (nlk:make-json-object))
                     "request_b64" (getf challenge :request-b64)
                     "expires" (or (getf challenge :expires) "")
                     "digest" (or (getf challenge :digest) "")
                     "opaque" (or (getf challenge :opaque) "")
                     "header" (or (getf challenge :header) ""))
   "amount" (or (challenge-field challenge "amount") "")
   "currency" (or (challenge-field challenge "currency") "")
   "recipient" (or (challenge-field challenge "recipient") "")))

(defun payer-credential (challenge url method)
  "CHALLENGE for URL through the section's payer command => the Authorization
value the retry carries, `Payment <credential>'."
  (let ((input (nlk:encode-json-object (payer-input challenge url method)))
        (output nil) (errors nil) (code nil))
    (handler-case
        (multiple-value-setq (output errors code)
          (uiop:run-program (list "/bin/sh" "-c" (setting :payer-command))
                            :input input :output :string :error-output :string
                            :ignore-error-status t))
      (error (condition)
        (fail "the payer command did not run: ~a" (nlk:one-line (princ-to-string condition)))))
    (let ((text (and (stringp output) (nonblank (nlk:trimmed output)))))
      (cond
        ((null text)
         (fail "the payer command printed no credential (exit ~a)~@[: ~a~]"
               code (nonblank errors)))
        (t (let ((text (nlk:clip text +max-credential-chars+)))
             (cond ((uiop:string-prefix-p "{" text)
                    (let* ((json (ignore-errors (nlk:decode-json text)))
                           (value (and (hash-table-p json)
                                       (or (nlk:json-value json :string "authorization")
                                           (nlk:json-value json :string "credential")))))
                      (unless (nonblank value)
                        (fail "the payer command's JSON names no authorization"))
                      (if (uiop:string-prefix-p "payment " (string-downcase value))
                          value
                          (format nil "Payment ~a" value))))
                   ((uiop:string-prefix-p "payment " (string-downcase text)) text)
                   (t (format nil "Payment ~a" text)))))))))

;;; --- one request ------------------------------------------------------------------

(defun visible-headers (headers)
  "HEADERS an answer repeats: the first +max-headers+, minus any that may be a key."
  (let ((visible '()) (count 0))
    (loop for (name . value) in headers
          while (< count +max-headers+)
          when (and (stringp name)
                    (notany (lambda (marker) (search marker (string-downcase name)))
                            +sensitive-markers+))
            do (push (cons name (if (stringp value) (nlk:clip value +max-header-chars+) value))
                     visible)
               (incf count))
    (nreverse visible)))

(defun alist-object (alist)

  "ALIST as a JSON object of strings."

  (apply #'nlk:make-json-object

         (loop for (name . value) in alist

               append (list name (if (stringp value) value (princ-to-string value))))))

(defun header-alist (object)
  "OBJECT, a JSON object of request headers, as an alist of (name . value)."
  (when (hash-table-p object)
    (loop for name being the hash-keys of object using (hash-value value)
          when (stringp value) collect (cons name value))))

(defun request-content (body json)
  "The bytes one request carries: BODY as it is, else JSON encoded."
  (cond (body body)
        ((eq json :null) "null")
        (json (nlk:encode-json-object json))
        (t nil)))

(defun send (method url headers content)
  "One exchange, every status a value => (values BODY STATUS HEADERS)."
  (let ((timeout (setting :timeout-seconds)))
    (handler-case
        (values-list (multiple-value-list
                      (nlk:http method url :headers headers :content content
                                :timeout timeout :connect-timeout (min 10 timeout))))
      (error (condition)
        (fail "~a did not answer: ~a" url (nlk:one-line (princ-to-string condition)))))))

(defun result-object (url status body headers &key paid challenges refusal)
  "The answer mpp_fetch carries back, the shape the original answers: status,
url, headers, body, truncated."
  (let* ((body (if (stringp body) body (princ-to-string (or body ""))))
         (cut (> (length body) (setting :max-body-chars))))
    (nlk:json-object
     "status" (or status 0)
     "url" url
     "headers" (alist-object (visible-headers headers))
     "body" (if cut (subseq body 0 (setting :max-body-chars)) body)
     "truncated" (and cut t)
     "paid" (and paid t)
     "payment" (or paid (nlk:make-json-object))
     "challenges" (or challenges (vector))
     "refusal" (or refusal ""))))

(defun challenge-summary (challenge)
  "CHALLENGE as the price the model reads when it is not paid."
  (nlk:json-object "method" (or (getf challenge :method) "")
                    "intent" (or (getf challenge :intent) "")
                    "amount" (or (challenge-field challenge "amount") "")
                    "currency" (or (challenge-field challenge "currency") "")
                    "recipient" (or (challenge-field challenge "recipient") "")))

(defun fetch-url (url &key (method "GET") headers body json)
  "URL fetched once, and paid for and retried when it answers a 402 challenge
the section allows. => the JSON answer."
  (let* ((url (nonblank url))
         (method (string-upcase (or (nonblank method) "GET")))
         (origin (and url (origin-of url))))
    (unless url (fail "no url"))
    (unless origin (fail "~s is not an absolute http or https URL" url))
    (when (private-host-p url) (fail "~s is a private or internal URL" url))
    (when (and body json) (fail "body and json are both set: send one"))
    (let* ((content (request-content body json))
           (request-headers (header-alist headers)))
      (multiple-value-bind (answer status response-headers)
          (send (intern method :keyword) url request-headers content)
        (if (/= (or status 0) 402)
            (result-object url status answer response-headers)
            (let* ((challenges (challenges-of response-headers))
                   (summaries (map 'vector #'challenge-summary challenges))
                   (challenge (first challenges))
                   (refusal (if challenge
                                (challenge-refusal challenge origin)
                                "the 402 carried no readable Payment challenge")))
              (if refusal
                  (result-object url status answer response-headers
                                 :challenges summaries :refusal refusal)
                  (let* ((credential (payer-credential challenge url method))
                         (retry-headers
                           (cons (cons "authorization" credential)
                                 (remove-if (lambda (pair)
                                              (string-equal (car pair) "authorization"))
                                            request-headers)))
                         (payment (nlk:json-object
                                   "amount" (or (challenge-field challenge "amount") "")
                                   "currency" (or (challenge-field challenge "currency") "")
                                   "recipient" (or (challenge-field challenge "recipient") "")
                                   "method" (or (getf challenge :method) ""))))
                    (multiple-value-bind (paid-answer paid-status paid-headers)
                        (send (intern method :keyword) url retry-headers content)
                      (declare (ignore paid-status))
                      (result-object url (or paid-status status) paid-answer paid-headers
                                     :paid payment :challenges summaries))))))))))

;;; --- what the model reads ------------------------------------------------------------

(defun format-answer (json)
  "JSON, an answer, as the line a /mpp command shows."
  (let ((status (nlk:json-value json :integer "status"))
        (paid (nlk:json-value json :boolean "paid"))
        (refusal (nlk:json-value json :string "refusal")))
    (format nil "HTTP ~a~@[ (paid ~a ~a)~]~@[ -- not paid: ~a~]"
            status
            (and paid (nlk:json-value (nlk:json-value json :object "payment") :string "amount"))
            (and paid (nlk:json-value (nlk:json-value json :object "payment") :string "currency"))
            (nonblank refusal))))

;;; --- the tools and the command ---------------------------------------------------

(nlk:define-contract mpp-fetch
  (:doc "Send one HTTP request, paying for it when the API answers an MPP 402 challenge: the section's payer command signs the charge and the request is retried once. Use it for any HTTP API; a free answer passes through, and a challenge the mpp section does not allow comes back with its price in `challenges' and why in `refusal'.")
  ("url" :text :required t :doc "the absolute http or https URL to fetch")
  ("method" :text :slot http-method :default "GET" :doc "the HTTP method, GET unless the API says otherwise")
  ("headers" :object :doc "request headers as a JSON object of strings")
  ("body" :text :doc "a raw request body; not with json")
  ("json" :any :doc "a JSON body: object, array, scalar or null; not with body"))

(defun mpp-fetch-tool (call)
  "The mpp_fetch tool: CALL's request, paid for when the answer asks."
  (handler-case
      (nlk:encode-json-object
       (fetch-url (mpp-fetch-url call)
                  :method (mpp-fetch-http-method call)
                  :headers (mpp-fetch-headers call)
                  :body (mpp-fetch-body call)
                  :json (mpp-fetch-json call)))
    (mpp-error (condition)
      (nle:failure (format nil "ERROR: ~a" (mpp-error-detail condition))))
    (nlk:contract-refusal (condition)
      (nle:failure (format nil "ERROR: ~a" (princ-to-string condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: mpp_fetch: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun status-line ()

  "What /mpp answers with no URL: what the section pays with, and how far."

  (let ((origins (setting :allowed-origins))

        (cap (setting :max-amount)))

    (format nil "MPP: payer ~s; pays ~:[any origin~;~:*~{~a~^, ~}~]; ~:[no cap~:;at most ~:*~a per payment~].~%Paid requests spend real funds; the payer command signs the challenge it reads on stdin."

            (setting :payer-command) origins cap)))

(defun run-command (args session-id)
  "/mpp URL: fetch URL through the same path the tool uses, and say what it cost."
  (declare (ignore session-id))
  (let ((url (nlk:trimmed (or args ""))))
    (if (plusp (length url))
        (handler-case
            (format-answer (nlk:decode-json
                            (nlk:encode-json-object (fetch-url url))))
          (mpp-error (condition) (format nil "ERROR: ~a" (mpp-error-detail condition)))
          (error (condition) (format nil "ERROR: ~a" (nlk:one-line (princ-to-string condition)))))
        (status-line))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell mpp
  (:section ("mpp")
    (:guide "payer_command is the program that signs an MPP charge: it reads the challenge JSON on stdin and prints the Authorization value (Payment <token>, or JSON with authorization); allowed_origins lists the exact origins it may pay, empty meaning any; max_amount bounds one payment in the challenge's own currency units, 0 meaning no bound; next step: ask for a paid URL")
    ("payer_command" :string :required t
     :doc "the command that signs a challenge, e.g. a script over the pympp library")
    ("allowed_origins" :list
     :doc "exact origins it may pay, e.g. https://mpp.dev; empty pays any origin that asks")
    ("max_amount" :integer :default 0 :min 0
     :doc "the most one payment may cost, in the challenge's currency units; 0 leaves it unbounded")
    ("timeout_seconds" :integer :default 60 :min 1 :doc "one exchange's ceiling")
    ("max_body_chars" :integer :default 50000 :min 1 :doc "how much of an answer's body comes back"))
  (:settings #'read-settings)
  (:tool "mpp_fetch" (nlk:contract 'mpp-fetch) #'mpp-fetch-tool)
  (:command "mpp" #'run-command
            :description "What the wallet pays, or fetch one paid URL"
            :argument-hint "URL")
  (:help :mpp "mpp: HTTP that pays its own 402 - (nodecode-mpp:fetch-url \"https://mpp.dev/api/ping/paid\")"
   +primer+))
