;;;; client.lisp --- the one network seam, and the API's own verbs.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every byte this cell moves goes through *REQUEST*, so a test rebinds one
;;;; name and never touches the network. What crosses is the Memoria REST API
;;;; the original plugin was written against: a bearer key, the profile's
;;;; logical subject, the branch, and one JSON object each way.
;;;;
;;;; A status is a value, never a signal: *REQUEST* answers the parsed body
;;;; and the status, and API decides what the caller reads. Only transport
;;;; failure -- DNS, TLS, a timeout -- is a MEMORIA-ERROR, because there is
;;;; no status to report.

(in-package #:nodecode-memoria)

;;; --- the seam -------------------------------------------------------------

(defun http-json (method url body)
  "METHOD URL, BODY a JSON string or NIL => (VALUES PARSED STATUS)."
  (handler-case
      (multiple-value-bind (text status)
          (nlk:http method url
                    :content body
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))
                    :headers (list (cons "authorization" (format nil "Bearer ~a" (api-key)))
                                   '("accept" . "application/json")
                                   '("content-type" . "application/json")))
        (values (and (stringp text)
                     (plusp (length (string-trim '(#\Space #\Tab #\Newline) text)))
                     (handler-case (nlk:decode-json text) (error () nil)))
                status))
    ((or error sb-sys:deadline-timeout) (condition)
      (fail "memoria could not reach ~a: ~a"
            (setting :api-url) (nle:transport-failure-label condition url)))))

(defparameter *request* #'http-json
  "The cell's one network call: (METHOD URL BODY) => (VALUES PARSED STATUS),
BODY a JSON string or NIL, PARSED what the API's JSON decoded to. A test
rebinds this name and answers canned objects.")

;;; --- the binding ----------------------------------------------------------

(defvar *subject* nil
  "This profile's logical subject while the cell runs: a hash of the home,
so nothing of the machine's own paths leaves it, and a second profile is a
second subject on the same account.")

(defun subject ()
  "The subject every request binds, or a refusal when the cell is not running."
  (or *subject*
      (fail "the memoria cell is not running: (restart-cells \"memoria\")")))

(defun api-key ()
  "The memory-service API key, or a refusal naming the member it belongs in."
  (let ((key (setting :api-key)))
    (if (and (stringp key) (plusp (length (string-trim '(#\Space #\Tab) key))))
        (string-trim '(#\Space #\Tab) key)
        (fail "memoria is not set up: paste the Memoria memory-service API key into ~
               memoria.api_key in /settings (it is not the website's session token), then ~
               call this again"))))

(defun scope (&optional session)
  "The object every read and write carries: this subject, the branch, and
SESSION when the caller knows one."
  (let ((object (nlk:make-json-object "subject_id" (subject)
                                       "branch" (setting :branch))))
    (when (and (stringp session) (plusp (length session)))
      (setf (gethash "session_id" object) session))
    object))

;;; --- urls -----------------------------------------------------------------

(defun url-encode (text)
  "TEXT with everything but RFC 3986's unreserved characters percent-encoded."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (princ-to-string text)
                                                     :external-format :utf-8)
          do (if (and (< octet 128)
                      (let ((c (code-char octet)))
                        (or (alphanumericp c) (find c "-._~"))))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0x" octet)))))

(defun api-url (path &optional query)
  "PATH under memoria.api_url, QUERY -- a list of (NAME VALUE) -- appended."
  (format nil "~a~a~@[?~{~a=~a~^&~}~]"
          (setting :api-url) path
          (and query (loop for (name value) in query
                           collect (url-encode name) collect (url-encode value)))))

;;; --- one exchange ---------------------------------------------------------

(defun status-text (status parsed)
  "STATUS as the model should read it, with the API's own sentence when it
gave one."
  (let ((detail (and (hash-table-p parsed)
                     (or (nlk:json-value parsed :string "detail")
                         (nlk:json-value parsed :string "error")
                         (nlk:json-value parsed :string "message")))))
    (format nil "~a~@[ (~a)~]"
            (case status
              (400 "the request was refused")
              (401 "the API key was refused")
              (403 "the key may not do that")
              (404 "no such memory")
              (409 "the memory changed under the write")
              (413 "the text was too long")
              (429 "rate limited: wait a moment")
              (t (format nil "HTTP ~d" status)))
            (and (stringp detail) (plusp (length detail)) (subseq detail 0 (min 240 (length detail)))))))

(defun api (method path &key body query)
  "METHOD PATH => the parsed answer, or a refusal naming the status. BODY a
JSON object or NIL; QUERY a list of (NAME VALUE) or NIL. A 2xx with no body
answers T."
  ;; No key, no exchange: the refusal names the member to fill in, and
  ;; nothing leaves the machine to learn the same thing.
  (api-key)
  (multiple-value-bind (parsed status)
      (funcall *request* method (api-url path query) (and body (nlk:encode-json-object body)))
    (if (and (integerp status) (<= 200 status 299))
        (or parsed t)
        (fail "memoria: ~a" (status-text status parsed)))))

;;; --- ids and records -------------------------------------------------------

(defun checked-id (id)
  "ID when it is a memory id this cell may put in a URL, else a refusal."
  (unless (and (stringp id)
               (<= 1 (length id) 200)
               (every (lambda (c) (and (< (char-code c) 128)
                                       (or (alphanumericp c) (char= c #\-) (char= c #\_))))
                      id))
    (fail "memoria: ~s is no memory id: use the id a search or the profile returned"
          id))
  id)

(defun checked-record (record what)
  "RECORD, a memory the API answered, once it is this subject's; else a refusal."
  (unless (hash-table-p record)
    (fail "memoria: the API answered no ~a" what))
  (let ((owner (nlk:json-value record :string "subject_id")))
    (unless (equal owner (subject))
      (fail "memoria: the API answered another subject's ~a (~a), not this profile's (~a)"
            what owner (subject))))
  record)

(defun checked-write (record)
  "RECORD, a write's answer, once it carries a memory id of this subject."
  (let ((id (and (hash-table-p record) (nlk:json-value record :string "memory_id"))))
    (unless (and (stringp id) (plusp (length id)))
      (fail "memoria: the API answered no memory_id; the write may or may not have landed"))
    (let ((owner (nlk:json-value record :string "subject_id")))
      (unless (or (null owner) (equal owner (subject)))
        (fail "memoria: the API answered a write for another subject (~a)" owner))))
  record)

(defun memory-rows (parsed)
  "PARSED, a retrieve answer, as a list of rows, each this subject's."
  (unless (and (vectorp parsed) (every #'hash-table-p parsed))
    (fail "memoria: the API answered something that is not a list of memories"))
  (let ((rows (coerce parsed 'list)))
    (dolist (row rows)
      (unless (equal (nlk:json-value row :string "subject_id") (subject))
        (fail "memoria: the API answered a memory of another subject (~a), not this profile's (~a)"
              (nlk:json-value row :string "subject_id") (subject))))
    rows))

;;; --- the verbs ------------------------------------------------------------

(defun api-retrieve (query top-k &optional session)
  "POST /v1/memories/retrieve => the rows it matched, nearest first."
  (let ((body (scope session)))
    (setf (gethash "query" body) query
          (gethash "top_k" body) top-k)
    (memory-rows (api :post "/v1/memories/retrieve" :body body))))

(defun api-store (content memory-type &optional session)
  "POST /v1/memories => the write's record, which carries the new id."
  (let ((body (scope session)))
    (setf (gethash "content" body) content
          (gethash "memory_type" body) (or memory-type "semantic")
          (gethash "extra_metadata" body)
          (nlk:make-json-object "source" "nodecode" "write_origin" "explicit"))
    (checked-write (api :post "/v1/memories" :body body))))

(defun api-record (memory-id)
  "GET /v1/memories/ID => the record, once it is this subject's."
  (checked-record (api :get (format nil "/v1/memories/~a" (checked-id memory-id))
                       :query (list (list "branch" (setting :branch))))
                  "memory"))

(defun api-correct (memory-id content)
  "PUT /v1/memories/ID/correct => the corrected record, with its new id."
  (checked-write
   (api :put (format nil "/v1/memories/~a/correct" (checked-id memory-id))
        :body (nlk:make-json-object "new_content" content
                                     "branch" (setting :branch)))))

(defun api-forget (memory-id)
  "DELETE /v1/memories/ID => MEMORY-ID."
  (api :delete (format nil "/v1/memories/~a" (checked-id memory-id))
       :query (list (list "branch" (setting :branch))))
  memory-id)

(defun api-feedback (memory-id signal)
  "POST /v1/memories/ID/feedback => SIGNAL. The service takes feedback on the
main branch only: it has no branch parameter of its own."
  (unless (equal (setting :branch) "main")
    (fail "memoria: feedback is only taken on the main branch; this cell is on ~a"
          (setting :branch)))
  (api :post (format nil "/v1/memories/~a/feedback" (checked-id memory-id))
       :body (nlk:make-json-object "signal" signal))
  signal)

(defun api-profile (limit cursor)
  "GET /v1/memories?memory_type=profile => (VALUES ROWS NEXT-CURSOR)."
  (let* ((query (list (list "subject_id" (subject))
                       (list "branch" (setting :branch))
                       (list "memory_type" "profile")
                       (list "limit" (princ-to-string limit))))
         (query (if (and (stringp cursor) (plusp (length cursor)))
                    (append query (list (list "cursor" cursor)))
                    query))
         (page (api :get "/v1/memories" :query query)))
    (unless (hash-table-p page)
      (fail "memoria: the API answered no profile page"))
    (let ((items (gethash "items" page))
          (next (gethash "next_cursor" page)))
      (unless (and (vectorp items) (every #'hash-table-p items))
        (fail "memoria: the profile page carries no items"))
      (dolist (row (coerce items 'list))
        (unless (equal (nlk:json-value row :string "subject_id") (subject))
          (fail "memoria: the profile page carries another subject's memory (~a)"
                (nlk:json-value row :string "subject_id"))))
      (unless (or (null next) (and (stringp next) (plusp (length next))))
        (fail "memoria: the profile page's cursor is not a cursor"))
      (values (coerce items 'list) next))))
