;;;; wire.lisp --- Membase's HTTP API, one function per operation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three calls, the same three the original Hermes plugin makes through the
;;;; unibaseio-membase SDK:
;;;;
;;;;   POST {base_url}/v1/search          {q, limit, mode}
;;;;   POST {base_url}/v1/memories/add    {content, title}
;;;;   GET  {base_url}/v1/profile-facts?limit=50
;;;;
;;;; One key is one person's account, and no call names a Memory: a search
;;;; covers every Memory the key reaches, a save lands in the key's write
;;;; target. The key travels as a bearer token and in nothing else; it never
;;;; reaches an error text. *HTTP* is the one seam a test stubs.

(in-package #:nodecode-membase)

(defparameter *http* #'nlk:http
  "The exchange this cell dials through: NLK:HTTP's own argument list,
answering (values BODY STATUS HEADERS TRUNCATED-P). A test binds this name.")

(defparameter +default-base-url+ "https://api.app.membase.com"
  "Membase's hosted API, where a developer key alone is enough.")

(defparameter +key-page+ "https://app.membase.com"
  "Where a developer key is made: the Membase app, Connect > Developer keys.")

(defparameter *search-chars* 2000
  "How much of a turn's own input one recall query carries, as the plugin does.")

(defvar *last-error* nil
  "Why the last call that failed failed, for /membase; NIL once one answers.")

;;; --- who and where ------------------------------------------------------------

(defun secret (name)
  "The environment variable NAME's value, trimmed, or NIL."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun api-key ()
  "The key the section carries, else $MEMBASE_API_KEY."
  (or (let ((key (nlk:trimmed (or (setting :api-key) ""))))
        (and (plusp (length key)) key))
      (secret "MEMBASE_API_KEY")))

(defun configured-p ()
  "Whether there is a key to ask with at all."
  (and (api-key) t))

(defun unaware ()
  "Refuse: nothing can be asked without a key."
  (unless (configured-p)
    (fail "no Membase API key: set membase.api_key in /settings, or export MEMBASE_API_KEY (a ~
           developer key from ~a, Connect > Developer keys)"
          +key-page+))
  t)

(defun base-url ()
  "The API endpoint: the section's, else Membase's hosted one."
  (string-right-trim "/" (or (setting :base-url) +default-base-url+)))

(defun endpoint (path)
  "PATH under the configured API."
  (format nil "~a~a" (base-url) path))

(defun headers ()
  `(("authorization" . ,(format nil "Bearer ~a" (api-key)))
    ("content-type" . "application/json")
    ("accept" . "application/json")))

;;; --- one exchange -------------------------------------------------------------

(defun sentence (body)
  "What a refused response said, one line, at most 300 characters, or NIL."
  (let ((text (typecase body
                (string body)
                (t (ignore-errors (nlk:encode-json-object body))))))
    (when (and text (plusp (length text)))
      (let ((flat (substitute #\Space #\Newline text)))
        (subseq flat 0 (min 300 (length flat)))))))

(defun answered-p (status)
  "Whether STATUS is a success."
  (and (integerp status) (<= 200 status 299)))

(defun refused (what status body)
  "Refuse: WHAT was answered STATUS, saying BODY where it said anything."
  (cond ((eql status 403)
         (fail "~a was refused (HTTP 403): this Membase key was narrowed or revoked, or the action is ~
                above its access level; do not retry it, and check Connect > Developer keys in the ~
                Membase app. The API said: ~a" what (sentence body)))
        (t (fail "~a answered HTTP ~d~@[: ~a~]" what status (sentence body)))))

(defun checked (what value status)
  "VALUE when STATUS is a success; otherwise the refusal the API's answer makes."
  (if (answered-p status) value (refused what status value)))

(defun exchange (method path &key content)
  "One request to the Membase API at PATH. => (values DECODED STATUS); a body
that is not JSON comes back as the string the API sent."
  (unaware)
  (let ((timeout (setting :timeout)))
    (multiple-value-bind (body status)
        (funcall *http* method (endpoint path)
                 :headers (headers)
                 :content content
                 :timeout timeout
                 :connect-timeout (min 10 timeout))
      (values (let ((value (ignore-errors (nlk:decode-json body))))
                (or value body))
              status))))

;;; --- the three operations -----------------------------------------------------

(defun search-response (query &key limit (mode (setting :recall-mode)))
  "POST /v1/search: the passages the Memories in reach hold on QUERY.
=> (values DECODED STATUS). MODE \"fast\" is one round of retrieval with no
model call; LIMIT is how many passages in all (1-50)."
  (let ((body (nlk:json-object "q" query
                                "mode" (if (and (stringp mode) (string-equal mode "fast"))
                                            "fast" "full"))))
    (when limit (setf (gethash "limit" body) (max 1 (min 50 limit))))
    (exchange "POST" "/v1/search" :content (nlk:encode-json-object body))))

(defun save-response (content &key (title ""))
  "POST /v1/memories/add: save one note into the key's write target.
=> (values DECODED STATUS); the receipt is {id: \"doc_..\", kind, status, learning}."
  (exchange "POST" "/v1/memories/add"
            :content (nlk:encode-json-object (nlk:json-object "content" content "title" title))))

(defun profile-response (&key (limit 50))
  "GET /v1/profile-facts: the user's standing statements, newest first.
=> (values DECODED STATUS). Needs the key's profile tick."
  (exchange "GET" (format nil "/v1/profile-facts?limit=~d" limit)))

;;; --- reading what came back ----------------------------------------------------

(defun items-of (value)
  "VALUE's \"items\" array, or an empty one."
  (or (nlk:json-value value :array "items") #()))

(defun passage-memory (item)
  "The Memory a passage came from."
  (or (nlk:json-value item :string "memory_name")
      (nlk:json-value item :string "container_name")
      "Membase"))

(defun unavailable-memories (value)
  "The Memories that could not answer VALUE's search; one whose passages only
lost to the limit did answer."
  (loop for entry across (or (nlk:json-value value :array "partial") #())
        when (and (hash-table-p entry)
                  (equal "error" (nlk:json-value entry :string "reason")))
          collect (or (nlk:json-value entry :string "memory_name") "a Memory")))

(defun learning-memories (value)
  "The Memories a save's receipt says are learning the note."
  (loop for entry across (or (nlk:json-value value :array "learning") #())
        for name = (and (hash-table-p entry) (nlk:json-value entry :string "memory_name"))
        when name collect name))

(defun clip (text limit)
  "TEXT trimmed, its newlines flattened to spaces, clipped to LIMIT characters."
  (let* ((flat (substitute #\Space #\Newline (nlk:trimmed (princ-to-string (or text "")))))
         (flat (nlk:trimmed flat)))
    (if (<= (length flat) limit)
        flat
        (concatenate 'string (string-right-trim " " (subseq flat 0 (1- limit))) "..."))))
