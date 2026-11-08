;;;; wire.lisp --- Hindsight's HTTP API, one function per operation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three calls, the same three the original plugin makes through the
;;;; Hindsight client:
;;;;
;;;;   POST {api_url}/v1/default/banks/{bank}/memories/recall  {query, budget, max_tokens}
;;;;   POST {api_url}/v1/default/banks/{bank}/memories         {items: [...], async}
;;;;   POST {api_url}/v1/default/banks/{bank}/reflect          {query, budget}
;;;;
;;;; The bank is the data partition: every memory this cell writes and reads
;;;; lives in the one `bank_id' names. `default' in the path is Hindsight's
;;;; workspace segment, not a choice of ours. Recall and reflect answer a
;;;; bank nothing was retained to yet with 404: that is an empty memory, not
;;;; a failure. *HTTP* is the one seam a test stubs.

(in-package #:nodecode-hindsight)

(defparameter *http* #'nlk:http
  "The exchange this cell dials through: NLE's HTTP, stubbed by a test.")

(defparameter +default-api-url+ "https://api.hindsight.vectorize.io"
  "Hindsight Cloud, where a key alone is enough.")

(defparameter +workspace+ "default"
  "The workspace segment of the API path; Hindsight's own default.")

(defparameter +default-bank+ "nodecode"
  "The bank this cell reads and writes unless the section names another.")

(defparameter *recall-max-tokens-default* 4096
  "The plugin's own default for how much one recall may return.")

(defvar *last-error* nil
  "Why the last call that failed failed, for /hindsight; NIL once one answers.")

;;; --- who and where ------------------------------------------------------------

(defun secret (name)
  "The environment variable NAME's value, trimmed, or NIL."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun api-key ()
  "The key the section carries, else $HINDSIGHT_API_KEY."
  (or (let ((key (nlk:trimmed (or (setting :api-key) ""))))
        (and (plusp (length key)) key))
      (secret "HINDSIGHT_API_KEY")))

(defun configured-p ()
  "Whether a key is there to ask with at all."
  (and (api-key) t))

(defun unaware ()
  "Refuse: nothing can be asked without a key."
  (unless (configured-p)
    (fail "no Hindsight API key: set hindsight.api_key in /settings, or export HINDSIGHT_API_KEY"))
  t)

(defun api-url ()
  (string-right-trim "/" (or (setting :api-url) +default-api-url+)))

(defun bank-id ()
  (or (setting :bank-id) +default-bank+))

(defun endpoint (path)
  "PATH under the configured bank's API."
  (format nil "~a/v1/~a/banks/~a/~a" (api-url) +workspace+ (bank-id) path))

(defun headers ()
  `(("authorization" . ,(format nil "Bearer ~a" (api-key)))
    ("content-type" . "application/json")
    ("accept" . "application/json")))

(defun sentence (body)
  "What a refused response said, one line, at most 300 characters, or NIL."
  (let ((text (typecase body
                (string body)
                (t (ignore-errors (nlk:encode-json-object body))))))
    (when (and text (plusp (length text)))
      (let ((flat (substitute #\Space #\Newline text)))
        (subseq flat 0 (min 300 (length flat)))))))

(defun refused (what status body)
  "Refuse: WHAT was answered STATUS, saying BODY where it said anything."
  (fail "~a answered HTTP ~d~@[: ~a~]" what status (sentence body)))

(defun exchange (path object)
  "POST OBJECT as JSON to PATH under the bank. => (values DECODED STATUS)."
  (unaware)
  (let ((timeout (setting :timeout)))
    (multiple-value-bind (body status)
        (funcall *http* "POST" (endpoint path)
                 :headers (headers)
                 :content (nlk:encode-json-object object)
                 :timeout timeout
                 :connect-timeout (min 10 timeout))
      (values (let ((value (ignore-errors (nlk:decode-json body))))
                (or value body))
              status))))

;;; --- the three operations -----------------------------------------------------

(defun recall-texts (query &key (budget (setting :budget)) (max-tokens (setting :recall-max-tokens)))
  "The memories QUERY recalls, best first, as texts. A bank nothing was
retained to yet answers 404, which is an empty memory."
  (multiple-value-bind (value status)
      (exchange "memories/recall"
                (nlk:json-object "query" query
                                  "budget" budget
                                  "max_tokens" (or max-tokens *recall-max-tokens-default*)))
    (cond ((= status 404) '())
          ((not (<= 200 status 299)) (refused "recall" status value))
          (t (loop for result across (or (nlk:json-value value :array "results") #())
                   for text = (nlk:json-value result :text "text")
                   when text collect text)))))

(defun remember-once (content &key context tags occurred-at (async nil) document-id)
  "Store CONTENT as one memory. ASYNC lets the server answer before it has
extracted anything; DOCUMENT-ID makes a re-send on it an append, not a
second document."
  (let ((item (nlk:json-object
               "content" content
               "timestamp" (or occurred-at (iso-now))))
        (body (nlk:json-object "async" (if async t :false))))
    (when context (setf (gethash "context" item) context))
    (when tags (setf (gethash "tags" item) (coerce tags 'vector)))
    (setf (gethash "metadata" item) (nlk:json-object "source" "nodecode"
                                                      "client" "nodecode-hindsight"))
    (setf (gethash "items" body) (vector item))
    (when document-id (setf (gethash "document_id" body) document-id))
    (multiple-value-bind (value status) (exchange "memories" body)
      (cond ((not (<= 200 status 299)) (refused "retain" status value))
            (t value)))))

(defun reflect-text (query &key (budget (setting :budget)))
  "Hindsight's synthesized answer to QUERY, or NIL where the bank is empty."
  (multiple-value-bind (value status)
      (exchange "reflect" (nlk:json-object "query" query "budget" budget))
    (cond ((= status 404) nil)
          ((not (<= 200 status 299)) (refused "reflect" status value))
          (t (nlk:json-value value :text "text")))))

(defun iso-now ()
  "The current time, UTC, as an ISO-8601 instant Hindsight accepts."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))
