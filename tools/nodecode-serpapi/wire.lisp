;;;; wire.lisp --- the one network seam: the key, the request, and what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through CALL, so a test stubs the one
;;;; name NLK:HTTP and never touches the network. SerpApi answers one endpoint
;;;; and selects the engine by parameter; with output=md it answers Markdown
;;;; that a model reads directly, with output=json the structured response.
;;;; Anything but a 2xx is SERPAPI-ERROR with the service's own message in
;;;; it, the key redacted out of every byte the model sees.

(in-package #:nodecode-serpapi)

(defparameter +base-url+ "https://serpapi.com/search"
  "SerpApi's search endpoint, where every tool goes unless the section names another.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, trimmed, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun api-key ()
  "The key the section names inline or by variable, else SERPAPI_API_KEY."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "SERPAPI_API_KEY"))))

(defun check-config ()
  "Refuse a section that could reach SerpApi with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "serpapi: this cell needs a key: set SERPAPI_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the serpapi section; a key is made at "
                                 "serpapi.com/manage-api-key"))))

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

(defun flag (value)
  "VALUE — a contract's boolean — as the \"true\" a SerpApi parameter carries, or NIL."
  (and value "true"))

(defun query-string (pairs)
  "PAIRS — (NAME . VALUE) — as a query string, leaving out every NIL or empty VALUE."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in pairs
                when (and value (not (equal value "")))
                  collect (format nil "~a=~a"
                                  name
                                  (quri:url-encode (princ-to-string value) :encoding :utf-8)))))

(defun body-error (answer)
  "What a refused body says, in one bounded line."
  (let* ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer))))
         (message (and body (nlk:json-value body :text "error"))))
    (or message
        (and (stringp answer) (subseq answer 0 (min 300 (length answer)))))))

(defun raw (engine params &key output accept timeout)
  "One GET of ENGINE with PARAMS. => the body as text. A transport failure or
a refused status signals SERPAPI-ERROR, the key redacted out of the message."
  (let* ((url (format nil "~a?~a" (root)
                      (query-string (append params
                                            (list (cons "engine" engine)
                                                  (cons "api_key" (or (api-key) ""))
                                                  (cons "output" output))))))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :get url
                    :headers (list (cons "accept" accept))
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "serpapi ~a: could not reach SerpApi - ~a"
              engine (redact (nle:transport-failure-label condition (root))))))
    (cond ((and (integerp status) (< status 300))
           (redact (if (stringp answer) answer "")))
          ((and (integerp status) (= status 401))
           (fail "serpapi ~a: HTTP 401, SerpApi rejected the API key; check api_key in the serpapi section"
                 engine))
          ((and (integerp status) (= status 429))
           (fail "serpapi ~a: HTTP 429, the account's quota is spent; do not retry at once" engine))
          ((and (integerp status) (>= status 500))
           (fail "serpapi ~a: HTTP ~a, SerpApi had an upstream error; try again shortly" engine status))
          (t (fail "serpapi ~a: HTTP ~a~@[ - ~a~]" engine status (body-error answer))))))

;;; --- what a Markdown answer says ------------------------------------------------

(defun normalize-heading (line)
  "LINE — a Markdown line — as lowercase words, so a heading matches its name."
  (with-output-to-string (out)
    (let ((space nil))
      (loop for character across (string-left-trim "# " line)
            do (if (alphanumericp character)
                   (progn (when space (write-char #\Space out))
                          (setf space nil)
                          (write-char (char-downcase character) out))
                   (setf space t))))))

(defun heading-p (line)
  (and (plusp (length line)) (char= #\# (char line 0))))

(defun heading-matches-p (line heading)
  (and (heading-p line)
       (let ((text (normalize-heading line))
             (wanted (normalize-heading heading)))
         (or (string= text wanted)
             (let ((prefix (concatenate 'string wanted " ")))
               (and (> (length text) (length prefix))
                    (string= prefix text :end2 (length prefix))))))))

(defun markdown-error (text)
  "TEXT, SerpApi's Markdown answer, as its top-level Error section, or NIL."
  (when (and (stringp text) (>= (length text) 3) (string= "---" (subseq text 0 3)))
    (let ((lines (uiop:split-string text :separator '(#\Newline))))
      (loop for (line . rest) on lines
            when (and (heading-matches-p line "error")
                      (let ((trimmed (string-left-trim "# " line)))
                        (plusp (length trimmed))))
              return (let ((body (loop for following in rest
                                       until (heading-p following)
                                       collect following)))
                       (let ((message (string-trim '(#\Space #\Newline #\Return #\Tab)
                                                   (format nil "~{~a~^ ~}" body))))
                         (if (plusp (length message))
                             (subseq message 0 (min 500 (length message)))
                             "Unknown SerpApi error")))))))

(defun join-lines (lines trailing-newline)
  (with-output-to-string (out)
    (loop for line in lines
          for first = t then nil
          do (unless first (write-char #\Newline out))
             (write-string line out))
    (when trailing-newline (write-char #\Newline out))))

(defun limit-table (markdown heading limit)
  "MARKDOWN with the body rows of the table under HEADING cut to LIMIT rows;
everything outside that table is left byte for byte."
  (let* ((lines (uiop:split-string markdown :separator '(#\Newline)))
         (trailing (and (plusp (length markdown))
                        (member (char markdown (1- (length markdown))) '(#\Newline #\Return))))
         (start (position-if (lambda (line) (heading-matches-p line heading)) lines)))
    (if (null start)
        markdown
        (let ((header (position-if (lambda (line) (and (plusp (length line)) (char= #\| (char line 0))))
                                   lines :start (1+ start))))
          (if (or (null header) (<= (length lines) (+ header 2)))
              markdown
              (let ((kept (list (nth header lines) (nth (1+ header) lines)))
                    (rest-lines nil)
                    (count 0)
                    (index (+ header 2)))
                (loop while (and (< index (length lines))
                                 (plusp (length (nth index lines)))
                                 (char= #\| (char (nth index lines) 0)))
                      do (when (< count limit) (push (nth index lines) kept))
                         (incf count)
                         (incf index))
                (setf rest-lines (nthcdr index lines))
                (join-lines (append (subseq lines 0 header) (nreverse kept) rest-lines)
                            trailing)))))))

;;; --- the two shapes an answer takes --------------------------------------------

(defun call-markdown (engine params &key (timeout (setting :timeout-seconds)))
  "ENGINE with PARAMS, as the Markdown SerpApi renders for a model; its own
Error section, when it carries one, is this call's failure."
  (let ((text (raw engine params :output "md" :accept "text/markdown" :timeout timeout)))
    (let ((error (markdown-error text)))
      (when error (fail "serpapi ~a: ~a" engine error)))
    (if (plusp (length (nlk:trimmed text)))
        text
        (fail "serpapi ~a: SerpApi answered an empty Markdown response" engine))))

(defun call-json (engine params &key (timeout (setting :timeout-seconds)))
  "ENGINE with PARAMS, as the decoded JSON response."
  (let ((text (raw engine params :output "json" :accept "application/json" :timeout timeout)))
    (let ((body (handler-case (nlk:decode-json text :whole t)
                  (error () (fail "serpapi ~a: SerpApi answered malformed JSON" engine)))))
      (unless (hash-table-p body)
        (fail "serpapi ~a: SerpApi answered an unexpected response" engine))
      (let ((error (nlk:json-value body :text "error")))
        (when (and error (plusp (length error)))
          (fail "serpapi ~a: ~a" engine error)))
      body)))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))
