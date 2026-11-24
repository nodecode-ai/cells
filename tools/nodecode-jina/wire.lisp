;;;; wire.lisp --- the two Jina endpoints, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through ASK, so a test stubs one name and
;;;; never touches the network. The plugin had one HTTP client and the same two
;;;; calls: a POST of {"url": ...} to r.jina.ai, and a GET of ?q=&num= to
;;;; s.jina.ai. Each answers one JSON envelope, {"code": 200, "data": ...},
;;;; and a refused status is a JINA-ERROR carrying Jina's own message -- the
;;;; text the model repairs from.

(in-package #:nodecode-jina)

(defparameter +reader-url+ "https://r.jina.ai/"
  "Jina Reader: the url posted here comes back as the page's Markdown.")
(defparameter +search-url+ "https://s.jina.ai/"
  "Jina Search: the query rides the query string, the sources come back as JSON.")
(defparameter +key-url+ "https://jina.ai/api-dashboard/"
  "Where a key is made: the link the plugin's own error text named.")
(defparameter +user-agent+ "nodecode-jina/0.1.0"
  "What a request calls itself, as the plugin named itself to Jina.")
(defparameter +search-limit-max+ 20
  "Jina's ceiling on a search: the plugin clamped every request to it.")
(defparameter +blank+ '(#\Space #\Tab #\Newline #\Return)
  "What a body of only whitespace is trimmed of before it counts as empty.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names inline or by variable, else JINA_API_KEY, as the
plugin read it out of the harness' environment. NIL is a working state:
reading needs no key."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "JINA_API_KEY"))))

(defun endpoint (override default)
  "OVERRIDE -- the section's own address -- or DEFAULT, ending in one slash."
  (let ((given (nonempty override)))
    (if given (concatenate 'string (string-right-trim "/" given) "/") default)))

(defun headers (&key key extra)
  "The headers the plugin sent: JSON asked for, the client named, the key as a
bearer token when there is one, and EXTRA on top."
  (append (list (cons "accept" "application/json")
                (cons "user-agent" +user-agent+))
          (when key (list (cons "authorization" (format nil "Bearer ~a" key))))
          extra))

(defun web-url-p (url)
  "Whether URL is an absolute http or https url, as the plugin checked before
spending a request: anything else is refused before one is made."
  (let ((uri (ignore-errors (quri:uri url))))
    (and uri
         (member (string-downcase (or (quri:uri-scheme uri) "")) '("http" "https")
                 :test #'equal)
         (let ((host (quri:uri-host uri)))
           (and host (plusp (length host))))
         t)))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded JSON object, or NIL."
  (cond ((hash-table-p answer) answer)
        ((stringp answer)
         (if (plusp (length (string-trim +blank+ answer)))
             (let ((decoded (handler-case (nlk:decode-json answer) (error () nil))))
               (and (hash-table-p decoded) decoded))
             nil))
        (t nil)))

(defun message-of (body)
  "Jina's own message, whichever member carries it: the plugin said the
service' text never holds the request's credentials."
  (or (nlk:json-value body :text "readableMessage")
      (nlk:json-value body :text "message")
      (nlk:json-value body :text "error")))

(defun status-hint (status keyed)
  "What to do about a refused STATUS, in the plugin's own terms."
  (cond ((eql status 401) (if keyed
                               "the key was refused; check it in the jina section"
                               (format nil "this Jina endpoint needs JINA_API_KEY (~a)" +key-url+)))
        ((eql status 402) "the Jina account is out of balance; top up at https://jina.ai/api-dashboard/")
        ((eql status 403) "Jina refused the request; check the key's account")
        ((eql status 404) "Jina could not fetch that page")
        ((eql status 429) (if keyed
                               "rate limit reached; try again later"
                               "anonymous rate limit reached (about 20 a minute); a key raises it"))
        ((and (integerp status) (>= status 500)) "try again later, or check https://status.jina.ai/")
        (t nil)))

(defun ask (method url &key content headers (timeout (setting :timeout-seconds)))
  "METHOD URL as one exchange under the section's ceiling => (values BODY
STATUS). A transport failure is a JINA-ERROR, one line the model reads."
  (let ((answer nil) (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers headers
                    :content content
                    :timeout timeout
                    :connect-timeout (min 10 (max 1 timeout))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "jina: ~a" (nle:transport-failure-label condition url))))
    (values answer status)))

(defun data-of (answer status where &key keyed)
  "ANSWER and STATUS of one Jina call => its envelope's `data'. A refused
status, or a body whose own code is not 200, is a JINA-ERROR naming WHERE,
Jina's message and what to do next."
  (let ((body (decode-body answer)))
    (when (and (integerp status) (>= status 400))
      (fail "~a: HTTP ~d~@[ - ~a~]~@[ - ~a~]"
            where status (message-of body) (status-hint status keyed)))
    (unless body (fail "~a: Jina answered something that is not a JSON object" where))
    (let ((code (nlk:json-value body :integer "code")))
      (when (and code (/= code 200))
        (fail "~a: Jina reported code ~d~@[ - ~a~]" where code (message-of body))))
    (nlk:json-value body :any "data")))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]" (subseq text 0 limit)
                   (- (length text) limit)))))

(defun page-text (url data limit)
  "URL's page read out of DATA, the reader's `data' object => (values TEXT NIL)
or (values NIL ERROR): the final url, the title, the description, Jina's own
warning when it served a cached copy, then the Markdown, cut to LIMIT."
  (if (not (hash-table-p data))
      (values nil "Jina returned an invalid response.")
      (let ((content (nlk:json-value data :string "content")))
        (if (or (null content) (not (plusp (length (string-trim +blank+ content)))))
            (values nil "Jina returned no content for this page.")
            (values
             (with-output-to-string (stream)
               (let ((final (or (nlk:json-value data :text "url") url))
                     (title (nlk:json-value data :text "title"))
                     (description (nlk:json-value data :text "description"))
                     (warning (nlk:json-value data :text "warning")))
                 (format stream "url: ~a~%" final)
                 (when title (format stream "title: ~a~%" title))
                 (when description (format stream "description: ~a~%" description))
                 (when warning (format stream "warning: ~a~%" warning))
                 (format stream "~%~a~%" (clip content limit))))
             nil)))))

(defun rows-of (data)
  "DATA -- a search envelope's `data' -- as the rows that carry a url: the
plugin kept exactly those."
  (and (vectorp data)
       (remove-if-not (lambda (row)
                        (and (hash-table-p row) (nlk:json-value row :text "url")))
                      (coerce data 'list))))

(defun results-text (query data)
  "DATA -- the `data' array Jina's search answered -- as numbered sources:
title, url, then the snippet, which is all the plugin asked for."
  (if (not (vectorp data))
      (values nil "Jina returned an invalid search response.")
      (let ((rows (rows-of data)))
        (values
         (with-output-to-string (stream)
           (if (null rows)
               (format stream "no results for ~s~%" query)
               (loop for row in rows
                     for index from 1
                     do (format stream "~d. ~a~%   ~a~%~@[   ~a~%~]"
                                index
                                (or (nlk:json-value row :text "title") "(untitled)")
                                (nlk:json-value row :text "url")
                                (nlk:json-value row :text "description")))))
         nil))))
