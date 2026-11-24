;;;; client.lisp --- the one network seam, and past's own endpoints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through *REQUEST*, so a test rebinds one
;;;; name and never touches the network. What crosses is the wire the original
;;;; plugin was written against: one bearer project key, one JSON object each
;;;; way, POST /api/v1/recall and POST /api/v1/ingest/batch on the address the
;;;; section names.
;;;;
;;;; A status is a value, never a signal: *REQUEST* answers the parsed body
;;;; and the status, and the verbs decide what a caller reads.

(in-package #:nodecode-past)

;;; --- the seam -------------------------------------------------------------

(defun http-json (method url body)
  "METHOD URL, BODY a JSON string or NIL => (VALUES PARSED STATUS); STATUS a
number, or the transport fact when no answer came at all."
  (handler-case
      (multiple-value-bind (text status)
          (nlk:http method url
                    :content body
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))
                    :headers (list (cons "authorization" (format nil "Bearer ~a" (key-value)))
                                   '("accept" . "application/json")
                                   '("content-type" . "application/json")))
        (values (and (stringp text)
                     (plusp (length (string-trim '(#\Space #\Tab #\Newline) text)))
                     (handler-case (nlk:decode-json text) (error () nil)))
                status))
    ((or error sb-sys:deadline-timeout) (condition)
      (values nil (nle:transport-failure-label condition url)))))

(defparameter *request* #'http-json
  "The cell's one network call: (METHOD URL BODY-JSON) => (VALUES PARSED STATUS).
A test rebinds this name and answers canned objects.")

;;; --- the section, read back -------------------------------------------------

(defun key-value ()
  "The project key the section names, inline or by environment variable; \"\"
when it names none."
  (let ((inline (setting :api-key)))
    (if (and (stringp inline) (plusp (length (string-trim '(#\Space #\Tab) inline))))
        (string-trim '(#\Space #\Tab) inline)
        (let ((variable (setting :api-key-env)))
          (or (and (stringp variable) (plusp (length variable)) (uiop:getenv variable)) "")))))

(defun key-text ()
  "The project key, or a refusal naming the member it belongs in."
  (let ((key (key-value)))
    (if (plusp (length key))
        key
        (fail "past is not set up: paste a past.dev project key (past_sk_...) into past.api_key ~
               in /settings, or set PAST_API_KEY, then call this again"))))

(defun identity-text ()
  "Who the person is in past, or NIL: recall is off until it is set."
  (let ((identity (setting :identity)))
    (and (stringp identity)
         (plusp (length (string-trim '(#\Space #\Tab) identity)))
         (string-trim '(#\Space #\Tab) identity))))

(defun checked-origin (url)
  "URL once it is an http(s) origin this cell may dial, else a config refusal."
  (let* ((trimmed (string-right-trim "/" (or url "")))
         (scheme (cond ((uiop:string-prefix-p "https://" trimmed) "https")
                       ((uiop:string-prefix-p "http://" trimmed) "http")))
         (rest (and scheme (subseq trimmed (+ 3 (length scheme)))))
         (cut (and rest (position-if (lambda (c) (find c "/?#")) rest)))
         (host (and rest (subseq rest 0 (or cut (length rest))))))
    (unless (and scheme (plusp (length (or host ""))) (null cut))
      (nlk:config-error "past.api_url is ~s: an http:// or https:// origin, with no path, query or fragment"
                        url))
    (when (and (string= scheme "http")
               (not (member (string-downcase host) '("localhost" "127.0.0.1" "[::1]" "::1")
                            :test #'string=)))
      (nlk:config-error "past.api_url is ~s: use https:// for anything but a loopback address" url))
    trimmed))

;;; --- one exchange ------------------------------------------------------------

(defun failure-text (status parsed)
  "STATUS as the model reads it, with past's own sentence when it gave one."
  (let ((detail (and (hash-table-p parsed)
                      (or (nlk:json-value parsed :string "detail")
                          (nlk:json-value parsed :string "message")
                          (nlk:json-value parsed :string "error")))))
    (format nil "~a~@[ (~a)~]"
            (if (integerp status)
                (case status
                  (400 "the request was refused")
                  (401 "the key was refused")
                  (403 "the key may not do that")
                  (404 "no such endpoint")
                  (413 "the text was too long")
                  (429 "rate limited: wait a moment")
                  (t (format nil "HTTP ~d" status)))
                (princ-to-string status))
            (and (stringp detail) (plusp (length detail))
                 (subseq detail 0 (min 240 (length detail)))))))

(defun post (path body)
  "POST BODY, a JSON object, at PATH on the configured origin => (VALUES PARSED STATUS)."
  (funcall *request* :post (concatenate 'string (setting :api-url) path)
           (and body (nlk:encode-json-object body))))
