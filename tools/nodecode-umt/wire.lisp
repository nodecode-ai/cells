;;;; wire.lisp --- one dial, JSON, the entities an API emits, the text shape.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call in this cell is one GET through *HTTP*, which is NLK:HTTP
;;;; unless a test binds a stub. A status outside 2xx, a body that is not
;;;; JSON, or a transport failure is a UMT-ERROR naming the host and what it
;;;; said, which is what the model reads; nothing here retries, and nothing
;;;; carries a credential -- the three APIs are public and keyless.

(in-package #:nodecode-umt)

(defparameter +user-agent+ "nodecode-umt/0.1 (+https://github.com/nodecode-ai/cells)"
  "The one user agent every call sends; arXiv's API etiquette asks callers to say who they are.")

(defvar *http* #'nlk:http
  "The exchange every call dials, called as NLK:HTTP is: (METHOD URL &KEY HEADERS TIMEOUT)
=> (VALUES BODY STATUS). A test binds its stub here; nothing else changes.")

(defun url-host (url)
  "URL's host, for a refusal that says who failed; URL itself when it carries none."
  (or (ppcre:register-groups-bind (host) ("^[a-zA-Z]+://([^/?#]+)" url) host) url))

(defun http-get (url &key accept)
  "URL's body as text. A status outside 2xx, or a transport failure, is a
UMT-ERROR naming the host, the status and the first of what it said."
  (multiple-value-bind (body status)
      (handler-case
          (funcall *http* :get url
                        :headers (list (cons "accept" (or accept "application/json"))
                                       (cons "user-agent" +user-agent+))
                        :timeout (setting :timeout))
        (error (condition)
          (fail "~a did not answer: ~a" (url-host url)
                (nlk:one-line (princ-to-string condition) :cap 200))))
    (unless (and (integerp status) (<= 200 status 299))
      (fail "~a answered HTTP ~a~@[: ~a~]" (url-host url) status
            (and (stringp body) (plusp (length body))
                 (nlk:one-line (nlk:clip body 200 :ellipsis "...") :cap 200))))
    (if (stringp body) body (princ-to-string body))))

(defun http-json (url &key (what "The API"))
  "URL's body decoded, or a UMT-ERROR naming WHAT as not answering JSON."
  (let ((text (http-get url)))
    (handler-case (nlk:decode-json text)
      (error ()
        (fail "~a answered something that is not JSON" what)))))

;;; --- text -----------------------------------------------------------------------

(defparameter +entities+
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . " ") ("ndash" . "-") ("mdash" . "-") ("hellip" . "...")
    ("rsquo" . "'") ("lsquo" . "'") ("ldquo" . "\"") ("rdquo" . "\"")
    ("copy" . "(c)") ("middot" . "."))
  "The named entities the two feeds emit, without their & and ;.")

(defun entity-value (name)
  "NAME's replacement, for the name between an & and its ;."
  (or (cdr (assoc name +entities+ :test #'string-equal))
      (when (and (> (length name) 1) (char= (char name 0) #\#))
        (let ((code (ignore-errors
                      (if (and (> (length name) 2) (char-equal (char name 1) #\x))
                          (parse-integer name :start 2 :radix 16)
                          (parse-integer name :start 1)))))
          (and (integerp code) (plusp code) (< code 1114112) (code-char code))))))

(defun decode-entities (text)
  "TEXT with &amp;, &#39; and the rest of them as the characters they name.
An & that names nothing stays as it was."
  (if (not (and (stringp text) (find #\& text)))
      text
      (with-output-to-string (out)
        (loop with length = (length text)
              with index = 0
              while (< index length)
              do (let ((amp (position #\& text :start index)))
                   (if (null amp)
                       (progn (write-string text out :start index) (setf index length))
                       (progn
                         (write-string text out :start index :end amp)
                         (let* ((semi (position #\; text :start (1+ amp) :end (min length (+ amp 12))))
                                (value (and semi (entity-value (subseq text (1+ amp) semi)))))
                           (cond (value (write-string value out) (setf index (1+ semi)))
                                 (t (write-char #\& out) (setf index (1+ amp))))))))))))

(defun plain-text (html &key (limit 600))
  "HTML as the text a reader sees: a paragraph break for <p>, every other tag
gone, the entities decoded, whitespace collapsed, clipped to LIMIT characters."
  (when (and (stringp html) (plusp (length html)))
    (let ((text (ppcre:regex-replace-all "(?i)</p\\s*>|<p\\s*[^>]*>" html (format nil "~%~%"))))
      (setf text (ppcre:regex-replace-all "(?i)<br\\s*/?>|<[^>]*>" text ""))
      (setf text (decode-entities text))
      (setf text (nlk:trimmed text))
      (if (plusp (length text)) (nlk:clip text limit :ellipsis "...") nil))))

(defun timestamp (seconds)
  "Unix SECONDS as an ISO-8601 UTC stamp, or NIL; a story's date as a reader wants it."
  (when (and (integerp seconds) (plusp seconds))
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time (+ 2208988800 seconds) 0)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
              year month day hour minute second))))

(defun bounded (value low high)
  "VALUE as an integer between LOW and HIGH; LOW when it is no integer."
  (max low (min high (if (integerp value) value low))))

(defparameter +unreserved+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~")

(defun url-encode (text)
  "TEXT as one query value: every byte outside RFC 3986's unreserved set is %XX."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (princ-to-string text) :external-format :utf-8)
          for character = (and (< octet 128) (code-char octet))
          do (if (and character (find character +unreserved+))
                 (write-char character out)
                 (format out "%~2,'0X" octet)))))
