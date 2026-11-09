;;;; wire.lisp --- one REST exchange with the GitHub API, and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell's only way out is HTTP-SEND, which a test replaces: every
;;;; github_* call goes through it, so a stub sees exactly what GitHub would.
;;;; The original spoke urllib to api.github.com and refused redirects, to keep
;;;; the token on the origin it was meant for; API-BASE holds the same
;;;; guarantee here -- an https URL, no credentials, no query, no fragment --
;;;; and the token rides only that base.

(in-package #:nodecode-github-app)

(defparameter +default-api+ "https://api.github.com"
  "Where the REST API answers, unless the section names another base.")

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun api-base ()
  "The section's API base, normalized, or a refusal naming what is wrong.
The rules are the original's (github_app/auth.py, validate_api_base): https
only, no user, password, query or fragment, so a token cannot be steered to a
plain-text or foreign origin by a stray character."
  (let* ((raw (or (nonblank (setting :api-url)) +default-api+))
         (url (string-right-trim "/" (nlk:trimmed raw)))
         (rest (and (> (length url) (length "https://"))
                    (subseq url (length "https://")))))
    (unless (and (uiop:string-prefix-p "https://" url)
                 rest
                 (not (find #\Space url))
                 (not (find #\? url))
                 (not (find #\# url))
                 (not (find #\@ rest))
                 (not (find #\% rest)))
      (fail "api_url must be an https API base without credentials, query or fragment, not ~s" raw))
    url))

(defun url-encode (text)
  "TEXT, percent-encoded, as a query value is spelled."
  (with-output-to-string (out)
    (loop for char across (princ-to-string text)
          do (if (or (alphanumericp char) (find char "-._~" :test #'char=))
                 (write-char char out)
                 (format out "%~2,'0x" (char-code char))))))

(defun with-params (url params)
  "URL with PARAMS, an alternating key/value list, as its query string; a NIL
or empty value is not sent at all."
  (let ((pairs (loop for (key value) on params by #'cddr
                     when (and value (not (equal value "")))
                       collect (format nil "~a=~a" (url-encode key) (url-encode value)))))
    (if pairs (format nil "~a?~{~a~^&~}" url pairs) url)))

(defun http-send (method url &rest options)
  "The cell's one network call, NLK:HTTP; a test stubs this name."
  (apply #'nlk:http method url options))

(defun github-message (text)
  "What GitHub said in an error body, in one line."
  (let ((json (ignore-errors (nlk:decode-json (if (stringp text) text "")))))
    (if (hash-table-p json)
        (or (nonblank (nlk:json-value json :string "message"))
            (nlk:one-line (nlk:encode-json-object json) :cap 300))
        (nlk:one-line (if (stringp text) text "") :cap 300))))

(defun api-json (method path &key token params body)
  "One GitHub REST call => the decoded body. PATH is absolute
(\"/repos/...\"); a status of 400 and up refuses with GitHub's own words."
  (let ((url (with-params (concatenate 'string (api-base) path) params))
        (content (and body (nlk:encode-json-object body))))
    (multiple-value-bind (text status)
        (handler-case
            (http-send method url
                       :headers (append (list (cons "accept" "application/vnd.github+json")
                                              (cons "x-github-api-version" "2022-11-28")
                                              (cons "user-agent" "nodecode-github-app")
                                              (cons "content-type" "application/json"))
                                        (and token
                                             (list (cons "authorization"
                                                         (format nil "Bearer ~a" token)))))
                       :content content
                       :timeout (setting :timeout)
                       :connect-timeout (setting :timeout))
          (error (condition)
            (fail "GitHub did not answer ~(~a~) ~a: ~a"
                  method path (nlk:one-line (princ-to-string condition)))))
      (let ((text (if (stringp text) text (and text (nlk:body-text text)))))
        (cond ((and (integerp status) (< status 400))
               (and text (plusp (length text)) (ignore-errors (nlk:decode-json text))))
              (t (fail "GitHub answered ~a to ~(~a~) ~a: ~a"
                       status method path (github-message text))))))))
