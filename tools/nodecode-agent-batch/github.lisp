;;;; github.lisp --- the token, the one network seam, and what it reads back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through *HTTP* -- NLK:HTTP -- so a test
;;;; stubs that one name and never opens a socket. The original did the same
;;;; calls with urllib and the user's own GITHUB_TOKEN: a workflow_dispatch to
;;;; launch one phase, the repository's runs and its open pull requests to show
;;;; what a dispatch did. Nothing else is asked of GitHub, and no other host is
;;;; addressed anywhere in this cell.

(in-package #:nodecode-agent-batch)

(defparameter +api+ "https://api.github.com"
  "Where GitHub's REST API answers: the original's one host.")

(defparameter *http* #'nlk:http
  "The cell's one network call. A test binds this, so nothing dials GitHub.")

(defun secret-text (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun token ()
  "The GitHub token the section names: token_env (GITHUB_TOKEN is read either
way), then token_file. A refusal says where one goes; the value is never in
the config file when a file or a variable holds it."
  (or (let ((variable (secret-text (setting :token-env))))
        (and variable (secret-text (uiop:getenv variable))))
      (secret-text (uiop:getenv "GITHUB_TOKEN"))
      (let ((file (secret-text (setting :token-file))))
        (and file (secret-text (ignore-errors (uiop:read-file-string (nlk:expand-home file))))))
      (fail "no GitHub token: set token_env in the agent-batch section to the name of an ~
             environment variable holding one (GITHUB_TOKEN is read either way), or token_file ~
             to a file holding it")))

(defun github-message (text)
  "What a failed answer said, in GitHub's own words: its message, else the body."
  (let ((object (and (stringp text) (ignore-errors (nlk:decode-json text)))))
    (or (and (hash-table-p object) (nlk:json-value object :string "message"))
        (nlk:one-line (if (stringp text) text "") :cap 300))))

(defun github (method path &key body)
  "METHOD PATH on https://api.github.com -- PATH absolute, or a whole URL -- BODY
its JSON object. => the decoded answer, NIL when empty. A transport failure or a
status from 300 up refuses with AGENT-BATCH-ERROR."
  (let* ((url (if (uiop:string-prefix-p "https://" path)
                  path
                  (concatenate 'string +api+ path)))
         (text nil)
         (status nil)
         (limit (setting :timeout-seconds))
         (headers (list (cons "authorization" (format nil "Bearer ~a" (token)))
                        (cons "accept" "application/vnd.github+json")
                        (cons "x-github-api-version" "2022-11-28")
                        (cons "user-agent" "nodecode-agent-batch")
                        (cons "content-type" "application/json"))))
    (handler-case
        (multiple-value-setq (text status)
          (funcall *http* method url :headers headers
                   :content (and body (nlk:encode-json-object body))
                   :timeout limit :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "GitHub did not answer ~a ~a: ~a" (string-downcase (string method)) path
              (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300))
           (and text (plusp (length text)) (nlk:decode-json text)))
          (t (fail "GitHub answered ~a to ~a ~a~@[: ~a~]" status method path
                   (github-message text))))))
