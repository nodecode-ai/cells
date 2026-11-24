;;;; client.lisp --- the section read once, and the one seam to Browserless.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Browserless is a hosted browser: one HTTP request renders one page and
;;;; answers its content, a screenshot or a PDF. This file is the only place
;;;; that talks to it.
;;;;
;;;; Every exchange goes through *HTTP* (NLK:HTTP by default), so a test
;;;; binds that one name and no socket is opened. A refused status is a
;;;; VALUE here, not a condition: (CALL ...) answers the body and the
;;;; status, and the caller decides what the status means for its tool.
;;;;
;;;; The token rides the query string, as Browserless' API takes it, and is
;;;; the one thing that must never reach a message: REDACT takes it back out
;;;; of anything this cell says, and the peripheral's :FILTER is that same
;;;; function.

(in-package #:nodecode-browserless)

;;; --- the seam ------------------------------------------------------------

(defparameter +account-help+
  "A Browserless API token comes from your account page, https://www.browserless.io/account."
  "Where the token comes from, said in every message that needs one.")

(defvar *http* #'nlk:http
  "The cell's one network seam: NLK:HTTP's own argument list, answering
(values BODY STATUS HEADERS TRUNCATED-P). A test binds this name.")

(defun http (method url &key headers content timeout binary)
  "One exchange through *HTTP*, connect and read alike under TIMEOUT."
  (funcall *http* method url :headers headers :content content
                    :timeout timeout :connect-timeout (min 10 timeout) :binary binary))

;;; --- the section ----------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed when it is a string holding more than spaces, else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun two-letter-p (text)
  "Whether TEXT is exactly two ASCII letters, as a proxy country code is."
  (and (= 2 (length text))
       (every (lambda (char) (and (alpha-char-p char) (< (char-code char) 128))) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the API token the section names: read once, here."
  (declare (ignore table))
  (let ((base (string-right-trim "/" (or (getf values :base-url) "")))
        (env (nonblank (getf values :token-env)))
        (file (nonblank (getf values :token-file)))
        (proxy (string-downcase (or (nonblank (getf values :proxy)) "")))
        (country (string-downcase (or (nonblank (getf values :proxy-country)) "")))
        (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (unless (or (uiop:string-prefix-p "https://" base) (uiop:string-prefix-p "http://" base))
      (nlk:config-error "base_url is ~s; it is the Browserless endpoint your account answers on, ~
https:// or http:// included, e.g. \"https://production-sfo.browserless.io\"" base))
    (unless (or (zerop (length proxy)) (string= proxy "residential"))
      (nlk:config-error "proxy is ~s; it is \"residential\" for a residential IP, or empty for ~
Browserless' own" proxy))
    (when (and (plusp (length country)) (not (two-letter-p country)))
      (nlk:config-error "proxy_country is ~s; it is a two-letter country code such as \"us\", ~
\"gb\" or \"de\"" country))
    (setf (getf plist :base-url) base
          (getf plist :proxy) proxy
          (getf plist :proxy-country) country
          (getf plist :token)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "token_env names ~a, which is unset or empty. ~a"
                                           env +account-help+)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "token_file ~a cannot be read, or is empty. ~a"
                                            file +account-help+)))
                (t (nlk:config-error "no token: set token_env to the environment variable ~
holding a Browserless API token, or token_file to a file holding one. ~a" +account-help+))))
    plist))

(defun redact (text)
  "TEXT with the configured API token taken out of it."
  ;; Read off the settings plist, not through SETTING: FAIL runs this while
  ;; the cell may not be started at all.
  (let ((token (getf *browserless* :token)))
    (if (and (stringp text) (stringp token) (plusp (length token)))
        (uiop:frob-substrings text (list token) "[redacted]")
        text)))

(defun base-url ()
  "The configured Browserless endpoint, trailing slashes off; \"\" when unset."
  (or (setting :base-url) ""))

(defun require-base-url ()
  "The endpoint, or a refusal naming the member that is missing."
  (let ((url (base-url)))
    (if (plusp (length url))
        url
        (fail "browserless.base_url is not set: put the Browserless endpoint your account ~
answers on in the browserless section, e.g. {\"browserless\": {\"base_url\": ~
\"https://production-sfo.browserless.io\"}}."))))

;;; --- one exchange ---------------------------------------------------------

(defun timeout-ms ()
  "The operator's timeout, in the milliseconds Browserless counts in."
  (* 1000 (setting :timeout-seconds)))

(defun launch-params ()
  "The query Browserless reads the launch options off, token first."
  (let ((params (list (cons "token" (setting :token))
                      (cons "timeout" (princ-to-string (timeout-ms))))))
    (when (setting :stealth)
      (push (cons "stealth" "true") params))
    (let ((proxy (setting :proxy)))
      (when (and proxy (plusp (length proxy)))
        (push (cons "proxy" proxy) params)
        (let ((country (setting :proxy-country)))
          (when (and country (plusp (length country)))
            (push (cons "proxyCountry" country) params)))
        (when (setting :proxy-sticky)
          (push (cons "proxySticky" "true") params))))
    (when (setting :block-ads)
      (push (cons "blockAds" "true") params))
    (when (setting :solve-captchas)
      (push (cons "solveCaptchas" "true") params))
    (nreverse params)))

(defun endpoint (path &key params)
  "PATH on the configured endpoint with LAUNCH-PARAMS and PARAMS after it."
  (format nil "~a~a?~a" (require-base-url) path
          (quri:url-encode-params (append (launch-params) params))))

(defun goto-options ()
  "How long the page may take to load, as Browserless reads it."
  (nlk:make-json-object "waitUntil" "networkidle2" "timeout" (timeout-ms)))

(defun selector-wait (selector)
  "The waitForSelector object for a CSS SELECTOR, or NIL."
  (and (nonblank selector)
       (nlk:make-json-object "selector" (nonblank selector)
                              "timeout" (timeout-ms))))

(defun call (method path &key params object timeout binary)
  "One exchange with Browserless => (values BODY STATUS).
A transport failure signals BROWSERLESS-ERROR naming the endpoint and the hint."
  (let ((timeout (or timeout (setting :timeout-seconds))))
    (handler-case
        (multiple-value-bind (body status) (http method (endpoint path :params params)
                                                 :headers '(("accept" . "*/*"))
                                                 :content (and object (nlk:encode-json-object object))
                                                 :timeout timeout :binary binary)
          (values body status))
      (browserless-error (condition) (error condition))
      (nlk:turn-cancelled-condition (condition) (error condition))
      (error (condition)
        (fail "cannot reach Browserless at ~a (~a). Is this machine online? ~a"
              (base-url) (nlk:one-line (princ-to-string condition)) +account-help+)))))

(defun refused-reason (status body)
  "Browserless' own words for a refused status, in a sentence the model can act on."
  (let ((detail (and (stringp body) (nlk:trimmed body))))
    (cond ((member status '(401 403))
           (format nil "Browserless rejected the API token (HTTP ~d). Check token_env, or ~
token_file, in the browserless section. ~a" status +account-help+))
          ((= status 429)
           "Browserless' plan concurrency limit is reached (HTTP 429): too many browsers open at once. ~
Try again when a task has finished, or raise the plan's limit.")
          ((and detail (plusp (length detail)))
           (format nil "Browserless refused it (HTTP ~d): ~a" status (nlk:clip detail 300)))
          (t (format nil "Browserless refused it (HTTP ~d) with no explanation." status)))))

(defun require-status (status body what)
  "Fail with Browserless' reason unless STATUS is under 300."
  (when (>= status 300)
    (fail "~a. ~a" (refused-reason status body) what))
  status)

(defun answer (&rest members)
  "MEMBERS as the JSON text a tool answers with."
  (nlk:encode-json-object (apply #'nlk:make-json-object members)))

;;; --- files ----------------------------------------------------------------

(defun save-bytes (octets name &key (type "png"))
  "Write OCTETS to a file named NAME under the home's browserless folder,
and answer its namestring."
  (let ((path (nlk:home (format nil "browserless/~a.~a" name type))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    (namestring path)))

(defun hex-of (octets n)
  "The first N of OCTETS as hex digits, so two files of one page do not collide."
  (let ((digits "0123456789abcdef"))
    (with-output-to-string (out)
      (loop for i from 0 below (min n (length octets))
            for octet = (elt octets i)
            do (write-char (char digits (ldb (byte 4 4) octet)) out)
               (write-char (char digits (ldb (byte 4 0) octet)) out)))))

(defun file-name-for (url)
  "A file name stem for URL: its host and path, cut to something a filesystem takes."
  (let* ((uri (ignore-errors (quri:uri url)))
         (host (and uri (quri:uri-host uri)))
         (path (and uri (quri:uri-path uri)))
         (raw (format nil "~a~a" (or host "page") (or path "")))
         (clean (with-output-to-string (out)
                  (loop for char across raw
                        do (write-char (if (or (alphanumericp char) (member char '(#\- #\_)))
                                           (char-downcase char)
                                           #\-)
                                       out)))))
    (format nil "~a-~a" (nlk:clip clean 60) (hex-of (sb-ext:string-to-octets url) 6))))
