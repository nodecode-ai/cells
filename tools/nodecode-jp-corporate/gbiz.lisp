;;;; gbiz.lisp --- the one network seam: the gBizINFO REST API v2.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request reads the token out of the cell's section (or the
;;;; environment) and sends it to one host, api.info.gbiz.go.jp, in the
;;;; X-hojinInfo-api-token header the API asks for. gBizINFO is a free
;;;; government API whose terms forbid bulk or excessive access, so this seam
;;;; is deliberately conservative: at most one request every 0.5 seconds, a
;;;; ten-minute in-memory cache (nothing is written to disk), no retries and
;;;; no pagination of its own.
;;;;
;;;; Every byte goes through NLK:HTTP, so a test stubs that one function and
;;;; never touches the network. A status other than 200 is JP-CORPORATE-ERROR
;;;; carrying what to do next; a 404 is NIL, gBizINFO's answer for "no
;;;; corporation with that number".

(in-package #:nodecode-jp-corporate)

(defparameter +endpoint+ "https://api.info.gbiz.go.jp/hojin/v2/hojin"
  "The API base the plugin's client names: every route hangs under it.")

(defparameter +site+ "https://info.gbiz.go.jp/"
  "The registry's own site, for the attribution line and the page url.")

(defparameter +apply-url+ "https://info.gbiz.go.jp/hojin/various_registration/form"
  "Where a person applies for the free token, once.")

(defparameter +token-variable+ "GBIZINFO_API_TOKEN"
  "The environment variable the plugin read, and this cell reads too.")

(defparameter +version+ "0.1.0")

(defparameter +cache-seconds+ 600
  "How long one answer is reused: the API's terms ask for restraint, not freshness.")

(defparameter +cache-size+ 256)

(defparameter +min-interval+ 0.5
  "The least time between two requests this cell sends, in seconds.")

(defvar *cache* (make-hash-table :test #'equal)
  "URL -> (TIME . OBJECT) for answers already paid for; in memory only.")

(defvar *wire-lock* (bt2:make-lock :name "jp-corporate wire"))

(defvar *last-request* 0
  "The internal-time slot of the last request, for the spacing above.")

(defvar *requests* 0
  "How many requests this cell has actually sent since the last reset.")

;;; --- the token, and the section -------------------------------------------------

(defun nonempty (value)
  "VALUE trimmed when it holds anything, else NIL."
  (let ((text (and (stringp value) (string-trim '(#\Space #\Tab #\Newline) value))))
    (and text (plusp (length text)) text)))

(defun api-token ()
  "The token the section names inline or by variable, else GBIZINFO_API_TOKEN."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv +token-variable+))))

(defun check-config ()
  "Refuse a section that would reach the API with no token, naming what to do."
  (unless (api-token)
    (error 'nlk:config-refusal
           :detail (format nil "jp-corporate: this cell needs a free gBizINFO API token: apply once at ~a ~
(申請区分「トークン利用(WebAPI・データダウンロード)」), then set \"api_key\" (or name the variable in ~
\"api_key_env\") in the jp-corporate section, or export ~a. Apply once and keep that token: the API terms ~
forbid extra tokens made to get around usage limits."
                           +apply-url+ +token-variable+))))

(defun endpoint ()
  "The API base this cell talks to."
  (let ((given (nonempty (setting :endpoint))))
    (string-right-trim "/" (or given +endpoint+))))

(defun route-url (path params)
  "The URL of one route under the API base, PARAMS an alist of (NAME . VALUE)
with every NIL or empty VALUE left out."
  (let* ((route (string-trim "/" (or path "")))
         (url (if (zerop (length route))
                  (endpoint)
                  (format nil "~a/~a" (endpoint) route)))
         (kept (loop for (name . value) in params
                     for text = (and value (princ-to-string value))
                     when (and text (plusp (length text)))
                       collect (cons name text))))
    (if kept
        (format nil "~a?~a" url (quri:url-encode-params kept :encoding :utf-8))
        url)))

;;; --- spacing and the cache -------------------------------------------------------

(defun throttle ()
  "Keep at least +MIN-INTERVAL+ seconds between two requests."
  (let ((wait 0))
    (bt2:with-lock-held (*wire-lock*)
      (let* ((now (get-internal-real-time))
             (slot (max now (+ *last-request*
                               (round (* +min-interval+ internal-time-units-per-second))))))
        (setf *last-request* slot)
        (setf wait (/ (- slot now) (float internal-time-units-per-second)))))
    (when (plusp wait) (sleep wait))))

(defun cache-get (url)
  "=> (values OBJECT PRESENT-P): a cached answer no older than +CACHE-SECONDS+."
  (bt2:with-lock-held (*wire-lock*)
    (let ((entry (gethash url *cache*)))
      (if (and entry (< (- (get-internal-real-time) (car entry))
                        (* +cache-seconds+ internal-time-units-per-second)))
          (values (cdr entry) t)
          (values nil nil)))))

(defun cache-put (url object)
  "Remember OBJECT for URL, dropping the oldest entry when the cache is full."
  (bt2:with-lock-held (*wire-lock*)
    (when (>= (hash-table-count *cache*) +cache-size+)
      (let ((oldest nil) (oldest-url nil))
        (maphash (lambda (key entry)
                   (when (or (null oldest) (< (car entry) oldest))
                     (setf oldest (car entry) oldest-url key)))
                 *cache*)
        (when oldest-url (remhash oldest-url *cache*))))
    (setf (gethash url *cache*) (cons (get-internal-real-time) object)))
  object)

(defun clear-cache ()
  "Forget every answer this cell still holds."
  (bt2:with-lock-held (*wire-lock*)
    (clrhash *cache*)
    (setf *last-request* 0
          *requests* 0)))

(defun api-requests ()
  "How many requests this cell has sent since the last reset."
  (bt2:with-lock-held (*wire-lock*) *requests*))

;;; --- one exchange ----------------------------------------------------------------

(defun redacted (text)
  "TEXT with every spelling of the token replaced, so a refusal never echoes it."
  (let ((token (api-token)))
    (if token
        (replace-all (princ-to-string text) token "[redacted]")
        (princ-to-string text))))

(defun status-message (status)
  "The one line a refused STATUS answers, saying what to do next."
  (cond
    ((= status 401)
     (format nil "gBizINFO rejected the API token (401). Check that ~a (or the jp-corporate section's ~
\"api_key\") is exactly the token shown by the link in the 「Web API 利用申請完了」 email, with no quotes ~
or spaces around it. A revoked token is asked about at https://help.info.gbiz.go.jp/hc/ja/requests/new, ~
not replaced by a second application."
             +token-variable+))
    ((member status '(403 429))
     (format nil "gBizINFO is limiting requests for this token (~d): wait several minutes before the next ~
lookup and avoid calling the tools in a loop, since the API terms allow suspending tokens that send ~
excessive requests. Its download files are the way to read bulk data." status))
    ((= status 400)
     "gBizINFO rejected the request parameters (400): re-check the name or the address that was sent.")
    (t
     (format nil "gBizINFO answered HTTP ~d. That is usually maintenance or a temporary fault: try again ~
later (maintenance is announced at ~a)." status +site+))))

(defun decode-body (body)
  "BODY — a response — as a decoded JSON object, or a refusal when it is not JSON."
  (let ((value (handler-case (nlk:decode-json body) (error () nil))))
    (or value
        (fail "gBizINFO returned something that is not JSON (often a maintenance page): try again later, ~
maintenance is announced at ~a." +site+))))

(defun request (url)
  "One GET to URL, with the token in the header the API names => the decoded
object, or NIL when gBizINFO answered 404 (no corporation with that number)."
  (throttle)
  (bt2:with-lock-held (*wire-lock*) (incf *requests*))
  (let ((timeout (setting :timeout-seconds)))
    (multiple-value-bind (body status)
        (handler-case
            (nlk:http :get url
                      :headers (list (cons "X-hojinInfo-api-token" (or (api-token) ""))
                                     (cons "accept" "application/json")
                                     (cons "user-agent" (format nil "nodecode-jp-corporate/~a" +version+)))
                      :timeout timeout
                      :connect-timeout (min 10 timeout))
          (error (condition)
            (fail "jp-corporate: ~a" (nle:transport-failure-label condition url))))
      (cond ((eql status 200) (decode-body body))
            ((eql status 404) nil)
            (t (fail "~a" (status-message status)))))))

(defun gbiz-get (path &optional params)
  "GET one route under the API base => the decoded object, or NIL for a 404."
  (unless (api-token)
    (fail "jp-corporate: no gBizINFO API token: set \"api_key\" in the jp-corporate section, or export ~a ~
(free, applied for once at ~a)." +token-variable+ +apply-url+))
  (let ((url (route-url path params)))
    (multiple-value-bind (object present) (cache-get url)
      (if present
          object
          (cache-put url (request url))))))
