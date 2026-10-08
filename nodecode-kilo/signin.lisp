;;;; signin.lisp --- signing in to Kilo Gateway with a device code, keeping the token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/kilo.kdl (a custom login, hook kilo) and ai/src/registry/oauth/
;;;; kilo.ts (loginKilo).
;;;;
;;;; This machine asks Kilo for a code, the operator opens the verification
;;;; page Kilo names on any device and approves the code, and this machine
;;;; polls until Kilo hands back the token. Nothing listens here. The token
;;;; is the gateway's bearer as it is: there is no refresh token and no
;;;; refresh, and omp keeps it for a year, after which the operator signs in
;;;; again. It is kept in the shared auth.json under oauth_tokens.kilo:
;;;; access_token, refresh_token (empty, as omp keeps it), expires_at (epoch
;;;; seconds), written the way the core writes api_keys: read, one entry set,
;;;; replaced atomically at mode 0600.

(in-package #:nodecode-kilo)

(defparameter +device-auth+ "https://api.kilo.ai/api/device-auth"
  "Kilo's device authorization: POST <this>/codes starts one, GET
<this>/codes/<code> asks whether it was approved.")

(defparameter +poll-seconds+ 5
  "Seconds between two polls (omp's POLL_INTERVAL_MS).")

(defparameter +token-seconds+ (* 365 24 60 60)
  "How long omp keeps a Kilo token before it asks for a new sign-in (ONE_YEAR_MS).")

(defparameter +exchange-seconds+ 15
  "How long one exchange with Kilo may take.")

;;; A sign-in that cannot go on says why with FAIL, which signals KILO-ERROR
;;; (both from NLK:DEFINE-PERIPHERAL in package.lisp).

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- the store ---------------------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "kilo store")
  "Held across a read and write of this cell's entry.")

(defun stored-entry (auth)
  "The oauth_tokens.kilo entry of the parsed store AUTH, or NIL."
  (let ((entry (nlk:json-value auth :object "oauth_tokens" +provider+)))
    (and (nlk:json-value entry :text "access_token") entry)))

(defun save-entry (path entry)
  "Write ENTRY as oauth_tokens.kilo of the auth.json at PATH, or take it out
when ENTRY is NIL; every other field of the file is kept."
  (let* ((path (merge-pathnames path))
         (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
         (tokens (or (nlk:json-value auth :object "oauth_tokens")
                     (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
    (if entry
        (setf (gethash +provider+ tokens) entry)
        (remhash +provider+ tokens))
    (nlk:write-file-atomically path (shasht:write-json auth nil) :mode #o600 :directory-mode #o700)
    entry))

(defun expired-p (entry)
  "Whether ENTRY's token has passed the expiry omp gave it."
  (let ((expiry (nlk:json-value entry :integer "expires_at")))
    (and expiry (<= expiry (unix-seconds)))))

;;; --- one exchange ------------------------------------------------------------------------

(defun body-string (body)
  "An HTTP BODY dexador answered, as text."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun exchange (method url)
  "One METHOD (:post or :get) to URL within +EXCHANGE-SECONDS+: (values JSON
STATUS), any status answered as a value; a transport failure signals KILO-ERROR."
  (handler-case
      (sb-sys:with-deadline (:seconds +exchange-seconds+)
        (multiple-value-bind (body status)
            (handler-case (if (eq method :post)
                              (dex:post url :headers '(("content-type" . "application/json"))
                                            :connect-timeout +exchange-seconds+
                                            :read-timeout +exchange-seconds+
                                            :use-connection-pool nil)
                              (dex:get url :connect-timeout +exchange-seconds+
                                           :read-timeout +exchange-seconds+
                                           :use-connection-pool nil))
              (dex:http-request-failed (condition)
                (values (dex:response-body condition) (dex:response-status condition))))
          (values (ignore-errors (nlk:decode-json (body-string body))) status)))
    (kilo-error (condition) (error condition))
    ((or error sb-sys:deadline-timeout) (condition)
      (fail "~a did not answer: ~a" url (nle:transport-failure-label condition url)))))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

;;; --- the device grant ------------------------------------------------------------------

(defstruct (login (:copier nil))
  "One device sign-in waiting for its code to be approved."
  (code "")
  ;; the epoch second Kilo stops honouring the code
  (deadline 0)
  (auth-path nil)
  (cancelled nil)
  (thread nil))

(defvar *login* nil
  "The sign-in waiting for its code to be approved, or NIL.")

(defun pause (seconds)
  "Wait SECONDS between two polls."
  (sleep seconds))

(defun request-code ()
  "(values CODE VERIFICATION-URL EXPIRES-IN): Kilo's answer to a new device sign-in."
  (multiple-value-bind (json status) (exchange :post (format nil "~a/codes" +device-auth+))
    (cond ((eql status 429)
           (fail "Too many pending authorization requests. Please try again later."))
          ((not (ok-p status))
           (fail "Failed to initiate device authorization: ~a" status)))
    (let ((code (nlk:json-value json :text "code"))
          (url (nlk:json-value json :text "verificationUrl"))
          (expires-in (nlk:json-value json :number "expiresIn")))
      (unless (and code url expires-in (plusp expires-in))
        (fail "Kilo device authorization response missing required fields"))
      (values code url expires-in))))

(defun poll (login)
  "Ask once whether LOGIN's code was approved: the token once it was, NIL
while it waits."
  (multiple-value-bind (json status)
      (exchange :get (format nil "~a/codes/~a" +device-auth+ (quri:url-encode (login-code login))))
    (cond ((eql status 202) nil)
          ((eql status 403) (fail "Authorization was denied"))
          ((eql status 410) (fail "Authorization code expired. Please try again."))
          ((not (ok-p status)) (fail "Failed to poll device authorization: ~a" status))
          (t (let ((state (nlk:json-value json :string "status"))
                   (token (nlk:json-value json :text "token")))
               (cond ((and (equal state "approved") token) token)
                     ((equal state "denied") (fail "Authorization was denied"))
                     ((equal state "expired") (fail "Authorization code expired. Please try again."))
                     (t nil)))))))

(defun token-entry (token)
  "The stored entry TOKEN makes: omp's credentials for a Kilo sign-in."
  (nlk:json-object "provider" +provider+
                   "access_token" token
                   "refresh_token" ""
                   "expires_at" (+ (unix-seconds) +token-seconds+)))

(defun say (text &optional (level :info))
  "Tell the operator TEXT once."
  (nle:notice text :level level))

(defun finish-login (login)
  "LOGIN's thread: poll until the code is approved or expires, keep the token,
say how it went."
  ;; The first poll goes at once, as omp's does; every wait after is five seconds.
  (unwind-protect
       (handler-case
           (loop
             (when (login-cancelled login) (return))
             (when (>= (unix-seconds) (login-deadline login))
               (fail "Authentication timed out. Please try again."))
             (alexandria:when-let (token (poll login))
               (unless (login-cancelled login)
                 (bt2:with-lock-held (*store-lock*)
                   (save-entry (login-auth-path login) (token-entry token)))
                 ;; a sign-in that was needed is no longer
                 (nle:notice nil :key +key+)
                 (say "kilo: signed in; /models lists the Kilo Gateway models under kilo/"))
               (return))
             (pause +poll-seconds+))
         (error (condition)
           (unless (login-cancelled login)
             (say (format nil "kilo: sign-in failed: ~a" condition) :warning))))
    (when (eq *login* login) (setf *login* nil))))

(defun cancel-login ()
  "Stop the sign-in waiting for its code, if one is."
  (alexandria:when-let (login (shiftf *login* nil))
    (setf (login-cancelled login) t)))

(defun start-login (auth-path)
  "Ask Kilo for a code and answer what the operator does with it; the
polling runs on a thread of its own."
  (cancel-login)
  (multiple-value-bind (code url expires-in) (request-code)
    (let ((login (make-login :code code :deadline (+ (unix-seconds) (floor expires-in))
                             :auth-path auth-path)))
      (setf *login* login
            (login-thread login) (bt2:make-thread (lambda () (finish-login login))
                                                  :name "kilo sign-in"))
      (format nil "Open ~a and enter the code~%~%    ~a~%~%~
                   This machine checks every ~d seconds for ~d minutes and finishes the sign-in once the code is approved."
              url code +poll-seconds+ (ceiling expires-in 60)))))

;;; --- the slash command -----------------------------------------------------------------

(defun status-text (path)
  "What /kilo status says of the sign-in kept at PATH."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file path)))))
    (cond ((null entry)
           (format nil "kilo: not signed in; /kilo login signs in with a device code~@[, ~a holds a key~]"
                   (some (lambda (name) (and (nle::credential-env name) name)) +env+)))
          ((expired-p entry)
           "kilo: the sign-in has expired; /kilo login signs in again")
          (t (let ((expiry (nlk:json-value entry :integer "expires_at")))
               (format nil "kilo: signed in~@[; the token is good for ~d more days~]"
                       (and expiry (floor (- expiry (unix-seconds)) 86400))))))))

(defun run-command (args session-id)
  "/kilo login | logout | status."
  (declare (ignore session-id))
  (let* ((args (string-trim " " (or args "")))
         (space (position #\Space args))
         (verb (string-downcase (subseq args 0 (or space (length args)))))
         (path nle::*auth-file-path*))
    (handler-case
        (cond ((equal verb "login") (start-login path))
              ((equal verb "logout")
               (cancel-login)
               (bt2:with-lock-held (*store-lock*) (save-entry path nil))
               "kilo: signed out; the kilo token is gone from auth.json")
              ((member verb '("" "status") :test #'equal) (status-text path))
              (t "usage: /kilo login | logout | status"))
      (kilo-error (condition) (format nil "kilo: ~a" condition)))))
