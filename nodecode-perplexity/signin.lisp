;;;; signin.lisp --- signing in to a Perplexity Pro/Max account.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/perplexity.kdl (a custom login, hook perplexity: expiry
;;;; jwt-or-never, refresh none) and ai/src/registry/oauth/perplexity.ts
;;;; (loginPerplexity, httpEmailLogin, extractFromNativeApp).
;;;;
;;;; omp's login has three ways in, tried in this order:
;;;;
;;;;   the desktop app   on macOS, the legacy ai.perplexity.mac app keeps its
;;;;                     session token in its defaults; the login borrows it
;;;;                     (unless the section says borrow_app_session false)
;;;;   browser SSO       a host-managed browser window whose session cookie
;;;;                     the host captures; Nodecode has no such window (see
;;;;                     README, Gaps)
;;;;   an email code     Perplexity mails a code; the code (and, for an
;;;;                     account with an authenticator, its code too) buys the
;;;;                     session token
;;;;
;;;; The email way is two commands here, because the operator types a code
;;;; that arrives elsewhere: /perplexity login EMAIL asks Perplexity to mail
;;;; it, /perplexity code CODE hands it back. The cookies Perplexity sets
;;;; between the two ride in a jar kept with the waiting sign-in, as omp keeps
;;;; its CookieMap. The session token is kept in the shared auth.json under
;;;; oauth_tokens.perplexity (provider.lisp, the store).

(in-package #:nodecode-perplexity)

(defstruct (pending (:copier nil))
  "An email sign-in waiting for its code."
  (email "")
  (csrf "")
  (jar nil)
  ;; the authenticator challenge Perplexity answered the email code with, or NIL
  (challenge nil)
  (auth-path nil))

(defvar *pending* nil
  "The email sign-in waiting for a code, or NIL.")

(defun app-headers (&optional json)
  "The headers a sign-in exchange carries: the app's identity, and a JSON
content type when it sends a body."
  `(("user-agent" . ,+app-user-agent+)
    ("x-app-apiversion" . ,+api-version+)
    ,@(when json '(("content-type" . "application/json")))))

(defun post-json (url object jar)
  "POST OBJECT as JSON to URL with the sign-in's cookies in JAR: (values JSON STATUS TEXT)."
  (multiple-value-bind (text status)
      (exchange :post url :headers (app-headers t) :content (nlk:encode-json-object object) :cookie-jar jar)
    (values (decode text) status text)))

(defun jar-session (jar)
  "The session token among JAR's cookies, or NIL."
  (some (lambda (name)
          (let ((cookie (find name (cl-cookie:cookie-jar-cookies jar)
                              :key #'cl-cookie:cookie-name :test #'string=)))
            (and cookie (cl-cookie:cookie-value cookie))))
        +session-cookies+))

(defun refusal (json fallback)
  "Why a verification JSON says it failed: its text, its error code, its status."
  (or (nlk:json-value json :text "text")
      (nlk:json-value json :text "error_code")
      (nlk:json-value json :text "status")
      fallback))

;;; --- the desktop app -----------------------------------------------------------------

(defun macos-p ()
  "Whether this machine is a Mac."
  (uiop:os-macosx-p))

(defun app-session ()
  "The session token the legacy macOS app keeps, or NIL (another platform,
the app absent, or the section saying not to borrow it)."
  ;; Newer Mac apps keep theirs in a restricted Keychain, out of reach.
  (when (and (macos-p) (setting :borrow-app-session))
    (let ((token (ignore-errors
                  (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (uiop:run-program '("defaults" "read" "ai.perplexity.mac" "authToken")
                                                 :output :string)))))
      (and (plusp (length token)) (string/= token "(null)") token))))

;;; --- the email code ------------------------------------------------------------------

(defun send-code (email auth-path)
  "Ask Perplexity to mail EMAIL a sign-in code: the PENDING sign-in."
  (let ((jar (cl-cookie:make-cookie-jar)))
    (multiple-value-bind (text status)
        (exchange :get (format nil "~a/api/auth/csrf" +site+) :headers (app-headers) :cookie-jar jar)
      (unless (ok-p status)
        (fail "Perplexity CSRF request failed: ~a" status))
      (let ((csrf (nlk:json-value (decode text) :text "csrfToken")))
        (unless csrf
          (fail "Perplexity CSRF response missing csrfToken"))
        (multiple-value-bind (json status text)
            (post-json (format nil "~a/api/auth/signin-email" +site+)
                       (nlk:json-object "email" email "csrfToken" csrf) jar)
          (declare (ignore json))
          (unless (ok-p status)
            (fail "Perplexity send login code failed (~a): ~a" status (clip text)))
          (make-pending :email email :csrf csrf :jar jar :auth-path auth-path))))))

(defun keep (pending token)
  "Keep the session TOKEN PENDING bought, and end PENDING."
  (bt2:with-lock-held (*store-lock*)
    (save-entry (pending-auth-path pending) (token-entry token (pending-email pending))))
  (when (eq *pending* pending) (setf *pending* nil))
  (format nil "perplexity: signed in~@[ as ~a~]; (perplexity:search ...) now asks with your account's models"
          (pending-email pending)))

(defun accept (pending json)
  "Keep the token the verification JSON carries for PENDING, or say why not."
  (let ((token (or (nlk:json-value json :text "token") (nlk:json-value json :text "challenge_token")))
        (status (nlk:json-value json :text "status")))
    (when (or (null token)
              (nlk:json-value json :text "error_code")
              (and status (string/= status "success")))
      (fail "Perplexity OTP verification response rejected: ~a" (refusal json "missing token")))
    (keep pending token)))

(defun enter-code (pending code)
  "Hand PENDING the CODE the operator typed: the email code, or the
authenticator's once Perplexity asked for it. Answers what to say."
  (if (pending-challenge pending)
      (multiple-value-bind (json status)
          (post-json (format nil "~a/api/auth/totp/challenge-verify" +site+)
                     (nlk:json-object "token" (pending-challenge pending) "code" code)
                     (pending-jar pending))
        (unless (ok-p status)
          (fail "Perplexity authenticator verification failed: ~a" (refusal json "TOTP verification failed")))
        ;; an answer with no token left the session in the cookies
        (accept pending (if (nlk:json-value json :text "token")
                            json
                            (nlk:json-object "token" (jar-session (pending-jar pending)) "status" "success"))))
      (multiple-value-bind (json status)
          (post-json (format nil "~a/api/auth/signin-otp" +site+)
                     (nlk:json-object "email" (pending-email pending) "otp" code
                                      "csrfToken" (pending-csrf pending))
                     (pending-jar pending))
        (unless (ok-p status)
          (fail "Perplexity OTP verification failed: ~a" (refusal json "OTP verification failed")))
        (let ((challenge (nlk:json-value json :text "challenge_token")))
          (if (and challenge (equal "totp_challenge_required" (nlk:json-value json :string "status")))
              (progn
                (setf (pending-challenge pending) challenge)
                "perplexity: this account also asks for an authenticator code; type /perplexity code CODE with the code your authenticator app shows")
              (accept pending json))))))

;;; --- the slash command -----------------------------------------------------------------

(defun start-login (email auth-path)
  "/perplexity login [EMAIL]: the desktop app's session when there is one,
else a code mailed to EMAIL."
  (setf *pending* nil)
  (alexandria:if-let (token (app-session))
    (keep (make-pending :email nil :auth-path auth-path) token)
    (cond ((zerop (length email))
           "usage: /perplexity login EMAIL -- Perplexity mails a sign-in code to EMAIL")
          (t (setf *pending* (send-code email auth-path))
             (format nil "Perplexity mailed a sign-in code to ~a. Type~%~%    /perplexity code CODE~%~%~
                          with the code from that mail to finish signing in." email)))))

(defun status-text (path)
  "What /perplexity status says of the sign-in kept at PATH."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file path)))))
    (cond ((null entry)
           (format nil "perplexity: not signed in; /perplexity login EMAIL signs in with a mailed code~
                        ~@[; ~a is set~]~@[; ~a is set~]"
                   (and (nle::credential-env +cookies-env+) +cookies-env+)
                   (and (nle::credential-env +key-env+) +key-env+)))
          ((not (usable-p entry))
           "perplexity: the session has expired; /perplexity login signs in again")
          (t (let ((expiry (nlk:json-value entry :integer "expires_at")))
               (format nil "perplexity: signed in~@[ as ~a~]; ~:[the session does not expire~;~:*the session is good for ~d more minutes~]"
                       (nlk:json-value entry :text "email")
                       (and expiry (floor (- expiry (unix-seconds)) 60))))))))

(defun run-command (args session-id)
  "/perplexity login [EMAIL] | code CODE | logout | status."
  (declare (ignore session-id))
  (let* ((args (string-trim " " (or args "")))
         (space (position #\Space args))
         (verb (string-downcase (subseq args 0 (or space (length args)))))
         (rest (string-trim " " (if space (subseq args space) "")))
         (path nle::*auth-file-path*))
    (handler-case
        (cond ((equal verb "login") (start-login rest path))
              ((equal verb "code")
               (cond ((null *pending*)
                      "perplexity: no sign-in is waiting for a code; /perplexity login EMAIL first")
                     ((zerop (length rest)) "usage: /perplexity code CODE")
                     (t (enter-code *pending* rest))))
              ((equal verb "logout")
               (setf *pending* nil)
               (bt2:with-lock-held (*store-lock*) (save-entry path nil))
               "perplexity: signed out; the perplexity session is gone from auth.json")
              ((member verb '("" "status") :test #'equal) (status-text path))
              (t "usage: /perplexity login [EMAIL] | code CODE | logout | status"))
      (perplexity-error (condition) (format nil "perplexity: ~a" condition)))))
