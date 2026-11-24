;;;; signin.lisp --- the OAuth 2.1 sign-in, with a device code.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original signed in from Hermes's own command line: `hermes mcp add
;;;; ... --auth oauth', or `hermes mcp login --flow device' for a short code.
;;;; A cell has neither, so the device flow is here: register this client,
;;;; ask for a code, keep it in front of the operator, and poll the token
;;;; endpoint in the background until they approve. The token lands at 0600 in
;;;; the cell's own state folder and is refreshed here when it ages out.

(in-package #:nodecode-super-artifacts)

(defstruct (pending (:copier nil))
  "A device authorization waiting for the operator's approval."
  (device-code "")
  (user-code "")
  (verification-url "")
  (client-id "")
  (origin "")
  (interval 5)
  (deadline 0)
  (failures 0))

(defvar *pending* nil
  "The device authorization being polled, or NIL.")

(defvar *worker* nil
  "The thread polling it, or NIL.")

(defvar *retrying* nil
  "Whether the request in hand already retried with a refreshed token.")

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun notice-key () "nodecode-super-artifacts")

(defun say (text &key (level :info))
  (nle:notice text :level level :key (notice-key)))

(defun form-encode (pairs)
  "PAIRS -- (NAME . VALUE) ... -- as an application/x-www-form-urlencoded body."
  (with-output-to-string (out)
    (loop for (name . value) in pairs
          for first = t then nil
          do (progn (unless first (write-char #\& out))
                    (format out "~a=~a" (quri:url-encode name)
                            (quri:url-encode (princ-to-string value)))))))

(defun oauth-post (path pairs &key (content-type "application/x-www-form-urlencoded") (content nil))
  "One POST to the product's origin, answering (VALUES JSON STATUS)."
  (multiple-value-bind (text status)
      (funcall *http* :post (format nil "~a~a" (origin-text) path)
               :headers (list (cons "content-type" content-type)
                              (cons "accept" "application/json")
                              (cons "user-agent" "nodecode-super-artifacts/0.1"))
               :content (or content (form-encode pairs))
               :timeout (setting :timeout-seconds))
    (values (decode-answer (body-text text)) status)))

;;; --- the token a finished sign-in keeps ------------------------------------------

(defun saved-token ()
  "The token a sign-in kept, as decoded JSON, or NIL."
  (let ((text (ignore-errors (file-text (token-path)))))
    (let ((json (and text (decode-answer text))))
      (and (hash-table-p json) json))))

(defun save-token (access &key refresh client-id expires-in origin)
  "Keep ACCESS, its REFRESH token and what a refresh needs, at 0600."
  (nlk:write-file-atomically
   (token-path)
   (nlk:encode-json-object
    (nlk:make-json-object
     "access_token" access
     "refresh_token" (or refresh "")
     "client_id" (or client-id "")
     "origin" (or origin (origin-text))
     "expires_at" (+ (unix-seconds) (or expires-in 3600))
     "scope" +scope+))
   :mode :private :directory-mode #o700)
  (setf *token* access)
  (wake)
  access)

(defun expired-p (json)
  "Whether JSON's access token is spent, with a minute of slack."
  (let ((expires (nlk:json-value json :integer "expires_at")))
    (and expires (< expires (+ (unix-seconds) 60)))))

(defun refresh-saved-token ()
  "Spend the kept refresh token for a new access token. Answers it, or NIL
when there is nothing to refresh."
  (let ((json (saved-token)))
    (when (and json (null (key-from-file)) (null (key-from-environment)))
      (let ((refresh (nlk:json-value json :string "refresh_token"))
            (client-id (nlk:json-value json :string "client_id")))
        (when (and refresh client-id (plusp (length refresh)))
          (multiple-value-bind (answer status)
              (oauth-post "/oauth/token"
                          (list (cons "grant_type" "refresh_token")
                                (cons "refresh_token" refresh)
                                (cons "client_id" client-id)))
            (let ((access (and (hash-table-p answer) (nlk:json-value answer :string "access_token"))))
              (when (and access (< (or status 500) 300))
                (save-token access
                            :refresh (or (nlk:json-value answer :string "refresh_token") refresh)
                            :client-id client-id
                            :expires-in (nlk:json-value answer :integer "expires_in")
                            :origin (nlk:json-value json :string "origin"))))))))))

(defun saved-access-token ()
  "The kept access token, refreshed when it has aged out, or NIL."
  (let ((json (saved-token)))
    (when json
      (if (expired-p json) (refresh-saved-token)
          (nlk:json-value json :string "access_token")))))

(defun retry-with-refreshed-token (method params &key notify)
  "One more try after a 401, on a token the refresh token bought."
  (if (and (not *retrying*) (refresh-saved-token))
      (let ((*retrying* t))
        (wake)
        (rpc method params :notify notify))
      (fail "the server refused the token: run /super-artifacts signin, or set a fresh key_file")))

;;; --- the device flow -------------------------------------------------------------

(defun register-client ()
  "This client's id, registered on the fly the way the transport expects."
  (multiple-value-bind (json status)
      (oauth-post "/oauth/register"
                  nil
                  :content-type "application/json"
                  :content (nlk:encode-json-object
                            (nlk:make-json-object
                             "client_name" "Nodecode"
                             "redirect_uris" (vector "http://127.0.0.1/oauth/callback")
                             "grant_types" (vector "authorization_code"
                                                   "urn:ietf:params:oauth:grant-type:device_code"
                                                   "refresh_token")
                             "response_types" (vector "code")
                             "token_endpoint_auth_method" "none"
                             "scope" +scope+)))
    (let ((client-id (and (hash-table-p json) (nlk:json-value json :string "client_id"))))
      (unless (and client-id (< (or status 500) 300))
        (fail "the sign-in could not register a client: ~a" (refusal-line json status)))
      client-id)))

(defun verification-url-p (value origin user-code)
  "Whether VALUE is ORIGIN's own page and carries the code: nowhere else is
the operator sent to approve a token."
  (let ((uri (and (nlk:trimmed (or value "")) (ignore-errors (quri:uri value)))))
    (and uri
         (string-equal (or (quri:uri-scheme uri) "") (quri:uri-scheme (quri:uri origin)))
         (string-equal (or (quri:uri-host uri) "") (quri:uri-host (quri:uri origin)))
         (or (search (string-downcase user-code) (string-downcase (or (quri:uri-path uri) "")))
             (search (string-downcase user-code) (string-downcase (or (quri:uri-query uri) "")))))))

(defun begin-signin ()
  "Ask for a device code, start polling, and answer the words the operator reads."
  (let* ((client-id (register-client))
         (origin (origin-text)))
    (multiple-value-bind (json status)
        (oauth-post "/oauth/device_authorization"
                    (list (cons "client_id" client-id)
                          (cons "scope" +scope+)))
      (let ((device-code (and (hash-table-p json) (nlk:json-value json :string "device_code")))
            (user-code (and (hash-table-p json) (nlk:json-value json :string "user_code")))
            (verification (and (hash-table-p json)
                               (or (nlk:json-value json :string "verification_uri_complete")
                                   (nlk:json-value json :string "verification_uri"))))
            (expires-in (and (hash-table-p json) (nlk:json-value json :number "expires_in"))))
        (unless (and device-code user-code verification (integerp expires-in) (plusp expires-in))
          (fail "the sign-in answered without a code to type: ~a" (refusal-line json status)))
        (unless (verification-url-p verification origin user-code)
          (fail "the sign-in's page is not the product's own: ~s" verification))
        (setf *pending* (make-pending :device-code device-code
                                       :user-code user-code
                                       :verification-url verification
                                       :client-id client-id
                                       :origin origin
                                       :interval (max 1 (or (nlk:json-value json :number "interval") 5))
                                       :deadline (+ (unix-seconds) expires-in)))
        (start-poller)
        (format nil "Open ~a~%and enter the code ~a (it expires in ~d minutes).~%This cell polls until you approve; /super-artifacts status says whether it landed, and /super-artifacts cancel stops waiting."
                verification user-code (ceiling expires-in 60))))))

(defun start-poller ()
  (stop-poller)
  (setf *worker* (nlk:worker-start "super-artifacts-signin" #'poll-attempt
                                    :wake (lambda ()
                                            (if *pending* (pending-interval *pending*) 5))
                                    :prime t)))

(defun stop-poller ()
  (when *worker*
    (setf *worker* (nlk:worker-stop *worker*))))

(defun end-signin (text &key (level :info))
  (setf *pending* nil)
  (when text (say text :level level)))

(defun poll-attempt ()
  "One poll of the token endpoint. Answering :STOP ends the worker."
  (let ((pending *pending*))
    (cond
      ((null pending) :stop)
      ((>= (unix-seconds) (pending-deadline pending))
       (end-signin "super-artifacts: the sign-in code expired; run /super-artifacts signin again"
                   :level :warning)
       :stop)
      (t
       (multiple-value-bind (json status)
           (handler-case
               (oauth-post "/oauth/token"
                           (list (cons "grant_type" "urn:ietf:params:oauth:grant-type:device_code")
                                 (cons "device_code" (pending-device-code pending))
                                 (cons "client_id" (pending-client-id pending))))
             (super-artifacts-error (condition)
               (when (zerop (pending-failures pending))
                 (say (format nil "super-artifacts: waiting, but the sign-in did not answer: ~a" condition)
                      :level :warning))
               (incf (pending-failures pending))
               (values nil nil)))
         (let ((error (and (hash-table-p json) (nlk:json-value json :string "error"))))
           (cond
             ((equal error "authorization_pending") nil)
             ((equal error "slow_down") (incf (pending-interval pending) 5) nil)
             ((member error '("expired_token" "authorization_expired") :test #'equal)
              (end-signin "super-artifacts: the sign-in code expired; run /super-artifacts signin again"
                          :level :warning)
              :stop)
             ((member error '("access_denied" "denied") :test #'equal)
              (end-signin "super-artifacts: the sign-in was declined" :level :warning)
              :stop)
             ((or error (not (and (integerp status) (< status 300))))
              (end-signin (format nil "super-artifacts: the sign-in failed: ~a" (refusal-line json status))
                          :level :error)
              :stop)
             (t (finish-signin pending json) :stop))))))))

(defun finish-signin (pending json)
  "Keep the token the approval bought, and say where it went."
  (let ((access (nlk:json-value json :string "access_token")))
    (unless access (fail "the approved sign-in answered without an access token"))
    (save-token access
                :refresh (nlk:json-value json :string "refresh_token")
                :client-id (pending-client-id pending)
                :expires-in (nlk:json-value json :integer "expires_in")
                :origin (pending-origin pending))
    (end-signin (format nil "super-artifacts: signed in; the token is kept in ~a (0600)"
                        (namestring (token-path))))
    (format nil "Signed in. The token is kept in ~a (0600) and is used from now on.~%Try super_whoami."
            (namestring (token-path)))))

(defun cancel-signin ()
  "Stop waiting for an approval."
  (let ((waiting (and *pending* t)))
    (setf *pending* nil)
    (stop-poller)
    (if waiting "Stopped waiting for the approval; /super-artifacts signin starts again."
        "Nothing was waiting for an approval.")))

;;; --- what /super-artifacts says ---------------------------------------------------

(define-verb status-line ()
  "What this cell is pointed at, what it signs in with, and what is still missing."
  (let* ((endpoint (ignore-errors (endpoint-text)))
         (key (ignore-errors (bearer-token)))
         (saved (saved-token))
         (source (cond ((key-from-file) (format nil "key_file ~a" (setting :key-file)))
                       ((key-from-environment) (format nil "the variable ~a" (or (setting :key-env) +key-variable+)))
                       (saved (format nil "a signed-in token in ~a" (namestring (token-path))))
                       (t "none"))))
    (with-output-to-string (out)
      (format out "Super Artifacts: ~a~%" (if (and endpoint key) "ready" "not ready yet"))
      (format out "  server: ~a~%" (or endpoint "none -- base_url is not an https URL"))
      (format out "  key:    ~a~%" (if key source
                                       (format nil "none -- run /super-artifacts signin, or set the variable ~a"
                                               (or (nlk:trimmed (or (setting :key-env) "")) +key-variable+))))
      (format out "  tools:  super_whoami, super_publish, super_data, super_artifacts, super_tool~%")
      (when *pending*
        (format out "  waiting: open ~a and enter ~a~%"
                (pending-verification-url *pending*) (pending-user-code *pending*))))))
