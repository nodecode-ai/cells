;;;; signin.lisp --- the GitLab sign-in for the Duo Agent Platform: its own record, its own copy.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/gitlab-duo-agent.kdl (the login and refresh rules), ai/src/registry/
;;;; engine/oauth-code.ts, refresh.ts and common.ts (the engine those rules
;;;; drive), ai/src/registry/oauth/callback-server.ts (the pasted-code parse)
;;;; and oauth/pkce.ts. This is its own sign-in, kept apart from
;;;; gitlab-duo's: cells share no code.
;;;;
;;;; GitLab's authorization-code flow with PKCE, scope api, through GitLab's
;;;; official VS Code OAuth application, whose redirect is VS Code's own
;;;; scheme, vscode://gitlab.gitlab-workflow/authentication. Nothing here can
;;;; receive that, so the flow is paste-only: the operator copies the
;;;; vscode://... address the browser (or VS Code) is sent to and pastes it.
;;;; GITLAB_TOKEN skips the sign-in for a personal access token.
;;;;
;;;; Kept in auth.json under oauth_tokens.gitlab-duo-agent:
;;;;   {"access_token", "refresh_token", "expires_at"}
;;;; expires_at in epoch seconds, five minutes inside what GitLab granted
;;;; (omp's skew); the credential refreshes the token when it is within a
;;;; minute of that, sending the redirect again as this application's refresh
;;;; requires, and writes the new one back.

(in-package #:nodecode-gitlab-duo-agent)

(defparameter +client-id+ "36f2a70cddeb5a0889d4fd8295c241b7e9848e89cf9e599d0eed2d8e5350fbf5"
  "GitLab's VS Code extension OAuth application.")

(defparameter +redirect-uri+ "vscode://gitlab.gitlab-workflow/authentication"
  "The application's registered redirect: VS Code's scheme.")

(defparameter +scope+ "api")

(defparameter +login-seconds+ 300
  "How long a sign-in waits for its code: omp's callback timeout.")

(defparameter +skew-seconds+ 300
  "How far inside GitLab's grant the kept expiry is: omp's skew-ms.")

(defparameter +refresh-margin-seconds+ 60
  "How close to its expiry a token is refreshed before it is sent.")

;;; --- words and bytes ------------------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ (cl-base64:usb8-array-to-base64-string octets)))))

(defun sha256-octets (text)
  "The SHA-256 of TEXT's UTF-8 bytes, as 32 octets."
  (let ((hex (subseq (nlk:sha256-text text) 7))
        (out (make-array 32 :element-type '(unsigned-byte 8))))
    (dotimes (i 32 out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun pkce ()
  "(values VERIFIER CHALLENGE): 96 random bytes as base64url, and its S256
challenge. omp's generatePKCE."
  (let ((verifier (base64url (nlk:random-bytes 96))))
    (values verifier (base64url (sha256-octets verifier)))))

(defun random-hex (count)
  "COUNT random bytes as lowercase hex: omp's default state."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes count) 'list)))

(defun form-decode (text)
  "TEXT, form-encoded, decoded: + is a space, %XX a byte."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length text))
          do (let ((char (char text i)))
               (cond ((char= char #\+) (vector-push-extend 32 octets) (incf i))
                     ((and (char= char #\%) (<= (+ i 3) (length text))
                           (digit-char-p (char text (+ i 1)) 16) (digit-char-p (char text (+ i 2)) 16))
                      (vector-push-extend (parse-integer text :start (1+ i) :end (+ i 3) :radix 16) octets)
                      (incf i 3))
                     (t (loop for byte across (sb-ext:string-to-octets (string char) :external-format :utf-8)
                              do (vector-push-extend byte octets))
                        (incf i)))))
    (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defun query-params (text)
  "The name/value alist of TEXT, a query string with or without its ? or #."
  (loop for pair in (uiop:split-string (string-left-trim "?#" text) :separator "&")
        for equals = (position #\= pair)
        when (plusp (length pair))
          collect (cons (form-decode (subseq pair 0 equals))
                        (if equals (form-decode (subseq pair (1+ equals))) ""))))

(defun param (params name)
  "NAME's value in PARAMS, the first one, or NIL."
  (cdr (assoc name params :test #'string=)))

(defun url-p (text)
  "Whether TEXT reads as an absolute URL: a scheme, then a colon."
  (let ((colon (position #\: text)))
    (and colon (plusp colon) (alpha-char-p (char text 0))
         (every (lambda (char) (or (alphanumericp char) (find char "+.-"))) (subseq text 0 colon)))))

(defun url-query (url)
  "The query of URL, between its ? and its #, or \"\"."
  (let* ((hash (position #\# url))
         (question (position #\? url :end hash)))
    (if question (subseq url (1+ question) hash) "")))

(defun parse-callback-input (input)
  "(values CODE STATE) out of what the operator pasted: the redirect address,
a query string, or the bare code with its state after a #. omp's
parseCallbackInput."
  (let ((value (nlk:trimmed (or input ""))))
    (cond ((zerop (length value)) (values nil nil))
          ((url-p value)
           (let ((params (query-params (url-query value))))
             (values (param params "code") (param params "state"))))
          ((search "code=" value)
           (let ((params (query-params value)))
             (values (param params "code") (param params "state"))))
          (t (let* ((hash (position #\# value))
                    (next-hash (and hash (position #\# value :start (1+ hash)))))
               (if hash
                   (values (subseq value 0 hash) (subseq value (1+ hash) next-hash))
                   (values value nil)))))))

(defun excerpt (text)
  "TEXT's first 500 characters, as omp quotes a body."
  (subseq text 0 (min 500 (length text))))

;;; --- the token endpoint ---------------------------------------------------------------

(defun now ()
  "The time, in epoch seconds."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun token-entry (body previous what)
  "The oauth_tokens entry GitLab's token BODY grants: omp's credential map
(access_token, refresh_token kept from PREVIOUS when not rotated, expires_in
counted from created_at, less the skew). WHAT names the request in a refusal."
  (let ((access (nlk:json-value body :text "access_token"))
        (seconds (nlk:json-value body :number "expires_in"))
        (from (nlk:json-value body :number "created_at")))
    (unless access
      (error "gitlab-duo-agent ~a response missing access token: ~a" what
             (excerpt (if body (nlk:encode-json-object body) ""))))
    (unless seconds
      (error "gitlab-duo-agent ~a response missing expires_in" what))
    (nlk:json-object "access_token" access
                     "refresh_token" (or (nlk:json-value body :text "refresh_token")
                                         (nlk:json-value previous :text "refresh_token")
                                         "")
                     "expires_at" (- (+ (round (or from (now))) (round seconds)) +skew-seconds+))))

(defun token-request (pairs what)
  "POST the form PAIRS to GitLab's token endpoint; => the decoded answer."
  (multiple-value-bind (text status)
      (handler-case
          (multiple-value-bind (body status)
              (dex:post (format nil "~a/oauth/token" (string-right-trim "/" (setting :gitlab-url)))
                        :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                        :content (query-string pairs)
                        :connect-timeout 30 :read-timeout 30)
            (values (body-string body) status))
        (dex:http-request-failed (condition)
          (values (body-string (dex:response-body condition)) (dex:response-status condition))))
    (unless (ok-p status)
      (error "gitlab-duo-agent ~a failed: ~a ~a" what status (excerpt text)))
    (and (plusp (length text)) (ignore-errors (nlk:decode-json text)))))

(defun exchange-code (code verifier)
  "The oauth_tokens entry CODE signs in to. A `code#state' paste is split."
  (let ((code (subseq code 0 (position #\# code))))
    (token-entry (token-request `(("grant_type" . "authorization_code")
                                  ("client_id" . ,+client-id+)
                                  ("code" . ,code)
                                  ("redirect_uri" . ,+redirect-uri+)
                                  ("code_verifier" . ,verifier))
                                "token exchange")
                 nil "token exchange")))

(defun refresh-entry (entry)
  "ENTRY refreshed at GitLab's token endpoint, the redirect sent again."
  (token-entry (token-request `(("grant_type" . "refresh_token")
                                ("client_id" . ,+client-id+)
                                ("refresh_token" . ,(nlk:json-value entry :string "refresh_token"))
                                ("redirect_uri" . ,+redirect-uri+))
                              "token refresh")
               entry "token refresh"))

;;; --- where the token is kept ----------------------------------------------------------

(defvar *store-lock* (bt2:make-recursive-lock :name "nodecode-gitlab-duo-agent auth.json")
  "Held across one read-modify-write of auth.json, and across a refresh.")

(defun stored-entry (auth)
  "oauth_tokens.gitlab-duo-agent of the parsed AUTH, or NIL."
  (nlk:json-value auth :object "oauth_tokens" +provider+))

(defun save-entry (entry &optional (auth-path nle::*auth-file-path*))
  "Make ENTRY oauth_tokens.gitlab-duo-agent in the auth.json at AUTH-PATH, or
take it out when ENTRY is NIL, every other field kept: the way
NLE::SAVE-PROVIDER-API-KEY writes api_keys."
  (bt2:with-recursive-lock-held (*store-lock*)
    (let* ((path (merge-pathnames auth-path))
           (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
           (tokens (or (nlk:json-value auth :object "oauth_tokens")
                       (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
      (if entry
          (setf (gethash +provider+ tokens) entry)
          (remhash +provider+ tokens))
      (nlk:write-file-atomically path (shasht:write-json auth nil)
                                 :mode #o600 :directory-mode #o700)
      entry)))

(defun fresh-token (entry auth-path)
  "ENTRY's access token, refreshed first, and written back to AUTH-PATH, when
it expires within a minute; a failed refresh signals, saying to sign in again."
  (let ((expires (nlk:json-value entry :number "expires_at")))
    (if (or (null expires) (> expires (+ (now) +refresh-margin-seconds+)))
        (nlk:json-value entry :text "access_token")
        (bt2:with-recursive-lock-held (*store-lock*)
          (let* ((current (or (stored-entry (ignore-errors (nle::read-auth-file auth-path))) entry))
                 (expires (nlk:json-value current :number "expires_at")))
            (if (and expires (> expires (+ (now) +refresh-margin-seconds+)))
                (nlk:json-value current :text "access_token")
                (handler-case
                    (nlk:json-value (save-entry (refresh-entry current) auth-path) :text "access_token")
                  (error (condition)
                    ;; a state the model should keep seeing until a sign-in
                    ;; clears it: every round on this provider fails until then
                    (nle:notice "gitlab-duo-agent: the GitLab sign-in has expired and could not be refreshed; sign in again with /gitlab-duo-agent login"
                                :level :warning :key +key+)
                    (error 'nle::credential-error
                           :detail (format nil "the GitLab Duo Agent sign-in could not be refreshed (~a): sign in again with /gitlab-duo-agent login"
                                           condition))))))))))

(defun credential (op next)
  "The :CREDENTIAL answer for gitlab-duo-agent: the signed-in token,
refreshed when it is about to expire, else GITLAB_TOKEN. Nothing else: the
env ladder's family default is another provider's key."
  ;; A probe (no endpoint) asks where the credential comes from: it answers
  ;; the token as stored and refreshes nothing.
  (if (equal (getf op :provider) +provider+)
      (let ((entry (stored-entry (getf op :auth))))
        (cond ((and entry (nlk:json-value entry :text "access_token"))
               (nle:make-credential (if (getf op :endpoint)
                                        (fresh-token entry (or (getf op :auth-path) nle::*auth-file-path*))
                                        (nlk:json-value entry :text "access_token"))
                                    :oauth))
              ((env-key) (nle:make-credential (env-key) :env))
              (t (nle:make-credential "public" :public))))
      (funcall next op)))

(defun signed-in-token (&optional (auth-path nle::*auth-file-path*))
  "The GitLab token a round would send, or NIL: for discovery outside a round."
  (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :auth-path auth-path))))
    (and credential (not (eq :public (nle:credential-source credential))) (nle:credential-key credential))))

;;; --- the models the account offers ----------------------------------------------------

(defun discover (token &key announce)
  "Find the models TOKEN's namespace offers, on a thread of its own, and list
them; ANNOUNCE says what was found."
  (let ((base (setting :gitlab-url))
        (namespace-id (configured :namespace-id "GITLAB_DUO_NAMESPACE_ID"))
        (project (configured :project "GITLAB_DUO_PROJECT_ID" "GITLAB_DUO_PROJECT_PATH")))
    (bt2:make-thread
     (lambda ()
       (handler-case
           (let ((refs (discover-models base token :namespace-id namespace-id :project project)))
             (when refs
               (setf *discovered* refs)
               (forget-catalog))
             (when announce
               (nle:notice (if refs
                               (format nil "gitlab-duo-agent: ~d model~:p: ~{~a~^, ~}" (length refs) (mapcar #'car refs))
                               "gitlab-duo-agent: no namespace offered any model; set namespace_id in the gitlab-duo-agent section")
                           :level (if refs :info :warning))))
         (serious-condition (condition)
           (when announce
             (nle:notice (format nil "gitlab-duo-agent: model discovery failed: ~a" condition)
                         :level :warning)))))
     :name "nodecode-gitlab-duo-agent models")))

;;; --- the sign-in in flight -------------------------------------------------------------

(defstruct (flow (:constructor make-flow (state verifier challenge auth-path url)))
  "One sign-in waiting for its code."
  state verifier challenge auth-path url
  (mailbox (sb-concurrency:make-mailbox :name "nodecode-gitlab-duo-agent sign-in"))
  (thread nil))

(defvar *flow* nil "The sign-in waiting for its code, or NIL.")

(defvar *flow-lock* (bt2:make-lock :name "nodecode-gitlab-duo-agent sign-in"))

(defun authorize-url (state challenge)
  "The address the operator opens: GitLab's standard authorize request with PKCE."
  (format nil "~a/oauth/authorize?~a" (string-right-trim "/" (setting :gitlab-url))
          (query-string `(("client_id" . ,+client-id+)
                          ("response_type" . "code")
                          ("redirect_uri" . ,+redirect-uri+)
                          ("scope" . ,+scope+)
                          ("code_challenge" . ,challenge)
                          ("code_challenge_method" . "S256")
                          ("state" . ,state)))))

(defun say (text level)
  "TEXT to the operator, said once: an outcome is news, not a standing state."
  (nle:notice text :level level))

(defun finish-flow (flow)
  "Forget FLOW when it is still the one in flight."
  (bt2:with-lock-held (*flow-lock*)
    (when (eq *flow* flow) (setf *flow* nil))))

(defun run-flow (flow)
  "Wait for FLOW's code, trade it for a token, keep the token, say how it
went, and find the models the account offers."
  (unwind-protect
       (handler-case
           (let ((message (sb-concurrency:receive-message (flow-mailbox flow) :timeout +login-seconds+)))
             (case (first message)
               ((nil) (say (format nil "gitlab-duo-agent: the sign-in timed out after ~d minutes with no code; start again with /gitlab-duo-agent login"
                                   (floor +login-seconds+ 60))
                           :warning))
               (:code (let ((entry (exchange-code (second message) (flow-verifier flow))))
                        (save-entry entry (flow-auth-path flow))
                        (nle:notice nil :key +key+)
                        (say "gitlab-duo-agent: signed in to GitLab; pick a Duo Agent model with /models" :info)
                        (discover (nlk:json-value entry :string "access_token"))))))
         (serious-condition (condition)
           (say (format nil "gitlab-duo-agent: the sign-in failed: ~a" condition) :warning)))
    (finish-flow flow)))

(defun cancel-flow ()
  "End the sign-in in flight, if any, saying nothing."
  (let ((flow (bt2:with-lock-held (*flow-lock*) (shiftf *flow* nil))))
    (when flow
      (sb-concurrency:send-message (flow-mailbox flow) (list :cancel)))))

(defun start-login (&optional (auth-path nle::*auth-file-path*))
  "Start a sign-in that keeps its token at AUTH-PATH; => what the operator does next."
  (cancel-flow)
  (multiple-value-bind (verifier challenge) (pkce)
    (let* ((state (random-hex 16))
           (flow (make-flow state verifier challenge auth-path (authorize-url state challenge))))
      (bt2:with-lock-held (*flow-lock*) (setf *flow* flow))
      (setf (flow-thread flow)
            (bt2:make-thread (lambda () (run-flow flow)) :name "nodecode-gitlab-duo-agent sign-in"))
      (format nil "Open this address and sign in to GitLab:~%~a~%~%This is GitLab's VS Code application: GitLab then sends the browser to ~a?code=... (VS Code may open). Copy that whole address and paste it here as~%  /gitlab-duo-agent code <address or code>~%Or set GITLAB_TOKEN to a personal access token (scope api). The sign-in waits ~d minutes."
              (flow-url flow) +redirect-uri+ (floor +login-seconds+ 60)))))

(defun paste (text)
  "Hand the sign-in in flight the code TEXT carries; => what happened."
  (let ((flow *flow*))
    (if (null flow)
        "No sign-in is waiting for a code: start one with /gitlab-duo-agent login."
        (multiple-value-bind (code state) (parse-callback-input text)
          (cond ((null code)
                 "That carries no code: paste the whole vscode://gitlab.gitlab-workflow/authentication?... address, or the code in it.")
                ((and state (plusp (length state)) (string/= state (flow-state flow)))
                 "That address belongs to another sign-in (its state differs): paste the one this sign-in's page sent.")
                (t (sb-concurrency:send-message (flow-mailbox flow) (list :code code (or state "")))
                   "Code received: finishing the sign-in. The outcome follows as a notice."))))))

(defun status ()
  "Whether GitLab Duo Agent is signed in, and which models it offers."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file nle::*auth-file-path*)))))
    (format nil "~a~@[ Models: ~{~a~^, ~}.~]"
            (cond (*flow* "A sign-in is waiting for its code: paste it with /gitlab-duo-agent code <address or code>.")
                  ((nlk:json-value entry :text "access_token") "Signed in to GitLab.")
                  ((env-key) "Not signed in; GITLAB_TOKEN is set and is used.")
                  (t "Not signed in: /gitlab-duo-agent login, or set GITLAB_TOKEN."))
            (mapcar #'car *discovered*))))

(defun logout ()
  "Forget the GitLab token and every workflow waiting on it."
  (cancel-flow)
  (save-entry nil)
  (end-sessions)
  "Signed out: the GitLab token is gone from auth.json.")

(defun models ()
  "Find the models the account offers again."
  (let ((token (signed-in-token)))
    (if token
        (progn (discover token :announce t)
               "Looking for the models your GitLab namespace offers; the list follows as a notice.")
        "Not signed in: /gitlab-duo-agent login, or set GITLAB_TOKEN.")))

(defun run-command (args session-id)
  "/gitlab-duo-agent login | code TEXT | models | logout | status."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (subseq text 0 space))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((member verb '("" "status") :test #'string-equal) (status))
          ((string-equal verb "login") (start-login))
          ((string-equal verb "code") (paste rest))
          ((string-equal verb "models") (models))
          ((string-equal verb "logout") (logout))
          (t "Usage: /gitlab-duo-agent login | code <address or code> | models | logout | status"))))
