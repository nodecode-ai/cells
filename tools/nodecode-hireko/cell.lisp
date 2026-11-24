;;;; cell.lisp --- Hireko's recruiting tools, as seven tools and /hireko.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Hireko's Hermes MCP gateway (see NOTICE). The plugin was a
;;;; Node stdio MCP server: it signed in to Hireko's AWS Cognito pool and
;;;; proxied tool calls to Hireko's hosted MCP facade. Here the same calls
;;;; are made with NLK:HTTP, the tools are registered by the cell itself,
;;;; and the session is a JSON file of the home instead of the plugin's.
;;;;
;;;; The wire, unchanged: the tenant's configuration page says which Cognito
;;;; client and region answer for it, Cognito's InitiateAuth takes the
;;;; recruiter's email and password, and every tool call is one JSON-RPC
;;;; tools/call to the facade carrying the bearer token and the tenant.
;;;; The tool names are the original's, prefixed with the cell, so two
;;;; cells never collide in one image.

(defpackage #:nodecode-hireko
  (:use #:cl)
  (:export #:hireko-error #:hireko-error-detail))

(in-package #:nodecode-hireko)

(nlk:define-peripheral hireko :not-running t)

;;; --- what the original hard-codes ---------------------------------------------

(defparameter +client-id+ "7of3lvicphr935gjj4r74f9n87"
  "The Cognito app client the original falls back to when a tenant's
configuration page gives none.")

(defparameter +region+ "us-east-1"
  "The Cognito region, and the region every facade call names, when nothing
else says.")

(defparameter +candidate-settings+
  "eyJyZW5kZXJJRCI6ICJidWJibGUiLCAiaXNTY3JlZW5TaGFyZU1hbmRhdG9yeSI6IGZhbHNlfQ=="
  "The candidate page's settings query, as the original builds every
interview link: {\"renderID\": \"bubble\", \"isScreenShareMandatory\": false}.")

(defparameter +token-lead-seconds+ 60
  "How long before its expiry a saved token stops counting as one.")

(defparameter +detail-chars+ 400
  "How much of a failed answer's text a refusal repeats.")

(defparameter +primer+
  "Hireko is a recruiting platform: this cell signs in as a recruiter and reaches its
interview tools. All seven tools send their arguments to Hireko's hosted endpoint.
  hireko_auth_status {tenant?}          whether a session is saved, and for how long
  hireko_login {tenant?}                sign in with HIREKO_USERNAME and HIREKO_PASSWORD
  hireko_logout {tenant?}               forget the saved session
  hireko_create_interview_link {title, description?, company_name?, location?,
      interview_length?, start_date?, end_date?, company_culture?,
      employment_type?, language?, recruiter_email?, wait_seconds?}
      start a candidate screening interview and answer its link, polling for up to
      wait_seconds (150 by default) while Hireko writes the questions
  hireko_get_interview_link {external_job_group_id, wait_seconds?}
      keep polling one that was still generating
  hireko_get_search_score {job_id}      candidates' match scores for a job
  hireko_health {tenant?}               whether Hireko's own services answer
Ask for one question at a time when you collect an interview's details: title, duration,
location, company and dates are the recruiter's to give, never reuse an earlier job's, and
the job description can be written from the title. tenant and stage are the cell's
settings; a call names another tenant only when the recruiter does. ERROR: HIREKO-ERROR
names what went wrong; when it says signed out, tell the operator to run /hireko login."
  "What (help :hireko) answers while the cell runs.")

;;; --- small readers ----------------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun tenant-p (slug)
  "Whether SLUG reads as a Hireko tenant: lowercase letters, digits and dashes, at most 63."
  (and (stringp slug)
       (<= 1 (length slug) 63)
       (every (lambda (c)
                (or (and (< (char-code c) 128) (alphanumericp c)) (char= c #\-)))
              slug)))

(defun tenant-slug (raw)
  "RAW as a tenant slug."
  (let ((slug (string-downcase (nlk:trimmed (or raw "")))))
    (unless (tenant-p slug)
      (fail "~s is no Hireko tenant: lowercase letters, digits and dashes, at most 63" raw))
    slug))

(defun stage-word (raw)
  "RAW as the deploy stage a call names: prod, else beta."
  (if (string-equal (or raw "") "prod") "prod" "beta"))

(defun recruiter-host (tenant stage)
  "The host TENANT's recruiter site answers on, where its configuration page lives."
  (format nil "recruiter-~a~a.hireko.ai" tenant (if (string= stage "prod") "" ".beta")))

(defun interview-host (stage)
  "The host a candidate's interview session page is served from."
  (format nil "interview-test~a.hireko.ai" (if (string= stage "prod") "" (format nil ".~a" stage))))

(defun epoch-ms ()
  "Now, in epoch milliseconds, the unit Hireko's dates use."
  (* 1000 (- (get-universal-time) 2208988800)))

;;; --- the session, one JSON file a tenant under the home ----------------------------

(defun session-path (tenant)
  "Where TENANT's session is kept: in the home, readable by its owner only."
  (nlk:home (format nil "hireko/~a.json" tenant)))

(defun saved-session (tenant)
  "TENANT's saved session, decoded; NIL when there is none."
  (let ((path (session-path tenant)))
    (and (probe-file path) (ignore-errors (nlk:decode-json (uiop:read-file-string path))))))

(defun keep-session (tenant session)
  "Write SESSION for TENANT at 0600 => SESSION."
  (nlk:write-file-atomically (session-path tenant) (nlk:encode-json-object session)
                              :mode #o600 :directory-mode #o700)
  session)

(defun forget-session (tenant)
  "Delete TENANT's saved session => T when there was one."
  (let ((path (session-path tenant)))
    (if (probe-file path)
        (progn (ignore-errors (delete-file path)) t)
        nil)))

(defun session-token (session)
  "SESSION's access token, when it has one."
  (and session (nonblank (nlk:json-value session :string "access_token"))))

(defun session-username (session)
  "SESSION's recruiter, as the email it signed in with."
  (and session (nonblank (nlk:json-value session :string "username"))))

(defun fresh-session-p (session)
  "Whether SESSION's token has more than a minute left."
  (let ((expires (and session (nlk:json-value session :integer "expires_at"))))
    (and expires (> expires (+ (get-universal-time) +token-lead-seconds+)))))

;;; --- one POST, decoded ------------------------------------------------------------

(defun post-json (url body headers what)
  "POST BODY, a JSON object, to URL with HEADERS => the decoded answer; a
status outside 2xx is a refusal naming WHAT."
  (let ((answer nil) (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post url
                    :headers (cons (cons "accept" "application/json") headers)
                    :content (nlk:encode-json-object body)
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      (error (condition)
        (fail "~a did not answer: ~a" what (nlk:one-line (princ-to-string condition)))))
    (let ((decoded (or (ignore-errors (nlk:decode-json answer)) (nlk:json-object))))
      (unless (and (integerp status) (< status 300))
        (fail "~a answered HTTP ~a~@[: ~a~]" what status
              (or (nonblank (nlk:json-value decoded :string "message"))
                  (nonblank (nlk:json-value decoded :string "error"))
                  (and (stringp answer) (nonblank (nlk:clip (nlk:one-line answer) +detail-chars+))))))
      decoded)))

;;; --- sign-in: the tenant's page, then Cognito -------------------------------------

(defun tenant-page (tenant stage)
  "TENANT's configuration page, decoded; an empty object when it does not answer."
  (let ((url (format nil "https://~a/assets/Json/tenant_configuration.json"
                      (recruiter-host tenant stage))))
    (handler-case
        (multiple-value-bind (answer status)
            (nlk:http :get url :timeout 5 :connect-timeout 5)
          (if (and (integerp status) (< status 300))
              (or (ignore-errors (nlk:decode-json answer)) (nlk:json-object))
              (nlk:json-object)))
      (error () (nlk:json-object)))))

(defun pool-region (pool)
  "POOL's region: what its identifier says before the underscore, when that
reads like a region."
  (let ((cut (and (stringp pool) (position #\_ pool))))
    (when (and cut (> cut 0))
      (let ((region (subseq pool 0 cut)))
        (when (every (lambda (c)
                       (or (and (< (char-code c) 128) (alphanumericp c)) (char= c #\-)))
                     region)
          region)))))

(defun tenant-cognito (tenant stage)
  "=> (values CLIENT-ID REGION) for TENANT: the section's, else its page's,
else the original's defaults."
  (let ((client (nonblank (setting :client-id)))
        (region (nonblank (setting :region))))
    (if (and client region)
        (values client region)
        (let ((page (tenant-page tenant stage)))
          (values (or client (nonblank (nlk:json-value page :string "cognito" "cognito_client_id"))
                      +client-id+)
                  (or region (pool-region (nlk:json-value page :string "cognito" "cognito_user_pool_id"))
                      +region+))))))

(defun sign-in (tenant stage)
  "Cognito password sign-in for TENANT => the session, saved. The password is
the section's environment, never a tool argument."
  (let ((username (nonblank (setting :username)))
        (password (nonblank (setting :password))))
    (unless (and username password)
      (fail "Hireko is signed out for tenant ~s and there is no password to sign in with: put the recruiter's email and password in HIREKO_USERNAME and HIREKO_PASSWORD (or in the variables the section's username_env and password_env name), then run /hireko login"
            tenant))
    (multiple-value-bind (client region) (tenant-cognito tenant stage)
      (let* ((url (format nil "https://cognito-idp.~a.amazonaws.com/" region))
             (body (nlk:json-object "AuthFlow" "USER_PASSWORD_AUTH"
                                     "ClientId" client
                                     "AuthParameters" (nlk:json-object "USERNAME" username
                                                                        "PASSWORD" password)))
             (answer (post-json url body
                                (list (cons "content-type" "application/x-amz-json-1.1")
                                      (cons "x-amz-target"
                                            "AWSCognitoIdentityProviderService.InitiateAuth"))
                                "Cognito"))
             (result (nlk:json-value answer :object "AuthenticationResult")))
        (unless (and result (nonblank (nlk:json-value result :string "AccessToken")))
          (fail "Cognito refused ~s: ~a" username
                (or (nonblank (nlk:json-value answer :string "message"))
                    (nonblank (nlk:json-value answer :string "__type"))
                    "its answer carried no access token")))
        (keep-session tenant
                      (nlk:json-object "tenant" tenant
                                       "stage" stage
                                       "username" username
                                       "access_token" (nlk:json-value result :string "AccessToken")
                                       "id_token" (or (nlk:json-value result :string "IdToken") "")
                                       "refresh_token" (or (nlk:json-value result :string "RefreshToken") "")
                                       "expires_at" (+ (get-universal-time)
                                                        (or (nlk:json-value result :integer "ExpiresIn") 3600))))))))

(defun ensure-token (tenant stage)
  "A live access token for TENANT: the saved one, or a fresh sign-in."
  (let ((session (saved-session tenant)))
    (if (and (fresh-session-p session) (session-token session))
        (session-token session)
        (session-token (sign-in tenant stage)))))

;;; --- the facade ----------------------------------------------------------------

(defun first-block-text (result)
  "RESULT's first content block's text, when it carries one."
  (let ((blocks (nlk:json-array result "content")))
    (and blocks (plusp (length blocks))
         (nonblank (nlk:json-value (aref blocks 0) :string "text")))))

(defun result-object (result)
  "RESULT's payload as a decoded object: its structured content, else its
first text block read as JSON, else an empty object."
  (or (let ((structured (nlk:json-value result :object "structuredContent")))
        (and structured (plusp (hash-table-count structured)) structured))
      (let ((text (first-block-text result)))
        (and text (ignore-errors (nlk:decode-json text))))
      (nlk:json-object)))

(defun result-text (result)
  "RESULT as the text the model reads: its payload, one JSON object."
  (nlk:encode-json-object (result-object result)))

(defun facade-call (name arguments tenant stage)
  "One JSON-RPC tools/call to Hireko's hosted facade => its result object.
ARGUMENTS is a plist of string keys, as the original sends them."
  (let* ((token (ensure-token tenant stage))
         (params (apply #'nlk:make-json-object
                        (append arguments (list "tenant" tenant "stage" stage))))
         (body (nlk:json-object "jsonrpc" "2.0" "id" 1 "method" "tools/call"
                                 "params" (nlk:json-object "name" name "arguments" params)))
         (answer (post-json (setting :facade-url) body
                             (list (cons "authorization" (format nil "Bearer ~a" token))
                                   (cons "authorizationtenant" tenant)
                                   (cons "authorizationregion" +region+)
                                   (cons "x-tenant" tenant))
                             (format nil "Hireko's ~a" name)))
         (refusal (nlk:json-value answer :object "error"))
         (result (nlk:json-value answer :object "result")))
    (when refusal
      (fail "Hireko refused ~a: ~a" name
            (or (nonblank (nlk:json-value refusal :string "message")) "no reason given")))
    (unless result
      (fail "Hireko answered ~a with no result" name))
    (when (nlk:json-value result :boolean "isError")
      (fail "Hireko refused ~a: ~a" name (or (first-block-text result) "no reason given")))
    result))

;;; --- interview links -------------------------------------------------------------

(defun slug-of (text)
  "TEXT as a slug: lowercase letters, digits and single dashes."
  (let ((out '()) (dash nil))
    (loop for raw across (string-downcase (or text ""))
          for c = (and (< (char-code raw) 128) (alphanumericp raw) raw)
          do (if c
                 (progn (push c out) (setf dash nil))
                 (unless (or dash (null out)) (push #\- out) (setf dash t))))
    (string-trim "-" (coerce (reverse out) 'string))))

(defun iso-ms (text)
  "TEXT read as a YYYY-MM-DD date, in epoch milliseconds at UTC midnight; NIL
when it is no such date."
  (let* ((date (subseq text 0 (min (length text) 10)))
         (year (ignore-errors (parse-integer date :start 0 :end 4)))
         (month (and (= (length date) 10) (ignore-errors (parse-integer date :start 5 :end 7))))
         (day (and month (ignore-errors (parse-integer date :start 8 :end 10)))))
    (when (and year month day (char= (char date 4) #\-) (char= (char date 7) #\-)
               (<= 1970 year) (<= 1 month 12) (<= 1 day 31))
      (* 1000 (- (encode-universal-time 0 0 0 day month year 0) 2208988800)))))

(defun date-ms (text default)
  "TEXT as epoch milliseconds: digits as they are, a YYYY-MM-DD date at its
UTC midnight; DEFAULT when it reads as neither."
  (let ((text (nonblank (and (stringp text) (nlk:trimmed text)))))
    (cond ((null text) default)
          ((every #'digit-char-p text) (or (ignore-errors (parse-integer text)) default))
          (t (or (iso-ms text) default)))))

(defun candidate-link (job stage)
  "The candidate interview page for JOB on STAGE, with the settings query the
original appends to every one."
  (format nil "https://~a/anonymousInterviewSession/~a?settings=~a"
          (interview-host stage) job +candidate-settings+))

(defun link-of (answer stage)
  "=> (values LINK JOB) for ANSWER, a get_interview_link payload: the
candidate link it describes and the job id beside it, both NIL until the
session exists."
  (let* ((job (or (nonblank (nlk:json-value answer :string "jobId"))
                  (nonblank (nlk:json-value answer :string "job_id"))))
         (given (nonblank (nlk:json-value answer :string "interview_link"))))
    (when (and (null job) given)
      (let* ((marker "anonymousInterviewSession/")
             (start (search marker given)))
        (when start
          (let ((end (or (position #\? given :start (+ start (length marker))) (length given))))
            (setf job (subseq given (+ start (length marker)) end))))))
    (values (cond (given given)
                  ((and job (uiop:string-prefix-p "job_" job)) (candidate-link job stage))
                  (t nil))
            job)))

(defun poll-link (external tenant stage wait)
  "Ask Hireko for EXTERNAL's interview session until it has a link or WAIT
seconds pass => the completed answer text, else NIL."
  (let ((deadline (+ (get-universal-time) (max 1 wait)))
        (interval (setting :poll-interval-seconds)))
    (loop
      (let* ((result (ignore-errors
                       (facade-call "get_interview_link"
                                     (list "external_job_group_id" external) tenant stage)))
             (answer (and result (result-object result))))
        (multiple-value-bind (link job) (link-of answer stage)
          (when link
            (return (format nil "COMPLETED: the candidate interview link is ready.~%interview_link: ~a~@[~%job_id: ~a~]~%Share the link with the candidate."
                            link job)))))
      ;; an interval of 0 is one look, so a caller that does not want to wait
      ;; decides the deadline itself
      (when (or (null interval) (zerop interval) (>= (get-universal-time) deadline))
        (return nil))
      (sleep interval))))

(defun waiting-text (external wait)
  "EXTERNAL's interview, still generating."
  (format nil "STARTED: Hireko is still writing the interview questions~@[ (waited ~ds)~]. Call hireko_get_interview_link with external_job_group_id \"~a\" in a minute."
          wait external))

(defun recruiter-email (call tenant)
  "Who the interview is filed under: the call's, else the saved session's."
  (or (nonblank (hireko-interview-recruiter-email call))
      (session-username (saved-session tenant))
      "recruiter@hireko.ai"))

(defun interview-arguments (call tenant)
  "=> (values ARGUMENTS EXTERNAL): the facade arguments the original builds
for one interview, and the job group id it names."
  (let* ((title (hireko-interview-title call))
         (start (date-ms (hireko-interview-start-date call) (epoch-ms)))
         (end (date-ms (hireko-interview-end-date call) (+ start (* 7 24 60 60 1000))))
         (slug (let ((slug (slug-of title))) (if (plusp (length slug)) slug "interview")))
         (external (format nil "~a-~a-~d" slug tenant (epoch-ms))))
    (values (list "title" title
                  "description" (or (nonblank (hireko-interview-description call))
                                     (format nil "~a technical screening and candidate interview assessment" title))
                  "company_name" (or (nonblank (hireko-interview-company-name call)) "Hireko Partner")
                  "location" (or (nonblank (hireko-interview-location call)) "Remote")
                  "interview_length" (or (nonblank (hireko-interview-interview-length call)) "30")
                  "start_date" start
                  "end_date" end
                  "recruiter_email" (recruiter-email call tenant)
                  "company_culture" (or (hireko-interview-company-culture call) "")
                  "employment_type" (or (nonblank (hireko-interview-employment-type call)) "Full Time")
                  "language" (or (nonblank (hireko-interview-language call)) "en")
                  "status" "active"
                  "external_job_group_id" external
                  "funnel" "AllFunnels")
            external)))

;;; --- the seven tools -----------------------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (hireko-error (condition)
       (nle:failure (format nil "ERROR: ~a" (hireko-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Hireko: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun auth-status (call)
  "The hireko_auth_status tool: what session the home holds for the tenant."
  (running-settings)
  (answering
    (let* ((tenant (tenant-slug (or (nonblank (hireko-auth-tenant call)) (setting :tenant))))
           (stage (setting :stage))
           (session (saved-session tenant)))
      (if (and (fresh-session-p session) (session-token session))
          (format nil "Authenticated for tenant ~s (~a stage) as ~a; the token is good for ~d more minutes."
                  tenant stage (or (session-username session) "the saved recruiter")
                  (round (- (nlk:json-value session :integer "expires_at") (get-universal-time)) 60))
          (format nil "Signed out of tenant ~s (~a stage): run /hireko login, or set HIREKO_USERNAME and HIREKO_PASSWORD."
                  tenant stage)))))

(defun login-tool (call)
  "The hireko_login tool: password sign-in from the section's environment."
  (running-settings)
  (answering
    (let* ((tenant (tenant-slug (or (nonblank (hireko-login-tenant call)) (setting :tenant))))
           (session (sign-in tenant (setting :stage))))
      (format nil "Signed in to tenant ~s (~a stage) as ~a."
              tenant (setting :stage) (or (session-username session) "the recruiter")))))

(defun logout-tool (call)
  "The hireko_logout tool: forget the saved session."
  (running-settings)
  (answering
    (let ((tenant (tenant-slug (or (nonblank (hireko-logout-tenant call)) (setting :tenant)))))
      (if (forget-session tenant)
          (format nil "Signed out of tenant ~s: its saved session is gone." tenant)
          (format nil "Tenant ~s had no saved session." tenant)))))

(defun create-interview (call)
  "The hireko_create_interview_link tool: start one interview, poll for its link."
  (running-settings)
  (answering
    (let* ((tenant (setting :tenant))
           (stage (setting :stage)))
      (multiple-value-bind (arguments external) (interview-arguments call tenant)
        (let* ((answer (result-object (facade-call "create_interview_link" arguments tenant stage)))
               (actual (or (nonblank (nlk:json-value answer :string "data" "external_job_group_id"))
                           (nonblank (nlk:json-value answer :string "external_job_group_id"))
                           external))
               (wait (hireko-interview-wait-seconds call)))
          (if (and wait (plusp wait))
              (or (poll-link actual tenant stage wait) (waiting-text actual wait))
              (waiting-text actual nil)))))))

(defun get-interview-link (call)
  "The hireko_get_interview_link tool: keep polling one interview."
  (running-settings)
  (answering
    (let* ((external (hireko-link-external-job-group-id call))
           (wait (hireko-link-wait-seconds call)))
      (if (and wait (plusp wait))
          (or (poll-link external (setting :tenant) (setting :stage) wait)
              (waiting-text external wait))
          (waiting-text external nil)))))

(defun search-score (call)
  "The hireko_get_search_score tool: candidates' scores for one job."
  (running-settings)
  (answering
    (result-text (facade-call "get_search_score"
                               (list "job_id" (hireko-score-job-id call))
                               (setting :tenant) (setting :stage)))))

(defun health-tool (call)
  "The hireko_health tool: whether Hireko's own services answer."
  (running-settings)
  (answering
    (let ((tenant (tenant-slug (or (nonblank (hireko-health-tenant call)) (setting :tenant)))))
      (result-text (facade-call "health" '() tenant (setting :stage))))))

;;; --- /hireko -------------------------------------------------------------------

(defun status-text ()
  "What /hireko answers with nothing after it."
  (let* ((tenant (setting :tenant))
         (stage (setting :stage))
         (session (saved-session tenant)))
    (if (and (fresh-session-p session) (session-token session))
        (format nil "hireko: signed in for tenant ~s (~a stage) as ~a; good for ~d more minutes."
                tenant stage (or (session-username session) "the saved recruiter")
                (round (- (nlk:json-value session :integer "expires_at") (get-universal-time)) 60))
        (format nil "hireko: signed out of tenant ~s (~a stage); /hireko login reads HIREKO_USERNAME and HIREKO_PASSWORD."
                tenant stage))))

(defun run-command (args session-id)
  "/hireko [login|out]: the operator's way in, and out."
  (declare (ignore session-id))
  (running-settings)
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (handler-case
        (cond ((member word '("out" "logout" "sign-out" "signout") :test #'string=)
               (let ((tenant (setting :tenant)))
                 (if (forget-session tenant)
                     (format nil "hireko: signed out of tenant ~s." tenant)
                     (format nil "hireko: tenant ~s had no saved session." tenant))))
              ((member word '("in" "login" "sign-in" "signin") :test #'string=)
               (let ((session (sign-in (setting :tenant) (setting :stage))))
                 (format nil "hireko: signed in to tenant ~s as ~a."
                         (setting :tenant) (or (session-username session) "the recruiter"))))
              (t (status-text)))
      (hireko-error (condition) (format nil "ERROR: ~a" (hireko-error-detail condition)))
      (error (condition)
        (format nil "ERROR: Hireko: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the declaration -----------------------------------------------------------

(nlk:define-contract hireko-auth
  (:doc "Check the Hireko recruiter session this machine holds: the tenant, the stage and the minutes left on the token.")
  ("tenant" :string :doc "the tenant slug to report on; unset, the section's"))

(nlk:define-contract hireko-login
  (:doc "Sign in to Hireko as a recruiter with the email and password the cell's environment holds (HIREKO_USERNAME and HIREKO_PASSWORD), and save the session for the tenant.")
  ("tenant" :string :doc "the tenant slug to sign in to; unset, the section's"))

(nlk:define-contract hireko-logout
  (:doc "Sign out of Hireko: delete the saved session for the tenant.")
  ("tenant" :string :doc "the tenant slug to sign out of; unset, the section's"))

(nlk:define-contract hireko-interview
  (:doc "Create a Hireko candidate screening interview link. Hireko writes the questions asynchronously, so the call polls for up to wait_seconds and answers the candidate link once the session exists. Ask the recruiter for the title, the duration, the location, the company and the dates, one question at a time; the description can be written from the title.")
  ("title" :text :required t :doc "the job title, as the recruiter says it")
  ("description" :string :default "" :doc "the job description and interview expectations; unset, one is written from the title")
  ("company_name" :string :default "" :doc "the hiring company")
  ("location" :string :default "" :doc "where the job is, e.g. Remote or New York, NY")
  ("interview_length" :string :default "30" :doc "the interview's length in minutes")
  ("start_date" :string :default "" :doc "epoch milliseconds or a YYYY-MM-DD date; unset, today")
  ("end_date" :string :default "" :doc "epoch milliseconds or a YYYY-MM-DD date; unset, seven days after the start")
  ("company_culture" :string :default "" :doc "culture notes the questions may draw on")
  ("employment_type" :string :default "Full Time" :doc "the employment type")
  ("language" :string :default "en" :doc "the questions' language, a two-letter code")
  ("recruiter_email" :string :default "" :doc "who the interview is filed under; unset, the signed-in recruiter")
  ("wait_seconds" :integer :default 150 :doc "how long to poll for the generated questions, 0 to answer the job group id at once"))

(nlk:define-contract hireko-link
  (:doc "Fetch a Hireko candidate interview link by the external_job_group_id an earlier create call answered, polling for up to wait_seconds while the questions are still generating.")
  ("external_job_group_id" :text :required t :doc "the id hireko_create_interview_link answered")
  ("wait_seconds" :integer :default 150 :doc "how long to poll, 0 to look once"))

(nlk:define-contract hireko-score
  (:doc "Read the candidate match scores and search relevance Hireko computed for one job posting.")
  ("job_id" :text :required t :doc "the job id to read scores for"))

(nlk:define-contract hireko-health
  (:doc "Check whether Hireko's downstream recruiter services answer, through the facade and the saved session.")
  ("tenant" :string :doc "the tenant slug to check; unset, the section's"))

(defun read-settings (values table)
  "The declared VALUES, plus the recruiter's sign-in read once, here: the
variables the section names, else HIREKO_USERNAME and HIREKO_PASSWORD."
  (declare (ignore table))
  (let* ((username-env (nonblank (getf values :username-env)))
         (password-env (nonblank (getf values :password-env)))
         (username (or (and username-env (nonblank (uiop:getenv username-env)))
                       (nonblank (uiop:getenv "HIREKO_USERNAME"))))
         (password (or (and password-env (nonblank (uiop:getenv password-env)))
                       (nonblank (uiop:getenv "HIREKO_PASSWORD"))))
         (root (string-right-trim "/" (or (getf values :facade-url) "")))
         (plist (copy-list values)))
    (when (and username-env (null username))
      (nlk:config-error "username_env names ~a, but nothing is set there: put the recruiter's email in it, or clear the member to read HIREKO_USERNAME" username-env))
    (unless (or (uiop:string-prefix-p "https://" root) (uiop:string-prefix-p "http://" root))
      (nlk:config-error "facade_url ~s has no http:// or https:// scheme: set it back to Hireko's hosted endpoint, or to the proxy that answers for it" root))
    (let ((tenant (string-downcase (nlk:trimmed (or (getf values :tenant) "test")))))
      (unless (tenant-p tenant)
        (nlk:config-error "tenant ~s is no Hireko tenant: lowercase letters, digits and dashes, at most 63" tenant))
      (setf (getf plist :tenant) tenant))
    (setf (getf plist :stage) (stage-word (getf values :stage))
          (getf plist :facade-url) root
          (getf plist :username) username
          (getf plist :password) password)
    plist))

(nle:define-cell hireko
  (:section ("hireko")
    (:guide "put the recruiter's email and password in HIREKO_USERNAME and HIREKO_PASSWORD, then /hireko login; tenant is the organization slug Hireko gave you; stage is beta unless Hireko said prod; next step: ask for a candidate screening interview link")
    ("tenant" :string :default "test"
     :doc "the Hireko organization slug every call names")
    ("stage" :choice :options '("beta" "prod") :default "beta"
     :doc "which Hireko deployment answers: beta, or prod when Hireko runs you there")
    ("facade_url" :string
     :default "https://sm3m7mrvpfdovsnjoza4xkil5u0gqnke.lambda-url.us-west-2.on.aws/mcp"
     :doc "the hosted MCP endpoint every tool call goes to")
    ("username_env" :env
     :doc "the variable holding the recruiter's email (HIREKO_USERNAME is read either way)")
    ("password_env" :env
     :doc "the variable holding the recruiter's password (HIREKO_PASSWORD is read either way)")
    ("client_id" :string
     :doc "the Cognito app client, when the tenant's configuration page should not be read for it")
    ("region" :string
     :doc "the Cognito region, when the tenant's configuration page should not be read for it")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one sign-in's or tool call's ceiling, in seconds")
    ("poll_timeout_seconds" :integer :default 150 :min 0
     :doc "how long one interview call waits for its link when the call names no wait_seconds")
    ("poll_interval_seconds" :integer :default 6 :min 0
     :doc "how often it asks again while it waits"))
  (:settings #'read-settings)
  (:help :hireko
         "hireko: sign in to Hireko and create candidate screening interview links"
         +primer+)
  (:tool "hireko_auth_status" (nlk:contract 'hireko-auth) #'auth-status)
  (:tool "hireko_login" (nlk:contract 'hireko-login) #'login-tool)
  (:tool "hireko_logout" (nlk:contract 'hireko-logout) #'logout-tool)
  (:tool "hireko_create_interview_link" (nlk:contract 'hireko-interview) #'create-interview)
  (:tool "hireko_get_interview_link" (nlk:contract 'hireko-link) #'get-interview-link)
  (:tool "hireko_get_search_score" (nlk:contract 'hireko-score) #'search-score)
  (:tool "hireko_health" (nlk:contract 'hireko-health) #'health-tool)
  (:command "hireko" 'run-command
            :description "Sign in to Hireko, out, or check the session"
            :argument-hint "[login|out]"))
