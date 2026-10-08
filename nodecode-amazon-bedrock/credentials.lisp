;;;; credentials.lisp --- the AWS credential chain a SigV4 round is signed with.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/ai/src/providers/
;;;; aws-credentials.ts (the chain, every source in it, the cache),
;;;; ai/src/registry/aws.ts (whether a source is configured) and
;;;; ai/src/utils/aws-profile.ts (profileHasCredentialSource). The chain,
;;;; first hit wins:
;;;;
;;;;   1. AWS_ACCESS_KEY_ID + AWS_SECRET_ACCESS_KEY [+ AWS_SESSION_TOKEN]
;;;;   2. web identity: AWS_WEB_IDENTITY_TOKEN_FILE + AWS_ROLE_ARN, traded at
;;;;      STS AssumeRoleWithWebIdentity
;;;;   3. the profile in ~/.aws/credentials and ~/.aws/config: static keys,
;;;;      SSO (the AWS CLI's cached token, refreshed through SSO OIDC and
;;;;      written back, then GetRoleCredentials), credential_process, or
;;;;      role_arn chaining (source_profile, web_identity_token_file,
;;;;      credential_source) through STS AssumeRole, signed here
;;;;   4. ECS / container credentials (AWS_CONTAINER_CREDENTIALS_*)
;;;;   5. EC2 IMDSv2, unless AWS_EC2_METADATA_DISABLED is true
;;;;
;;;; Credentials are kept in memory per profile and region and replaced a
;;;; minute before they expire; session keys read from the credentials file
;;;; are kept five minutes, since tools rotate them in place. A credential is
;;;; a plist (:access-key :secret-key :session-token :expires-at), EXPIRES-AT
;;;; in epoch seconds or NIL for one that never expires.

(in-package #:nodecode-amazon-bedrock)

(defparameter +refresh-skew+ 60
  "Seconds before expiry a cached credential is replaced.")

(defparameter +file-session-seconds+ 300
  "How long session keys read from the credentials file are kept.")

(defparameter +exchange-seconds+ 30
  "How long one credential exchange may take.")

(defparameter +imds-seconds+ 1
  "How long the instance metadata service is given per request.")

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun credentials-error (format-control &rest arguments)
  "A credential chain that cannot answer, said in omp's words."
  (error 'nle::provider-config-error :detail (apply #'format nil format-control arguments)))

(defun make-creds (access-key secret-key &key session-token expires-at)
  (list :access-key access-key :secret-key secret-key :session-token session-token :expires-at expires-at))

(defun iso-seconds (text)
  "The epoch second an ISO 8601 / RFC 3339 time TEXT names, or NIL."
  (ppcre:register-groups-bind ((#'parse-integer year month day hour minute second) nil zone sign zh zm)
      ("^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?(Z|UTC|([+-])(\\d{2}):?(\\d{2}))$"
       (string-trim " " (or text "")))
    (let ((offset (if (member zone '("Z" "UTC") :test #'equal)
                      0
                      (* (if (equal sign "-") -1 1) (+ (* 3600 (parse-integer zh)) (* 60 (parse-integer zm)))))))
      (- (encode-universal-time second minute hour day month year 0)
         #.(encode-universal-time 0 0 0 1 1 1970 0)
         offset))))

(defun iso-time (seconds)
  "Epoch SECONDS as YYYY-MM-DDTHH:MM:SSZ, the way the AWS CLI writes a cache."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (+ seconds #.(encode-universal-time 0 0 0 1 1 1970 0)) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h mi s)))

(defun body-text (body)
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun aws-http (method url &key headers content (seconds +exchange-seconds+))
  "One exchange within SECONDS: (values TEXT STATUS); STATUS NIL and TEXT the
transport fact when nothing answered."
  (handler-case
      (sb-sys:with-deadline (:seconds seconds)
        (multiple-value-bind (body status)
            (nlk:http method url :headers headers :content content :timeout seconds :connect-timeout seconds)
          (values (body-text body) status)))
    ((or error sb-sys:deadline-timeout) (condition)
      (values (nle:transport-failure-label condition url) nil))))

(defun ok-p (status) (and (integerp status) (< status 300)))

(defun form (&rest pairs)
  "PAIRS, alternating names and values, as an x-www-form-urlencoded body; a
NIL value is left out."
  (quri:url-encode-params (loop for (name value) on pairs by #'cddr when value collect (cons name value))
                          :space-to-plus t))

(defun xml-tag (xml tag)
  "The text of the first <TAG> in XML, its entities decoded, or NIL."
  (let ((value (ppcre:register-groups-bind (value)
                   ((format nil "<~a>([\\s\\S]*?)</~a>" tag tag) (or xml ""))
                 value)))
    (when (plusp (length value))
      (loop for (entity . char) in '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'") ("&amp;" . "&"))
            do (setf value (ppcre:regex-replace-all entity value char)))
      value)))

(defun sts-endpoint (region)
  (format nil "https://sts.~a.~a/" region (if (uiop:string-prefix-p "cn-" region) "amazonaws.com.cn" "amazonaws.com")))

(defun session-name ()
  (format nil "nodecode-~d" (sb-posix:getpid)))

(defun sts-credentials (xml source)
  "The credentials an STS AssumeRole or AssumeRoleWithWebIdentity answer XML
carries; SOURCE names it in a refusal."
  (let ((access (xml-tag xml "AccessKeyId"))
        (secret (xml-tag xml "SecretAccessKey"))
        (token (xml-tag xml "SessionToken"))
        (expires (iso-seconds (xml-tag xml "Expiration"))))
    (unless (and access secret token)
      (credentials-error "~a response is missing credentials." source))
    (unless expires
      (credentials-error "~a response has a missing or invalid Expiration." source))
    (make-creds access secret :session-token token :expires-at expires)))

;;; --- environment and web identity ---------------------------------------------------

(defun env-creds ()
  (let ((access (env "AWS_ACCESS_KEY_ID")) (secret (env "AWS_SECRET_ACCESS_KEY")))
    (and access secret (make-creds access secret :session-token (env "AWS_SESSION_TOKEN")))))

(defun assume-role-with-web-identity (role-arn token-file session region)
  "Trade the web identity token in TOKEN-FILE for ROLE-ARN's credentials."
  (let ((token (string-trim '(#\Space #\Newline #\Return #\Tab)
                            (or (ignore-errors (uiop:read-file-string token-file))
                                (credentials-error "Unable to read AWS web identity token file: ~a" token-file)))))
    (when (zerop (length token)) (credentials-error "AWS web identity token file is empty."))
    (multiple-value-bind (xml status)
        (aws-http :post (sts-endpoint region)
                  :headers '(("content-type" . "application/x-www-form-urlencoded"))
                  :content (form "Action" "AssumeRoleWithWebIdentity" "Version" "2011-06-15"
                                 "RoleArn" role-arn "RoleSessionName" (or session (session-name))
                                 "WebIdentityToken" token))
      (unless (ok-p status)
        (credentials-error "AWS AssumeRoleWithWebIdentity failed: ~a ~a" (or status "no answer")
                           (or (xml-tag xml "Message") (subseq xml 0 (min 200 (length xml))))))
      (sts-credentials xml "AWS web identity"))))

(defun web-identity-creds (region)
  (let ((token-file (env "AWS_WEB_IDENTITY_TOKEN_FILE")) (role (env "AWS_ROLE_ARN")))
    (and token-file role (assume-role-with-web-identity role token-file (env "AWS_ROLE_SESSION_NAME") region))))

;;; --- STS AssumeRole ------------------------------------------------------------------

(defun sts-assume-role (base role-arn region &key session duration external-id)
  "Trade BASE credentials for ROLE-ARN's, the request signed with BASE."
  (let* ((payload (form "Action" "AssumeRole" "Version" "2011-06-15" "RoleArn" role-arn
                        "RoleSessionName" (or session (session-name))
                        "DurationSeconds" duration "ExternalId" external-id))
         (endpoint (sts-endpoint region))
         (uri (quri:uri endpoint))
         (content-type "application/x-www-form-urlencoded")
         (signed (sign-request :host (quri:uri-host uri) :path (quri:uri-path uri) :body (octets payload)
                               :region region :service "sts"
                               :headers `(("content-type" . ,content-type))
                               :access-key (getf base :access-key) :secret-key (getf base :secret-key)
                               :session-token (getf base :session-token))))
    (multiple-value-bind (xml status)
        (aws-http :post endpoint :headers (append signed `(("content-type" . ,content-type))) :content payload)
      (unless (ok-p status)
        (credentials-error "AWS AssumeRole failed: ~a ~a" (or status "no answer")
                           (or (xml-tag xml "Message") (subseq xml 0 (min 200 (length xml))))))
      (sts-credentials xml "AWS AssumeRole"))))

;;; --- SSO -------------------------------------------------------------------------------

(defun sso-cache-dir ()
  (merge-pathnames ".aws/sso/cache/" (aws-home)))

(defun load-sso-token (start-url session-name)
  "(values TOKEN FILE): the AWS CLI's cached SSO token for START-URL, the
file named by the SHA-1 of SESSION-NAME (or START-URL) tried first, or NIL."
  (let* ((directory (sso-cache-dir))
         (files (mapcar #'file-namestring (directory (merge-pathnames "*.json" directory))))
         (hashed (format nil "~a.json" (sha1-hex (or session-name start-url))))
         (candidates (cons hashed (remove hashed files :test #'equal))))
    (dolist (file candidates)
      (when (member file files :test #'equal)
        (let ((token (ignore-errors (nlk:decode-json (uiop:read-file-string (merge-pathnames file directory))))))
          (when (and (hash-table-p token)
                     (or (equal (nlk:json-value token :string "startUrl") start-url)
                         (and session-name (equal file hashed))))
            (return (values token file))))))))

(defun refresh-sso-token (token file sso-region)
  "TOKEN refreshed through SSO OIDC CreateToken and written back to FILE, as
the AWS CLI does on every command; NIL when it cannot be refreshed."
  (let ((refresh (nlk:json-value token :text "refreshToken"))
        (client (nlk:json-value token :text "clientId"))
        (secret (nlk:json-value token :text "clientSecret"))
        (registration (iso-seconds (nlk:json-value token :string "registrationExpiresAt"))))
    (when (and refresh client secret (not (and registration (<= registration (unix-seconds)))))
      (multiple-value-bind (text status)
          (aws-http :post (format nil "https://oidc.~a.amazonaws.com/token" sso-region)
                    :headers '(("content-type" . "application/json"))
                    :content (nlk:encode-json-object
                              (nlk:json-object "clientId" client "clientSecret" secret
                                               "grantType" "refresh_token" "refreshToken" refresh)))
        (let ((answer (and (ok-p status) (ignore-errors (nlk:decode-json text)))))
          (when (nlk:json-value answer :text "accessToken")
            (let ((updated (nlk:copy-json-object token)))
              (setf (gethash "accessToken" updated) (nlk:json-value answer :text "accessToken")
                    (gethash "expiresAt" updated) (iso-time (+ (unix-seconds)
                                                               (floor (or (nlk:json-value answer :number "expiresIn") 0))))
                    (gethash "refreshToken" updated) (or (nlk:json-value answer :text "refreshToken") refresh))
              ;; a write that fails leaves the token in memory, as omp does
              (ignore-errors
               (nlk:write-file-atomically (merge-pathnames file (sso-cache-dir))
                                          (nlk:encode-json-object updated) :mode #o600))
              updated)))))))

(defun sso-creds (profile config)
  "The role credentials an SSO PROFILE (an alist) gets: the cached token,
refreshed when due, then GetRoleCredentials."
  (flet ((field (key &optional (alist profile)) (cdr (assoc key alist :test #'equal))))
    (let* ((session-name (field "sso_session"))
           (session (and session-name (ini-section config (format nil "sso-session:~a" session-name))))
           (start-url (or (field "sso_start_url") (field "sso_start_url" session)))
           (sso-region (or (field "sso_region") (field "sso_region" session))))
      (when (and start-url sso-region)
        (multiple-value-bind (token file) (load-sso-token start-url session-name)
          (unless (nlk:json-value token :text "accessToken")
            (credentials-error "AWS SSO token for ~a not found in ~~/.aws/sso/cache. Run 'aws sso login' first."
                               start-url))
          (let* ((access (nlk:json-value token :text "accessToken"))
                 (expires (iso-seconds (nlk:json-value token :string "expiresAt")))
                 (expired (and expires (<= expires (unix-seconds)))))
            (when (and expires (<= (- expires +refresh-skew+) (unix-seconds)))
              (let ((refreshed (refresh-sso-token token file sso-region)))
                (cond (refreshed (setf access (nlk:json-value refreshed :text "accessToken")))
                      (expired (credentials-error "AWS SSO token for ~a has expired. Run 'aws sso login' to refresh."
                                                  start-url)))))
            (multiple-value-bind (text status)
                (aws-http :get (format nil "https://portal.sso.~a.amazonaws.com/federation/credentials?account_id=~a&role_name=~a"
                                       sso-region (quri:url-encode (field "sso_account_id"))
                                       (quri:url-encode (field "sso_role_name")))
                          :headers `(("x-amz-sso_bearer_token" . ,access)))
              (unless (ok-p status)
                (credentials-error "AWS SSO GetRoleCredentials failed: ~a ~a" (or status "no answer")
                                   (subseq text 0 (min 200 (length text)))))
              (let ((role (nlk:json-value (ignore-errors (nlk:decode-json text)) :object "roleCredentials")))
                (unless role (credentials-error "AWS SSO GetRoleCredentials: missing roleCredentials in response"))
                (make-creds (nlk:json-value role :string "accessKeyId") (nlk:json-value role :string "secretAccessKey")
                            :session-token (nlk:json-value role :string "sessionToken")
                            :expires-at (let ((ms (nlk:json-value role :number "expiration")))
                                          (and ms (floor ms 1000))))))))))))

;;; --- credential_process --------------------------------------------------------------------

(defun tokenize-posix (command)
  "COMMAND split as a POSIX shell splits it, as botocore's compat_shell_split
does: single quotes literal, double quotes escaping only $ ` \" \\."
  (let ((tokens '()) (current (make-string-output-stream)) (token-p nil) (mode :normal)
        (length (length command)))
    (loop with i = 0
          while (< i length)
          do (let ((char (char command i)))
               (ecase mode
                 (:normal
                  (cond ((char= char #\') (setf mode :single token-p t))
                        ((char= char #\") (setf mode :double token-p t))
                        ((and (char= char #\\) (< (1+ i) length))
                         (incf i) (write-char (char command i) current) (setf token-p t))
                        ((member char '(#\Space #\Tab #\Newline #\Return))
                         (when token-p (push (get-output-stream-string current) tokens) (setf token-p nil)))
                        (t (write-char char current) (setf token-p t))))
                 (:single
                  (if (char= char #\') (setf mode :normal) (write-char char current)))
                 (:double
                  (cond ((char= char #\") (setf mode :normal))
                        ((and (char= char #\\) (< (1+ i) length) (find (char command (1+ i)) "$`\"\\"))
                         (incf i) (write-char (char command i) current))
                        (t (write-char char current)))))
               (incf i)))
    (unless (eq mode :normal) (credentials-error "AWS credential_process command has an unterminated quote."))
    (when token-p (push (get-output-stream-string current) tokens))
    (nreverse tokens)))

(defun process-creds (profile command)
  "The credentials an external credential_process COMMAND prints (Version 1)."
  (let ((argv (tokenize-posix command)))
    (unless argv (credentials-error "AWS credential_process for profile '~a' is empty." profile))
    (multiple-value-bind (output errors code)
        (handler-case (uiop:run-program argv :output :string :error-output :string :ignore-error-status t
                                             :input nil)
          (error (condition) (credentials-error "AWS credential_process for profile '~a' did not run: ~a" profile condition)))
      (unless (eql code 0)
        (let ((tail (string-trim '(#\Space #\Newline) (if (plusp (length (string-trim " " errors))) errors output))))
          (credentials-error "AWS credential_process for profile '~a' exited ~a: ~a" profile code
                             (if (plusp (length tail)) (subseq tail (max 0 (- (length tail) 512))) "(no output)"))))
      (let ((parsed (ignore-errors (nlk:decode-json output))))
        (unless (hash-table-p parsed)
          (credentials-error "AWS credential_process for profile '~a' did not emit valid JSON" profile))
        (unless (eql 1 (nlk:json-value parsed :integer "Version"))
          (credentials-error "AWS credential_process for profile '~a' returned unsupported Version ~a; expected 1."
                             profile (or (nlk:json-value parsed :any "Version") "<missing>")))
        (let ((access (nlk:json-value parsed :text "AccessKeyId"))
              (secret (nlk:json-value parsed :text "SecretAccessKey"))
              (token (nlk:json-value parsed :text "SessionToken"))
              (expiration (nlk:json-value parsed :string "Expiration")))
          (unless (and access secret)
            (credentials-error "AWS credential_process for profile '~a' returned envelope without AccessKeyId/SecretAccessKey."
                               profile))
          ;; an expiry missing or malformed keeps nothing cached
          (make-creds access secret :session-token token
                                    :expires-at (and (or token expiration)
                                                     (or (iso-seconds expiration) (unix-seconds)))))))))

;;; --- containers and instances ----------------------------------------------------------------

(defun local-host-p (host)
  "Whether HOST is loopback, private, link-local or a metadata host (omp's
isLocalOrMetadataHost): none is reachable through a remote proxy."
  (let ((host (string-downcase (string-trim "[]" host))))
    (or (member host '("localhost" "metadata.google.internal" "::1") :test #'equal)
        (uiop:string-suffix-p host ".localhost")
        (ppcre:register-groups-bind ((#'parse-integer a b)) ("^(\\d{1,3})\\.(\\d{1,3})\\.\\d{1,3}\\.\\d{1,3}$" host)
          (or (member a '(127 10 0)) (and (= a 169) (= b 254)) (and (= a 192) (= b 168)) (and (= a 172) (<= 16 b 31)))))))

(defun container-creds ()
  (let ((relative (env "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"))
        (full (env "AWS_CONTAINER_CREDENTIALS_FULL_URI")))
    (when (or relative full)
      (let ((endpoint
              (if relative
                  (progn
                    (unless (and (uiop:string-prefix-p "/" relative) (not (uiop:string-prefix-p "//" relative)))
                      (credentials-error "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI must be a single-host absolute path."))
                    (concatenate 'string "http://169.254.170.2" relative))
                  (let ((uri (ignore-errors (quri:uri full))))
                    (unless (and uri (quri:uri-host uri))
                      (credentials-error "AWS_CONTAINER_CREDENTIALS_FULL_URI is invalid: ~a" full))
                    (unless (or (equal (quri:uri-scheme uri) "https") (local-host-p (quri:uri-host uri)))
                      (credentials-error "AWS_CONTAINER_CREDENTIALS_FULL_URI must use HTTPS or a local metadata host."))
                    full)))
            (authorization (or (env "AWS_CONTAINER_AUTHORIZATION_TOKEN")
                               (alexandria:when-let (file (env "AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE"))
                                 (string-trim '(#\Space #\Newline #\Return)
                                              (or (ignore-errors (uiop:read-file-string file))
                                                  (credentials-error "Unable to read AWS container authorization token file: ~a"
                                                                     file)))))))
        (multiple-value-bind (text status)
            (aws-http :get endpoint :headers (and authorization `(("authorization" . ,authorization))))
          (unless (ok-p status)
            (credentials-error "AWS container credential endpoint failed: ~a ~a" (or status "no answer")
                               (subseq text 0 (min 200 (length text)))))
          (let ((body (ignore-errors (nlk:decode-json text))))
            (unless (and (nlk:json-value body :text "AccessKeyId") (nlk:json-value body :text "SecretAccessKey")
                         (nlk:json-value body :text "Token"))
              (credentials-error "AWS container credential response is missing AccessKeyId/SecretAccessKey/Token."))
            (make-creds (nlk:json-value body :text "AccessKeyId") (nlk:json-value body :text "SecretAccessKey")
                        :session-token (nlk:json-value body :text "Token")
                        :expires-at (or (iso-seconds (nlk:json-value body :string "Expiration"))
                                        (credentials-error "AWS container credential response has a missing or invalid Expiration.")))))))))

(defun imds-base ()
  (let ((base (or (env "AWS_EC2_METADATA_SERVICE_ENDPOINT")
                  (if (string-equal (env "AWS_EC2_METADATA_SERVICE_ENDPOINT_MODE") "ipv6")
                      "http://[fd00:ec2::254]/"
                      "http://169.254.169.254/"))))
    (if (uiop:string-suffix-p base "/") base (concatenate 'string base "/"))))

(defun imds-creds ()
  "The instance role's credentials through IMDSv2, or NIL when it does not answer."
  (ignore-errors
   (let ((base (imds-base)))
     (multiple-value-bind (token status)
         (aws-http :put (concatenate 'string base "latest/api/token")
                   :headers '(("x-aws-ec2-metadata-token-ttl-seconds" . "21600")) :seconds +imds-seconds+)
       (when (ok-p status)
         (multiple-value-bind (role status)
             (aws-http :get (concatenate 'string base "latest/meta-data/iam/security-credentials/")
                       :headers `(("x-aws-ec2-metadata-token" . ,token)) :seconds +imds-seconds+)
           (let ((role (string-trim '(#\Space #\Newline #\Return) role)))
             (when (and (ok-p status) (plusp (length role)))
               (multiple-value-bind (text status)
                   (aws-http :get (format nil "~alatest/meta-data/iam/security-credentials/~a" base (quri:url-encode role))
                             :headers `(("x-aws-ec2-metadata-token" . ,token)) :seconds +imds-seconds+)
                 (let* ((body (and (ok-p status) (ignore-errors (nlk:decode-json text))))
                        (expires (iso-seconds (nlk:json-value body :string "Expiration"))))
                   (when (and (nlk:json-value body :text "AccessKeyId") (nlk:json-value body :text "SecretAccessKey")
                              (nlk:json-value body :text "Token") expires)
                     (make-creds (nlk:json-value body :text "AccessKeyId") (nlk:json-value body :text "SecretAccessKey")
                                 :session-token (nlk:json-value body :text "Token") :expires-at expires))))))))))))

(defun imds-enabled-p ()
  (not (string-equal (env "AWS_EC2_METADATA_DISABLED") "true")))

;;; --- profiles ------------------------------------------------------------------------------

(defun merged-profile (name credentials config)
  "Profile NAME's settings: the config file's, the credentials file's over them."
  (let ((merged (copy-alist (ini-section config name))))
    (loop for (key . value) in (ini-section credentials name)
          do (setf merged (cons (cons key value) (remove key merged :key #'car :test #'equal))))
    merged))

(defun profile-creds (name credentials config region seen)
  "Profile NAME's credentials, following role_arn chains; SEEN guards against
source_profile cycles."
  (when (member name seen :test #'equal)
    (credentials-error "AWS profile role chain contains a cycle at '~a'." name))
  (let ((seen (cons name seen))
        (profile (merged-profile name credentials config)))
    (flet ((field (key) (cdr (assoc key profile :test #'equal))))
      (cond ((null profile) nil)
            ((field "role_arn") (assume-role-from-profile name profile credentials config region seen))
            ((and (field "aws_access_key_id") (field "aws_secret_access_key"))
             (make-creds (field "aws_access_key_id") (field "aws_secret_access_key")
                         :session-token (field "aws_session_token")
                         :expires-at (and (field "aws_session_token") (+ (unix-seconds) +file-session-seconds+))))
            ((and (field "sso_account_id") (field "sso_role_name")) (sso-creds profile config))
            ((field "credential_process") (process-creds name (field "credential_process")))))))

(defun credential-source-creds (source region)
  (declare (ignore region))
  (cond ((equal source "Environment") (env-creds))
        ((equal source "Ec2InstanceMetadata") (and (imds-enabled-p) (imds-creds)))
        ((equal source "EcsContainer") (container-creds))
        (t (credentials-error "Unsupported AWS credential_source '~a'." source))))

(defun assume-role-from-profile (name profile credentials config region seen)
  (flet ((field (key) (cdr (assoc key profile :test #'equal))))
    (let ((role (field "role_arn")))
      (cond ((field "web_identity_token_file")
             (assume-role-with-web-identity role (field "web_identity_token_file") (field "role_session_name") region))
            ((field "mfa_serial")
             (credentials-error "AWS profile '~a' requires MFA (mfa_serial), which is not supported for non-interactive credential resolution."
                                name))
            (t
             (let ((base (cond ((field "source_profile")
                                (or (profile-creds (field "source_profile") credentials config region seen)
                                    (credentials-error "AWS profile '~a' references source_profile '~a', which has no usable credentials."
                                                       name (field "source_profile"))))
                               ((field "credential_source")
                                (or (credential-source-creds (field "credential_source") region)
                                    (credentials-error "AWS profile '~a' credential_source '~a' produced no credentials."
                                                       name (field "credential_source"))))
                               (t (credentials-error "AWS profile '~a' sets role_arn without source_profile, credential_source, or web_identity_token_file."
                                                     name)))))
               (sts-assume-role base role region :session (field "role_session_name")
                                                 :duration (field "duration_seconds")
                                                 :external-id (field "external_id"))))))))

(defun shared-profile-creds (region)
  (multiple-value-bind (credentials-path config-path) (shared-file-paths)
    (profile-creds (profile-name) (read-ini credentials-path)
                   (and (load-shared-config-p) (read-ini config-path)) region '())))

;;; --- the chain and its cache ------------------------------------------------------------------

(defun resolve-fresh (region)
  (or (env-creds)
      (web-identity-creds region)
      (shared-profile-creds region)
      (container-creds)
      (and (imds-enabled-p) (imds-creds))
      (credentials-error "Unable to resolve AWS credentials. Configure static environment keys, web identity, ~
                          an AWS profile, ECS credentials, or an EC2 instance role.")))

(defvar *creds* (make-hash-table :test 'equal)
  "(PROFILE REGION LOAD-CONFIG-P) -> the credentials resolved for it: omp's cache.")

(defvar *creds-lock* (bt2:make-lock :name "amazon-bedrock credentials")
  "Held across a cache read and the resolution that fills it, so concurrent
rounds share one resolution (omp's inflight).")

(defun cache-key (region)
  (list (profile-name) region (and (load-shared-config-p) t)))

(defun aws-credentials (region)
  "The credentials a SigV4 round in REGION is signed with (omp's resolveAwsCredentials)."
  (bt2:with-lock-held (*creds-lock*)
    (let* ((key (cache-key region))
           (hit (gethash key *creds*)))
      (if (and hit (or (null (getf hit :expires-at)) (> (- (getf hit :expires-at) +refresh-skew+) (unix-seconds))))
          hit
          (setf (gethash key *creds*) (resolve-fresh region))))))

(defun forget-credentials (&optional region)
  "Drop the cached credentials of REGION, or all of them: a 401 or 403 means
they went stale (rotated session keys)."
  (bt2:with-lock-held (*creds-lock*)
    (if region (remhash (cache-key region) *creds*) (clrhash *creds*))))

(defun profile-has-source-p (name credentials config seen)
  "Whether profile NAME ends in a usable credential source, as the
resolver dispatches (omp's profileHasCredentialSource)."
  (unless (member name seen :test #'equal)
    (let ((profile (merged-profile name credentials config)))
      (flet ((field (key) (cdr (assoc key profile :test #'equal))))
        (cond ((field "role_arn")
               (cond ((field "web_identity_token_file") t)
                     ((field "mfa_serial") nil)
                     ((field "credential_source")
                      (let ((source (field "credential_source")))
                        (cond ((equal source "Environment") (and (env-creds) t))
                              ((equal source "EcsContainer")
                               (and (or (env "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI") (env "AWS_CONTAINER_CREDENTIALS_FULL_URI")) t))
                              ((equal source "Ec2InstanceMetadata") (imds-enabled-p)))))
                     ((field "source_profile")
                      (profile-has-source-p (field "source_profile") credentials config (cons name seen)))))
              ((and (field "aws_access_key_id") (field "aws_secret_access_key")) t)
              ((field "credential_process") t)
              ((not (and (field "sso_account_id") (field "sso_role_name"))) nil)
              ((and (field "sso_start_url") (field "sso_region")) t)
              (t (let ((session (and (field "sso_session")
                                     (ini-section config (format nil "sso-session:~a" (field "sso_session"))))))
                   (and (cdr (assoc "sso_start_url" session :test #'equal))
                        (cdr (assoc "sso_region" session :test #'equal)) t))))))))

(defun aws-source-p ()
  "Whether an AWS credential source is configured, asking no network (omp's
hasAwsCredentialSource, without its EC2 hardware probe: an instance is
assumed only when AWS_EC2_METADATA_SERVICE_ENDPOINT names its service)."
  (or (env-creds)
      (and (env "AWS_WEB_IDENTITY_TOKEN_FILE") (env "AWS_ROLE_ARN"))
      (env "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI") (env "AWS_CONTAINER_CREDENTIALS_FULL_URI")
      (and (imds-enabled-p) (env "AWS_EC2_METADATA_SERVICE_ENDPOINT"))
      (multiple-value-bind (credentials-path config-path) (shared-file-paths)
        (profile-has-source-p (profile-name) (read-ini credentials-path)
                              (and (load-shared-config-p) (read-ini config-path)) '()))))
