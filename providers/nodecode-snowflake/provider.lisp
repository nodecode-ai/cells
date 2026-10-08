;;;; provider.lisp --- what Snowflake Cortex is: its account address, its models, its wires.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/snowflake.kdl (a custom login, a refresh, a structured key) and
;;;; providers/snowflake.kdl (the roster, the default model, SNOWFLAKE_PAT,
;;;; the chat wire's quirks), ai/src/registry/snowflake.ts (the account URL
;;;; rules and the per-request address), and the bundled rows of
;;;; catalog/src/models.json, which models.json in this folder carries
;;;; (tools/omp-models.py wrote it).
;;;;
;;;; Snowflake serves the Cortex REST API on the account's own host,
;;;; https://<org>-<account>.snowflakecomputing.com, under /api/v2/cortex:
;;;; the Claude models on the Messages wire at /api/v2/cortex/v1/messages,
;;;; the GPT models on the chat wire at /api/v2/cortex/v1/chat/completions.
;;;; The bearer is a programmatic access token (PAT) or a Snowflake OAuth
;;;; access token; omp sends either as `Authorization: Bearer'. The chat wire
;;;; refuses max_tokens and store, so the cap rides as max_completion_tokens.

(in-package #:nodecode-snowflake)

(defparameter +placeholder-origin+ "https://snowflake-account.invalid"
  "The origin omp's rows carry until a request names the account: `.invalid'
never resolves, so an address left unrewritten fails before any host.")

(defparameter +env+ '("SNOWFLAKE_PAT")
  "The environment variables a PAT is read from, in order.")

(defparameter +account-env+ "SNOWFLAKE_ACCOUNT"
  "Where omp reads the account when the credential names none.")

(defparameter +cortex-path+ "/api/v2/cortex/v1"
  "The Cortex REST API's path on the account host, both wires' base.")

;;; --- the account's address -----------------------------------------------------

(defparameter +invalid-account+
  "Paste your Snowflake account identifier (orgname-accountname) or account URL")

(defun dns-label-p (label)
  (and (<= (length label) 63) (ppcre:scan "^[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$" label) t))

(defun strip-443 (host)
  (if (uiop:string-suffix-p host ":443") (subseq host 0 (- (length host) 4)) host))

(defun normalize-account-url (input)
  "INPUT, an account identifier, an account URL or a Snowsight link, as the
account's https origin; anything that could hand a credential to another
host signals SNOWFLAKE-ERROR (omp's normalizeSnowflakeAccountUrl)."
  (let* ((value (string-trim '(#\Space #\Tab #\Newline #\Return) (or input "")))
         (scheme (nth-value 1 (ppcre:scan-to-strings "(?i)^([a-z][a-z0-9+.-]*)://" value)))
         (host nil))
    (when (zerop (length value))
      (fail "Snowflake account is required: run /snowflake login ACCOUNT, set snowflake.account, or set ~a"
            +account-env+))
    (when (and scheme (string-not-equal (aref scheme 0) "https"))
      (fail "Snowflake account URL must use https"))
    (when (find #\\ value) (fail "~a" +invalid-account+))
    (flet ((normal (text) (strip-443 (substitute #\- #\_ (string-downcase text)))))
      (if scheme
          (multiple-value-bind (match groups) (ppcre:scan-to-strings "(?i)^https://([^/?#]+)(.*)$" value)
            (unless match (fail "~a" +invalid-account+))
            (setf host (normal (aref groups 0)))
            (when (equal host "app.snowflake.com")
              (let* ((path (first (ppcre:split "[?#]" (aref groups 1) :limit 2)))
                     (segments (uiop:split-string (or path "") :separator "/"))
                     (org (substitute #\- #\_ (string-downcase (or (second segments) ""))))
                     (account (substitute #\- #\_ (string-downcase (or (third segments) "")))))
                (unless (and (dns-label-p org) (dns-label-p account))
                  (fail "~a" +invalid-account+))
                ;; legacy links are /<cloud-region>/<locator>: no account host in them
                (when (ppcre:scan "^[a-z]{2}(?:-gov)?-[a-z]+-\\d+$" org)
                  (fail "Legacy Snowsight links don't identify the account host; paste your account ~
                         identifier (orgname-accountname) or account URL"))
                (setf host (format nil "~a-~a.snowflakecomputing.com" org account)))))
          (progn
            (when (find-if (lambda (char) (find char "/?#")) value) (fail "~a" +invalid-account+))
            (setf host (normal value))
            (unless (or (uiop:string-suffix-p host ".snowflakecomputing.com")
                        (uiop:string-suffix-p host ".snowflakecomputing.cn"))
              (setf host (concatenate 'string host ".snowflakecomputing.com"))))))
    (when (uiop:string-suffix-p host ".snowflakecomputing.cn")
      (fail "Snowflake Cortex REST API is not available in China-region accounts (.snowflakecomputing.cn)"))
    (let ((suffix ".snowflakecomputing.com"))
      (unless (and (uiop:string-suffix-p host suffix) (> (length host) (length suffix))
                   (every #'dns-label-p (uiop:split-string host :separator ".")))
        (fail "~a" +invalid-account+)))
    (format nil "https://~a" host)))

(defun configured-account ()
  "The account the section or SNOWFLAKE_ACCOUNT names, unnormalized, or NIL."
  (let ((value (setting :account)))
    (if (and (stringp value) (plusp (length (string-trim " " value))))
        value
        (nle::credential-env +account-env+))))

(defun parse-credential (text)
  "(values TOKEN ACCOUNT-URL) out of a stored key TEXT: a bare PAT or token,
or omp's structured OAuth key ({token, enterpriseUrl, ...}), whose account
it must name; malformed JSON is refused, never sent as a bearer (omp's
parseSnowflakeCredential)."
  (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (cond ((zerop (length value)) nil)
          ((char/= (char value 0) #\{) value)
          (t (let* ((parsed (ignore-errors (nlk:decode-json value)))
                    (token (nlk:json-value parsed :text "token"))
                    (account (nlk:json-value parsed :text "enterpriseUrl")))
               (unless (and token account)
                 (fail "Invalid Snowflake credential; run /snowflake login again"))
               (values (string-trim " " token)
                       (handler-case (normalize-account-url account)
                         (snowflake-error (condition)
                           (fail "Invalid Snowflake credential account: ~a; run /snowflake login again"
                                 condition)))))))))

(defun account-base (account-url)
  "The Cortex REST base on ACCOUNT-URL, both wires' (the lane appends
/messages or /chat/completions)."
  (concatenate 'string (string-right-trim "/" account-url) +cortex-path+))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-snowflake" "models.json")))
  "omp's bundled Snowflake rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun model-lane (model-id)
  "The lane MODEL-ID rides: its row's wire; a model off the roster rides the
Messages wire, the provider's default."
  (if (equal (nlk:json-value (model-row model-id) :string "api") "openai-completions")
      "openai-completions"
      "anthropic"))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (let ((cost (value :object "cost")))
      (nle::make-catalog-model
       (value :string "name")
       (value :integer "context")
       (value :integer "output")
       (or (value :array "input") #("text"))
       #("text")
       (sort (remove-if-not #'nle::effort-rank (coerce (or (value :array "efforts") #()) 'list))
             #'< :key #'nle::effort-rank)
       (value :boolean "reasoning")
       nil
       t
       ;; CATALOG-PRICE's shape: omp's estimate of the AI credits at $2 each
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Snowflake as a models.dev provider: the Messages lane's package (each
model's own lane is RESOLVE-MODEL-LANE's answer), the Cortex base on the
account the section names (omp's placeholder origin when none does), the PAT
variable, and the bundled models over PRIOR's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Snowflake Cortex"
                     "npm" "@ai-sdk/anthropic"
                     "api" (account-base (or (ignore-errors (normalize-account-url (configured-account)))
                                             +placeholder-origin+))
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun listing-rows ()
  "The roster as a provider listing answers it: omp marks it
credential-scoped and never fetches it, so the picker is answered from the
roster without a request."
  (loop for row across +models+
        collect (list :id (nlk:json-value row :string "id")
                      :display (nlk:json-value row :string "name")
                      :context-window (nlk:json-value row :integer "context"))))

(defun env-key ()
  "The first PAT one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- one round -------------------------------------------------------------------

(defun round-route (config)
  "(values TOKEN BASE) for the round CONFIG freezes: the bearer, and the
Cortex base on its account: the sign-in's account, else the structured key's,
else the section's, else SNOWFLAKE_ACCOUNT's (omp's snowflakeTransport)."
  (multiple-value-bind (token key-account)
      (handler-case (parse-credential (nle::effective-provider-config-api-key config))
        (snowflake-error (condition)
          (error 'nle::provider-config-error :detail (princ-to-string condition))))
    (unless (and token (string/= token "public"))
      (error 'nle::provider-config-error
             :detail (format nil "Snowflake is not signed in: run /snowflake login ACCOUNT, or save a ~
                                  programmatic access token with /connect or ~a (and name the account ~
                                  in snowflake.account or ~a)" (first +env+) +account-env+)))
    (let ((account (or (nle::credential-attribute config :account-url)
                       key-account
                       (handler-case (normalize-account-url (configured-account))
                         (snowflake-error (condition)
                           (error 'nle::provider-config-error :detail (princ-to-string condition)))))))
      (values token (account-base account)))))

(defun round-headers (headers token)
  "HEADERS, the lane's own, with the bearer in place of every credential
header the lane wrote: omp sends a non-Anthropic host's key as a bearer."
  (append (remove-if (lambda (name) (member name '("authorization" "x-api-key") :test #'string-equal))
                     headers :key #'car)
          `(("Authorization" . ,(format nil "Bearer ~a" token)))))

(defun round-endpoint (base lane)
  "The address a round on LANE posts to under the Cortex BASE."
  (concatenate 'string base (if (equal lane "anthropic") "/messages" "/chat/completions")))
