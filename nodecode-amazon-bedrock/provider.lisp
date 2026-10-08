;;;; provider.lisp --- what Amazon Bedrock is: its models, its regions, its address.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/amazon-bedrock.kdl and providers/amazon-bedrock.kdl, ai/src/
;;;; providers/amazon-bedrock.ts (the region rules, the request address),
;;;; ai/src/utils/aws-profile.ts (the shared config and credentials files,
;;;; the profile, its region), ai/src/registry/aws.ts (whether a credential
;;;; source is configured, the bearer token), and the bundled rows of
;;;; catalog/src/models.json, which models.json in this folder carries with
;;;; the facts its own generator adds (tools/bedrock-models.py).
;;;;
;;;; Bedrock serves the Converse Stream API at
;;;; https://bedrock-runtime.<region>.amazonaws.com/model/<model>/converse-stream.
;;;; The region is the request's: the section's, else one an ARN model id
;;;; names, else the ambient one (AWS_REGION, AWS_DEFAULT_REGION, the
;;;; profile's) when it can serve a cross-region inference profile's geo,
;;;; else that geo's default region, else us-east-1.

(in-package #:nodecode-amazon-bedrock)

(defparameter +bearer-env+ "AWS_BEARER_TOKEN_BEDROCK"
  "The variable a Bedrock API key (a bearer token) is read from.")

(defparameter +geo-regions+
  '(("us" . "us-east-1") ("us-gov" . "us-gov-west-1") ("eu" . "eu-west-1")
    ("apac" . "ap-southeast-1") ("au" . "ap-southeast-2") ("jp" . "ap-northeast-1"))
  "Each cross-region inference profile geo's default region: a geo-prefixed
profile is servable only from its own geo's regions.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-amazon-bedrock" "models.json")))
  "omp's bundled Bedrock rows, read when this file loads: a vector of objects.")

(defvar *rows* (let ((table (make-hash-table :test 'equal)))
                 (loop for row across +models+ do (setf (gethash (nlk:json-value row :string "id") table) row))
                 table)
  "Model id -> its bundled row.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (gethash model-id *rows*))

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
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun listing-rows ()
  "The roster as a provider listing answers it: Bedrock's own listing is a
signed control-plane call, so the picker is answered from the roster
without a request."
  (loop for row across +models+
        collect (list :id (nlk:json-value row :string "id")
                      :display (nlk:json-value row :string "name")
                      :context-window (nlk:json-value row :integer "context"))))

;;; --- the shared config and credentials files ---------------------------------------

(defun present (value)
  "VALUE trimmed when it is a non-empty string, else NIL."
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun env (name)
  "The environment's NAME, trimmed, or NIL."
  (nle::credential-env name))

(defun aws-home ()
  "The home the ~/.aws files are read under."
  (user-homedir-pathname))

(defun shared-file-paths ()
  "(values CREDENTIALS-PATH CONFIG-PATH), honouring their variables."
  (values (or (env "AWS_SHARED_CREDENTIALS_FILE") (merge-pathnames ".aws/credentials" (aws-home)))
          (or (env "AWS_CONFIG_FILE") (merge-pathnames ".aws/config" (aws-home)))))

(defun parse-ini (text)
  "TEXT, an AWS shared file, as an alist of (SECTION . ALIST): `profile ' and
`sso-session ' prefixes normalized (omp's parseAwsIni)."
  (let ((sections '()) (current nil))
    (dolist (raw (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((line (string-trim '(#\Space #\Tab #\Return) raw)))
        (cond ((or (zerop (length line)) (find (char line 0) "#;")))
              ((and (char= (char line 0) #\[) (char= (char line (1- (length line))) #\]))
               (let ((name (string-trim " " (subseq line 1 (1- (length line))))))
                 (cond ((uiop:string-prefix-p "profile " name)
                        (setf name (string-trim " " (subseq name 8))))
                       ((uiop:string-prefix-p "sso-session " name)
                        (setf name (format nil "sso-session:~a" (string-trim " " (subseq name 12))))))
                 (setf current (or (assoc name sections :test #'equal)
                                   (car (push (list name) sections))))))
              (current
               (alexandria:when-let (equals (position #\= line))
                 (let ((key (string-trim " " (subseq line 0 equals)))
                       (value (string-trim " " (subseq line (1+ equals)))))
                   (setf (cdr current) (cons (cons key value) (remove key (cdr current) :key #'car :test #'equal)))))))))
    sections))

(defun read-ini (path)
  "The parsed shared file at PATH, or NIL when it cannot be read."
  (let ((text (and path (ignore-errors (uiop:read-file-string path)))))
    (and text (parse-ini text))))

(defun ini-section (ini name)
  "The alist of section NAME of the parsed INI, or NIL."
  (cdr (assoc name ini :test #'equal)))

(defun configured-profile ()
  "The profile the section names, or NIL."
  (present (setting :profile)))

(defun profile-name ()
  "The profile credentials are read from: the section's, else AWS_PROFILE, else default."
  (or (configured-profile) (env "AWS_PROFILE") "default"))

(defun load-shared-config-p ()
  "Whether ~/.aws/config takes part: a named profile says so, the default one
only under AWS_SDK_LOAD_CONFIG (omp's shouldLoadAwsSharedConfig)."
  (or (configured-profile) (env "AWS_PROFILE")
      (member (env "AWS_SDK_LOAD_CONFIG") '("1" "true") :test #'string-equal)))

(defun profile-region ()
  "The region the active profile's config names, or NIL."
  (when (load-shared-config-p)
    (let ((config (read-ini (nth-value 1 (shared-file-paths)))))
      (cdr (assoc "region" (ini-section config (profile-name)) :test #'equal)))))

(defun ambient-region ()
  "The region the environment or the active profile names, or NIL."
  (or (env "AWS_REGION") (env "AWS_DEFAULT_REGION") (profile-region)))

(defun credential-region ()
  "The region credential exchanges (STS, SSO) use: the section's, else the
ambient one, else us-east-1 (omp's resolveAwsRegion)."
  (or (present (setting :region)) (ambient-region) "us-east-1"))

;;; --- a request's region ----------------------------------------------------------

(defun arn-region (model-id)
  "The region an arn:...:bedrock:<region>:... MODEL-ID names, or NIL."
  (let ((parts (uiop:split-string (or model-id "") :separator ":")))
    (and (>= (length parts) 4) (equal (first parts) "arn") (equal (third parts) "bedrock")
         (present (fourth parts)))))

(defun profile-geo (model-id)
  "The geo of a cross-region inference profile MODEL-ID (eu.anthropic... is
eu), or NIL."
  (let ((dot (position #\. model-id)))
    (and dot (plusp dot) (car (assoc (subseq model-id 0 dot) +geo-regions+ :test #'equal)))))

(defun region-serves-geo-p (region geo)
  "Whether REGION can serve a GEO profile; the ap- regions overlap, so the
Australian and Japanese geos pin their own."
  (cond ((equal geo "us-gov") (uiop:string-prefix-p "us-gov-" region))
        ((equal geo "us") (and (uiop:string-prefix-p "us-" region) (not (uiop:string-prefix-p "us-gov-" region))))
        ((equal geo "eu") (uiop:string-prefix-p "eu-" region))
        ((equal geo "apac") (uiop:string-prefix-p "ap-" region))
        ((equal geo "au") (member region '("ap-southeast-2" "ap-southeast-4") :test #'equal))
        ((equal geo "jp") (member region '("ap-northeast-1" "ap-northeast-3") :test #'equal))))

(defun guardrail-identifier ()
  (present (setting :guardrail-identifier)))

(defun bedrock-region (model-id)
  "The region a round of MODEL-ID goes to (omp's resolveBedrockRegion)."
  (or (present (setting :region))
      (arn-region model-id)
      (let ((ambient (ambient-region))
            (guardrail (arn-region (guardrail-identifier)))
            (geo (profile-geo model-id)))
        (if geo
            (cond ((and ambient (region-serves-geo-p ambient geo)) ambient)
                  ((and guardrail (region-serves-geo-p guardrail geo)) guardrail)
                  (t (cdr (assoc geo +geo-regions+ :test #'equal))))
            (or ambient guardrail "us-east-1")))))

;;; --- a request's address ------------------------------------------------------------

(defun encode-uri-component (text)
  "TEXT as JavaScript's encodeURIComponent writes it: UTF-8, every octet but
A-Z a-z 0-9 - _ . ! ~ * ' ( ) percent-encoded in upper-case hex."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          for char = (code-char octet)
          do (if (and (< octet 128) (or (alphanumericp char) (find char "-_.!~*'()")))
                 (write-char char out)
                 (format out "%~2,'0X" octet)))))

(defun regional-host-p (host)
  "Whether HOST is AWS's own regional Bedrock runtime host, whose region a
request re-points."
  (and (ppcre:scan "^bedrock-runtime\\.[a-z0-9-]+\\.amazonaws\\.com$" host) t))

(defun request-address (model-id region)
  "(values URL HOST PATH QUERY) a Converse Stream round of MODEL-ID in REGION
posts to: the section's base verbatim (a VPC endpoint, a gateway, its path
and query kept), AWS's own regional host re-pointed at REGION."
  (let* ((base (or (present (setting :base-url)) (format nil "https://bedrock-runtime.~a.amazonaws.com" region)))
         (uri (quri:uri base))
         (host (quri:uri-host uri))
         (port (quri:uri-port uri))
         (default-port-p (or (null port) (eql port (quri.port:scheme-default-port (quri:uri-scheme uri))))))
    (when (regional-host-p host)
      (setf host (format nil "bedrock-runtime.~a.amazonaws.com" region)))
    (let* ((authority (if default-port-p host (format nil "~a:~d" host port)))
           (path (format nil "~a/model/~a/converse-stream"
                         (string-right-trim "/" (or (quri:uri-path uri) ""))
                         (encode-uri-component model-id)))
           (query (present (quri:uri-query uri))))
      (values (format nil "~a://~a~a~@[?~a~]" (quri:uri-scheme uri) authority path query)
              authority path query))))

(defun catalog-row (&optional prior)
  "Amazon Bedrock as a models.dev provider: this cell's lane, the region's
runtime host, the bearer variable, and the bundled models over PRIOR's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Amazon Bedrock"
                     "npm" "nodecode-amazon-bedrock"
                     "api" (or (present (setting :base-url))
                               (format nil "https://bedrock-runtime.~a.amazonaws.com"
                                       (or (present (setting :region)) (ambient-region) "us-east-1")))
                     "env" (vector +bearer-env+)
                     "models" models)))
