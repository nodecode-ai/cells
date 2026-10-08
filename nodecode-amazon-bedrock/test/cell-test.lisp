;;;; cell-test.lisp --- the amazon-bedrock cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every AWS
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every ~/.aws file a temp file
;;;; (the home a stubbed AWS-HOME), every Converse round a stubbed dex:post
;;;; answering event-stream frames built here from the spec, every
;;;; credential exchange a stubbed dex:request: nothing touches the network,
;;;; the environment, AWS's files or the operator's.

(in-package #:nodecode.test)

(define-test-slice "amazon-bedrock" "AMAZON-BEDROCK-CELL-" :start nodecode-amazon-bedrock:start-cell)

(define-cell-lifecycle-tests "amazon-bedrock"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models :credential)
  (:running (is (nle::find-lane-by-name "amazon-bedrock" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "amazon-bedrock" nil)) "and taken out"))
  (:refused ("region" 5) ("guardrail_trace" "loud")))

(defun ab-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun ab-hex-octets (hex)
  (coerce (loop for at from 0 below (length hex) by 2 collect (parse-integer hex :start at :end (+ at 2) :radix 16))
          '(vector (unsigned-byte 8))))

(defun ab-u32 (value) (list (ldb (byte 8 24) value) (ldb (byte 8 16) value) (ldb (byte 8 8) value) (ldb (byte 8 0) value)))

(defun ab-frame (headers payload)
  "One event-stream frame as the spec lays it out: HEADERS, an alist of
string values (or (NAME TYPE . OCTETS) for another type), and PAYLOAD text."
  (let* ((header-octets
           (loop for (name . value) in headers
                 append (let ((name-octets (coerce (sb-ext:string-to-octets name :external-format :utf-8) 'list)))
                          (append (list (length name-octets)) name-octets
                                  (if (stringp value)
                                      (let ((value-octets (coerce (sb-ext:string-to-octets value :external-format :utf-8) 'list)))
                                        (append (list 7 (ldb (byte 8 8) (length value-octets)) (ldb (byte 8 0) (length value-octets)))
                                                value-octets))
                                      value)))))
         (payload-octets (coerce (sb-ext:string-to-octets payload :external-format :utf-8) 'list))
         (total (+ 12 (length header-octets) (length payload-octets) 4))
         (prelude (coerce (append (ab-u32 total) (ab-u32 (length header-octets))) '(vector (unsigned-byte 8))))
         (message (concatenate '(vector (unsigned-byte 8)) prelude
                               (ab-u32 (nodecode-amazon-bedrock::crc32 prelude)) header-octets payload-octets)))
    (concatenate '(vector (unsigned-byte 8)) message (ab-u32 (nodecode-amazon-bedrock::crc32 message)))))

(defun ab-event (type payload)
  (ab-frame `((":event-type" . ,type) (":content-type" . "application/json") (":message-type" . "event")) payload))

(defun ab-octet-stream (&rest frames)
  (flexi-streams:make-in-memory-input-stream (apply #'concatenate '(vector (unsigned-byte 8)) frames)))

(defun ab-answer-frames ()
  "A Converse answer that thinks, says ok, and calls a tool."
  (list (ab-event "messageStart" "{\"p\":\"ab\",\"role\":\"assistant\"}")
        (ab-event "contentBlockDelta" "{\"contentBlockIndex\":0,\"delta\":{\"reasoningContent\":{\"text\":\"pondering\"}}}")
        (ab-event "contentBlockDelta" "{\"contentBlockIndex\":0,\"delta\":{\"reasoningContent\":{\"signature\":\"sig-1\"}}}")
        (ab-event "contentBlockStop" "{\"contentBlockIndex\":0}")
        (ab-event "contentBlockDelta" "{\"contentBlockIndex\":1,\"delta\":{\"text\":\"ok\"}}")
        (ab-event "contentBlockStop" "{\"contentBlockIndex\":1}")
        (ab-event "contentBlockStart" "{\"contentBlockIndex\":2,\"start\":{\"toolUse\":{\"toolUseId\":\"tooluse_1\",\"name\":\"eval\"}}}")
        (ab-event "contentBlockDelta" "{\"contentBlockIndex\":2,\"delta\":{\"toolUse\":{\"input\":\"{\\\"form\\\":\"}}}")
        (ab-event "contentBlockDelta" "{\"contentBlockIndex\":2,\"delta\":{\"toolUse\":{\"input\":\"\\\"(+ 1 2)\\\"}\"}}}")
        (ab-event "contentBlockStop" "{\"contentBlockIndex\":2}")
        (ab-event "messageStop" "{\"stopReason\":\"tool_use\"}")
        (ab-event "metadata" "{\"usage\":{\"inputTokens\":10,\"outputTokens\":5,\"cacheReadInputTokens\":3,\"cacheWriteInputTokens\":2,\"totalTokens\":20},\"metrics\":{\"latencyMs\":9}}")))

(defun ab-env (alist)
  "A CREDENTIAL-ENV stub body over ALIST."
  (lambda (name) (cdr (assoc name alist :test #'equal))))

(defmacro with-ab-env ((alist &key (home "/nonexistent/home/")) &body body)
  "BODY with the environment ALIST and HOME as ~/'s parent of .aws."
  `(let ((env-fn (ab-env ,alist)))
     (with-stubbed-fdefinitions ((nle::credential-env (name) (funcall env-fn name))
                                 (nodecode-amazon-bedrock::aws-home () (pathname ,home)))
       ,@body)))

(defmacro with-ab-round ((url headers body result &key (env ''(("AWS_ACCESS_KEY_ID" . "AKIDTEST")
                                                                ("AWS_SECRET_ACCESS_KEY" . "secret")
                                                                ("AWS_SESSION_TOKEN" . "session-1")))
                                                    (section ''()) (frames '(ab-answer-frames)) effort context
                                                    key)
                         model &body forms)
  "FORMS with the cell started on SECTION and one Bedrock round of MODEL at
EFFORT captured: URL, HEADERS and BODY (decoded) as dex:post saw them, RESULT
the list of what NLE::CALL-PROVIDER answered; the round streams FRAMES."
  `(with-ab-env (,env)
     (with-cell-stop ((apply #'amazon-bedrock-start ,section))
       (let ((nle::*provider* "amazon-bedrock") (nle::*model* ,model) (nle::*api-key* ,key)
             (nle::*reasoning-effort* ,effort) (nle::*endpoint* nil)
             (,url nil) (,headers nil) (,body nil) (,result nil))
         (declare (ignorable ,url ,headers ,body ,result))
         (with-temp-auth (auth "{}")
           (let ((nle::*auth-file-path* (pathname auth)))
             (with-stubbed-fdefinition
                 (dex:post (asked &rest args)
                  (setf ,url asked ,headers (getf args :headers)
                        ,body (nlk:decode-json (sb-ext:octets-to-string (getf args :content) :external-format :utf-8)))
                  (values (apply #'ab-octet-stream ,frames) 200
                          (nlk:json-object "content-type" "application/vnd.amazon.eventstream")))
               (setf ,result (multiple-value-list (nle::call-provider (or ,context (user-context))))))))
         ,@forms))))

;;; --- the catalog --------------------------------------------------------------------

(deftest amazon-bedrock-cell-puts-its-row-and-lane-in-the-catalog ()
  (with-ab-env ('())
    (with-cell-stop ((amazon-bedrock-start))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "amazon-bedrock")))
        (is (equal "Amazon Bedrock" (nlk:json-value row :string "name")))
        (is (equal "https://bedrock-runtime.us-east-1.amazonaws.com" (nlk:json-value row :string "api")))
        (is (= 212 (hash-table-count (nlk:json-value row :object "models"))))
        (is (equal "amazon-bedrock" (nle::configured-provider-lane "amazon-bedrock")) "the cell's own lane drives it")
        (is (equal "https://bedrock-runtime.us-east-1.amazonaws.com" (nle::lane-endpoint "amazon-bedrock" "amazon-bedrock")))
        (is (find "us.anthropic.claude-opus-5-5" (nle::list-provider-models "amazon-bedrock")
                  :key (lambda (row) (getf row :id)) :test #'equal)))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "amazon-bedrock"))))))

(deftest amazon-bedrock-cell-says-a-connect-key-was-not-checked ()
  ;; The roster answered /connect's key check with no reason, which the core
  ;; reads as a key Bedrock took: any key read `works'. Bedrock is asked
  ;; nothing, so the verdict is unchecked, even where an asked endpoint would
  ;; have refused the key.
  (with-ab-env ('())
    (with-cell-stop ((amazon-bedrock-start))
      (with-temp-file (nle::*provider-models-cache-path*)
        (let ((asked '()))
          (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                     (push url asked)
                                     (values "{\"error\":{\"type\":\"authentication_error\"}}" 401))
            (multiple-value-bind (verdict words) (nle::provider-key-check "amazon-bedrock" "ABSK-wrong")
              (is (eq :unchecked verdict))
              (is (search "first turn tries the key" words)))
            (is (null asked) "nothing was asked")
            (multiple-value-bind (rows reason) (nle::list-provider-models "amazon-bedrock")
              (is rows)
              (is (null reason) "the picker's listing, with no key, is the roster as before"))))))))

;;; --- SigV4 ----------------------------------------------------------------------------

(deftest amazon-bedrock-cell-derives-the-documented-signing-keys ()
  (is (equal "f4780e2d9f65fa895f9c67b32ce1baf0b0d8a43505a000a1a9e090d414db404d"
             (nodecode-amazon-bedrock::hex (nodecode-amazon-bedrock::signing-key
                                             "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" "20120215" "us-east-1" "iam")))
      "AWS's documented signing key")
  (is (equal "c4afb1cc5771d871763a393e44b703571b55cc28424d1a5e86da6ed3c154a4b9"
             (nodecode-amazon-bedrock::hex (nodecode-amazon-bedrock::signing-key
                                             "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" "20150830" "us-east-1" "iam"))))
  (is (equal "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
             (nodecode-amazon-bedrock::hex (nodecode-amazon-bedrock::hmac-sha256 "Jefe" "what do ya want for nothing?")))
      "RFC 4231 test case 2")
  (is (equal "a9993e364706816aba3e25717850c26c9cd0d89d" (nodecode-amazon-bedrock::sha1-hex "abc")) "FIPS 180 SHA-1 of abc"))

(deftest amazon-bedrock-cell-signs-the-documented-s3-get-example ()
  ;; AWS's SigV4 example "GET Object": the same canonical request omp's
  ;; signer writes, x-amz-content-sha256 signed
  (let ((headers (nodecode-amazon-bedrock::sign-request
                  :method "GET" :host "examplebucket.s3.amazonaws.com" :path "/test.txt"
                  :headers '(("Range" . "bytes=0-9")) :body #() :region "us-east-1" :service "s3"
                  :access-key "AKIAIOSFODNN7EXAMPLE" :secret-key "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
                  :time (encode-universal-time 0 0 0 24 5 2013 0))))
    (is (equal (concatenate 'string
                            "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, "
                            "SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, "
                            "Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41")
               (ab-header headers "authorization")))
    (is (equal "20130524T000000Z" (ab-header headers "x-amz-date")))
    (is (equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (ab-header headers "x-amz-content-sha256")))
    (is (null (ab-header headers "x-amz-security-token")))))

(deftest amazon-bedrock-cell-escapes-the-path-once-more ()
  (is (equal "/model/us.anthropic.claude-sonnet-4-5-20250929-v1%253A0/converse-stream"
             (nodecode-amazon-bedrock::canonical-path "/model/us.anthropic.claude-sonnet-4-5-20250929-v1%3A0/converse-stream")))
  (is (equal "%25e2%25b6=1&a=b&x=y%2Bz" (nodecode-amazon-bedrock::canonical-query "x=y+z&a=b&%25e2%25b6=1"))
      "each pair decoded and encoded again (a + stays a plus), sorted by the encoded names")
  (is (equal "arn%3Aaws%3Abedrock%3Aus-west-2%3A1%3Ainference-profile%2Fp"
             (nodecode-amazon-bedrock::encode-uri-component "arn:aws:bedrock:us-west-2:1:inference-profile/p"))))

;;; --- the event stream ----------------------------------------------------------------------

(deftest amazon-bedrock-cell-decodes-a-frame-made-elsewhere ()
  ;; built by Python's zlib and struct from the spec, independently of this cell
  (let ((frame (ab-hex-octets
                "0000009f00000057c37babff0b3a6576656e742d74797065070011636f6e74656e74426c6f636b44656c74610d3a636f6e74656e742d747970650700106170706c69636174696f6e2f6a736f6e0d3a6d6573736167652d747970650700056576656e747b22636f6e74656e74426c6f636b496e646578223a302c2264656c7461223a7b2274657874223a224869227d2c2270223a2261626364227df2ace5ff")))
    (is (= #xCBF43926 (nodecode-amazon-bedrock::crc32 (sb-ext:string-to-octets "123456789"))) "the CRC-32 check value")
    (multiple-value-bind (headers payload) (nodecode-amazon-bedrock::decode-message frame)
      (is (equal '((":event-type" . "contentBlockDelta") (":content-type" . "application/json") (":message-type" . "event"))
                 headers))
      (is (equal "Hi" (nlk:json-value (nlk:decode-json (sb-ext:octets-to-string payload)) :string "delta" "text"))))
    (is (equalp frame (ab-event "contentBlockDelta" "{\"contentBlockIndex\":0,\"delta\":{\"text\":\"Hi\"},\"p\":\"abcd\"}"))
        "the test's own encoder writes the same bytes")))

(deftest amazon-bedrock-cell-reads-every-header-type ()
  (let ((frame (ab-frame `(("t" 0) ("f" 1) ("b" 2 #xff) ("s" 3 #x01 #x00) ("i" 4 #xff #xff #xff #xfe)
                           ("l" 5 0 0 0 0 0 0 0 7) ("x" 6 0 2 #xde #xad) ("ts" 8 0 0 1 #x8c #x8a #xd5 #x90 #x00)
                           ("u" 9 ,@(loop for i below 16 collect i)))
                         "")))
    (let ((headers (nodecode-amazon-bedrock::decode-message frame)))
      (flet ((value (name) (cdr (assoc name headers :test #'equal))))
        (is (equal "true" (value "t")))
        (is (equal "false" (value "f")))
        (is (equal "-1" (value "b")))
        (is (equal "256" (value "s")))
        (is (equal "-2" (value "i")))
        (is (equal "7" (value "l")))
        (is (equal "3q0=" (value "x")) "a byte array as base64")
        (is (equal "2023-12-21T05:25:01.056Z" (value "ts")))
        (is (equal "00010203-0405-0607-0809-0a0b0c0d0e0f" (value "u")))))))

(deftest amazon-bedrock-cell-refuses-a-corrupt-or-cut-frame ()
  (let ((frame (ab-event "messageStop" "{\"stopReason\":\"end_turn\"}")))
    (let ((corrupt (copy-seq frame)))
      (setf (aref corrupt 20) (logxor (aref corrupt 20) 1))
      (is (search "message CRC mismatch"
                  (princ-to-string (signals-error nle::provider-stream-incomplete
                                     (nodecode-amazon-bedrock::decode-message corrupt))))))
    (let ((corrupt (copy-seq frame)))
      (setf (aref corrupt 9) (logxor (aref corrupt 9) 1))
      (is (search "prelude CRC mismatch"
                  (princ-to-string (signals-error nle::provider-stream-incomplete
                                     (nodecode-amazon-bedrock::decode-message corrupt))))))
    (is (search "truncated"
                (princ-to-string (signals-error nle::provider-stream-incomplete
                                   (nodecode-amazon-bedrock::read-frame
                                    (flexi-streams:make-in-memory-input-stream (subseq frame 0 30)) 5)))))
    (is (null (nodecode-amazon-bedrock::read-frame (flexi-streams:make-in-memory-input-stream #()) 5))
        "a clean end is no frame")))

;;; --- the region ----------------------------------------------------------------------------

(deftest amazon-bedrock-cell-picks-the-region-as-omp-does ()
  (flet ((region (model &key (env '()) (section '()))
           (with-ab-env (env)
             (with-cell-stop ((apply #'amazon-bedrock-start section))
               (nodecode-amazon-bedrock::bedrock-region model)))))
    (is (equal "us-east-1" (region "us.anthropic.claude-opus-5-5")) "the us geo's default")
    (is (equal "eu-west-1" (region "eu.anthropic.claude-opus-4-6-v1")) "the eu geo's default")
    (is (equal "eu-central-1" (region "eu.anthropic.claude-opus-4-6-v1" :env '(("AWS_REGION" . "eu-central-1"))))
        "an ambient region that serves the geo")
    (is (equal "eu-west-1" (region "eu.anthropic.claude-opus-4-6-v1" :env '(("AWS_REGION" . "us-west-2"))))
        "one that cannot is passed over")
    (is (equal "ap-southeast-2" (region "au.anthropic.claude-sonnet-4-5" :env '(("AWS_REGION" . "ap-southeast-1")))))
    (is (equal "us-west-2" (region "global.anthropic.claude-opus-5-5" :env '(("AWS_DEFAULT_REGION" . "us-west-2")))))
    (is (equal "us-east-1" (region "global.anthropic.claude-opus-5-5")))
    (is (equal "ap-northeast-1" (region "arn:aws:bedrock:ap-northeast-1:123:inference-profile/x"
                                        :env '(("AWS_REGION" . "us-west-2"))))
        "an ARN names its own")
    (is (equal "ca-central-1" (region "us.anthropic.claude-opus-5-5" :section '("region" "ca-central-1")
                                                                     :env '(("AWS_REGION" . "us-west-2"))))
        "the section outranks everything")))

(deftest amazon-bedrock-cell-reads-the-profiles-region ()
  (with-temp-file (config :contents (format nil "[default]~%region = us-west-1~%[profile work]~%region = eu-north-1~%")
                          :type "ini")
    (with-ab-env (`(("AWS_CONFIG_FILE" . ,config) ("AWS_PROFILE" . "work")))
      (with-cell-stop ((amazon-bedrock-start))
        (is (equal "eu-north-1" (nodecode-amazon-bedrock::ambient-region)))))
    (with-ab-env (`(("AWS_CONFIG_FILE" . ,config)))
      (with-cell-stop ((amazon-bedrock-start))
        (is (null (nodecode-amazon-bedrock::ambient-region)) "the default profile's config only under AWS_SDK_LOAD_CONFIG")))
    (with-ab-env (`(("AWS_CONFIG_FILE" . ,config) ("AWS_SDK_LOAD_CONFIG" . "1")))
      (with-cell-stop ((amazon-bedrock-start))
        (is (equal "us-west-1" (nodecode-amazon-bedrock::ambient-region)))))))

;;; --- the credential ------------------------------------------------------------------------

(deftest amazon-bedrock-cell-answers-the-credential-point ()
  (with-cell-stop ((amazon-bedrock-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-ab-env ('(("AWS_BEARER_TOKEN_BEDROCK" . "bedrock-key") ("AWS_ACCESS_KEY_ID" . "AKID") ("AWS_SECRET_ACCESS_KEY" . "s")))
          (let ((credential (nle::resolve-provider-credential "amazon-bedrock" :auth-path auth :probe t)))
            (is (equal "bedrock-key" (nle:credential-key credential)) "a Bedrock API key first, as omp's bearer")
            (is (eq :env (nle:credential-source credential)))))
        (with-ab-env ('(("AWS_ACCESS_KEY_ID" . "AKID") ("AWS_SECRET_ACCESS_KEY" . "s")))
          (let ((credential (nle::resolve-provider-credential "amazon-bedrock" :auth-path auth :probe t)))
            (is (equal "aws-sigv4" (nle:credential-key credential)) "else SigV4 over the chain")
            (is (eq :aws (nle:credential-source credential)))))
        (with-ab-env ('(("OPENAI_API_KEY" . "sk") ("ANTHROPIC_API_KEY" . "sk-ant")))
          (is (eq :public (nle:credential-source (nle::resolve-provider-credential "amazon-bedrock" :auth-path auth :probe t)))
              "no source configured: nothing, and no other family's variable"))))
    (with-temp-auth (auth "{\"api_keys\":{\"amazon-bedrock\":{\"provider\":\"amazon-bedrock\",\"key\":\"saved-key\"}}}")
      (let ((nle::*api-key* nil))
        (with-ab-env ('(("AWS_BEARER_TOKEN_BEDROCK" . "bedrock-key")))
          (is (equal "saved-key" (nle:credential-key (nle::resolve-provider-credential "amazon-bedrock" :auth-path auth :probe t)))
              "a key /connect saved outranks the variable"))))))

(deftest amazon-bedrock-cell-finds-a-profile-as-a-source ()
  (with-temp-file (credentials :contents (format nil "[default]~%aws_access_key_id = AKIDFILE~%aws_secret_access_key = file-secret~%")
                               :type "ini")
    (with-ab-env (`(("AWS_SHARED_CREDENTIALS_FILE" . ,credentials)))
      (with-cell-stop ((amazon-bedrock-start))
        (is (nodecode-amazon-bedrock::aws-source-p))
        (let ((creds (nodecode-amazon-bedrock::aws-credentials "us-east-1")))
          (is (equal "AKIDFILE" (getf creds :access-key)))
          (is (null (getf creds :expires-at)) "static keys never expire"))))
    (with-ab-env (`(("AWS_SHARED_CREDENTIALS_FILE" . ,credentials) ("AWS_ACCESS_KEY_ID" . "AKIDENV")
                    ("AWS_SECRET_ACCESS_KEY" . "env-secret")))
      (with-cell-stop ((amazon-bedrock-start))
        (is (equal "AKIDENV" (getf (nodecode-amazon-bedrock::aws-credentials "us-east-1") :access-key))
            "the environment outranks the profile")))
    (with-ab-env (`(("AWS_SHARED_CREDENTIALS_FILE" . ,credentials) ("AWS_PROFILE" . "absent")))
      (with-cell-stop ((amazon-bedrock-start))
        (is (not (nodecode-amazon-bedrock::aws-source-p)) "a profile the files do not name is no source")))))

(deftest amazon-bedrock-cell-runs-a-credential-process ()
  (with-temp-file (script :contents (format nil "#!/bin/sh~%echo '{\"Version\":1,\"AccessKeyId\":\"AKIDPROC\",\"SecretAccessKey\":\"proc-secret\",\"SessionToken\":\"proc-token\",\"Expiration\":\"2099-01-01T00:00:00Z\"}'~%")
                          :type "sh")
    (with-temp-file (credentials :contents (format nil "[default]~%credential_process = /bin/sh \"~a\"~%" script) :type "ini")
      (with-ab-env (`(("AWS_SHARED_CREDENTIALS_FILE" . ,credentials)))
        (with-cell-stop ((amazon-bedrock-start))
          (let ((creds (nodecode-amazon-bedrock::aws-credentials "us-east-1")))
            (is (equal "AKIDPROC" (getf creds :access-key)))
            (is (equal "proc-token" (getf creds :session-token)))
            (is (= 4070908800 (getf creds :expires-at)))))))))

(deftest amazon-bedrock-cell-assumes-a-role-through-sts ()
  (with-temp-file (credentials :contents (format nil "[base]~%aws_access_key_id = AKIDBASE~%aws_secret_access_key = base-secret~%")
                               :type "ini")
    (with-temp-file (config :contents (format nil "[profile work]~%role_arn = arn:aws:iam::123456789012:role/dev~%source_profile = base~%external_id = ext-1~%")
                            :type "ini")
      (with-ab-env (`(("AWS_SHARED_CREDENTIALS_FILE" . ,credentials) ("AWS_CONFIG_FILE" . ,config)))
        (with-cell-stop ((amazon-bedrock-start "profile" "work"))
          (let ((asked '()))
            (with-stubbed-fdefinition (dex:request (url &rest args)
                                       (push (list url (getf args :headers) (getf args :content)) asked)
                                       (values "<AssumeRoleResponse><AssumeRoleResult><Credentials><AccessKeyId>ASIAROLE</AccessKeyId><SecretAccessKey>role-secret</SecretAccessKey><SessionToken>role-token</SessionToken><Expiration>2099-01-01T00:00:00Z</Expiration></Credentials></AssumeRoleResult></AssumeRoleResponse>"
                                               200))
              (let ((creds (nodecode-amazon-bedrock::aws-credentials "us-west-2")))
                (is (equal "ASIAROLE" (getf creds :access-key)))
                (is (equal "role-token" (getf creds :session-token)))))
            (is (= 1 (length asked)))
            (destructuring-bind (url headers content) (first asked)
              (let ((form (quri:url-decode-params content)))
                (is (equal "https://sts.us-west-2.amazonaws.com/" url))
                (is (equal "AssumeRole" (cdr (assoc "Action" form :test #'equal))))
                (is (equal "arn:aws:iam::123456789012:role/dev" (cdr (assoc "RoleArn" form :test #'equal))))
                (is (equal "ext-1" (cdr (assoc "ExternalId" form :test #'equal))))
                (is (search "Credential=AKIDBASE/" (ab-header headers "authorization")) "signed with the source profile's keys")
                (is (search "/us-west-2/sts/aws4_request" (ab-header headers "authorization")))))))))))

(deftest amazon-bedrock-cell-uses-an-sso-session ()
  (let* ((home (uiop:ensure-directory-pathname
                (format nil "~a/ab-home-~d/" (string-right-trim "/" (namestring (uiop:temporary-directory))) (random 1000000))))
         (cache (merge-pathnames ".aws/sso/cache/" home)))
    (unwind-protect
         (progn
           (ensure-directories-exist cache)
           (with-open-file (out (merge-pathnames (format nil "~a.json" (nodecode-amazon-bedrock::sha1-hex "my-sso")) cache)
                                :direction :output)
             (write-string (format nil "{\"startUrl\":\"https://my.awsapps.com/start\",\"region\":\"us-east-1\",~
                                        \"accessToken\":\"sso-old\",\"expiresAt\":\"2000-01-01T00:00:00Z\",~
                                        \"refreshToken\":\"sso-refresh\",\"clientId\":\"cid\",\"clientSecret\":\"cs\",~
                                        \"registrationExpiresAt\":\"2099-01-01T00:00:00Z\"}")
                           out))
           (with-temp-file (config :contents (format nil "[profile sso]~%sso_session = my-sso~%sso_account_id = 111122223333~%sso_role_name = Dev~%[sso-session my-sso]~%sso_start_url = https://my.awsapps.com/start~%sso_region = us-east-1~%")
                                   :type "ini")
             (with-ab-env (`(("AWS_CONFIG_FILE" . ,config)) :home home)
               (with-cell-stop ((amazon-bedrock-start "profile" "sso"))
                 (let ((asked '()))
                   (with-stubbed-fdefinition (dex:request (url &rest args)
                                              (push (list url (getf args :headers) (getf args :content)) asked)
                                              (if (search "oidc." url)
                                                  (values "{\"accessToken\":\"sso-new\",\"expiresIn\":3600,\"refreshToken\":\"sso-refresh-2\"}" 200)
                                                  (values "{\"roleCredentials\":{\"accessKeyId\":\"ASIASSO\",\"secretAccessKey\":\"sso-secret\",\"sessionToken\":\"sso-token\",\"expiration\":4070908800000}}" 200)))
                     (let ((creds (nodecode-amazon-bedrock::aws-credentials "us-east-1")))
                       (is (equal "ASIASSO" (getf creds :access-key)))
                       (is (= 4070908800 (getf creds :expires-at)))))
                   (setf asked (reverse asked))
                   (is (= 2 (length asked)) "a refresh, then the role's credentials")
                   (is (equal "https://oidc.us-east-1.amazonaws.com/token" (first (first asked))))
                   (is (equal "refresh_token" (nlk:json-value (nlk:decode-json (third (first asked))) :string "grantType")))
                   (is (equal "https://portal.sso.us-east-1.amazonaws.com/federation/credentials?account_id=111122223333&role_name=Dev"
                              (first (second asked))))
                   (is (equal "sso-new" (ab-header (second (second asked)) "x-amz-sso_bearer_token")) "the refreshed token")
                   (let ((written (nlk:decode-json (uiop:read-file-string
                                                    (merge-pathnames (format nil "~a.json" (nodecode-amazon-bedrock::sha1-hex "my-sso"))
                                                                     cache)))))
                     (is (equal "sso-new" (nlk:json-value written :string "accessToken")) "written back as the AWS CLI does")
                     (is (equal "sso-refresh-2" (nlk:json-value written :string "refreshToken")))))))))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest amazon-bedrock-cell-trades-a-web-identity ()
  (with-temp-file (token :contents "eyJ.web.identity" :type "txt")
    (with-ab-env (`(("AWS_WEB_IDENTITY_TOKEN_FILE" . ,token) ("AWS_ROLE_ARN" . "arn:aws:iam::1:role/pod")))
      (with-cell-stop ((amazon-bedrock-start))
        (let ((form nil))
          (with-stubbed-fdefinition (dex:request (url &rest args)
                                     (setf form (quri:url-decode-params (getf args :content)))
                                     (values "<Credentials><AccessKeyId>ASIAWEB</AccessKeyId><SecretAccessKey>w</SecretAccessKey><SessionToken>t</SessionToken><Expiration>2099-01-01T00:00:00Z</Expiration></Credentials>" 200))
            (is (equal "ASIAWEB" (getf (nodecode-amazon-bedrock::aws-credentials "us-east-1") :access-key))))
          (is (equal "AssumeRoleWithWebIdentity" (cdr (assoc "Action" form :test #'equal))))
          (is (equal "eyJ.web.identity" (cdr (assoc "WebIdentityToken" form :test #'equal)))))))))

(deftest amazon-bedrock-cell-reads-a-container-and-an-instance-role ()
  (with-ab-env ('(("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" . "/v2/credentials/abc")
                  ("AWS_CONTAINER_AUTHORIZATION_TOKEN" . "ecs-auth")))
    (with-cell-stop ((amazon-bedrock-start))
      (let ((asked nil))
        (with-stubbed-fdefinition (dex:request (url &rest args)
                                   (setf asked (list url (getf args :headers)))
                                   (values "{\"AccessKeyId\":\"ASIAECS\",\"SecretAccessKey\":\"e\",\"Token\":\"t\",\"Expiration\":\"2099-01-01T00:00:00Z\"}" 200))
          (is (equal "ASIAECS" (getf (nodecode-amazon-bedrock::aws-credentials "us-east-1") :access-key))))
        (is (equal "http://169.254.170.2/v2/credentials/abc" (first asked)))
        (is (equal "ecs-auth" (ab-header (second asked) "authorization"))))))
  (with-ab-env ('())
    (with-cell-stop ((amazon-bedrock-start))
      (let ((asked '()))
        (with-stubbed-fdefinition (dex:request (url &rest args)
                                   (push (list (getf args :method) url) asked)
                                   (cond ((search "api/token" url) (values "imds-token" 200))
                                         ((uiop:string-suffix-p url "security-credentials/") (values "ec2-role" 200))
                                         (t (values "{\"AccessKeyId\":\"ASIAEC2\",\"SecretAccessKey\":\"e\",\"Token\":\"t\",\"Expiration\":\"2099-01-01T00:00:00Z\"}" 200))))
          (is (equal "ASIAEC2" (getf (nodecode-amazon-bedrock::aws-credentials "us-east-1") :access-key))))
        (is (equal '((:put "http://169.254.169.254/latest/api/token")
                     (:get "http://169.254.169.254/latest/meta-data/iam/security-credentials/")
                     (:get "http://169.254.169.254/latest/meta-data/iam/security-credentials/ec2-role"))
                   (reverse asked))
            "IMDSv2: a token, the role, its credentials")))))

(deftest amazon-bedrock-cell-says-when-no-source-answers ()
  (with-ab-env ('(("AWS_EC2_METADATA_DISABLED" . "true")))
    (with-cell-stop ((amazon-bedrock-start))
      (is (search "Unable to resolve AWS credentials"
                  (nle::provider-error-detail
                   (signals-error nle::provider-config-error (nodecode-amazon-bedrock::aws-credentials "us-east-1"))))))))

;;; --- a round -------------------------------------------------------------------------------

(deftest amazon-bedrock-cell-sends-a-signed-converse-round ()
  (with-ab-round (url headers body result :effort "high") "us.anthropic.claude-opus-5-5"
    (is (equal "https://bedrock-runtime.us-east-1.amazonaws.com/model/us.anthropic.claude-opus-5-5/converse-stream" url))
    (is (equal "application/vnd.amazon.eventstream" (ab-header headers "accept")))
    (is (ppcre:scan "^AWS4-HMAC-SHA256 Credential=AKIDTEST/\\d{8}/us-east-1/bedrock/aws4_request, SignedHeaders=accept;content-type;host;x-amz-content-sha256;x-amz-date;x-amz-security-token, Signature=[0-9a-f]{64}$"
                    (ab-header headers "authorization")))
    (is (equal "session-1" (ab-header headers "x-amz-security-token")))
    (is (equal "bedrock-runtime.us-east-1.amazonaws.com" (ab-header headers "host")))
    ;; the body: messages, the system prompt, the tools, the thinking
    (let ((messages (nlk:json-value body :array "messages")))
      (is (equal "user" (nlk:json-value (aref messages 0) :string "role")))
      (is (nlk:json-value (aref (nlk:json-value (aref messages 0) :array "content") 0) :string "text"))
      (is (nlk:json-value (let ((content (nlk:json-value (aref messages 0) :array "content")))
                            (aref content (1- (length content))))
                          :object "cachePoint")
          "the final user message carries a cache point"))
    (is (nlk:json-value (aref (nlk:json-value body :array "system") 1) :object "cachePoint") "and so does the system prompt")
    (is (nlk:json-value (aref (nlk:json-value body :array "toolConfig" "tools") 0) :object "toolSpec" "inputSchema" "json"))
    (is (equal "adaptive" (nlk:json-value body :string "additionalModelRequestFields" "thinking" "type")))
    (is (equal "summarized" (nlk:json-value body :string "additionalModelRequestFields" "thinking" "display")))
    (is (equal "high" (nlk:json-value body :string "additionalModelRequestFields" "output_config" "effort")))
    (is (equal "drop_block" (nlk:json-value body :string "additionalModelRequestFields" "thinking" "block_binding"
                                            "prefix_mismatch_behavior"))
        "a prefix-bound model's thinking drops a block whose prefix changed")
    (is (equal "thinking-binding-controls-2026-08-01"
               (aref (nlk:json-value body :array "additionalModelRequestFields" "anthropic_beta") 0)))
    (is (equal "/input_transformations" (aref (nlk:json-value body :array "additionalModelResponseFieldPaths") 0)))
    (is (null (nlk:json-value body :any "inferenceConfig" "temperature")) "no sampling for a model that refuses it")
    ;; the answer, folded
    (destructuring-bind (message usage finish &rest rest) result
      (declare (ignore rest))
      (is (equal "ok" (nlk:json-value message :string "content")))
      (is (equal "pondering" (nlk:json-value message :string "reasoning_content")))
      (is (equal "sig-1" (nlk:json-value message :string "reasoning_signature")))
      (let ((call (aref (nlk:json-value message :array "tool_calls") 0)))
        (is (equal "tooluse_1" (nlk:json-value call :string "id")))
        (is (equal "eval" (nlk:json-value call :string "function" "name")))
        (is (equal "{\"form\":\"(+ 1 2)\"}" (nlk:json-value call :string "function" "arguments"))))
      (is (equal "tool_calls" finish))
      (is (= 10 (nle::provider-usage-input-tokens usage)))
      (is (= 5 (nle::provider-usage-output-tokens usage)))
      (is (= 3 (nle::provider-usage-cached-input-tokens usage)))
      (is (= 2 (nle::provider-usage-cache-write-tokens usage))))))

(deftest amazon-bedrock-cell-replays-a-signed-thought-and-demotes-an-unsigned-one ()
  (with-ab-round (url headers body result
                      :context (compiled-context
                                (list (nle::message "user" "first")
                                      (nlk:json-object "role" "assistant" "content" "answer" "reasoning_content" "thought"
                                                       "reasoning_signature" "sig-0")
                                      (nle::message "user" "second"))))
      "us.anthropic.claude-sonnet-4-5-20250929-v1:0"
    (is (equal "https://bedrock-runtime.us-east-1.amazonaws.com/model/us.anthropic.claude-sonnet-4-5-20250929-v1%3A0/converse-stream" url)
        "the model id escaped in the path")
    (let ((assistant (aref (nlk:json-value body :array "messages") 1)))
      (is (equal "sig-0" (nlk:json-value (aref (nlk:json-value assistant :array "content") 0) :string
                                         "reasoningContent" "reasoningText" "signature")))
      (is (equal "answer" (nlk:json-value (aref (nlk:json-value assistant :array "content") 1) :string "text")))))
  (with-ab-round (url headers body result
                      :context (compiled-context
                                (list (nle::message "user" "first")
                                      (nlk:json-object "role" "assistant" "content" "answer" "reasoning_content" "thought")
                                      (nle::message "user" "second"))))
      "deepseek.v3-v1:0"
    (let ((assistant (aref (nlk:json-value body :array "messages") 1)))
      (is (equal (format nil "<think>~%thought~%</think>")
                 (nlk:json-value (aref (nlk:json-value assistant :array "content") 0) :string "text"))
          "an unsigned thought is replayed as text, never as reasoningContent"))))

(deftest amazon-bedrock-cell-sends-a-bedrock-api-key-as-a-bearer ()
  (with-ab-round (url headers body result :env '(("AWS_BEARER_TOKEN_BEDROCK" . "bedrock-key") ("AWS_REGION" . "us-west-2")))
      "openai.gpt-oss-120b-1:0"
    (is (equal "https://bedrock-runtime.us-west-2.amazonaws.com/model/openai.gpt-oss-120b-1%3A0/converse-stream" url))
    (is (equal "Bearer bedrock-key" (ab-header headers "authorization")))
    (is (null (ab-header headers "x-amz-date")) "no signature beside a bearer")
    (is (null (nlk:json-value body :any "additionalModelRequestFields")) "no effort asked: no thinking")))

(deftest amazon-bedrock-cell-asks-each-thinking-mode-its-own-way ()
  (with-ab-round (url headers body result :effort "medium") "us.anthropic.claude-sonnet-4-5-20250929-v1:0"
    (is (equal "enabled" (nlk:json-value body :string "additionalModelRequestFields" "thinking" "type")))
    (is (= 8192 (nlk:json-value body :integer "additionalModelRequestFields" "thinking" "budget_tokens")))
    (is (> (nlk:json-value body :integer "inferenceConfig" "maxTokens") 8192) "the cap leaves the answer room"))
  (with-ab-round (url headers body result :effort "high") "us.openai.gpt-6-sol"
    (is (equal "high" (nlk:json-value body :string "additionalModelRequestFields" "reasoning" "effort")))))

(deftest amazon-bedrock-cell-follows-the-section-base ()
  (with-ab-round (url headers body result :section '("base_url" "https://vpce-1.bedrock-runtime.us-east-1.vpce.amazonaws.com/gw?key=1"))
      "us.anthropic.claude-opus-5-5"
    (is (equal "https://vpce-1.bedrock-runtime.us-east-1.vpce.amazonaws.com/gw/model/us.anthropic.claude-opus-5-5/converse-stream?key=1" url)
        "a base of its own is kept verbatim, its path and query too")))

(deftest amazon-bedrock-cell-fails-an-in-stream-exception-with-its-status ()
  (with-ab-env ('(("AWS_BEARER_TOKEN_BEDROCK" . "k")))
    (with-cell-stop ((amazon-bedrock-start))
      (let ((nle::*provider* "amazon-bedrock") (nle::*model* "us.anthropic.claude-opus-5-5") (nle::*api-key* nil)
            (nle::*endpoint* nil))
        (with-temp-auth (auth "{}")
          (let ((nle::*auth-file-path* (pathname auth)))
            (with-stubbed-fdefinition
                (dex:post (asked &rest args)
                 (values (ab-octet-stream
                          (ab-event "messageStart" "{\"role\":\"assistant\"}")
                          (ab-frame '((":exception-type" . "throttlingException") (":content-type" . "application/json")
                                      (":message-type" . "exception"))
                                    "{\"message\":\"Too many requests\"}"))
                         200 (nlk:json-object "content-type" "application/vnd.amazon.eventstream")))
              (let ((condition (signals-error nle::provider-error
                                 (nodecode-amazon-bedrock::call-bedrock-streaming (user-context)))))
                (is (eql 429 (nle::provider-error-status condition)) "the shape's status, so retry reads it")
                (is (search "throttlingException: Too many requests" (nle::provider-error-detail condition)))))
            (with-stubbed-fdefinition
                (dex:post (asked &rest args)
                 (values (ab-octet-stream (ab-event "messageStart" "{\"role\":\"assistant\"}")
                                          (ab-event "messageStop" "{\"stopReason\":\"guardrail_intervened\"}"))
                         200 (nlk:json-object "content-type" "application/vnd.amazon.eventstream")))
              (is (search "blocked by Amazon Bedrock guardrail"
                          (nle::provider-error-detail
                           (signals-error nle::provider-error
                             (nodecode-amazon-bedrock::call-bedrock-streaming (user-context)))))))
            (with-stubbed-fdefinition
                (dex:post (asked &rest args)
                 (values (ab-octet-stream (ab-event "messageStart" "{\"role\":\"assistant\"}")
                                          (ab-event "contentBlockDelta" "{\"contentBlockIndex\":0,\"delta\":{\"text\":\"half\"}}"))
                         200 (nlk:json-object "content-type" "application/vnd.amazon.eventstream")))
              (signals-error nle::provider-stream-incomplete
                (nodecode-amazon-bedrock::call-bedrock-streaming (user-context))))))))))

(deftest amazon-bedrock-cell-retries-once-without-a-thought-bound-elsewhere ()
  (with-ab-env ('(("AWS_BEARER_TOKEN_BEDROCK" . "k")))
    (with-cell-stop ((amazon-bedrock-start))
      (let ((nle::*provider* "amazon-bedrock") (nle::*model* "us.anthropic.claude-opus-5-5") (nle::*api-key* nil)
            (nle::*endpoint* nil) (bodies '()))
        (with-temp-auth (auth "{}")
          (let ((nle::*auth-file-path* (pathname auth)))
            (with-stubbed-fdefinition
                (dex:post (endpoint &rest args)
                 (push (nlk:decode-json (sb-ext:octets-to-string (getf args :content) :external-format :utf-8)) bodies)
                 (if (= 1 (length bodies))
                     (error 'dex:http-request-failed
                            :body "{\"message\":\"messages.1.content.0: Invalid signature in thinking block. The block is bound to a different conversation\"}"
                            :status 400 :headers (nlk:json-object "content-type" "application/json")
                            :uri (quri:uri endpoint) :method :post)
                     (values (apply #'ab-octet-stream (ab-answer-frames)) 200
                             (nlk:json-object "content-type" "application/vnd.amazon.eventstream"))))
              (nodecode-amazon-bedrock::call-bedrock-streaming
               (compiled-context
                (list (nle::message "user" "first")
                      (nlk:json-object "role" "assistant" "content" "answer" "reasoning_content" "thought"
                                       "reasoning_signature" "sig-0")
                      (nle::message "user" "second")))))
            (setf bodies (reverse bodies))
            (is (= 2 (length bodies)) "one retry")
            (flet ((reasoning-p (body)
                     (some (lambda (block) (nlk:json-value block :object "reasoningContent"))
                           (coerce (nlk:json-value (aref (nlk:json-value body :array "messages") 1) :array "content") 'list))))
              (is (reasoning-p (first bodies)))
              (is (not (reasoning-p (second bodies))) "the retry carries no bound thought"))))))))

(deftest amazon-bedrock-cell-leaves-other-providers-alone ()
  (with-cell-stop ((amazon-bedrock-start))
    (let ((nle::*provider* "anthropic") (nle::*model* "claude-opus-5") (nle::*api-key* "sk-ant")
          (nle::*endpoint* nil) (url nil) (headers nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers))
           (values (make-truncated-sse-stream
                    "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"model\":\"k\",\"usage\":{\"input_tokens\":1}}}"
                    "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
                    "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
                    "{\"type\":\"message_stop\"}")
                   200))
        (nle::call-provider (user-context)))
      (is (equal "https://api.anthropic.com/v1/messages" url))
      (is (equal "sk-ant" (ab-header headers "x-api-key"))))))
