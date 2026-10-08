;;;; cell-test.lisp --- the google-vertex cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key and
;;;; ADC variable a stubbed NLE::CREDENTIAL-ENV, every ADC file a temp file
;;;; (the user ADC path a stubbed USER-ADC-PATH), every wire a stubbed
;;;; dex:post and every token exchange a stubbed dex:request: nothing touches
;;;; the network, the environment, gcloud's config or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "google-vertex" "GOOGLE-VERTEX-CELL-" :start nodecode-google-vertex:start-cell)

(define-cell-lifecycle-tests "google-vertex"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models 'nle::resolve-model-lane :credential
          'nle::google-request-body 'nle::anthropic-request-body 'nle::request-body 'nle::walk-provider-stream)
  (:refused ("project" 5) ("location" 7)))

(defparameter +gv-key+
  "-----BEGIN PRIVATE KEY-----
MIICdQIBADANBgkqhkiG9w0BAQEFAASCAl8wggJbAgEAAoGBALuAPnCd9P8eF5ai
QUMUJKLeumt0pntaRJQbX5bcZ+sHKicCopctqasRQnFXaQSq/g+f0nIBrveW8mgk
WY91xIjWo50GX5jVCFlv16Hil9ldvBAHzPCThO1MZPv5R26qau4z+PpsH5yxNy0Q
0XoM2+MLDGOcFZlIVMywId3Ns2hlAgMBAAECgYAgECsdSPWnVrcD7FUqqlwled75
FyaM+3+0sAWln6UpnL0JlLrHDWjxNu9cwGzV/oOZmmP7WOxZrHdhda3XCXWFAfY1
62fQYyrBH2/oFttAjabezNmZgaQF7hSi0Z1I45SIHacfvfj8G4MB7z2uQu+AM/zQ
uuwVfdTW2jtjPDTSYQJBAN7cIOhCl4fJ0qgpdK6XmhyG9BAzQZfj3jylW2YVBGU/
J2IdRONNR9utaV4JJsXu7v8PhHswghc35Ik21UEleI0CQQDXYhCyRzky24TVf+hi
C1y5ZZ3+w4veMgeLOWHMDOshQJ3EVGy1jd6WjseZTHW1w73aNncERurJiSqZrNgt
WBU5AkBiLSlWCFgG2tMxf6nGbETZAl6scFgaGKlDoDjmfKXGEI9B+tDpLZdVYEyF
v5RUKBEjTeu39UOqBNZp2D0UCPTVAkBa/4LAX9kpxJdtwLnE2roVVnqXTbUFbqvD
Rb4tAPRCu1MsxOKdHlCB2dc4zJYa8pV+4W4Nb4z5Eyvde6pmFgX5AkB39dUHvosG
8X3Rxzs9QMlK+tOYagU+kwzNOoSE0/BVtLe/1Dt60OL6WmlRqN1aAoG5OFVFPbuw
g9DTyRzg+Fbm
-----END PRIVATE KEY-----
"
  "A throwaway 1024-bit RSA key in PKCS#8, made for this test with openssl genpkey.")

(defparameter +gv-signature+
  "nnxrFvviIegABMSimB3lAUdxKwHHJxN2hZxwoFEoY573dRe/iMOY18UkJhb38FUwN8iXbMqFZYY1N+sR+lzPDSD9TcZuYsdFC/TtlCI4fj5y7vdfAsyviRiyJFn/fAiEHzakMVgnouZkwM+vIan0qgjxk8+GxfAEKEeFdLopJVY="
  "What `openssl dgst -sha256 -sign' made of the bytes eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJ0ZXN0In0
under +GV-KEY+: PKCS#1 v1.5 is deterministic, so this is a known answer.")

(defun gv-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun gv-octets (text) (sb-ext:string-to-octets text :external-format :utf-8))

(defun gv-base64url-decode (text)
  (cl-base64:base64-string-to-usb8-array
   (concatenate 'string text (make-string (mod (- (length text)) 4) :initial-element #\.)) :uri t))

(defun gv-service-account ()
  (nlk:encode-json-object
   (nlk:json-object "type" "service_account" "project_id" "p1" "private_key_id" "kid-1"
                    "private_key" +gv-key+ "client_email" "sa@p1.iam.gserviceaccount.com")))

(defun gv-stream (lane)
  "One short answer as LANE's wire streams it."
  (cond ((equal lane "anthropic")
         (make-truncated-sse-stream
          "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"model\":\"k\",\"usage\":{\"input_tokens\":1}}}"
          "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
          "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
          "{\"type\":\"message_stop\"}"))
        ((equal lane "google")
         (make-truncated-sse-stream
          "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"ok\"}]},\"finishReason\":\"STOP\"}]}"))
        (t
         (make-truncated-sse-stream
          "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
          "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
          "[DONE]"))))

(defmacro with-gv-round ((url headers body exchanges &key (env ''()) (section ''("project" "p1" "location" "us-central1"))
                                                        key (adc-path "/nonexistent/adc.json")
                                                        (token-answer "{\"access_token\":\"ya29.adc\",\"expires_in\":3599}"))
                         model &body forms)
  "FORMS with the cell started on SECTION and one round of MODEL captured:
URL, HEADERS and BODY (decoded) as dex:post saw them, EXCHANGES the (METHOD
URL HEADERS CONTENT) of every token exchange dex:request saw, each answered
TOKEN-ANSWER; the environment is ENV (an alist), the config tier's key KEY,
gcloud's user ADC file ADC-PATH."
  `(with-stubbed-fdefinitions ((nle::credential-env (name) (cdr (assoc name ,env :test #'equal)))
                               (nodecode-google-vertex::user-adc-path () (pathname ,adc-path)))
     (with-cell-stop ((apply #'google-vertex-start ,section))
       (let ((nle::*provider* "google-vertex") (nle::*model* ,model) (nle::*api-key* ,key)
             (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil) (,exchanges '()))
         (declare (ignorable ,url ,headers ,body ,exchanges))
         (with-stubbed-fdefinitions
             ((dex:post (asked &rest args)
               (setf ,url asked ,headers (getf args :headers)
                     ,body (nlk:decode-json (getf args :content)))
               (values (gv-stream (nle::resolve-model-lane "google-vertex" ,model)) 200))
              (dex:request (asked &rest args)
               (push (list (getf args :method) asked (getf args :headers) (getf args :content)) ,exchanges)
               (values ,token-answer 200)))
           (nle::call-provider (user-context)))
         (setf ,exchanges (reverse ,exchanges))
         ,@forms))))

;;; --- the catalog and the addresses ----------------------------------------------------

(deftest google-vertex-cell-puts-its-row-in-the-catalog ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((google-vertex-start "project" "p1" "location" "us-central1"))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "google-vertex")))
        (is (equal "Google Vertex AI" (nlk:json-value row :string "name")))
        (is (equal "https://us-central1-aiplatform.googleapis.com/v1/projects/p1/locations/us-central1/publishers/google"
                   (nlk:json-value row :string "api")))
        (is (= 34 (hash-table-count (nlk:json-value row :object "models"))))
        (loop for (model lane) in '(("gemini-3.1-pro-preview" "google")
                                    ("claude-opus-5-5@default" "anthropic")
                                    ("openai/gpt-oss-120b-maas" "openai-completions"))
              do (is (equal lane (nle::resolve-model-lane "google-vertex" model)) model))
        (is (find "gemini-3.8-flash" (nle::list-provider-models "google-vertex")
                  :key (lambda (row) (getf row :id)) :test #'equal)))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "google-vertex"))))))

(deftest google-vertex-cell-says-a-connect-key-was-not-checked ()
  ;; The roster answered /connect's key check with no reason, which the core
  ;; reads as a key Vertex took: any key read `works'. Vertex is asked
  ;; nothing, so the verdict is unchecked, even where an asked endpoint would
  ;; have refused the key.
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((google-vertex-start))
      (with-temp-file (nle::*provider-models-cache-path*)
        (let ((asked '()))
          (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                     (push url asked)
                                     (values "{\"error\":{\"code\":400,\"details\":[{\"reason\":\"API_KEY_INVALID\"}]}}" 400))
            (multiple-value-bind (verdict words) (nle::provider-key-check "google-vertex" "AIza-wrong")
              (is (eq :unchecked verdict))
              (is (search "first turn tries the key" words)))
            (is (null asked) "nothing was asked")
            (multiple-value-bind (rows reason) (nle::list-provider-models "google-vertex")
              (is rows)
              (is (null reason) "the picker's listing, with no key, is the roster as before"))))))))

(deftest google-vertex-cell-names-the-host-a-location-is-served-at ()
  (is (equal "aiplatform.googleapis.com" (nodecode-google-vertex::vertex-host "global")))
  (is (equal "aiplatform.eu.rep.googleapis.com" (nodecode-google-vertex::vertex-host "eu")))
  (is (equal "aiplatform.us.rep.googleapis.com" (nodecode-google-vertex::vertex-host "us")))
  (is (equal "europe-west4-aiplatform.googleapis.com" (nodecode-google-vertex::vertex-host "europe-west4"))))

;;; --- RS256 -----------------------------------------------------------------------------

(deftest google-vertex-cell-signs-the-known-rs256-answer ()
  (let ((key (nodecode-google-vertex::rsa-private-key +gv-key+)))
    (is (= 65537 (getf key :e)))
    (is (= 1024 (integer-length (getf key :n))))
    (is (equal +gv-signature+
               (cl-base64:usb8-array-to-base64-string
                (nodecode-google-vertex::rsa-sign key (gv-octets "eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJ0ZXN0In0"))))
        "the signature openssl makes of the same bytes")))

(deftest google-vertex-cell-asserts-a-service-account-as-omp-does ()
  (let* ((jwt (nodecode-google-vertex::service-account-assertion (nlk:decode-json (gv-service-account)) 1800000000))
         (parts (uiop:split-string jwt :separator "."))
         (header (nlk:decode-json (sb-ext:octets-to-string (gv-base64url-decode (first parts)) :external-format :utf-8)))
         (claims (nlk:decode-json (sb-ext:octets-to-string (gv-base64url-decode (second parts)) :external-format :utf-8)))
         (key (nodecode-google-vertex::rsa-private-key +gv-key+)))
    (is (= 3 (length parts)))
    (is (equal "RS256" (nlk:json-value header :string "alg")))
    (is (equal "kid-1" (nlk:json-value header :string "kid")))
    (is (equal "sa@p1.iam.gserviceaccount.com" (nlk:json-value claims :string "iss")))
    (is (equal "https://www.googleapis.com/auth/cloud-platform" (nlk:json-value claims :string "scope")))
    (is (equal "https://oauth2.googleapis.com/token" (nlk:json-value claims :string "aud")))
    (is (= 1800000000 (nlk:json-value claims :integer "iat")))
    (is (= 1800003600 (nlk:json-value claims :integer "exp")))
    (is (= (nodecode-google-vertex::octets-integer
            (nodecode-google-vertex::pkcs1-encoding (gv-octets (format nil "~a.~a" (first parts) (second parts)))
                                                    (nodecode-google-vertex::modulus-octets key)))
           (nodecode-google-vertex::mod-expt (nodecode-google-vertex::octets-integer (gv-base64url-decode (third parts)))
                                             (getf key :e) (getf key :n)))
        "the signature verifies under the public key")))

;;; --- the credential ----------------------------------------------------------------------

(deftest google-vertex-cell-reads-the-api-key-and-never-the-gemini-one ()
  (with-cell-stop ((google-vertex-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinitions ((nle::credential-env (name)
                                     (cdr (assoc name '(("GOOGLE_CLOUD_API_KEY" . "AIza-vertex") ("GOOGLE_API_KEY" . "AIza-gemini"))
                                                 :test #'equal)))
                                    (nodecode-google-vertex::user-adc-path () #p"/nonexistent/adc.json"))
          (let ((credential (nle::resolve-provider-credential "google-vertex" :auth-path auth :probe t)))
            (is (equal "AIza-vertex" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinitions ((nle::credential-env (name) (and (equal name "GOOGLE_API_KEY") "AIza-gemini"))
                                    (nodecode-google-vertex::user-adc-path () #p"/nonexistent/adc.json"))
          (is (eq :public (nle:credential-source (nle::resolve-provider-credential "google-vertex" :auth-path auth :probe t)))
              "the Gemini API's variable never reaches Vertex, and no ADC is named"))
        (with-temp-file (adc :contents "{\"type\":\"authorized_user\"}")
          (with-stubbed-fdefinitions ((nle::credential-env (name) nil)
                                      (nodecode-google-vertex::user-adc-path () (pathname adc)))
            (let ((credential (nle::resolve-provider-credential "google-vertex" :auth-path auth :probe t)))
              (is (eq :adc (nle:credential-source credential)) "gcloud's user ADC file is a source")
              (is (equal "vertex-adc" (nle:credential-key credential))))))))))

(deftest google-vertex-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((google-vertex-start))
    (with-temp-auth (auth "{\"api_keys\":{\"google-vertex\":{\"provider\":\"google-vertex\",\"key\":\"AIza-saved\"}}}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "GOOGLE_CLOUD_API_KEY") "AIza-env"))
          (is (equal "AIza-saved" (nle:credential-key (nle::resolve-provider-credential "google-vertex" :auth-path auth :probe t)))))))))

;;; --- the rounds ---------------------------------------------------------------------------

(deftest google-vertex-cell-sends-gemini-with-an-api-key-to-the-express-address ()
  (with-gv-round (url headers body exchanges :key "AIza-key" :section '()) "gemini-3.8-flash"
    (is (equal "https://aiplatform.googleapis.com/v1/publishers/google/models/gemini-3.8-flash:streamGenerateContent?alt=sse" url)
        "no project, and the global host when no location is named")
    (is (equal "AIza-key" (gv-header headers "x-goog-api-key")))
    (is (null (gv-header headers "authorization")))
    (is (null exchanges) "a key needs no token")
    (is (= 4 (length (nlk:json-value body :array "safetySettings"))))
    (is (every (lambda (setting) (equal "OFF" (nlk:json-value setting :string "threshold")))
               (nlk:json-value body :array "safetySettings")))))

(deftest google-vertex-cell-sends-gemini-with-a-service-account ()
  (with-temp-file (sa :contents (gv-service-account))
    (with-gv-round (url headers body exchanges :env `(("GOOGLE_APPLICATION_CREDENTIALS" . ,sa)))
        "gemini-3.1-pro-preview"
      (is (equal "https://us-central1-aiplatform.googleapis.com/v1/projects/p1/locations/us-central1/publishers/google/models/gemini-3.1-pro-preview:streamGenerateContent?alt=sse"
                 url))
      (is (equal "Bearer ya29.adc" (gv-header headers "Authorization")))
      (is (null (gv-header headers "x-goog-api-key")) "the marker never rides as a key")
      (is (= 1 (length exchanges)))
      (destructuring-bind (method asked exchange-headers content) (first exchanges)
        (declare (ignore exchange-headers))
        (let ((form (quri:url-decode-params content)))
          (is (eq :post method))
          (is (equal "https://oauth2.googleapis.com/token" asked))
          (is (equal "urn:ietf:params:oauth:grant-type:jwt-bearer" (cdr (assoc "grant_type" form :test #'equal))))
          (is (= 3 (length (uiop:split-string (cdr (assoc "assertion" form :test #'equal)) :separator ".")))))))))

(deftest google-vertex-cell-keeps-the-token-until-it-is-due ()
  (with-temp-file (sa :contents (gv-service-account))
    (with-stubbed-fdefinitions ((nle::credential-env (name)
                                 (and (equal name "GOOGLE_APPLICATION_CREDENTIALS") sa))
                                (nodecode-google-vertex::user-adc-path () #p"/nonexistent/adc.json"))
      (with-cell-stop ((google-vertex-start "project" "p1" "location" "global"))
        (let ((exchanges 0))
          (with-stubbed-fdefinition (dex:request (&rest args)
                                     (incf exchanges)
                                     (values "{\"access_token\":\"ya29.once\",\"expires_in\":3599}" 200))
            (is (equal "ya29.once" (nodecode-google-vertex::access-token)))
            (is (equal "ya29.once" (nodecode-google-vertex::access-token)))
            (is (= 1 exchanges) "one exchange for both")
            (setf (cdr (gethash (format nil "gac:~a" sa) nodecode-google-vertex::*tokens*))
                  (+ (nodecode-google-vertex::unix-seconds) 30))
            (nodecode-google-vertex::access-token)
            (is (= 2 exchanges) "a token within the skew is replaced")))))))

(deftest google-vertex-cell-refreshes-a-gcloud-user ()
  (with-temp-file (adc :contents "{\"type\":\"authorized_user\",\"client_id\":\"cid\",\"client_secret\":\"cs\",\"refresh_token\":\"1//rt\"}")
    (with-gv-round (url headers body exchanges :adc-path adc) "gemini-2.5-pro"
      (is (equal "Bearer ya29.adc" (gv-header headers "Authorization")))
      (let ((form (quri:url-decode-params (fourth (first exchanges)))))
        (is (equal "refresh_token" (cdr (assoc "grant_type" form :test #'equal))))
        (is (equal "1//rt" (cdr (assoc "refresh_token" form :test #'equal))))
        (is (equal "cid" (cdr (assoc "client_id" form :test #'equal))))
        (is (equal "cs" (cdr (assoc "client_secret" form :test #'equal))))))))

(deftest google-vertex-cell-impersonates-a-service-account ()
  (with-temp-file (adc :contents (nlk:encode-json-object
                                  (nlk:json-object
                                   "type" "impersonated_service_account"
                                   "service_account_impersonation_url"
                                   "https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/target@p1.iam.gserviceaccount.com:generateAccessToken"
                                   "delegates" (vector "d@p1.iam.gserviceaccount.com")
                                   "source_credentials" (nlk:json-object "type" "authorized_user" "client_id" "cid"
                                                                         "client_secret" "cs" "refresh_token" "rt"))))
    (with-gv-round (url headers body exchanges
                        :adc-path adc
                        :token-answer "{\"access_token\":\"ya29.source\",\"expires_in\":3599,\"accessToken\":\"ya29.target\",\"expireTime\":\"2099-01-01T00:00:00Z\"}")
        "gemini-2.5-flash"
      (is (equal "Bearer ya29.target" (gv-header headers "Authorization")) "the target's token rides")
      (is (= 2 (length exchanges)))
      (destructuring-bind (method asked exchange-headers content) (second exchanges)
        (is (eq :post method))
        (is (equal "https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/target@p1.iam.gserviceaccount.com:generateAccessToken"
                   asked))
        (is (equal "Bearer ya29.source" (gv-header exchange-headers "Authorization")))
        (let ((request (nlk:decode-json content)))
          (is (equal "3600s" (nlk:json-value request :string "lifetime")))
          (is (equal "d@p1.iam.gserviceaccount.com" (aref (nlk:json-value request :array "delegates") 0))))))))

(deftest google-vertex-cell-asks-the-metadata-server-last ()
  (with-gv-round (url headers body exchanges) "gemini-2.5-flash"
    (is (equal "Bearer ya29.adc" (gv-header headers "Authorization")))
    (destructuring-bind (method asked exchange-headers content) (first exchanges)
      (declare (ignore content))
      (is (eq :get method))
      (is (equal "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token" asked))
      (is (equal "Google" (gv-header exchange-headers "Metadata-Flavor"))))))

(deftest google-vertex-cell-takes-an-explicit-access-token ()
  (with-gv-round (url headers body exchanges :env '(("GOOGLE_CLOUD_ACCESS_TOKEN" . "ya29.explicit")))
      "claude-sonnet-4-6@default"
    (is (equal "Bearer ya29.explicit" (gv-header headers "Authorization")))
    (is (null exchanges) "no exchange")))

(deftest google-vertex-cell-sends-claude-to-its-raw-predict-address ()
  (with-gv-round (url headers body exchanges :key "AIza-key" :env '(("GOOGLE_CLOUD_ACCESS_TOKEN" . "ya29.explicit")))
      "claude-opus-5-5@default"
    (is (equal "https://us-central1-aiplatform.googleapis.com/v1/projects/p1/locations/us-central1/publishers/anthropic/models/claude-opus-5-5@default:streamRawPredict"
               url))
    (is (equal "Bearer ya29.explicit" (gv-header headers "Authorization")) "Claude rides ADC even beside a key")
    (is (null (gv-header headers "x-api-key")))
    (is (null (nlk:json-value body :any "model")) "the address names the model")
    (is (equal "vertex-2023-10-16" (nlk:json-value body :string "anthropic_version")))
    (is (null (nlk:json-value body :any "output_config")) "Vertex refuses an output effort")))

(deftest google-vertex-cell-sends-a-partner-model-to-the-openapi-endpoint ()
  (with-gv-round (url headers body exchanges :env '(("GOOGLE_CLOUD_ACCESS_TOKEN" . "ya29.explicit"))
                                             :section '("project" "p1" "location" "global"))
      "openai/gpt-oss-120b-maas"
    (is (equal "https://aiplatform.googleapis.com/v1/projects/p1/locations/global/endpoints/openapi/chat/completions" url))
    (is (equal "openai/gpt-oss-120b-maas" (nlk:json-value body :string "model")))
    (is (equal "Bearer ya29.explicit" (gv-header headers "Authorization")))
    (is (null (nlk:json-value body :any "max_tokens")))))

(deftest google-vertex-cell-reads-the-project-and-location-variables ()
  (with-gv-round (url headers body exchanges :section '()
                                             :env '(("GOOGLE_CLOUD_ACCESS_TOKEN" . "ya29.explicit")
                                                    ("GCLOUD_PROJECT" . "p9") ("VERTEX_LOCATION" . "eu")))
      "gemini-3.6-flash"
    (is (equal "https://aiplatform.eu.rep.googleapis.com/v1/projects/p9/locations/eu/publishers/google/models/gemini-3.6-flash:streamGenerateContent?alt=sse"
               url))))

(deftest google-vertex-cell-refuses-adc-without-a-project ()
  (with-stubbed-fdefinitions ((nle::credential-env (name) (and (equal name "GOOGLE_CLOUD_ACCESS_TOKEN") "ya29.x"))
                              (nodecode-google-vertex::user-adc-path () #p"/nonexistent/adc.json"))
    (with-cell-stop ((google-vertex-start "location" "us-central1"))
      (let ((nle::*provider* "google-vertex") (nle::*model* "gemini-2.5-pro") (nle::*api-key* nil)
            (nle::*endpoint* nil) (posted nil))
        (with-stubbed-fdefinition (dex:post (&rest args) (setf posted t) (values nil 500))
          (let ((condition (handler-case (progn (nle::call-google-streaming (user-context)) nil)
                             (nle::provider-error (condition) condition))))
            (is (typep condition 'nle::provider-config-error))
            (is (search "requires a project ID" (nle::provider-error-detail condition)))
            (is (not posted))))))))

(deftest google-vertex-cell-leaves-gemini-alone ()
  (with-cell-stop ((google-vertex-start "project" "p1" "location" "global"))
    (let ((nle::*provider* "google") (nle::*model* "gemini-3.8-flash") (nle::*api-key* "AIza-gemini")
          (nle::*endpoint* nil) (url nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (gv-stream "google") 200))
        (nle::call-provider (user-context)))
      (is (equal "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.8-flash:streamGenerateContent?alt=sse" url))
      (is (equal "AIza-gemini" (gv-header headers "x-goog-api-key")))
      (is (null (nlk:json-value body :any "safetySettings"))))))
