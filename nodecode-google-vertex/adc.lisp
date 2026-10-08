;;;; adc.lisp --- Application Default Credentials: the bearer every ADC round carries.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/ai/src/providers/
;;;; google-auth.ts, which replaces google-auth-library with a direct REST
;;;; implementation. The token comes from, in order:
;;;;
;;;;   1. GOOGLE_CLOUD_ACCESS_TOKEN or CLOUDSDK_AUTH_ACCESS_TOKEN, as it is
;;;;      (`gcloud auth print-access-token'), never cached
;;;;   2. a token cached from an earlier exchange, while it has more than the
;;;;      refresh skew (GOOGLE_VERTEX_REFRESH_SKEW_MS, else a minute) left
;;;;   3. the file GOOGLE_APPLICATION_CREDENTIALS names, else gcloud's user ADC
;;;;      (~/.config/gcloud/application_default_credentials.json; under
;;;;      %APPDATA%\gcloud on Windows):
;;;;        service_account               an RS256 JWT assertion exchanged at
;;;;                                      oauth2.googleapis.com/token
;;;;        authorized_user               its refresh token exchanged there
;;;;        impersonated_service_account  its source's token, then IAM
;;;;                                      Credentials' generateAccessToken
;;;;   4. the GCE / Cloud Run metadata server
;;;;
;;;; RS256 is RSASSA-PKCS1-v1_5 over SHA-256, done here in Lisp: the key's
;;;; PKCS#8 DER read for its modulus and exponents, the digest from the
;;;; core's own SHA-256, the exponentiation by the Chinese remainder theorem.

(in-package #:nodecode-google-vertex)

(defparameter +oauth-token-url+ "https://oauth2.googleapis.com/token")

(defparameter +metadata-token-url+
  "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token")

(defparameter +scope+ "https://www.googleapis.com/auth/cloud-platform")

(defparameter +jwt-grant+ "urn:ietf:params:oauth:grant-type:jwt-bearer")

(defparameter +exchange-seconds+ 30
  "How long one token exchange may take (omp's SHARED_TOKEN_RESOLVE_TIMEOUT_MS).")

(defparameter +metadata-seconds+ 2
  "How long the metadata server is given to answer.")

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- octets ------------------------------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "." (cl-base64:usb8-array-to-base64-string octets :uri t)))

(defun utf-8 (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun sha256-octets (octets)
  "The SHA-256 digest of OCTETS."
  (let ((hex (subseq (nlk:sha256-text octets) 7)))
    (coerce (loop for at from 0 below 64 by 2
                  collect (parse-integer hex :start at :end (+ at 2) :radix 16))
            '(vector (unsigned-byte 8)))))

(defun octets-integer (octets &key (start 0) (end (length octets)))
  "The unsigned big-endian integer OCTETS spell between START and END."
  (loop with value = 0
        for at from start below end
        do (setf value (logior (ash value 8) (aref octets at)))
        finally (return value)))

(defun integer-octets (value length)
  "VALUE as LENGTH big-endian octets."
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (loop for at from (1- length) downto 0
          for shift from 0 by 8
          do (setf (aref octets at) (ldb (byte 8 shift) value)))
    octets))

;;; --- an RSA private key ---------------------------------------------------------------

(defun der-element (octets at)
  "(values TAG CONTENT-START CONTENT-END) of the DER element at AT."
  (let ((tag (aref octets at))
        (first (aref octets (1+ at))))
    (if (< first #x80)
        (values tag (+ at 2) (+ at 2 first))
        (let* ((count (logand first #x7f))
               (length (octets-integer octets :start (+ at 2) :end (+ at 2 count))))
          (values tag (+ at 2 count) (+ at 2 count length))))))

(defun der-children (octets start end)
  "The elements between START and END, as (TAG CONTENT-START CONTENT-END) lists."
  (loop with at = start
        while (< at end)
        collect (multiple-value-bind (tag content-start content-end) (der-element octets at)
                  (setf at content-end)
                  (list tag content-start content-end))))

(defun pem-octets (pem)
  "The DER a PEM block carries: every armour line and space taken out
(omp's pemToPkcs8)."
  (let ((body (cl-ppcre:regex-replace-all "(?s)-----(BEGIN|END) [^-]+-----|\\s+" pem "")))
    (when (zerop (length body))
      (error 'nle::provider-config-error :detail "Invalid PEM: empty body"))
    (cl-base64:base64-string-to-usb8-array body)))

(defun rsa-private-key (pem)
  "The RSA key PEM holds, PKCS#8 (a service account's) or PKCS#1, as a plist
of its integers: :n :e :d :p :q :dp :dq :qinv."
  (let* ((der (handler-case (pem-octets pem)
                (error () (error 'nle::provider-config-error :detail "the service account's private_key is not a PEM key"))))
         (outer (multiple-value-list (der-element der 0)))
         (children (der-children der (second outer) (third outer))))
    ;; PKCS#8: version, algorithm, OCTET STRING holding the PKCS#1 key
    (when (and (= 3 (length children)) (= #x04 (first (third children))))
      (destructuring-bind (tag start end) (third children)
        (declare (ignore tag end))
        (let ((inner (multiple-value-list (der-element der start))))
          (setf children (der-children der (second inner) (third inner))))))
    (unless (and (>= (length children) 9) (every (lambda (child) (= #x02 (first child))) (subseq children 0 9)))
      (error 'nle::provider-config-error :detail "the service account's private_key is not an RSA key"))
    (destructuring-bind (version n e d p q dp dq qinv &rest more)
        (mapcar (lambda (child) (octets-integer der :start (second child) :end (third child))) children)
      (declare (ignore version more))
      (list :n n :e e :d d :p p :q q :dp dp :dq dq :qinv qinv))))

(defun mod-expt (base exponent modulus)
  "BASE to the EXPONENT, modulo MODULUS."
  (loop with result = 1
        with square = (mod base modulus)
        for rest = exponent then (ash rest -1)
        while (plusp rest)
        do (when (oddp rest) (setf result (mod (* result square) modulus)))
           (setf square (mod (* square square) modulus))
        finally (return result)))

(defparameter +sha256-digest-info+
  (coerce '(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05 #x00 #x04 #x20)
          '(vector (unsigned-byte 8)))
  "The DER DigestInfo prefix of a SHA-256 digest (RFC 8017 9.2).")

(defun modulus-octets (key)
  "How many octets KEY's modulus takes."
  (ceiling (integer-length (getf key :n)) 8))

(defun pkcs1-encoding (octets length)
  "EMSA-PKCS1-v1_5 of OCTETS' SHA-256 digest, LENGTH octets long."
  (let* ((digest (concatenate '(vector (unsigned-byte 8)) +sha256-digest-info+ (sha256-octets octets)))
         (padding (- length (length digest) 3)))
    (when (< padding 8)
      (error 'nle::provider-config-error :detail "the RSA key is too short for an RS256 signature"))
    (concatenate '(vector (unsigned-byte 8))
                 (vector 0 1) (make-array padding :initial-element #xff) (vector 0) digest)))

(defun rsa-sign (key octets)
  "The RSASSA-PKCS1-v1_5 SHA-256 signature of OCTETS under KEY, as octets."
  (let* ((length (modulus-octets key))
         (message (octets-integer (pkcs1-encoding octets length))))
    (destructuring-bind (&key p q dp dq qinv &allow-other-keys) key
      ;; the CRT form of message^d mod n
      (let* ((m1 (mod-expt message dp p))
             (m2 (mod-expt message dq q))
             (h (mod (* qinv (- m1 m2)) p)))
        (integer-octets (+ m2 (* h q)) length)))))

(defun service-account-assertion (credentials &optional (now (unix-seconds)))
  "The RS256 JWT a service account exchanges for a token (omp's
exchangeJwtForToken): issued by its client_email, for the cloud-platform
scope, to the token endpoint, good for an hour."
  (let* ((header (nlk:json-object "alg" "RS256" "typ" "JWT"
                                  :opt "kid" (nlk:json-value credentials :text "private_key_id")))
         (claims (nlk:json-object "iss" (nlk:json-value credentials :text "client_email")
                                  "scope" +scope+
                                  "aud" +oauth-token-url+
                                  "exp" (+ now 3600)
                                  "iat" now))
         (payload (format nil "~a.~a" (base64url (utf-8 (nlk:encode-json-object header)))
                          (base64url (utf-8 (nlk:encode-json-object claims))))))
    (format nil "~a.~a" payload
            (base64url (rsa-sign (rsa-private-key (nlk:json-value credentials :text "private_key"))
                                 (utf-8 payload))))))

;;; --- one exchange -------------------------------------------------------------------------

(defun body-string (body)
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun exchange (method url &key headers content (seconds +exchange-seconds+))
  "One exchange within SECONDS: (values JSON STATUS TEXT); STATUS NIL and TEXT
the transport fact when nothing answered."
  (handler-case
      (sb-sys:with-deadline (:seconds seconds)
        (multiple-value-bind (body status) (nlk:http method url :headers headers :content content
                                                                :timeout seconds :connect-timeout seconds)
          (let ((text (body-string body)))
            (values (ignore-errors (nlk:decode-json text)) status text))))
    ((or error sb-sys:deadline-timeout) (condition)
      (values nil nil (nle:transport-failure-label condition url)))))

(defun form (&rest pairs)
  (quri:url-encode-params (loop for (name value) on pairs by #'cddr collect (cons name value))
                          :space-to-plus t))

(defun token-answer (json status text what)
  "(values TOKEN EXPIRES-IN) out of an OAuth token answer, or a provider
error saying WHAT failed with the answer's STATUS and TEXT."
  (unless (and (integerp status) (< status 300) (nlk:json-value json :text "access_token"))
    (error 'nle::provider-error :status status :scope :request
                                :detail (format nil "~a failed (~a): ~a" what (or status "no answer")
                                                (subseq text 0 (min 300 (length text))))))
  (values (nlk:json-value json :text "access_token")
          (or (nlk:json-value json :number "expires_in") 0)))

(defun post-for-token (form)
  "POST FORM to Google's token endpoint: (values TOKEN EXPIRES-IN)."
  (multiple-value-bind (json status text)
      (exchange :post +oauth-token-url+ :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                                        :content form)
    (token-answer json status text "Google OAuth token exchange")))

(defun service-account-token (credentials)
  (post-for-token (form "grant_type" +jwt-grant+ "assertion" (service-account-assertion credentials))))

(defun authorized-user-token (credentials)
  (post-for-token (form "client_id" (nlk:json-value credentials :text "client_id")
                        "client_secret" (nlk:json-value credentials :text "client_secret")
                        "refresh_token" (nlk:json-value credentials :text "refresh_token")
                        "grant_type" "refresh_token")))

(defun rfc3339-seconds (text)
  "The epoch second an RFC 3339 time TEXT names (fractions dropped), or NIL."
  (ppcre:register-groups-bind ((#'parse-integer year month day hour minute second) nil zone sign zh zm)
      ("^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?(Z|([+-])(\\d{2}):(\\d{2}))$" (or text ""))
    (let ((offset (if (equal zone "Z")
                      0
                      (* (if (equal sign "-") -1 1)
                         (+ (* 3600 (parse-integer zh)) (* 60 (parse-integer zm)))))))
      (- (encode-universal-time second minute hour day month year 0)
         #.(encode-universal-time 0 0 0 1 1 1970 0)
         offset))))

(defun impersonated-token (credentials)
  "The token an impersonated_service_account file buys: its source's token,
then IAM Credentials' generateAccessToken for the target principal."
  (let* ((url (or (nlk:json-value credentials :text "service_account_impersonation_url") ""))
         (target (ppcre:register-groups-bind (principal) ("([^/]+):(?:generateAccessToken|generateIdToken)$" url)
                   principal))
         (source (nlk:json-value credentials :object "source_credentials")))
    (unless target
      (error 'nle::provider-config-error :detail (format nil "Cannot extract target principal from ~a" url)))
    (let ((source-token (if (equal (nlk:json-value source :string "type") "service_account")
                            (service-account-token source)
                            (authorized-user-token source))))
      (multiple-value-bind (json status text)
          (exchange :post (format nil "https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/~a:generateAccessToken"
                                  target)
                    :headers `(("Content-Type" . "application/json")
                               ("Authorization" . ,(format nil "Bearer ~a" source-token)))
                    :content (nlk:encode-json-object
                              (nlk:json-object "delegates" (or (nlk:json-value credentials :array "delegates") #())
                                               "scope" (vector +scope+)
                                               "lifetime" "3600s")))
        (unless (and (integerp status) (< status 300) (nlk:json-value json :text "accessToken"))
          (error 'nle::provider-error :status status :scope :request
                                      :detail (format nil "Google Impersonation token exchange failed (~a): ~a"
                                                      (or status "no answer") (subseq text 0 (min 300 (length text))))))
        (values (nlk:json-value json :text "accessToken")
                (max 0 (- (or (rfc3339-seconds (nlk:json-value json :string "expireTime")) 0) (unix-seconds))))))))

(defun metadata-token ()
  "(values TOKEN EXPIRES-IN) from the metadata server, or NIL when it does not answer."
  (multiple-value-bind (json status)
      (exchange :get +metadata-token-url+ :headers '(("Metadata-Flavor" . "Google")) :seconds +metadata-seconds+)
    (when (and (integerp status) (< status 300) (nlk:json-value json :text "access_token"))
      (values (nlk:json-value json :text "access_token") (or (nlk:json-value json :number "expires_in") 0)))))

;;; --- the sources ---------------------------------------------------------------------------

(defun user-adc-path ()
  "gcloud's user ADC file: under %APPDATA%\\gcloud on Windows, ~/.config/gcloud elsewhere."
  (let ((appdata (and (eq (uiop:operating-system) :windows) (nle::credential-env "APPDATA"))))
    (if appdata
        (merge-pathnames "gcloud/application_default_credentials.json" (uiop:ensure-directory-pathname appdata))
        (merge-pathnames ".config/gcloud/application_default_credentials.json" (user-homedir-pathname)))))

(defun explicit-token ()
  (or (nle::credential-env "GOOGLE_CLOUD_ACCESS_TOKEN") (nle::credential-env "CLOUDSDK_AUTH_ACCESS_TOKEN")))

(defun adc-file ()
  "(values SOURCE CREDENTIALS): the ADC file in force and its parsed JSON, or NIL."
  (alexandria:if-let (path (nle::credential-env "GOOGLE_APPLICATION_CREDENTIALS"))
    (let ((text (ignore-errors (uiop:read-file-string path))))
      (unless text
        (error 'nle::provider-config-error
               :detail (format nil "GOOGLE_APPLICATION_CREDENTIALS points to a missing file: ~a" path)))
      (values (format nil "gac:~a" path) (nlk:decode-json text)))
    (let* ((path (user-adc-path))
           (text (and (probe-file path) (ignore-errors (uiop:read-file-string path)))))
      (when text
        (values (format nil "user:~a" (namestring path)) (nlk:decode-json text))))))

(defun adc-source-p ()
  "Whether an ADC source is named without asking the network: an explicit
token, GOOGLE_APPLICATION_CREDENTIALS, or gcloud's user ADC file."
  (or (explicit-token) (nle::credential-env "GOOGLE_APPLICATION_CREDENTIALS")
      (probe-file (user-adc-path))))

(defvar *tokens* (make-hash-table :test 'equal)
  "ADC source -> (TOKEN . EXPIRES-AT), epoch seconds: omp's tokenCache.")

(defvar *tokens-lock* (bt2:make-lock :name "google-vertex adc")
  "Held across a cache read and the exchange that fills it, so concurrent
rounds share one exchange (omp's inflight).")

(defun refresh-skew ()
  "Seconds before expiry a cached token is replaced: GOOGLE_VERTEX_REFRESH_SKEW_MS, else a minute."
  (let ((ms (ignore-errors (parse-integer (or (nle::credential-env "GOOGLE_VERTEX_REFRESH_SKEW_MS") "")))))
    (if (and ms (plusp ms)) (/ ms 1000) 60)))

(defun forget-tokens ()
  (bt2:with-lock-held (*tokens-lock*) (clrhash *tokens*)))

(defun fresh-token (source credentials)
  "(values TOKEN EXPIRES-IN) for SOURCE, its CREDENTIALS NIL for the metadata server."
  (if (null credentials)
      (metadata-token)
      (let ((type (nlk:json-value credentials :string "type")))
        (cond ((equal type "service_account") (service-account-token credentials))
              ((equal type "authorized_user") (authorized-user-token credentials))
              ((equal type "impersonated_service_account") (impersonated-token credentials))
              (t (error 'nle::provider-config-error
                        :detail (format nil "~a holds ADC of type ~a, which Vertex AI cannot use"
                                        (subseq source (1+ (position #\: source))) (or type "none"))))))))

(defun access-token ()
  "The bearer an ADC round sends (omp's getVertexAccessToken)."
  (or (explicit-token)
      (bt2:with-lock-held (*tokens-lock*)
        (let ((now (unix-seconds)) (skew (refresh-skew)))
          (or (loop for source being the hash-keys of *tokens* using (hash-value cached)
                    when (> (- (cdr cached) skew) now) return (car cached)
                      else do (remhash source *tokens*))
              (multiple-value-bind (source credentials) (adc-file)
                (multiple-value-bind (token expires-in) (fresh-token (or source "metadata") credentials)
                  (unless token
                    (error 'nle::provider-config-error
                           :detail "Vertex AI requires Application Default Credentials. Set GOOGLE_APPLICATION_CREDENTIALS, run `gcloud auth application-default login`, or run on a GCE/Cloud Run instance with a service account; or save an API key with /connect or set GOOGLE_CLOUD_API_KEY for Gemini."))
                  (setf (gethash (or source "metadata") *tokens*) (cons token (+ (unix-seconds) (floor expires-in))))
                  token)))))))
