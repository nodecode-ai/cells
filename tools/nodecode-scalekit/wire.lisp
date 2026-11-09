;;;; wire.lisp --- the one network seam, the token, and the person id.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through REQUEST, so a test stubs the one
;;;; name and never touches the network. Scalekit answers HTTP status as a
;;;; value; anything but a 2xx is SCALEKIT-ERROR with the service's own
;;;; message in it, which is what the model repairs from.
;;;;
;;;; The three values the original plugin read out of its environment are the
;;;; section's here: client_id, client_secret and environment_url, and an
;;;; exported SCALEKIT_CLIENT_ID, SCALEKIT_CLIENT_SECRET or
;;;; SCALEKIT_ENVIRONMENT_URL says the same thing.

(in-package #:nodecode-scalekit)

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun client-id ()
  "The Scalekit client id the section names, env fallback included."
  (nonempty (setting :client-id)))

(defun client-secret ()
  "The Scalekit client secret the section names, env fallback included."
  (nonempty (setting :client-secret)))

(defun root ()
  "The environment's address, without a trailing slash."
  (string-right-trim "/" (or (nonempty (setting :environment-url)) "")))

(defun check-config ()
  "Refuse a section that cannot reach Scalekit, naming the member to set."
  (let ((missing (remove nil
                         (list (unless (client-id) "client_id")
                               (unless (client-secret) "client_secret")
                               (unless (plusp (length (root))) "environment_url")))))
    (when missing
      (error 'nlk:config-refusal
             :detail (format nil "scalekit: this cell needs ~{~a~^, ~}: copy them from the Scalekit ~
dashboard (Developers, Settings, API Credentials) into the scalekit section, or export ~
SCALEKIT_CLIENT_ID, SCALEKIT_CLIENT_SECRET and SCALEKIT_ENVIRONMENT_URL"
                              missing)))))

;;; --- the person id ---------------------------------------------------------------

(defun identifier-file ()
  "Where this machine's generated person id is kept."
  (nlk:home "scalekit/identifier"))

(defun stored-identifier ()
  "The person id kept from an earlier start, or NIL."
  (let ((path (identifier-file)))
    (and (probe-file path)
         (nonempty (nlk:trimmed (uiop:read-file-string path))))))

(defun ensure-identifier ()
  "The person id: the section's, else the one this machine generated once."
  (or (nonempty (setting :identifier))
      (stored-identifier)
      (let ((path (identifier-file))
            (created (format nil "usr_~(~8,'0x~)" (random (expt 16 8)))))
        (ensure-directories-exist path)
        (with-open-file (stream path :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
          (format stream "~a~%" created))
        created)))

(defun person-id (given)
  "The identifier a call names, else this machine's stored one."
  (or (nonempty given) (ensure-identifier)))

;;; --- the seam --------------------------------------------------------------------

(defun url-encode (text)
  "TEXT percent-encoded for a query string or a form body."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets text :external-format :utf-8)
          do (if (or (<= 48 byte 57) (<= 65 byte 90) (<= 97 byte 122)
                     (member byte '(45 46 95 126)))
                 (write-char (code-char byte) out)
                 (format out "%~2,'0X" byte)))))

(defun query-string (pairs)
  "PAIRS, (NAME . VALUE) ... with string values, as a query string; NIL
values leave their pair out."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in pairs
                when value collect (format nil "~a=~a" (url-encode name) (url-encode value)))))

(defun request (method url &key headers content timeout)
  "One exchange with Scalekit => (values BODY STATUS)."
  (let ((limit (or timeout (setting :timeout-seconds))))
    (nlk:http method url
              :headers (cons (cons "accept" "application/json") headers)
              :content content
              :timeout limit
              :connect-timeout (min 10 limit))))

(defun decode (text)
  "TEXT — an HTTP body — as a decoded value; an empty body is an empty object."
  (cond ((stringp text)
         (if (plusp (length (nlk:trimmed text)))
             (handler-case (nlk:decode-json text) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((or (hash-table-p text) (vectorp text)) text)
        (t (nlk:json-object))))

(defun body-error-text (text)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp text) (ignore-errors (nlk:decode-json text)))))
    (or (nlk:json-value (nlk:json-value body :object "error") :text "message")
        (nlk:json-value body :text "error_description")
        (nlk:json-value body :text "message")
        (nlk:json-value body :text "error")
        (nlk:json-value body :text "detail")
        (and (stringp text) (subseq text 0 (min 200 (length text)))))))

;;; --- the token -------------------------------------------------------------------

(defvar *token* nil
  "The access token this start fetched, or NIL. Cleared by STOP-CELL.")

(defvar *token-root* nil
  "The environment the cached token was issued by.")

(defun drop-token ()
  "Forget the cached access token: the next call fetches one."
  (setf *token* nil *token-root* nil))

(defun token ()
  "The environment's access token: the cached one, else client_credentials."
  (let ((root (root)))
    (if (and *token* (equal *token-root* root))
        *token*
        (let ((body nil) (status nil))
          (handler-case
              (multiple-value-setq (body status)
                (request :post (concatenate 'string root "/oauth/token")
                         :headers (list (cons "content-type" "application/x-www-form-urlencoded"))
                         :content (format nil "grant_type=client_credentials&client_id=~a&client_secret=~a"
                                          (url-encode (client-id))
                                          (url-encode (or (client-secret) "")))))
            ((or error sb-sys:deadline-timeout) (condition)
              (fail "scalekit: the token request failed: ~a"
                    (nle:transport-failure-label condition (concatenate 'string root "/oauth/token")))))
          (unless (and (integerp status) (< status 300))
            (fail "scalekit: the token endpoint answered HTTP ~a: ~a"
                  status (body-error-text body)))
          (let ((issued (nlk:json-value (decode body) :text "access_token")))
            (unless issued
              (fail "scalekit: the token endpoint answered no access_token; check client_id, client_secret and environment_url"))
            (setf *token* issued *token-root* root)
            issued)))))

;;; --- the API ---------------------------------------------------------------------

(defun call-once (method path &key query body headers timeout)
  "One authorized exchange with PATH on the environment => (values TEXT STATUS)."
  (let ((url (concatenate 'string (root) path
                          (if query (concatenate 'string "?" (query-string query)) "")))
        (payload (and body (nlk:encode-json-object body))))
    (handler-case
        (request method url
                 :headers (append (list (cons "authorization" (format nil "Bearer ~a" (token))))
                                  (when payload (list (cons "content-type" "application/json")))
                                  headers)
                 :content payload
                 :timeout timeout)
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "scalekit: ~a ~a failed: ~a" method path (nle:transport-failure-label condition url))))))

(defun api (method path &key query body headers timeout (allow nil))
  "METHOD PATH on the environment, one retry after a 401.

=> (values DECODED STATUS). A status outside 2xx and ALLOW signals
SCALEKIT-ERROR naming the status and what Scalekit said."
  (multiple-value-bind (text status)
      (call-once method path :query query :body body :headers headers :timeout timeout)
    (when (eql status 401)
      ;; The cached token aged out under us: fetch one and try once more.
      (drop-token)
      (multiple-value-setq (text status)
        (call-once method path :query query :body body :headers headers :timeout timeout)))
    (unless (or (and (integerp status) (< status 300)) (member status allow))
      (fail "scalekit: ~a ~a answered HTTP ~a: ~a" method path status (body-error-text text)))
    (values (decode text) status)))

;;; --- the endpoints the four tools read ---------------------------------------------

(defun all-connections ()
  "Every connection the environment holds, page by page."
  (loop with page = ""
        with found = '()
        do (let ((decoded (api :get "/api/v1/connections/app"
                                :query (list (cons "page_size" "30") (cons "page_token" page)))))
             (setf found (append found (coerce (or (nlk:json-value decoded :array "connections") #()) 'list))
                   page (or (nlk:json-value decoded :text "next_page_token") ""))
             (when (zerop (length page)) (return found)))))

(defun find-connection (name)
  "The connection NAMED (its key_id, as AgentKit, Connections shows it), or NIL."
  (find name (all-connections) :test #'string=
                                :key (lambda (connection) (nlk:json-value connection :text "key_id"))))

(defun connected-account-status (name identifier)
  "The account's status, or NIL when the environment holds none for it."
  (multiple-value-bind (decoded status)
      (api :get "/api/v1/connected_accounts/auth"
           :query (list (cons "connector" name) (cons "identifier" identifier))
           :allow '(404))
    (and (not (eql status 404))
         (nlk:json-value decoded :text "connected_account" "status"))))

(defun create-connected-account (name identifier)
  "Make the connected account and answer its status."
  (nlk:json-value (api :post "/api/v1/connected_accounts"
                       :body (nlk:make-json-object "connector" name
                                                   "identifier" identifier
                                                   "connected_account" (nlk:json-object)))
                  :text "connected_account" "status"))

(defun ensure-connected-account (name identifier)
  "The account's status, creating the connected account when none exists."
  (or (connected-account-status name identifier)
      (create-connected-account name identifier)))

(defun authorization-link (name identifier)
  "The link that approves NAME for IDENTIFIER, or NIL."
  (nlk:json-value (api :post "/api/v1/connected_accounts/magic_link"
                       :body (nlk:make-json-object "connector" name "identifier" identifier))
                  :text "link"))

(defun known-tool-p (tool-name)
  "Whether the environment knows a tool called TOOL-NAME."
  (let ((decoded (api :get "/api/v1/tools" :query (list (cons "filter.tool_name" tool-name)))))
    (plusp (length (or (nlk:json-value decoded :array "tools") #())))))

(defun execute-tool-request (tool-name connection identifier params)
  "Scalekit's execute_tool, decoded."
  (api :post "/api/v1/execute_tool"
       :body (nlk:make-json-object "tool_name" tool-name
                                   "connector" connection
                                   "identifier" identifier
                                   "params" (or params (nlk:json-object)))))

(defun method-keyword (name)
  "NAME — an HTTP method as the model writes it — as the keyword NLK:HTTP wants."
  (intern (string-upcase (nlk:trimmed (or name "GET"))) :keyword))

(defun proxy-request (connection identifier path method query params)
  "PATH on CONNECTION's API through Scalekit => (values DECODED STATUS)."
  (multiple-value-bind (text status)
      (request (method-keyword method) (concatenate 'string (root) "/proxy" path
                                   (if query (concatenate 'string "?" (query-string query)) ""))
               :headers (append (list (cons "authorization" (format nil "Bearer ~a" (token)))
                                      (cons "connection_name" connection)
                                      (cons "identifier" identifier))
                                (when params (list (cons "content-type" "application/json"))))
               :content (and params (nlk:encode-json-object params)))
    (values (decode text) status)))
