;;;; cell-test.lisp --- the perplexity cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every Perplexity endpoint a stubbed dex:post or
;;;; dex:get, the macOS app a stubbed UIOP:RUN-PROGRAM: nothing reaches
;;;; Perplexity, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "perplexity" "PERPLEXITY-CELL-" :start nodecode-perplexity:start-cell)

(define-cell-lifecycle-tests "perplexity"
  (:help :perplexity)
  (:command "perplexity")
  (:running (is (equal nodecode-perplexity::+primer+ (nle:help :perplexity)) "(help :perplexity) is the primer"))
  (:refused ("model" 5) ("api_model" 5))
  (:idle perplexity:perplexity-error (perplexity:search "x")))

;;; --- fixtures --------------------------------------------------------------------

(defun pplx-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun pplx-jwt (claims)
  "A JWT whose payload is the JSON text CLAIMS, unsigned."
  (format nil "h.~a.s" (string-right-trim "." (cl-base64:string-to-base64-string claims :uri t))))

(defun pplx-sse (&rest events)
  "EVENTS, JSON texts, as one server-sent stream."
  (format nil "~{data: ~a~%~%~}" events))

(defun pplx-cookie (jar name value)
  "Set NAME=VALUE in JAR, as a response's Set-Cookie would."
  (cl-cookie:merge-cookies jar (list (cl-cookie:make-cookie :name name :value value
                                                            :domain "www.perplexity.ai" :path "/"))))

(defmacro with-pplx ((auth &key (store "{}") env (config ())) &body body)
  "BODY with the cell started on CONFIG (section pairs), AUTH a temp auth.json
holding STORE, and the variables ENV (an alist) the only ones set."
  `(with-cell-stop ((perplexity-start ,@config))
     (with-temp-auth (,auth ,store)
       (let ((nle::*auth-file-path* ,auth))
         (with-stubbed-fdefinition (nle::credential-env (name) (cdr (assoc name ,env :test #'equal)))
           ,@body)))))

(defmacro with-pplx-wire ((asked &rest routes) &body body)
  "BODY with dex:post and dex:get answering by URL: ROUTES are (FRAGMENT
FORM), FORM run with URL, ARGS and JAR bound and answering (values TEXT
STATUS); ASKED collects (METHOD URL HEADERS CONTENT JAR), oldest first once
BODY reads it."
  (let* ((log (gensym "LOG"))
         (answer `(lambda (method url args)
                    (let ((jar (getf args :cookie-jar)))
                      (declare (ignorable jar))
                      (push (list method url (getf args :headers) (getf args :content) jar) ,log)
                      (cond ,@(loop for (fragment form) in routes
                                    collect `((search ,fragment url) ,form))
                            (t (error "unexpected request to ~a" url)))))))
    `(let ((,log '()))
       (flet ((answer (method url args) (funcall ,answer method url args)))
         (with-stubbed-fdefinitions ((dex:post (url &rest args) (answer :post url args))
                                     (dex:get (url &rest args) (answer :get url args)))
           (symbol-macrolet ((,asked (reverse ,log)))
             ,@body))))))

(defun pplx-body (content)
  "A request's JSON CONTENT, decoded."
  (nlk:decode-json content))

;;; --- the sign-in -----------------------------------------------------------------

(defparameter +pplx-session+ (pplx-jwt "{\"sub\":\"u1\",\"exp\":4102444800}")
  "A session token whose exp is 2100-01-01.")

(deftest perplexity-cell-signs-in-with-a-mailed-code ()
  (with-pplx (auth)
    (let ((otp-status 200))
      (with-pplx-wire (asked ("/api/auth/csrf" (progn (pplx-cookie jar "next-auth.csrf-token" "c1")
                                                      (values "{\"csrfToken\":\"csrf-1\"}" 200)))
                             ("/api/auth/signin-email" (values "{}" 200))
                             ("/api/auth/signin-otp"
                              (values (format nil "{\"token\":\"~a\",\"status\":\"success\"}" +pplx-session+)
                                      otp-status)))
        (let ((answer (cell-entry "nodecode-perplexity" "perplexity" "login op@example.com")))
          (is (search "mailed a sign-in code to op@example.com" answer))
          (is (search "/perplexity code CODE" answer)))
        (is (search "signed in as op@example.com" (cell-entry "nodecode-perplexity" "perplexity" "code 123456")))
        (let ((requests asked))
          (is (= 3 (length requests)) "a CSRF token, a mailed code, a verified code")
          (destructuring-bind (method url headers content jar) (first requests)
            (declare (ignore content jar))
            (is (eq :get method))
            (is (equal "https://www.perplexity.ai/api/auth/csrf" url))
            (is (equal "Perplexity/641 CFNetwork/1568 Darwin/25.2.0" (pplx-header headers "user-agent")))
            (is (equal "2.18" (pplx-header headers "x-app-apiversion"))))
          (destructuring-bind (method url headers content jar) (second requests)
            (declare (ignore method headers))
            (is (equal "https://www.perplexity.ai/api/auth/signin-email" url))
            (is (equal "op@example.com" (nlk:json-value (pplx-body content) :string "email")))
            (is (equal "csrf-1" (nlk:json-value (pplx-body content) :string "csrfToken")))
            (is (find "next-auth.csrf-token" (cl-cookie:cookie-jar-cookies jar) :key #'cl-cookie:cookie-name
                                                                               :test #'string=)
                "the CSRF exchange's cookies ride along"))
          (destructuring-bind (method url headers content jar) (third requests)
            (declare (ignore method headers))
            (is (equal "https://www.perplexity.ai/api/auth/signin-otp" url))
            (is (equal "123456" (nlk:json-value (pplx-body content) :string "otp")))
            (is (equal "csrf-1" (nlk:json-value (pplx-body content) :string "csrfToken")))
            (is (eq jar (fifth (first requests))) "one cookie jar for the whole sign-in")))))
    (let ((entry (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "perplexity")))
      (is (equal +pplx-session+ (nlk:json-value entry :string "access_token")))
      (is (equal "op@example.com" (nlk:json-value entry :string "email")))
      (is (= (- 4102444800 300) (nlk:json-value entry :integer "expires_at"))
          "the token's exp, less omp's five minutes"))
    (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring auth))))))
    (is (search "signed in as op@example.com" (cell-entry "nodecode-perplexity" "perplexity" "status")))
    (is (search "signed out" (cell-entry "nodecode-perplexity" "perplexity" "logout")))
    (is (null (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "perplexity")))))

(deftest perplexity-cell-answers-an-authenticator-challenge ()
  (with-pplx (auth)
    (with-pplx-wire (asked ("/api/auth/csrf" (values "{\"csrfToken\":\"csrf-1\"}" 200))
                           ("/api/auth/signin-email" (values "{}" 200))
                           ("/api/auth/signin-otp"
                            (values "{\"status\":\"totp_challenge_required\",\"challenge_token\":\"ch-1\"}" 200))
                           ("/api/auth/totp/challenge-verify"
                            ;; no token in the body: the session is left in the cookies
                            (progn (pplx-cookie jar "__Secure-next-auth.session-token" "cookie-session")
                                   (values "{}" 200))))
      (cell-entry "nodecode-perplexity" "perplexity" "login op@example.com")
      (is (search "authenticator code" (cell-entry "nodecode-perplexity" "perplexity" "code 111111")))
      (is (search "signed in" (cell-entry "nodecode-perplexity" "perplexity" "code 654321")))
      (destructuring-bind (method url headers content jar) (fourth asked)
        (declare (ignore method headers jar))
        (is (equal "https://www.perplexity.ai/api/auth/totp/challenge-verify" url))
        (is (equal "ch-1" (nlk:json-value (pplx-body content) :string "token")))
        (is (equal "654321" (nlk:json-value (pplx-body content) :string "code")))))
    (let ((entry (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "perplexity")))
      (is (equal "cookie-session" (nlk:json-value entry :string "access_token")))
      (is (null (nth-value 1 (gethash "expires_at" entry))) "a token with no exp never expires"))))

(deftest perplexity-cell-says-a-refused-code ()
  (with-pplx (auth)
    (with-pplx-wire (asked ("/api/auth/csrf" (values "{\"csrfToken\":\"csrf-1\"}" 200))
                           ("/api/auth/signin-email" (values "{}" 200))
                           ("/api/auth/signin-otp" (values "{\"text\":\"invalid code\"}" 400)))
      (is (search "no sign-in is waiting" (cell-entry "nodecode-perplexity" "perplexity" "code 1")))
      (cell-entry "nodecode-perplexity" "perplexity" "login op@example.com")
      (is (search "Perplexity OTP verification failed: invalid code"
                  (cell-entry "nodecode-perplexity" "perplexity" "code 000000")))
      (is (null (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens")) "nothing kept"))))

(deftest perplexity-cell-says-a-failed-csrf ()
  (with-pplx (auth)
    (with-pplx-wire (asked ("/api/auth/csrf" (values "" 503)))
      (is (search "Perplexity CSRF request failed: 503"
                  (cell-entry "nodecode-perplexity" "perplexity" "login op@example.com")))
      (is (null nodecode-perplexity::*pending*)))))

(deftest perplexity-cell-borrows-the-mac-apps-session ()
  (with-pplx (auth)
    (with-pplx-wire (asked)
      (with-stubbed-fdefinitions ((nodecode-perplexity::macos-p () t)
                                  (uiop:run-program (command &rest args)
                                   (is (equal '("defaults" "read" "ai.perplexity.mac" "authToken") command))
                                   (format nil "app-session~%")))
        (is (search "signed in" (cell-entry "nodecode-perplexity" "perplexity" "login")))
        (is (null asked) "no request: the app's session is taken as it is")))
    (is (equal "app-session" (nlk:json-value (nle::read-auth-file auth) :string
                                             "oauth_tokens" "perplexity" "access_token"))))
  (with-pplx (auth :config ("borrow_app_session" nil))
    (with-stubbed-fdefinitions ((nodecode-perplexity::macos-p () t)
                                (uiop:run-program (command &rest args) "app-session"))
      (is (search "usage: /perplexity login EMAIL" (cell-entry "nodecode-perplexity" "perplexity" "login"))
          "the section can say not to borrow it"))))

;;; --- the search ------------------------------------------------------------------

(defparameter +pplx-ask-stream+
  (pplx-sse
   "{\"blocks\":[{\"intended_usage\":\"web_results\",\"web_result_block\":{\"web_results\":[{\"name\":\"Alpha\",\"url\":\"https://a.example/\",\"snippet\":\"about alpha\",\"timestamp\":\"2025-01-01\"}]}}]}"
   "{\"blocks\":[{\"intended_usage\":\"ask_text_0_markdown\",\"markdown_block\":{\"chunks\":[\"Hello \",\"wor\"]}}]}"
   "{\"blocks\":[{\"intended_usage\":\"ask_text_0_markdown\",\"markdown_block\":{\"chunks\":[\"world\"],\"chunk_starting_offset\":1}}],\"display_model\":\"pplx_pro\",\"uuid\":\"q-1\",\"final\":true}"
   "{\"blocks\":[{\"intended_usage\":\"ask_text_0_markdown\",\"markdown_block\":{\"chunks\":[\"after the end\"]}}]}")
  "An ask stream: a source, an answer streamed in two chunks spliced at an
offset, the final snapshot, and an event after it that is never read.")

(deftest perplexity-cell-searches-with-the-signed-in-session ()
  (with-pplx (auth :store (format nil "{\"oauth_tokens\":{\"perplexity\":{\"access_token\":\"sess-1\"}}}")
                   :env '(("PERPLEXITY_API_KEY" . "pplx-key")))
    (with-pplx-wire (asked ("perplexity_ask" (values +pplx-ask-stream+ 200)))
      (let ((text (perplexity:search "what is alpha" :recency "week"
                                                     :domains '("github.com/anthropics" "-reddit.com" "github.com"))))
        (is (search "Hello world" text) "the chunks spliced at their offset")
        (is (not (search "after the end" text)) "nothing after the final snapshot")
        (is (search "1. Alpha" text))
        (is (search "https://a.example/" text))
        (is (search "the Perplexity sign-in, model pplx_pro" text)))
      (is (= 1 (length asked)) "the session answered; the key beside it is never tried")
      (destructuring-bind (method url headers content jar) (first asked)
        (declare (ignore method jar))
        (is (equal "https://www.perplexity.ai/rest/sse/perplexity_ask" url))
        (is (equal "__Secure-next-auth.session-token=sess-1" (pplx-header headers "cookie")))
        (is (null (pplx-header headers "authorization")) "the session rides as a cookie, never a bearer")
        (is (equal "2.18" (pplx-header headers "x-app-apiversion")))
        (is (equal "submit" (pplx-header headers "x-perplexity-request-reason")))
        (let* ((body (pplx-body content))
               (params (nlk:json-value body :object "params")))
          (is (equal "what is alpha" (nlk:json-value body :string "query_str")))
          (is (equal "experimental" (nlk:json-value params :string "model_preference")))
          (is (equal "copilot" (nlk:json-value params :string "mode")))
          (is (equalp #("web") (nlk:json-value params :array "sources")))
          (is (equal "week" (nlk:json-value params :string "search_recency_filter")))
          (is (equalp #("github.com" "-reddit.com") (nlk:json-value params :array "search_domain_filter"))
              "bare hosts, each once")
          (is (null (nth-value 1 (gethash "send_back_text_in_streaming_api" params)))))))))

(deftest perplexity-cell-lets-a-date-bound-outrank-recency ()
  (with-pplx (auth :store "{\"oauth_tokens\":{\"perplexity\":{\"access_token\":\"sess-1\"}}}"
                   :config ("model" "pplx_alpha"))
    (with-pplx-wire (asked ("perplexity_ask" (values +pplx-ask-stream+ 200)))
      (perplexity:search "q" :recency "week" :after "2025-03-01" :language "en-US")
      (let ((params (nlk:json-value (pplx-body (fourth (first asked))) :object "params")))
        (is (eq :null (gethash "search_recency_filter" params :missing)) "a bound present: recency is null")
        (is (equal "3/1/2025" (nlk:json-value params :string "search_after_date_filter")))
        (is (equalp #("en") (nlk:json-value params :array "search_language_filter")))
        (is (equal "pplx_alpha" (nlk:json-value params :string "model_preference")) "the section's model")))))

(deftest perplexity-cell-searches-with-an-api-key ()
  (with-pplx (auth :store "{\"api_keys\":{\"perplexity\":{\"provider\":\"perplexity\",\"key\":\"pplx-saved\"}}}"
                   :env '(("PERPLEXITY_API_KEY" . "pplx-env")))
    (with-pplx-wire (asked ("api.perplexity.ai"
                            (values (pplx-sse "{\"id\":\"r1\",\"model\":\"sonar-pro\",\"choices\":[{\"delta\":{\"content\":\"Par\"}}]}"
                                              "{\"choices\":[{\"delta\":{\"content\":\"is\"}}],\"citations\":[\"https://c.example\"],\"search_results\":[{\"title\":\"Capitals\",\"url\":\"https://c.example\",\"snippet\":\"about capitals\",\"date\":\"2025-02-02\"}],\"related_questions\":[\"What about Lyon?\"]}"
                                              "[DONE]")
                                    200)))
      (let ((text (perplexity:search "capital of France" :n 1)))
        (is (search "Paris" text))
        (is (search "1. Capitals" text))
        (is (search "https://c.example" text))
        (is (search "What about Lyon?" text))
        (is (search "the Perplexity API key, model sonar-pro" text)))
      (destructuring-bind (method url headers content jar) (first asked)
        (declare (ignore method jar))
        (is (equal "https://api.perplexity.ai/chat/completions" url))
        (is (equal "Bearer pplx-saved" (pplx-header headers "authorization")) "the key /connect saved first")
        (let ((body (pplx-body content)))
          (is (equal "sonar-pro" (nlk:json-value body :string "model")))
          (is (equal "capital of France"
                     (nlk:json-value (aref (nlk:json-value body :array "messages") 0) :string "content")))
          (is (eql 8192 (nlk:json-value body :integer "max_tokens")))
          (is (eq t (gethash "stream" body)))
          (is (equal "web" (nlk:json-value body :string "search_mode")))
          (is (equal "pro" (nlk:json-value body :string "web_search_options" "search_type")))
          (is (eq t (gethash "return_related_questions" body))))))))

(deftest perplexity-cell-tries-the-next-credential ()
  (with-pplx (auth :store "{\"oauth_tokens\":{\"perplexity\":{\"access_token\":\"sess-1\"}}}"
                   :env '(("PERPLEXITY_COOKIES" . "a=1; b=2")))
    (with-pplx-wire (asked ("perplexity_ask"
                            (if (equal "a=1; b=2" (pplx-header (getf args :headers) "cookie"))
                                (values "{\"error\":\"unauthorized\"}" 401)
                                (values +pplx-ask-stream+ 200))))
      (is (search "Hello world" (perplexity:search "q")))
      (is (equal '("a=1; b=2" "__Secure-next-auth.session-token=sess-1")
                 (mapcar (lambda (request) (pplx-header (third request) "cookie")) asked))
          "PERPLEXITY_COOKIES first, then the sign-in"))))

(deftest perplexity-cell-asks-anonymously-only-with-nothing-else ()
  (with-pplx (auth)
    (with-pplx-wire (asked ("perplexity_ask"
                            (values (pplx-sse "{\"text\":\"Sign up and repeat your request.\",\"final\":true}") 200)))
      (is (search "no sources (likely signup wall"
                  (refusal-text perplexity:perplexity-error (perplexity:search "q"))))
      (destructuring-bind (method url headers content jar) (first asked)
        (declare (ignore method url jar))
        (is (search "Mozilla/5.0" (pplx-header headers "user-agent")))
        (is (null (pplx-header headers "cookie")))
        (is (null (pplx-header headers "x-app-apiversion")))
        (is (eq t (nlk:json-value (pplx-body content) :boolean "params" "send_back_text_in_streaming_api")))))))

(deftest perplexity-cell-reads-an-answer-from-a-text-payload ()
  (with-pplx (auth :store "{\"oauth_tokens\":{\"perplexity\":{\"access_token\":\"sess-1\"}}}")
    (let ((payload (nlk:encode-json-object
                    (nlk:json-object "answer" "From the payload"
                                     "web_results" (vector (nlk:json-object "name" "Beta" "url" "https://b.example"))))))
      (with-pplx-wire (asked ("perplexity_ask"
                              (values (pplx-sse (nlk:encode-json-object
                                                 (nlk:json-object "text" payload "status" "COMPLETED")))
                                      200)))
        (let ((text (perplexity:search "q")))
          (is (search "From the payload" text))
          (is (search "https://b.example" text)))))))

(deftest perplexity-cell-says-a-stream-error-and-retries-a-dropped-socket ()
  (with-pplx (auth :store "{\"oauth_tokens\":{\"perplexity\":{\"access_token\":\"sess-1\"}}}")
    (with-pplx-wire (asked ("perplexity_ask"
                            (values (pplx-sse "{\"error_code\":\"RATE\",\"error_message\":\"slow down\"}") 200)))
      (is (search "Perplexity ask stream error: slow down"
                  (refusal-text perplexity:perplexity-error (perplexity:search "q")))))
    (let ((calls 0))
      (with-pplx-wire (asked ("perplexity_ask"
                              (if (= 1 (incf calls))
                                  (error "connection reset by peer")
                                  (values +pplx-ask-stream+ 200))))
        (is (search "Hello world" (perplexity:search "q")))
        (is (= 2 calls) "a socket dropped before any answer is tried once more")))))

(deftest perplexity-cell-refuses-bad-arguments ()
  (with-pplx (auth)
    (is (search "non-empty" (refusal-text perplexity:perplexity-error (perplexity:search "  "))))
    (is (search "recency must be one of" (refusal-text perplexity:perplexity-error
                                           (perplexity:search "q" :recency "decade"))))
    (is (search "YYYY-MM-DD" (refusal-text perplexity:perplexity-error
                                (perplexity:search "q" :before "2025/01/01"))))))
