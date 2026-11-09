;;;; cell-test.lisp --- lifecycle, the four tools, and /scalekit, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Scalekit, needs a
;;;; key or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, and a missing credential refused), the four
;;;; tools through the seam the model calls them by -- their url, their body,
;;;; their headers and their answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *routes* '()
  "What the stubbed NLK:HTTP answers: (URL-FRAGMENT TEXT STATUS) in the order
they were added, the first whose fragment the url holds winning.")

(defun scalekit-route (fragment text &optional (status 200))
  "Answer every request whose url holds FRAGMENT with TEXT at STATUS."
  (setf *routes* (append *routes* (list (list fragment text status)))))

(defun scalekit-answer (url)
  "What the route URL matches, as (values TEXT STATUS)."
  (let ((route (find-if (lambda (route) (search (first route) url)) *routes*)))
    (if route
        (values (second route) (third route))
        (error "unexpected scalekit request: ~a" url))))

(defmacro with-scalekit-wire (&body body)
  "BODY with NLK:HTTP stubbed: every request is logged in *REQUESTS* and
answered by the route it matches, so no test dials Scalekit."
  `(let ((*requests* '())
         (*routes* '()))
     (scalekit-route "oauth/token" "{\"access_token\": \"tok-1\", \"expires_in\": 3600}")
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (scalekit-answer url))
       ,@body)))

(defmacro with-scalekit-running (&body body)
  "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((scalekit-start "client_id" "skc-test"
                                     "client_secret" "test-secret"
                                     "environment_url" "https://env.test"))
     ,@body))

(defun scalekit-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun scalekit-request (fragment)
  "The newest stubbed request whose url holds FRAGMENT, or NIL."
  (find fragment *requests* :test (lambda (needle request) (search needle (second request)))))

(defun scalekit-request-to (url)
  "The newest stubbed request whose url is exactly URL."
  (find url *requests* :test #'equal :key #'second))

(defun scalekit-argument (request key)
  "REQUEST's argument KEY — :content, :headers, :timeout — or NIL."
  (getf (cddr request) key))

(defun scalekit-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (scalekit-argument request :content)))

(defun scalekit-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (scalekit-argument request :headers) :test #'string-equal)))

(defun scalekit-called (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "scalekit-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "scalekit" "SCALEKIT-CELL-" :start nodecode-scalekit:start-cell)

(define-cell-lifecycle-tests "scalekit"
  (:fixture with-scalekit-wire)
  (:config "client_id" "skc-test" "client_secret" "test-secret" "environment_url" "https://env.test")
  (:help :scalekit)
  (:command "scalekit")
  (:running
   (is (nle::find-tool "scalekit_list_connections") "the connections tool is registered")
   (is (nle::find-tool "scalekit_authorize") "and approve")
   (is (nle::find-tool "scalekit_execute_tool") "and execute")
   (is (nle::find-tool "scalekit_proxy") "and the proxy, the original's four")
   (is (= 4 (count-if (lambda (entry) (eql 0 (search "scalekit_" (first entry))))
                      nle::*tools*)) "four of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "scalekit_list_connections")) "and stop takes every one back")
   (is (null (nle::find-tool "scalekit_proxy"))))
  (:refused ("timeout_seconds" 0)
            ("max_chars" 10)))

(deftest scalekit-cell-a-start-with-no-credentials-refuses-and-installs-nothing (with-scalekit-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (scalekit-start))))
      (is-carrying text "client_id" "client_secret" "environment_url")
      (is (null (nle::find-tool "scalekit_list_connections")) "nothing was installed"))))

(deftest scalekit-cell-the-credentials-may-come-from-the-environment (with-scalekit-wire)
  (with-stubbed-fdefinition (uiop:getenv (name)
                                (cond ((equal name "SCALEKIT_CLIENT_ID") "skc-env")
                                      ((equal name "SCALEKIT_CLIENT_SECRET") "env-secret")
                                      ((equal name "SCALEKIT_ENVIRONMENT_URL") "https://env.test")
                                      (t nil)))
    (scalekit-route "connections/app" "{\"connections\": []}")
    (with-cell-stop ((scalekit-start))
      (scalekit-called "scalekit_list_connections" "{}")
      (let ((token-request (scalekit-request "oauth/token")))
        (is (equal "https://env.test/oauth/token" (second token-request)) "the exported environment is dialled")
        (let ((form (scalekit-argument token-request :content)))
          (is-carrying form
            "grant_type=client_credentials" "client_id=skc-env" "client_secret=env-secret"))))))

;;; --- the token, and the connections list ------------------------------------------

(deftest scalekit-cell-one-token-serves-every-call (with-scalekit-wire)
  (scalekit-route "connections/app" "{\"connections\": []}")
  (with-scalekit-running
    (scalekit-called "scalekit_list_connections" "{}")
    (scalekit-called "scalekit_list_connections" "{}")
    (is (= 1 (count-if (lambda (request) (search "oauth/token" (second request)))
                        (scalekit-asked))) "the token is fetched once and reused")))

(deftest scalekit-cell-list-connections-names-each-one (with-scalekit-wire)
  (scalekit-route
   "connections/app"
   (concatenate 'string
                "{\"connections\": ["
                "{\"key_id\": \"gmail-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"},"
                "{\"key_id\": \"github-connect\", \"type\": \"OAUTH\", \"status\": \"INITIAL\"}]}"))
  (with-scalekit-running
    (let ((answer (scalekit-called "scalekit_list_connections" "{}")))
      (let ((request (scalekit-request "connections/app")))
        (is (eq :get (first request)) "one GET")
        (is (equal "https://env.test/api/v1/connections/app?page_size=30&page_token="
                   (second request)) "to the environment's connections, one page")
        (is (equal "Bearer tok-1"
                   (scalekit-request-header request "authorization")) "with the token the environment issued"))
      (is-carrying answer
        "2 connections on https://env.test" "gmail-connect" "COMPLETED"
        "github-connect" "INITIAL"))))

;;; --- authorize --------------------------------------------------------------------

(deftest scalekit-cell-authorize-answers-active-when-the-account-is-connected (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"gmail-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"connected_account\": {\"status\": \"ACTIVE\"}}")
  (with-scalekit-running
    (let ((answer (scalekit-called "scalekit_authorize" "{\"connection_name\": \"gmail-connect\"}")))
      (is-carrying answer "gmail-connect is ACTIVE" "scalekit_execute_tool")
      (is (null (scalekit-request "magic_link")) "no link was minted for an account that works"))))

(deftest scalekit-cell-authorize-mints-the-link-an-unapproved-account-needs (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"gmail-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"message\": \"not found\"}" 404)
  (scalekit-route "connected_accounts/magic_link" "{\"link\": \"https://dash.test/approve/1\"}")
  (scalekit-route "connected_accounts" "{\"connected_account\": {\"status\": \"PENDING\"}}")
  (with-scalekit-running
    (let ((answer (scalekit-called "scalekit_authorize" "{\"connection_name\": \"gmail-connect\"}")))
      (is-carrying answer "not authorized yet" "https://dash.test/approve/1")
      (let ((created (scalekit-request-to "https://env.test/api/v1/connected_accounts")))
        (let ((identifier (nlk:json-value (scalekit-request-body created) :text "identifier")))
          (is (equal "usr_" (subseq identifier 0 4)) "the person is the id this machine keeps"))))))

(deftest scalekit-cell-a-connection-that-is-not-completed-is-the-operators-step (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"gmail-connect\", \"type\": \"OAUTH\", \"status\": \"INITIAL\"}]}")
  (with-scalekit-running
    (let ((answer (scalekit-called "scalekit_authorize" "{\"connection_name\": \"gmail-connect\"}")))
      (is-carrying answer "ERROR: " "gmail-connect connection is INITIAL" "dashboard")
      (is (null (scalekit-request "connected_accounts")) "and nothing was created"))))

;;; --- execute, and the proxy --------------------------------------------------------

(deftest scalekit-cell-execute-runs-the-named-tool-and-keeps-the-token-out (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"gmail-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"connected_account\": {\"status\": \"ACTIVE\"}}")
  (scalekit-route "api/v1/tools" "{\"tools\": [{\"tool_name\": \"gmail_send\"}]}")
  (scalekit-route "execute_tool"
                   "{\"result\": {\"ok\": true}, \"access_token\": \"provider-token\"}")
  (with-scalekit-running
    (let ((answer (scalekit-called
                   "scalekit_execute_tool"
                   "{\"connection_name\": \"gmail-connect\", \"tool_name\": \"gmail_send\", \"tool_input\": {\"to\": \"a.test\"}}")))
      (let* ((request (scalekit-request "execute_tool"))
             (body (scalekit-request-body request)))
        (is (eq :post (first request)) "one POST to execute_tool")
        (is (equal "gmail_send" (nlk:json-value body :text "tool_name")) "the tool the call named")
        (is (equal "gmail-connect" (nlk:json-value body :text "connector")) "on the connection")
        (is (equal "a.test" (nlk:json-value (nlk:json-value body :object "params") :text "to")) "with the input it was given"))
      (is-carrying answer "ok: true" (:absent "provider-token") (:absent "access_token")))))

(deftest scalekit-cell-execute-refuses-a-tool-the-environment-does-not-know (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"gmail-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"connected_account\": {\"status\": \"ACTIVE\"}}")
  (scalekit-route "api/v1/tools" "{\"tools\": []}")
  (with-scalekit-running
    (let ((answer (scalekit-called
                   "scalekit_execute_tool"
                   "{\"connection_name\": \"gmail-connect\", \"tool_name\": \"gmail_nope\"}")))
      (is-carrying answer "no tool named gmail_nope")
      (is (null (scalekit-request "execute_tool")) "and nothing was run"))))

(deftest scalekit-cell-proxy-answers-the-api-under-the-connection (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"github-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"connected_account\": {\"status\": \"ACTIVE\"}}")
  (scalekit-route "proxy/user" "{\"login\": \"octocat\"}")
  (with-scalekit-running
    (let ((answer (scalekit-called
                   "scalekit_proxy"
                   "{\"connection_name\": \"github-connect\", \"path\": \"/user\"}")))
      (let ((request (scalekit-request "proxy/user")))
        (is (eq :get (first request)) "GET unless the call names another method")
        (is (equal "github-connect" (scalekit-request-header request "connection_name")) "the connection rides its own header")
        (is (eql 0 (search "usr_" (scalekit-request-header request "identifier"))) "with the person id, usr_..."))
      (is-carrying answer "login: octocat"))))

(deftest scalekit-cell-proxy-says-when-the-catalog-does-not-cover-the-action (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"github-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"connected_account\": {\"status\": \"ACTIVE\"}}")
  (scalekit-route "proxy/unknown" "{\"error\": {\"code\": \"TOOL_PROXY_DISABLED\"}}")
  (with-scalekit-running
    (let ((answer (scalekit-called
                   "scalekit_proxy"
                   "{\"connection_name\": \"github-connect\", \"path\": \"/unknown\"}")))
      (is-carrying answer "does not cover this action"))))

(deftest scalekit-cell-proxy-wants-a-path-from-the-api-s-root (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"github-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"connected_account\": {\"status\": \"ACTIVE\"}}")
  (with-scalekit-running
    (let ((answer (scalekit-called
                   "scalekit_proxy"
                   "{\"connection_name\": \"github-connect\", \"path\": \"user\"}")))
      (is-carrying answer "ERROR: " "/user")
      (is (null (scalekit-request "proxy/")) "and nothing was sent"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest scalekit-cell-the-command-lists-or-approves (with-scalekit-wire)
  (scalekit-route "connections/app"
                   "{\"connections\": [{\"key_id\": \"gmail-connect\", \"type\": \"OAUTH\", \"status\": \"COMPLETED\"}]}")
  (scalekit-route "connected_accounts/auth" "{\"message\": \"not found\"}" 404)
  (scalekit-route "connected_accounts/magic_link" "{\"link\": \"https://dash.test/approve/2\"}")
  (scalekit-route "connected_accounts" "{\"connected_account\": {\"status\": \"PENDING\"}}")
  (with-scalekit-running
    (let ((listed (nodecode-scalekit::run-command "" nil)))
      (is-carrying listed "gmail-connect" "COMPLETED" "https://env.test"))
    (let ((approved (nodecode-scalekit::run-command "  gmail-connect  " nil)))
      (is-carrying approved "https://dash.test/approve/2"))))
