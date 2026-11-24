;;;; cell-test.lisp --- lifecycle, the four tools, /composio, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy start
;;;; with no key, a bad root or a member under its floor refused, an api_key_env
;;;; naming an empty variable refused), the account tools against a scripted
;;;; `composio' CLI -- its argv, the rows it answers and the rules the original
;;;; put on top of them -- the catalog against a scripted HTTP answer with the
;;;; API key in its header, the connect link that has to be Composio's own, and
;;;; a disconnect that is only claimed once the list says the account is gone.
;;;; Nothing here runs the CLI or reaches the network: both seams are stubs.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *composio-cli-calls* '()
  "Every stubbed CLI call of the test, (ARGV :SECONDS N :INPUT TEXT), newest first.")

(defvar *composio-cli-answers* '()
  "What the stubbed CLI answers, one (OUTPUT STATUS) per call, in order.")

(defvar *composio-http-calls* '()
  "Every stubbed HTTP request of the test, (METHOD URL . ARGS), newest first.")

(defvar *composio-http-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-composio-wire (&body body)
  "BODY with the CLI and NLK:HTTP stubbed: a call is logged, answered from the
test's script, and nothing runs on this machine or reaches Composio. UIOP:GETENV
answers nothing either, so a key in the environment cannot reach a test."
  `(let ((*composio-cli-calls* '())
         (*composio-cli-answers* '())
         (*composio-http-calls* '())
         (*composio-http-fn* (lambda (method url args)
                                (declare (ignore method url args))
                                (error "unexpected Composio request"))))
     (with-stubbed-fdefinitions
         ((nlk:run-bounded (argv &rest args)
                            (push (list* argv args) *composio-cli-calls*)
                            (let ((next (pop *composio-cli-answers*)))
                              (cond ((null next)
                                     (error "the composio CLI was called more times than the test scripted"))
                                    ((consp next) (values (first next) "" (second next)))
                                    (t (values next "" 0)))))
          (nlk:http (method url &rest args)
                    (push (list* method url args) *composio-http-calls*)
                    (funcall *composio-http-fn* method url args))
          (uiop:getenv (name)
                        (declare (ignore name))
                        nil))
       ,@body)))

(defun composio-cli-says (&rest answers)
  "Script the CLI: ANSWERS are an OUTPUT string (exit 0) or (OUTPUT STATUS), in order."
  (setf *composio-cli-answers* (copy-list answers)))

(defun composio-cli-calls ()
  "The stubbed CLI calls, oldest first."
  (reverse *composio-cli-calls*))

(defun composio-http-calls ()
  "The stubbed HTTP requests, oldest first."
  (reverse *composio-http-calls*))

(defun composio-answers-http (text &optional (status 200))
  (setf *composio-http-fn* (lambda (method url args)
                              (declare (ignore method url args))
                              (values text status))))

(defun composio-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun composio-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defparameter +gmail-account+
  "{\"gmail\": [{\"id\": \"ca_1\", \"status\": \"ACTIVE\", \"alias\": \"work\"}]}"
  "One connected gmail account, as `composio connections list' prints it.")

(defparameter +one-toolkit+
  "{\"items\": [{\"slug\": \"gmail\", \"name\": \"Gmail\", \"meta\": {\"description\": \"Read and send mail\", \"categories\": [{\"name\": \"email\"}]}, \"composio_managed_auth_schemes\": [\"oauth2\"], \"no_auth\": false}], \"total_items\": 1, \"next_cursor\": null}"
  "One catalog page, as GET /api/v3/toolkits answers it.")

;;; --- lifecycle ------------------------------------------------------------------

(define-test-slice "composio" "COMPOSIO-CELL-")

(defun composio-config (&rest pairs)
  "A whole shared config with one `composio' section built from PAIRS."
  (nlk:json-object "composio" (apply #'nlk:make-json-object pairs)))

(defun composio-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-composio:start-cell (apply #'composio-config pairs)))

(define-cell-lifecycle-tests "composio"
  (:fixture with-composio-wire)
  (:help :composio)
  (:command "composio")
  (:running (is (nle::find-tool "composio_accounts") "composio_accounts is registered")
            (is (nle::find-tool "composio_services") "composio_services is registered")
            (is (nle::find-tool "composio_connect") "composio_connect is registered")
            (is (nle::find-tool "composio_disconnect") "composio_disconnect is registered")
            (is (equal "composio" (nodecode-composio::setting :cli)) "the CLI's default name")
            (is (equal "https://backend.composio.dev" (nodecode-composio::setting :base-url)) "the catalog's default root")
            (is (null (nodecode-composio::setting :key)) "no key: the account tools still run"))
  (:stopped (is (null (nle::find-tool "composio_accounts")) "stop unregisters the tools")
            (is (null (nle::find-tool "composio_disconnect")) "all four of them"))
  (:refused ("base_url" "backend.composio.dev") ("timeout_seconds" 0) ("page_limit" 0))
  (:idle nodecode-composio:composio-error (nodecode-composio::run-command "" nil)))

(deftest composio-cell-a-named-but-empty-key-variable-refuses ()
  (with-composio-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                       (composio-start "api_key_env" "COMPOSIO_TEST_KEY")))
                 ("api_key_env names COMPOSIO_TEST_KEY" "the refusal names the variable")
                 ("API keys" "and says where a key comes from"))
    (is (null (nle::find-tool "composio_accounts")) "a refused start installed nothing")))

(deftest composio-cell-the-key-may-come-from-the-environment ()
  (with-composio-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (cond ((string= name "COMPOSIO_HOME_KEY") "k-env")
                                     ((string= name "COMPOSIO_USER_API_KEY") "k-fallback")
                                     (t nil)))
      (with-cell-stop ((composio-start "api_key_env" "COMPOSIO_HOME_KEY"))
        (is (equal "k-env" (nodecode-composio::setting :key)) "api_key_env names the variable"))
      (with-cell-stop ((composio-start))
        (is (equal "k-fallback" (nodecode-composio::setting :key)) "COMPOSIO_USER_API_KEY is read either way")))))

;;; --- composio_accounts -----------------------------------------------------------

(deftest composio-cell-accounts-come-from-the-cli ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says +gmail-account+)
      (is-carrying (answer (composio-tool "composio_accounts" (nlk:json-object)))
                   ("1 connected Composio account")
                   ("gmail (work) -- ACTIVE -- selector ca_1"))
      (let ((call (first (composio-cli-calls))))
        (is (equal '("composio" "connections" "list") (first call)) "the CLI's own list")
        (is (= 30 (getf (rest call) :seconds)) "the section's ceiling")))))

(deftest composio-cell-a-toolkit-filter-keeps-one-slug ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says "{\"gmail\": [{\"id\": \"ca_1\", \"status\": \"ACTIVE\"}], \"github\": [{\"id\": \"ca_2\", \"status\": \"ACTIVE\"}]}")
      (let ((text (composio-tool "composio_accounts" (nlk:json-object "toolkit" "github"))))
        (is-carrying text ("github") (:absent "ca_1" "gmail is filtered out"))
        (is (search "ca_2" text) "the kept account's selector")))))

(deftest composio-cell-a-selector-two-rows-share-is-not-removable ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says "{\"gmail\": [{\"id\": \"ca_1\", \"status\": \"ACTIVE\"}, {\"id\": \"ca_1\", \"status\": \"EXPIRED\"}]}"
                         "{\"gmail\": [{\"id\": \"ca_1\", \"status\": \"ACTIVE\"}, {\"id\": \"ca_1\", \"status\": \"EXPIRED\"}]}")
      (is-carrying (answer (composio-tool "composio_accounts" (nlk:json-object)))
                   ("no single selector of its own"))
      (is-carrying (answer (composio-tool "composio_disconnect"
                                          (nlk:json-object "selector" "ca_1" "confirm" t)))
                   ("ERROR:") ("names 2 accounts" "the refusal counts them")))))

(deftest composio-cell-a-signed-out-cli-says-how-to-sign-in ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says (list "You are not logged in. Run `composio login'." 1))
      (is-carrying (answer (composio-tool "composio_accounts" (nlk:json-object)))
                   ("ERROR: the composio CLI is not signed in")
                   ("composio login" "and names the command")))))

;;; --- composio_connect ------------------------------------------------------------

(deftest composio-cell-connect-answers-composio-s-link ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says "{}"
                         "{\"redirect_url\": \"https://connect.composio.dev/link/abc_1\"}")
      (is-carrying (answer (composio-tool "composio_connect"
                                          (nlk:json-object "toolkit" "gmail" "alias" "work")))
                   ("https://connect.composio.dev/link/abc_1"))
      (let ((call (second (composio-cli-calls))))
        (is (equal '("composio" "link" "gmail" "--no-browser" "--no-wait" "--alias" "work")
                   (first call)) "the CLI's link command, its argv never a shell line")))))

(deftest composio-cell-connect-reads-the-link-out-of-prose ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says "{}"
                         "Open https://connect.composio.dev/link/xyz-9 to continue.")
      (is-carrying (answer (composio-tool "composio_connect" (nlk:json-object "toolkit" "gmail")))
                   ("https://connect.composio.dev/link/xyz-9")))))

(deftest composio-cell-connect-refuses-any-other-link ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says "{}" "{\"redirect_url\": \"https://example.com/link/abc\"}")
      (is-carrying (answer (composio-tool "composio_connect" (nlk:json-object "toolkit" "gmail")))
                   ("ERROR:") ("connect.composio.dev" "only Composio's own host is taken")))))

(deftest composio-cell-a-second-account-needs-a-fresh-alias ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says +gmail-account+)
      (is-carrying (answer (composio-tool "composio_connect" (nlk:json-object "toolkit" "gmail")))
                   ("name this one with an alias" "an existing account needs an alias"))
      (with-cell-stop ((composio-start))
        (composio-cli-says +gmail-account+)
        (is-carrying (answer (composio-tool "composio_connect"
                                            (nlk:json-object "toolkit" "gmail" "alias" "WORK")))
                     ("already has an account aliased" "a taken alias, case-insensitively"))))))

(deftest composio-cell-a-bad-toolkit-is-refused-before-the-cli-runs ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says +gmail-account+)
      (is-carrying (answer (composio-tool "composio_connect"
                                          (nlk:json-object "toolkit" "gmail gmail")))
                   ("is no toolkit slug")))))

;;; --- composio_services ------------------------------------------------------------

(deftest composio-cell-services-read-the-catalog-with-the-key ()
  (with-composio-wire
    (with-cell-stop ((composio-start "api_key" "k-test"))
      (composio-answers-http +one-toolkit+)
      (is-carrying (answer (composio-tool "composio_services" (nlk:json-object "query" "mail")))
                   ("1 of 1 Composio toolkits")
                   ("Gmail (gmail) -- email")
                   ("Read and send mail"))
      (let ((request (first (composio-http-calls))))
        (is (= 1 (length (composio-http-calls))) "one page, one request")
        (is (eq :get (first request)) "a GET")
        (is (uiop:string-prefix-p "https://backend.composio.dev/api/v3/toolkits?limit=1000"
                                   (second request)) "the v3 toolkit catalog, paged")
        (is (equal "k-test" (composio-header request "x-user-api-key")) "the section's key, in Composio's own header")))))

(deftest composio-cell-a-catalog-read-is-reused ()
  (with-composio-wire
    (with-cell-stop ((composio-start "api_key" "k-cache"))
      (composio-answers-http +one-toolkit+)
      (composio-tool "composio_services" (nlk:json-object))
      (composio-tool "composio_services" (nlk:json-object))
      (is (= 1 (length (composio-http-calls))) "the second call reads the cache"))))

(deftest composio-cell-services-refuse-without-a-key ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (is-carrying (answer (composio-tool "composio_services" (nlk:json-object)))
                   ("ERROR: the toolkit catalog needs a Composio API key")
                   ("COMPOSIO_USER_API_KEY" "and names the variable"))
      (is (null (composio-http-calls)) "nothing was asked of Composio"))))

(deftest composio-cell-a-refused-key-is-not-a-catalog ()
  (with-composio-wire
    (with-cell-stop ((composio-start "api_key" "k-bad"))
      (composio-answers-http "{\"message\": \"Invalid API key\"}" 401)
      (is-carrying (answer (composio-tool "composio_services" (nlk:json-object)))
                   ("Composio refused the API key (HTTP 401)")))))

(deftest composio-cell-linked-keeps-only-what-is-connected ()
  (with-composio-wire
    (with-cell-stop ((composio-start "api_key" "k-test"))
      (composio-cli-says +gmail-account+ +gmail-account+)
      (composio-answers-http +one-toolkit+)
      (is-carrying (answer (composio-tool "composio_services"
                                          (nlk:json-object "linked" "connected")))
                   ("(connected)") ("Gmail"))
      (is-carrying (answer (composio-tool "composio_services"
                                          (nlk:json-object "linked" "unconnected")))
                   ("No Composio toolkit matches")))))

;;; --- composio_disconnect ----------------------------------------------------------

(deftest composio-cell-disconnect-is-verified-by-the-next-list ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says +gmail-account+ "removed" "{}")
      (is-carrying (answer (composio-tool "composio_disconnect"
                                          (nlk:json-object "selector" "ca_1" "confirm" t)))
                   ("Disconnected gmail (ca_1)"))
      (let ((remove (second (composio-cli-calls))))
        (is (equal '("composio" "connections" "remove" "ca_1") (first remove)) "the CLI's remove, by selector")
        (is (search "y" (getf (rest remove) :input)) "the terminal prompt is answered")))))

(deftest composio-cell-an-unconfirmed-disconnect-does-nothing ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (is-carrying (answer (composio-tool "composio_disconnect"
                                          (nlk:json-object "selector" "ca_1" "confirm" nil)))
                   ("set confirm to true"))
      (is (null (composio-cli-calls)) "nothing was asked of the CLI"))))

(deftest composio-cell-an-unverified-removal-is-not-claimed ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says +gmail-account+ "removed" +gmail-account+)
      (is-carrying (answer (composio-tool "composio_disconnect"
                                          (nlk:json-object "selector" "ca_1" "confirm" t)))
                   ("ERROR:") ("still appears" "the list is read again and disagrees")))))

;;; --- /composio --------------------------------------------------------------------

(deftest composio-cell-the-command-answers-accounts-or-a-link ()
  (with-composio-wire
    (with-cell-stop ((composio-start))
      (composio-cli-says +gmail-account+)
      (is-carrying (answer (nodecode-composio::run-command "" nil))
                   ("gmail (work) -- ACTIVE"))
      (with-cell-stop ((composio-start))
        (composio-cli-says "{}" "{\"redirect_url\": \"https://connect.composio.dev/link/abc_1\"}")
        (is-carrying (answer (nodecode-composio::run-command "gmail" nil))
                     ("https://connect.composio.dev/link/abc_1"))))))
