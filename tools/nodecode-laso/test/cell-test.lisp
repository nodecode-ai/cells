;;;; cell-test.lisp --- lifecycle, the two tools, the sign-in and /laso, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Laso, needs a key
;;;; or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start and a refused one), the two tools through the
;;;; seam the model calls them by, the device sign-in it shares with the
;;;; plugin, the confirm gate on the 19 money-moving tools, and the operator's
;;;; command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-laso-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Laso. No key is left over from another test."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected laso request"))))
     (ignore-errors (delete-file (nodecode-laso::key-file)))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defmacro with-no-env-key (&body body)
  "BODY with no LASO_API_KEY in the environment."
  `(with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
     ,@body))

(defun laso-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun laso-reply (text &optional (status 200))
  "Answer every request with TEXT and STATUS."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun laso-answers (&rest pairs)
  "Answer by route: each of PAIRS is (PATH-SUFFIX TEXT STATUS), and the first
whose path the request's url carries answers it."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url args))
          (let ((pair (or (loop for one in pairs when (search (first one) url) return one)
                          (first pairs))))
            (values (second pair) (third pair))))))

(defun laso-answer-text (answer)
  "ANSWER, a tool result, as the text the model reads."
  (nle::message-content answer))

(defun laso-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (laso-answer-text (execute-wire-call "laso-call" name (nlk:decode-json arguments))))

(defun laso-form (request)
  "REQUEST's x-www-form-urlencoded body, as an alist."
  (loop for pair in (uiop:split-string (or (getf (cddr request) :content) "")
                                        :separator '(#\&))
        when (plusp (length pair))
          collect (let ((at (position #\= pair)))
                    (cons (subseq pair 0 at) (subseq pair (1+ at))))))

(defun laso-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun laso-json-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "laso" "LASO-CELL-" :start nodecode-laso:start-cell)

(define-cell-lifecycle-tests "laso"
  (:fixture with-laso-wire)
  (:config "agent_name" "Nodecode")
  (:help :laso)
  (:command "laso")
  (:running
   (is (nle::find-tool "laso_tools") "the catalog tool is registered")
   (is (nle::find-tool "laso_call") "and the dispatcher")
   (is (= 2 (count-if (lambda (entry) (eql 0 (search "laso_" (first entry))))
                      nle::*tools*)) "two of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "laso_tools")) "and stop takes both back")
   (is (null (nle::find-tool "laso_call"))))
  (:refused ("timeout_seconds" 0)))

(deftest laso-cell-a-start-with-no-agent-name-refuses-and-installs-nothing (with-laso-wire)
  (with-no-env-key
    (let ((text (refusal-text nlk:config-refusal (laso-start))))
      (is-carrying text "agent_name" "laso section")
      (is (null (nle::find-tool "laso_tools")) "nothing was installed"))))

(deftest laso-cell-a-base-url-that-is-not-one-https-host-refuses (with-laso-wire)
  (with-no-env-key
    (let ((text (refusal-text nlk:config-refusal
                               (laso-start "agent_name" "Nodecode"
                                           "base_url" "https://evil.test/path"))))
      (is-carrying text "evil.test" "laso.finance")
      (is (null (nle::find-tool "laso_call")) "and nothing was installed"))))

;;; --- the catalog the model reads ---------------------------------------------------

(deftest laso-cell-tools-lists-the-catalog-and-says-what-needs-confirm (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode"))
      (let ((all (laso-call "laso_tools" "{}")))
        (is (search "66 of 66 tools match" all) "the whole catalog")
        (is (search "order_card - Order a USA prepaid card (needs confirm)" all) "a tool that moves money is marked")
        (is (search "get_account_balance - Get account balance" all) "a read-only one is not")
        (is (search "not connected yet" all) "and the connection state is said")
        (is (null (laso-asked)) "reading the catalog asks the server nothing"))
      (let ((found (laso-call "laso_tools" "{\"query\": \"gift card\"}")))
        (is (search "order_gift_card" found) "a search finds by title")
        (is (not (search "get_account_balance" found)) "and leaves out what does not match"))
      (let ((by-name (laso-call "laso_tools" "{\"query\": \"withdraw\"}")))
        (is (search "withdraw -" by-name) "and by name")
        (is (search "withdraw_card_balance" by-name))))))

;;; --- the device sign-in, the plugin's own flow ---------------------------------------

(deftest laso-cell-a-call-with-no-key-hands-over-a-link-and-a-code (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode"))
      (laso-reply
       "{\"device_code\": \"dev-1\", \"user_code\": \"KJ7-2PQ\",
         \"verification_uri_complete\": \"https://laso.finance/device?c=KJ7-2PQ\",
         \"interval\": 0, \"expires_in\": 600}")
      (let ((answer (laso-call "laso_call" "{\"tool\": \"get_account_balance\"}"))
            (request (first (laso-asked))))
        (is (search "https://laso.finance/device?c=KJ7-2PQ" answer) "the link is handed over verbatim")
        (is (search "KJ7-2PQ" answer) "with the code to check against the page")
        (is (search "laso.finance/oauth/device_authorization" (second request)) "the flow starts at the plugin's own endpoint")
        (is (search "clients%2Fhermes.json"
                    (cdr (assoc "client_id" (laso-form request)
                                :test #'string=))) "the client document Laso publishes")
        (is (= 1 (length (laso-asked))) "and nothing was called before the operator approved")))))

(deftest laso-cell-the-next-call-finishes-the-sign-in-and-the-one-after-runs (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode"))
      (laso-reply
       "{\"device_code\": \"dev-1\", \"user_code\": \"KJ7-2PQ\",
         \"verification_uri_complete\": \"https://laso.finance/device?c=KJ7-2PQ\",
         \"interval\": 0, \"expires_in\": 600}")
      (laso-call "laso_call" "{\"tool\": \"get_account_balance\"}")
      (laso-answers (cons "/oauth/token" (list "{\"access_token\": \"lasoak-kept\"}" 200)))
      (let ((answer (laso-call "laso_call" "{\"tool\": \"get_account_balance\"}")))
        (is (search "now connected" answer) "the poll call answers that the sign-in finished")
        (is (search "/oauth/token" (second (second (laso-asked)))) "the second call polls the token endpoint")
        (is (search "device_code"
                    (cdr (assoc "grant_type" (laso-form (second (laso-asked)))
                                :test #'string=))) "the device grant, url-encoded")
        (is (equal "lasoak-kept" (nodecode-laso::saved-key)) "and keeps the key for the next process"))
      (laso-answers (cons "/mcp" (list "{\"jsonrpc\": \"2.0\", \"id\": 1, \"result\":
         {\"content\": [{\"type\": \"text\", \"text\": \"balance: 42.00 USDC\"}]}}" 200)))
      (let ((answer (laso-call "laso_call" "{\"tool\": \"get_account_balance\"}"))
            (request (third (laso-asked))))
        (is (search "42.00 USDC" answer) "the third call runs and answers the tool's text")
        (is (equal "Bearer lasoak-kept" (laso-header request "authorization")) "carrying the approved key")
        (is (equal "tools/call" (nlk:json-value (laso-json-body request) :text "method")) "as one JSON-RPC tools/call")
        (is (equal "get_account_balance"
                   (nlk:json-value (laso-json-body request) :text "params" "name")) "for the tool asked for")))))

(deftest laso-cell-an-event-stream-answer-is-read (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode" "api_key" "lasoak-test"))
      (laso-reply
       (format nil "event: message~%data: {\"jsonrpc\": \"2.0\", \"id\": 1,~%data:  \"result\": {\"content\": [{\"type\": \"text\",~%data:  \"text\": \"streamed answer\"}]}}~%~%"))
      (let ((answer (laso-call "laso_call" "{\"tool\": \"get_version\"}")))
        (is (search "streamed answer" answer) "the last message event is the reply")))))

;;; --- the confirm gate the approval hook became -----------------------------------------

(deftest laso-cell-a-money-tool-wants-confirm-before-it-runs (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode" "api_key" "lasoak-test"))
      (let ((answer (laso-call "laso_call"
                                "{\"tool\": \"order_card\",
                                  \"arguments\": {\"amountUsdc\": 25, \"cardType\": \"visa\"}}")))
        (is (search "ERROR: " answer) "without it the call is a failure the model repairs")
        (is (search "moves money" answer) "and says what kind of tool it is")
        (is (search "amountUsdc=25" answer) "with the amount in front of the human")
        (is (search "confirm true" answer) "and how to go ahead")
        (is (null (laso-asked)) "nothing moved"))
      (laso-reply "{\"result\": {\"content\": [{\"type\": \"text\", \"text\": \"ordered\"}]}}")
      (let ((answer (laso-call "laso_call"
                                "{\"tool\": \"order_card\", \"confirm\": true,
                                  \"arguments\": {\"amountUsdc\": 25}}"))
            (request (first (laso-asked))))
        (is (search "ordered" answer) "with confirm true it runs")
        (is (eql 25 (nlk:json-value (laso-json-body request) :integer "params"
                                     "arguments" "amountUsdc")) "arguments reach the server")))))

(deftest laso-cell-an-unknown-tool-names-where-to-look (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode" "api_key" "lasoak-test"))
      (let ((answer (laso-call "laso_call" "{\"tool\": \"laso_order_card\"}")))
        (is (search "no Laso tool named" answer) "the plugin's prefix is the usual mistake")
        (is (search "laso_tools" answer) "so the answer points at the catalog")
        (is (null (laso-asked)) "nothing was sent")))))

(deftest laso-cell-a-key-from-the-environment-is-the-one-sent (with-laso-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "LASO_API_KEY") "lasoak-env"))
    (with-cell-stop ((laso-start "agent_name" "Nodecode"))
      (laso-reply "{\"result\": {\"content\": [{\"type\": \"text\", \"text\": \"ok\"}]}}")
      (laso-call "laso_call" "{\"tool\": \"get_version\"}")
      (is (equal "Bearer lasoak-env"
                 (laso-header (first (laso-asked)) "authorization")) "the exported key"))))

(deftest laso-cell-a-key-laso-rejects-is-dropped-and-the-next-call-signs-in (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode" "api_key" "lasoak-stale"))
      (laso-reply "{\"error\": \"invalid key\"}" 401)
      (let ((answer (laso-call "laso_call" "{\"tool\": \"get_version\"}")))
        (is (search "HTTP 401" answer) "the status is named")
        (is (search "dropped" answer) "and the key's fate")))))

;;; --- the operator's command -----------------------------------------------------------

(deftest laso-cell-the-command-reports-the-connection-and-drops-the-key (with-laso-wire)
  (with-no-env-key
    (with-cell-stop ((laso-start "agent_name" "Nodecode" "api_key" "lasoak-test"))
      (let ((status (nodecode-laso::run-command "" nil)))
        (is (search "https://laso.finance" status) "no argument reports the host")
        (is (search "agent name: Nodecode" status) "and the name the dashboard shows")
        (is (search "key: set in the section" status) "and that a key is there")
        (is (search "66, 19 of them need confirm" status) "and the size of the catalog"))
      (let ((out (nodecode-laso::run-command "signout" nil)))
        (is (search "signed out" out) "signout says so")
        (is (null nodecode-laso::*fresh-key*) "and the key this process held is gone")))))
