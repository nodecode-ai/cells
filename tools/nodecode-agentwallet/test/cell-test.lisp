;;;; cell-test.lisp --- lifecycle, the five tools, and /agentwallet, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches OpenClawCash,
;;;; needs an agent key or touches the network. What is proved is the cell's
;;;; own wiring: the section (a healthy start, a wrong value and a bad host
;;;; refused), the five tools through the seam the model calls them by -- their
;;;; method, url, headers, body and answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-agentwallet-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials OpenClawCash."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected agentwallet request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun agentwallet-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun agentwallet-reply (text &optional (status 200))
  "Answer every request with TEXT and STATUS."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun agentwallet-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun agentwallet-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun agentwallet-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "agentwallet-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "agentwallet" "AGENTWALLET-CELL-" :start nodecode-agentwallet:start-cell)

(define-cell-lifecycle-tests "agentwallet"
  (:fixture with-agentwallet-wire)
  (:config "agent_key" "occ-test")
  (:help :agentwallet)
  (:command "agentwallet")
  (:running
   (is (nle::find-tool "agentwalletapi_wallets_list") "list is registered")
   (is (nle::find-tool "agentwalletapi_wallet_get") "and get")
   (is (nle::find-tool "agentwalletapi_wallet_rename") "and rename")
   (is (nle::find-tool "agentwalletapi_wallet_create") "and create")
   (is (nle::find-tool "agentwalletapi_transfer_send") "and transfer, the original's five")
   (is (= 5 (count-if (lambda (entry) (eql 0 (search "agentwalletapi_" (first entry))))
                       nle::*tools*)) "five of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "agentwalletapi_wallets_list")) "and stop takes every one back")
   (is (null (nle::find-tool "agentwalletapi_transfer_send"))))
  (:refused ("timeout_seconds" 0)))

(deftest agentwallet-cell-a-start-with-no-key-refuses-and-installs-nothing (with-agentwallet-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (agentwallet-start))))
      (is-carrying text "needs an agent key" "OPENCLAWCASH_AGENT_KEY")
      (is (null (nle::find-tool "agentwalletapi_wallets_list")) "nothing was installed"))))

(deftest agentwallet-cell-a-base-url-outside-the-allowlist-refuses (with-agentwallet-wire)
  (let ((text (refusal-text nlk:config-refusal
                             (agentwallet-start "agent_key" "occ-test" "base_url" "https://evil.test"))))
    (is-carrying text "openclawcash.com" "is not")
    (is (null (nle::find-tool "agentwalletapi_wallets_list")) "and nothing was installed")))

(deftest agentwallet-cell-the-key-may-come-from-the-environment (with-agentwallet-wire)
  (with-stubbed-fdefinition (uiop:getenv (name)
                               (and (equal name "OPENCLAWCASH_AGENT_KEY") "occ-env"))
    (with-cell-stop ((agentwallet-start))
      (agentwallet-reply "{\"wallets\": []}")
      (agentwallet-call "agentwalletapi_wallets_list" "{}")
      (is (equal "occ-env"
                 (agentwallet-request-header (first (agentwallet-asked)) "x-agent-key")) "the exported key is the one sent"))))

;;; --- the wallets a key reaches ---------------------------------------------------

(deftest agentwallet-cell-list-gets-the-wallets-and-reads-them (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (agentwallet-reply
     "{\"wallets\": [{\"walletId\": \"w1\", \"label\": \"Treasury\", \"chain\": \"evm\", \"address\": \"0xabc\", \"balances\": {\"ETH\": \"0.5\"}}]}")
    (let ((answer (agentwallet-call "agentwalletapi_wallets_list" "{\"include_balances\": true}"))
          (request (first (agentwallet-asked))))
      (is (eq :get (first request)) "one GET")
      (is (equal "https://openclawcash.com/api/agent/wallets?includeBalances=true" (second request)) "to the plugin's wallets endpoint, balances asked for")
      (is (equal "occ-test" (agentwallet-request-header request "x-agent-key")) "with the section's key")
      (is (search "1 wallet" answer) "the count")
      (is (search "Treasury" answer) "the label")
      (is (search "0xabc" answer) "the address")
      (is (search "walletId w1" answer) "and the id a write needs")
      (is (search "ETH=0.5" answer) "and the balance"))))

(deftest agentwallet-cell-get-wants-exactly-one-selector (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (let ((answer (agentwallet-call "agentwalletapi_wallet_get"
                                     "{\"wallet_id\": \"w1\", \"wallet_label\": \"Treasury\"}")))
      (is (search "ERROR: " answer) "two selectors are a failure the model repairs")
      (is (search "exactly one" answer) "and the refusal says which")
      (is (null (agentwallet-asked)) "nothing was sent"))
    (agentwallet-reply
     "{\"wallet\": {\"walletId\": \"w1\", \"label\": \"Treasury\", \"chain\": \"evm\", \"address\": \"0xabc\"}}")
    (let ((answer (agentwallet-call "agentwalletapi_wallet_get" "{\"wallet_id\": \"w1\", \"chain\": \"evm\"}")))
      (is (equal "https://openclawcash.com/api/agent/wallet?walletId=w1&chain=evm"
                 (second (first (agentwallet-asked)))) "one wallet read, by id and chain")
      (is (search "Treasury" answer) "the wallet it answered"))))

(deftest agentwallet-cell-rename-patches-the-label (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (agentwallet-reply "{\"wallet\": {\"walletId\": \"w1\", \"label\": \"Ops\"}}")
    (let ((answer (agentwallet-call "agentwalletapi_wallet_rename" "{\"wallet_id\": \"w1\", \"label\": \"Ops\"}")))
      (let* ((request (first (agentwallet-asked)))
             (body (agentwallet-request-body request)))
        (is (eq :patch (first request)) "one PATCH")
        (is (equal "https://openclawcash.com/api/agent/wallet" (second request)) "to the wallet endpoint")
        (is (equal "w1" (nlk:json-value body :text "walletId")) "carrying the id the call named")
        (is (equal "Ops" (nlk:json-value body :text "label")) "and the new label"))
      (is (search "renamed wallet w1 to Ops" answer) "the answer says what changed"))))

;;; --- the two writes -----------------------------------------------------------------

(deftest agentwallet-cell-create-needs-a-passphrase-and-takes-it-from-the-environment (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (let ((answer (agentwallet-call "agentwalletapi_wallet_create" "{\"label\": \"New\"}")))
        (is (search "OPENCLAWCASH_EXPORT_PASSPHRASE" answer) "a missing passphrase is named by its variable")
        (is (null (agentwallet-asked)) "and nothing was sent")))
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (and (equal name "OPENCLAWCASH_EXPORT_PASSPHRASE")
                                    "correct-horse-battery"))
      (agentwallet-reply "{\"wallet\": {\"walletId\": \"w2\", \"label\": \"New\", \"chain\": \"evm\"}}")
      (let ((answer (agentwallet-call "agentwalletapi_wallet_create"
                                       "{\"label\": \"New\", \"network\": \"mainnet\"}")))
        (let* ((request (first (agentwallet-asked)))
               (body (agentwallet-request-body request)))
          (is (eq :post (first request)) "one POST")
          (is (equal "https://openclawcash.com/api/agent/wallets/create" (second request)) "to the create endpoint")
          (is (equal "correct-horse-battery" (nlk:json-value body :text "exportPassphrase")) "with the passphrase from the environment, never an argument")
          (is (equal "mainnet" (nlk:json-value body :text "network")) "and the network asked for"))
        (is (search "created wallet" answer) "the answer names the new wallet")
        (is (search "OPENCLAWCASH_EXPORT_PASSPHRASE" answer) "and where the passphrase lives")))))

(deftest agentwallet-cell-transfer-posts-the-amount-and-destination (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (agentwallet-reply "{\"transfer\": {\"status\": \"broadcast\", \"txHash\": \"0xdead\"}}")
    (let ((answer (agentwallet-call "agentwalletapi_transfer_send"
                                     "{\"wallet_id\": \"w1\", \"to\": \"0xfeed\", \"amount_display\": \"0.05\", \"chain\": \"evm\"}")))
      (let* ((request (first (agentwallet-asked)))
             (body (agentwallet-request-body request)))
        (is (eq :post (first request)) "one POST")
        (is (equal "https://openclawcash.com/api/agent/transfer" (second request)) "to the transfer endpoint")
        (is (equal "w1" (nlk:json-value body :text "walletId")) "from the wallet named by id")
        (is (equal "0xfeed" (nlk:json-value body :text "to")) "to the address given")
        (is (equal "0.05" (nlk:json-value body :text "amountDisplay")) "for the amount")
        (is (null (nlk:json-value body :text "valueBaseUnits")) "and no second amount"))
      (is (search "0xdead" answer) "the answer carries the transaction"))))

;;; --- refusals, and the operator's command -------------------------------------------

(deftest agentwallet-cell-a-refused-key-is-the-model-s-repair (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (agentwallet-reply "{\"message\": \"invalid agent key\"}" 401)
    (let ((answer (agentwallet-call "agentwalletapi_wallets_list" "{}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "HTTP 401" answer) "and names the status")
      (is (search "agent_key" answer) "and tells the model what to check"))))

(deftest agentwallet-cell-a-refused-transfer-carries-the-service-s-own-reason (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (agentwallet-reply "{\"message\": \"insufficient funds for this transfer\"}" 400)
    (let ((answer (agentwallet-call "agentwalletapi_transfer_send" "{\"wallet_id\": \"w1\", \"to\": \"0xfeed\"}")))
      (is (search "HTTP 400" answer) "the status is named")
      (is (search "insufficient funds" answer) "and the service's own words reach the model"))))

(deftest agentwallet-cell-the-command-says-the-connection-or-lists-the-wallets (with-agentwallet-wire)
  (with-cell-stop ((agentwallet-start "agent_key" "occ-test"))
    (let ((status (nodecode-agentwallet::run-command "" nil)))
      (is (search "https://openclawcash.com" status) "no argument reports the host")
      (is (search "agent key: set" status) "and that a key is there")
      (is (null (search "occ-test" status)) "without ever printing the key")
      (is (null (agentwallet-asked)) "and sends nothing"))
    (agentwallet-reply "{\"wallets\": [{\"walletId\": \"w1\", \"label\": \"Ops\", \"chain\": \"solana\"}]}")
    (let ((answer (nodecode-agentwallet::run-command " wallets " nil)))
      (is (search "Ops" answer) "an argument lists the wallets"))))
