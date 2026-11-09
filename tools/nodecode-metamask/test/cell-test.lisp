;;;; cell-test.lisp --- the tools, the command, the refusals, the real program.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed CLI: that its
;;;; start installs the ten tools, the command and the manual and its stop
;;;; takes them back; that each tool asks mm the argv the plugin asked and
;;;; hands back what the CLI answered; that a notice line before the envelope
;;;; rides beside it; that a refusal names the setting to fix; and that mm's
;;;; own error reaches the model in its own words. One test runs a real
;;;; /bin/sh program, for the argv a stub cannot prove. Nothing here needs mm
;;;; installed, a wallet, or a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "metamask"
  (:help :metamask)
  (:command "metamask")
  (:running (is (= 8453 (nodecode-metamask::setting :default-chain-id)) "Base by default")
            (is (equal "usd" (nodecode-metamask::setting :default-currency)) "prices in dollars")
            (is (= 90 (nodecode-metamask::setting :timeout-seconds)) "a call gets ninety seconds"))
  (:refused ("program" "definitely-no-such-mm-program-xyz") ("timeout_seconds" 0)
            ("default_chain_id" 0) ("default_currency" "dollars"))
  (:idle metamask:metamask-error (metamask:status)))

(deftest metamask-cell-a-program-that-names-nothing-refuses-the-start ()
  "The section's one requirement is an mm that exists: an empty name, a name
nothing answers to, and a fiat code that is no code each refuse the start."
  (is (signals-error nlk:config-refusal (metamask-start "program" "")) "an empty program")
  (is (signals-error nlk:config-refusal
        (metamask-start "program" "definitely-no-such-mm-program-xyz")) "a program that is not there")
  (is (signals-error nlk:config-refusal (metamask-start "default_currency" "dollars")) "a currency that is no fiat code"))

;;; --- status ------------------------------------------------------------------------

(deftest metamask-cell-status-reads-the-cli-s-own-view ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:answers (list (answer "doctor"
                                :stdout (nlk:encode-json-object
                                         (nlk:make-json-object
                                          "ok" t "data" (metamask-table
                                                           "authenticated" t "initialized" t
                                                           "hints" (vector "run mm skills add")))))
                        (answer "wallet address" :stdout (metamask-envelope "address" +address+))
                        (answer "trading-mode" :stdout (metamask-envelope "mode" "guard"))
                        (answer "init show" :stdout (metamask-envelope "wallet" "server-wallet"))
                        (answer "policy get" :stdout (metamask-envelope
                                                       "allowlist" (vector +address+)))))
      (let ((answer (call-tool "metamask_status" "include_policy" t)))
        (is (search "\"authenticated\":true" answer) "signed in, as the CLI said")
        (is (search +address+ answer) "with the address MetaMask reported")
        (is (search "guard" answer) "and its trading mode")
        (is (null (search "skills add" answer)) "the CLI's own skill hint is dropped")
        (is (search "allowlist" answer) "include_policy carried the outflow policy"))
      (with-metamask-stub
          (:answers (list (answer "doctor"
                                  :stdout (nlk:encode-json-object
                                           (nlk:make-json-object
                                            "ok" t "data" (metamask-table
                                                             "authenticated" t "initialized" t))))
                          (answer "wallet address" :stdout (metamask-envelope "address" +address+))
                          (answer "trading-mode" :stdout (metamask-envelope "mode" "guard"))
                          (answer "init show" :stdout (metamask-envelope "wallet" "server-wallet"))))
        (let ((answer (call-tool "metamask_status")))
          (is (null (search "allowlist" answer)) "without it, no policy read is paid for")
          (is (null (ran "policy get")) "and mm is never asked for one"))))))

(deftest metamask-cell-status-says-what-is-left-to-do-when-nothing-is-set-up ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub (:stdout (nlk:encode-json-object
                                   (nlk:make-json-object
                                    "ok" t "data" (metamask-table
                                                     "authenticated" nil "initialized" nil))))
      (let ((answer (call-tool "metamask_status")))
        (is (search "Not signed in" answer) "the next step is named")
        (is (null (ran "wallet address")) "and no wallet command is run while signed out")))))

;;; --- balance -----------------------------------------------------------------------

(deftest metamask-cell-balance-passes-the-plugin-s-argv-and-shows-what-the-wallet-holds ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:stdout (metamask-envelope
                  "chains" (vector (metamask-table
                                     "chainId" 8453
                                     "tokens" (vector (metamask-table "symbol" "USDC"
                                                                       "amount" "12.5"))))))
      (let ((answer (call-tool "metamask_balance" "chain_ids" (vector "8453") "currency" "eur")))
        (is (equal '("wallet" "balance" "--chain-ids" "8453" "--currency" "eur" "--json")
                   (last-args)) "the argv is the plugin's")
        (is (search "12.5" answer) "the balances are the answer"))
      (is (search "fiat code" (refusal (lambda () (call-tool "metamask_balance" "currency" "dollars")))) "a currency that is no code says so")
      (is (search "40-hex" (refusal (lambda () (call-tool "metamask_balance" "address" "0xnope")))) "and so does an address that is no address"))))

;;; --- market ------------------------------------------------------------------------

(deftest metamask-cell-a-native-price-needs-no-search ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:stdout (metamask-envelope "eip155:8453/slip44:60" (metamask-table "usd" 3120.5)))
      (let ((answer (call-tool "metamask_market" "symbol" "ETH")))
        (is (equal '("price" "spot" "--asset-ids" "eip155:8453/slip44:60" "--vs" "usd" "--json")
                   (last-args)) "the chain's own coin is a CAIP-19 id, not a search")
        (is (search "eip155:8453/slip44:60" answer) "and the price is the answer")
        (is (= 1 (length *runs*)) "one run, no lookup")))))

(deftest metamask-cell-a-token-symbol-is-resolved-by-the-cli-s-own-search ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:answers (list (answer "token list search"
                                :stdout (nlk:encode-json-object
                                         (nlk:make-json-object
                                          "ok" t "data" (vector (metamask-table "symbol" "USDC"
                                                                                  "address" +usdc+)))))
                        (answer "price spot" :stdout (metamask-envelope
                                                       "prices" (metamask-table "usd" 0.9998)))))
      (call-tool "metamask_market" "symbol" "USDC")
      (is (search (format nil "eip155:8453/erc20:~a" (string-downcase +usdc+))
                  (format nil "~{~a~^ ~}" (ran "price spot"))) "the contract address the search found is what gets priced")
      (is (search "--limit 10" (format nil "~{~a~^ ~}" (ran "token list search"))) "the search is bounded, as the plugin bounded it"))
    (with-metamask-stub (:stdout (metamask-envelope))
      (is (search "could not find"
                  (refusal (lambda () (call-tool "metamask_market" "symbol" "NOPE")))) "a symbol nothing answers to says so"))))

;;; --- history, quotes, signatures ----------------------------------------------------

(deftest metamask-cell-history-and-a-quote-ask-the-plugin-s-commands ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:answers (list (answer "tx history" :stdout (metamask-envelope "transactions" (vector)))
                        (answer "decode" :stdout (metamask-envelope "function" "transfer(address,uint256)"))))
      (call-tool "metamask_history" "limit" 5 "type" "out")
      (is (equal '("tx" "history" "--limit" "5" "--type" "out" "--json") (last-args)) "the list argv is the plugin's")
      (is (search "transfer" (call-tool "metamask_history" "action" "decode"
                                          "payload" "0xa9059cbb")) "and so is decode's")
      (is (search "64-hex" (refusal (lambda () (call-tool "metamask_history" "action" "tx"
                                                           "hash" "0xdead")))) "a hash that is no hash says so"))
    (with-metamask-stub
        (:answers (list (answer "swap quote" :stdout (metamask-envelope "quotes" (vector)))
                        (answer "swap execute" :stdout (metamask-envelope "status" "CONFIRMED"))))
      (call-tool "metamask_swap_quote" "from_token" "eth" "to_token" "usdc" "amount" "0.1")
      (is (equal '("swap" "quote" "--from" "ETH" "--to" "USDC" "--amount" "0.1"
                   "--from-chain-id" "8453" "--all-quotes" "--json")
                 (last-args)) "compare-only, on the default chain")
      (is (search "CONFIRMED" (call-tool "metamask_swap_execute" "quote_id" "quote-1")) "and a quote id executes in one argument"))))

(deftest metamask-cell-a-signature-carries-the-words-in-one-argument ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:stdout (metamask-envelope "signature" "0xdeadbeef"))
      (let ((answer (call-tool "metamask_sign" "message" "hello there" "chain_id" 1)))
        (is (member "--message=hello there" (last-args) :test #'string=) "the words are one argv element, never split")
        (is (search "0xdeadbeef" answer) "the signature is the answer"))
      (is (search "non-empty" (refusal (lambda () (call-tool "metamask_sign" "message" "")))) "an empty message says so")
      (is (search "EIP-712" (refusal (lambda () (call-tool "metamask_sign" "kind" "typed_data"
                                                              "payload" (nlk:make-json-object))))) "and so does typed data with nothing in it"))))

;;; --- the writes and what waits -----------------------------------------------------

(deftest metamask-cell-a-transfer-asks-for-the-plugin-s-argv-and-says-what-waits ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:stdout (metamask-envelope "status" "AWAITING_MFA" "pollingId" "req-7"))
      (let ((answer (call-tool "metamask_transfer" "to" +address+ "amount" "20" "token" "usdc")))
        (is (equal (list "transfer" "--to" +address+ "--amount" "20"
                         "--chain-id" "8453" "--token" "USDC" "--json")
                   (last-args)) "the transfer argv is the plugin's")
        (is (search "AWAITING_MFA" answer) "the status is carried")
        (is (search "req-7" answer) "and the request to watch is named"))
      (is (search "40-hex" (refusal (lambda () (call-tool "metamask_transfer"
                                                             "to" "vitalik.eth" "amount" "1")))) "an ENS name is refused rather than sent to a resolver")
      (is (search "positive decimal" (refusal (lambda () (call-tool "metamask_transfer"
                                                                     "to" +address+ "amount" "0")))) "and an amount of nothing says so")
      (is (search "amount" (refusal (lambda () (call-tool "metamask_transfer" "to" +address+)))) "a required member left out is refused before anything runs"))))

(deftest metamask-cell-watching-a-request-waits-as-long-as-asked ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:stdout (metamask-envelope "status" "CONFIRMED" "txHash" +tx-hash+))
      (let ((answer (call-tool "metamask_requests" "action" "watch"
                                "polling_id" "req-7" "timeout" 30)))
        (is (equal '("wallet" "requests" "watch" "req-7" "--wallet-timeout" "30" "--json")
                   (last-args)) "the watch argv is the plugin's")
        (is (equal 60 (third (last-run))) "and the run is given longer than the wallet's own wait")
        (is (search "CONFIRMED" answer) "the settled status is the answer"))
      (is (search "5 to 600" (refusal (lambda () (call-tool "metamask_requests" "action" "watch"
                                                               "polling_id" "req-7" "timeout" 5000)))) "a wait past ten minutes is refused"))))

;;; --- what mm says, and how ---------------------------------------------------------

(deftest metamask-cell-a-notice-line-before-the-envelope-is-kept-beside-it ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub
        (:stdout (format nil "{\"_notice\":{\"step\":\"mfa\"}}~%{\"ok\":true,\"data\":{\"status\":\"AWAITING_MFA\"}}"))
      (let ((answer (call-tool "metamask_requests")))
        (is (search "AWAITING_MFA" answer) "the envelope is the answer")
        (is (search "_notice" answer) "with the notice line beside it, not lost")))))

(deftest metamask-cell-the-cli-s-own-refusal-reaches-the-model-whole ()
  (with-cell-stop ((metamask-start))
    (with-metamask-stub (:stdout (metamask-refusal-json "MM_TIMEOUT" "mm did not answer" "retry once"))
      (let ((text (refusal (lambda () (metamask:balance)))))
        (is (search "MM_TIMEOUT" text) "its code rides it")
        (is (search "retry once" text) "and its hint")))
    (with-metamask-stub (:stdout "not json at all" :exit 2)
      (is (search "printed no JSON" (refusal (lambda () (metamask:balance)))) "output that is no JSON says so"))
    (with-metamask-stub (:timed-out t)
      (is (search "did not finish" (refusal (lambda () (metamask:balance)))) "and so does a call past its deadline"))))

;;; --- the real program --------------------------------------------------------------

(deftest metamask-cell-the-program-is-really-run-and-its-argv-carries-the-command ()
  (let ((directory (merge-pathnames "metamask-running/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (with-cell-stop ((metamask-start "program" (metamask-recording-program directory)))
        (let ((answer (metamask:balance)))
          (is (search "\"ok\"" answer) "the program's answer is the answer"))
        (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
          (is (search "wallet" argv) "it was asked for the wallet")
          (is (search "balance" argv) "and its balance")
          (is (search "--json" argv) "in JSON mode"))))))
