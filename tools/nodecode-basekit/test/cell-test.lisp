;;;; cell-test.lisp --- lifecycle, the six tools and /basekit, against a faked chain.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP: the RPC endpoints answer a small
;;;; fixed Base, DefiLlama answers two prices. Nothing reaches the network and
;;;; no key is needed. What is proved is the cell's own wiring: the section (a
;;;; healthy start, wrong values refused), each tool's answer over the seam the
;;;; model calls it by, the operator's command, and the one failure a public
;;;; endpoint gives.

(in-package #:nodecode.test)

;;; --- the faked chain ---------------------------------------------------------------

(defparameter +usdc+ "0x833589fcd6edb6e08f4c7c32d4f71b54bda02913")
(defparameter +weth+ "0x4200000000000000000000000000000000000006")
(defparameter +owner+ "0x1111111111111111111111111111111111111111")
(defparameter +spender-a+ "0x2222222222222222222222222222222222222222")
(defparameter +spender-b+ "0x3333333333333333333333333333333333333333")
(defparameter +transfer-topic+ "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")
(defparameter +approval-topic+ "0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925")
(defparameter +impl-slot+ "0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc")

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")
(defvar *rpc-status* 200
  "The HTTP status the faked Base endpoints answer with.")

(defun word (number)
  "NUMBER as one 32-byte ABI word, 0x-prefixed."
  (format nil "0x~64,'0x" number))

(defun abi-text (text)
  "TEXT as an ABI dynamic string, 0x-prefixed."
  (let* ((hex (format nil "~{~2,'0x~}" (map 'list #'char-code text)))
         (pad (mod (- 64 (mod (length hex) 64)) 64)))
    (concatenate 'string "0x" (subseq (word 32) 2) (subseq (word (length text)) 2)
                 hex (make-string pad :initial-element #\0))))

(defun log-of (topics data block)
  "One faked log of USDC at BLOCK."
  (nlk:make-json-object
   "address" +usdc+
   "topics" (coerce topics 'vector)
   "data" data
   "blockNumber" (format nil "0x~x" block)
   "logIndex" "0x0"
   "transactionHash" (concatenate 'string "0x" (make-string 64 :initial-element #\a))))

(defparameter +fake-code+
  (concatenate 'string "0x608060405234801561001057600080fd5b50"
               "40c10f19" "5c975abb" "f2fde38b" "00"))

(defun fake-eth-call (params)
  "The faked answer to one eth_call."
  (let* ((call (aref params 0))
         (to (string-downcase (or (nlk:json-value call :string "to") "")))
         (data (string-downcase (or (nlk:json-value call :string "data") "0x")))
         (body (if (>= (length data) 2) (subseq data 2) ""))
         (selector (subseq body 0 (min 8 (length body)))))
    (cond
      ((string= selector "70a08231")
       (if (string= to +usdc+) (word (* 25 (expt 10 6))) (word 0)))
      ((string= selector "313ce567") (word (if (string= to +usdc+) 6 18)))
      ((string= selector "95d89b41") (abi-text "USDC"))
      ((string= selector "06fdde03") (abi-text "USD Coin"))
      ((string= selector "18160ddd") (word (* 1000000 (expt 10 6))))
      ((string= selector "8da5cb5b") (word (parse-integer +owner+ :start 2 :radix 16)))
      ((string= selector "dd62ed3e")
       (let ((spender (if (>= (length body) 136) (subseq body 72 136) "")))
         (if (string= spender (subseq (word (parse-integer +spender-a+ :start 2 :radix 16)) 2))
             (word (ash 1 255))
             (word 0))))
      (t "0x"))))

(defun fake-logs (params)
  "The faked eth_getLogs answer for one filter."
  (let* ((filter (aref params 0))
         (topics (nlk:json-value filter :array "topics"))
         (first-topic (and topics (plusp (length topics)) (nlk:json-value (aref topics 0) :string)))
         (second-topic (and topics (> (length topics) 1) (nlk:json-value (aref topics 1) :string))))
    (cond
      ((and first-topic (string-equal first-topic +transfer-topic+) second-topic)
       (vector (log-of (list +transfer-topic+
                             (word (parse-integer +owner+ :start 2 :radix 16))
                             (word (parse-integer +spender-a+ :start 2 :radix 16)))
                       (word (* 5 (expt 10 6))) 4000)))
      ((string-equal (or first-topic "") +transfer-topic+)
       (vector (log-of (list +transfer-topic+
                             (word (parse-integer +spender-b+ :start 2 :radix 16))
                             (word (parse-integer +owner+ :start 2 :radix 16)))
                       (word (* 2 (expt 10 6))) 4001)))
      ((string-equal (or first-topic "") +approval-topic+)
       (vector (log-of (list +approval-topic+
                             (word (parse-integer +owner+ :start 2 :radix 16))
                             (word (parse-integer +spender-a+ :start 2 :radix 16)))
                       (word (ash 1 255)) 4000)
               (log-of (list +approval-topic+
                             (word (parse-integer +owner+ :start 2 :radix 16))
                             (word (parse-integer +spender-b+ :start 2 :radix 16)))
                       (word 7) 4001)))
      (t #()))))

(defun fake-rpc-result (method params)
  "The faked result of one JSON-RPC METHOD."
  (cond
    ((string= method "eth_blockNumber") "0x1000")
    ((string= method "eth_chainId") "0x2105")
    ((string= method "eth_gasPrice") "0x3b9aca0")
    ((string= method "eth_getBlockByNumber")
     (nlk:make-json-object "baseFeePerGas" "0x3b9aca0" "timestamp" "0x65f00000"
                            "transactions" (vector "0xaa" "0xbb")))
    ((string= method "eth_getBalance") "0xde0b6b3a7640000")
    ((string= method "eth_getCode") +fake-code+)
    ((string= method "eth_getStorageAt")
     (if (and (stringp (aref params 1)) (string-equal (aref params 1) +impl-slot+))
         (word (parse-integer +spender-a+ :start 2 :radix 16))
         (word 0)))
    ((string= method "eth_call") (fake-eth-call params))
    ((string= method "eth_getLogs") (fake-logs params))
    ((string= method "eth_getTransactionByHash")
     (nlk:make-json-object "from" +owner+ "to" +usdc+ "value" "0x2386f26fc10000"
                            "gasPrice" "0x3b9aca0" "nonce" "0x1" "input" "0xa9059cbb"))
    ((string= method "eth_getTransactionReceipt")
     (nlk:make-json-object "status" "0x1" "blockNumber" "0xfa0" "gasUsed" "0x5208"
                            "effectiveGasPrice" "0x3b9aca0" "contractAddress" :null
                            "logs" (vector
                                    (log-of (list +transfer-topic+
                                                  (word (parse-integer +owner+ :start 2 :radix 16))
                                                  (word (parse-integer +spender-a+ :start 2 :radix 16)))
                                            (word (* 5 (expt 10 6))) 4000))))
    (t "0x")))

(defun fake-rpc-answer (payload)
  "The faked body of one JSON-RPC batch."
  (let* ((decoded (nlk:decode-json payload))
         (calls (nlk:json-value decoded :array))
         (items (loop for call across (or calls #())
                      collect (nlk:make-json-object
                               "jsonrpc" "2.0"
                               "id" (or (nlk:json-value call :integer "id") 0)
                               "result" (fake-rpc-result (nlk:json-value call :string "method")
                                                         (nlk:json-array call "params"))))))
    (nlk:encode-json-object (coerce items 'vector))))

(defparameter +prices-json+
  (nlk:encode-json-object
   (nlk:make-json-object
    "coins" (nlk:make-json-object
             (concatenate 'string "base:" +usdc+)
             (nlk:make-json-object "price" 1.0 "symbol" "USDC" "decimals" 6)
             (concatenate 'string "base:" +weth+)
             (nlk:make-json-object "price" 2900.5 "symbol" "WETH" "decimals" 18))))
  "DefiLlama's answer to any price query: USDC at a dollar, WETH at 2900.50.")

(defmacro with-basekit-chain (&body body)
  "BODY with NLK:HTTP stubbed: Base's endpoints answer the faked chain, DefiLlama
the two prices, and every request is logged in *REQUESTS*."
  `(let ((*requests* '())
         (*rpc-status* 200))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (cond
                                  ((search "coins.llama.fi" url)
                                   (values +prices-json+ 200))
                                  ((search "coinbase.com" url)
                                   (values "{\"data\":{\"amount\":\"2900.50\"}}" 200))
                                  ((some (lambda (host) (search host url))
                                         '("base-rpc.publicnode.com" "mainnet.base.org"
                                           "base.drpc.org" "base.meowrpc.com"))
                                   (if (= *rpc-status* 200)
                                       (values (fake-rpc-answer (getf args :content)) 200)
                                       (values "upstream is down" *rpc-status*)))
                                  (t (values "{}" 404))))
       ,@body)))

(defun basekit-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun basekit-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "basekit-call" name arguments)))

(defun basekit-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))



;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "basekit" "BASEKIT-CELL-" :start nodecode-basekit:start-cell)

(define-cell-lifecycle-tests "basekit"
  (:fixture with-basekit-chain)
  (:help :basekit)
  (:command "basekit")
  (:running
   (is (nle::find-tool "base_network") "network is registered")
   (is (nle::find-tool "base_portfolio") "and portfolio")
   (is (nle::find-tool "base_tx") "and tx")
   (is (nle::find-tool "base_token") "and token")
   (is (nle::find-tool "base_activity") "and activity")
   (is (nle::find-tool "base_allowances") "and allowances, the original's six")
   (is (= 6 (count-if (lambda (entry) (eql 0 (search "base_" (first entry)))) nle::*tools*)) "six of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "base_network")) "and stop takes every one back")
   (is (null (nle::find-tool "base_allowances"))))
  (:refused ("rpc_url" "ftp://node.test")
            ("watch_address" "0x12")))

;;; --- network and portfolio -------------------------------------------------------

(deftest basekit-cell-network-reads-the-head-and-costs (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (let ((answer (basekit-call "base_network" "{\"detail\": \"full\"}")))
      (let* ((request (first (basekit-asked)))
             (body (basekit-request-body request)))
        (is (eq :post (first request)) "one POST")
        (is (search "base-rpc.publicnode.com" (second request)) "to the public Base endpoint")
        (is (member "eth_blockNumber" (loop for call across (nlk:json-value body :array)
                                            collect (nlk:json-value call :string "method"))
                    :test #'string=) "carrying the block-number read in its batch"))
      (is (search "block 4096" answer) "the answer names the block it read")
      (is (search "gas 0.062500 gwei" answer) "and the gas price in gwei")
      (is (search "base fee 0.062500 gwei" answer) "and the base fee it read from the block")
      (is (search "ETH $2900.50" answer) "and ETH priced from DefiLlama")
      (is (search "a plain transfer $" answer) "and what a transfer costs")
      (is (search "read-only" answer) "and says it read only"))))

(deftest basekit-cell-portfolio-balances-and-prices (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (let ((answer (basekit-call "base_portfolio"
                                 "{\"address\": \"0x1111111111111111111111111111111111111111\"}")))
      (is (search "wallet 0x1111111111111111111111111111111111111111" answer) "the wallet read")
      (is (search "ETH: 1.000000" answer) "native ETH from eth_getBalance")
      (is (search "USDC: 25.000000" answer) "the ERC-20 balance the chain answered")
      (is (search "$2925.50" answer) "totaled with the two prices")
      (is (search "prices from defillama" answer) "naming where the prices came from"))))

(deftest basekit-cell-a-bad-address-is-the-model-s-repair (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (let ((answer (basekit-call "base_portfolio" "{\"address\": \"0xnothex\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "40 hex digits" answer) "and says what an address is")
      (is (null (basekit-asked)) "and nothing was sent"))))

;;; --- receipts, tokens, transfers, approvals ---------------------------------------

(deftest basekit-cell-a-receipt-decodes-the-movement-it-emitted (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (let ((answer (basekit-call "base_tx"
                                 (format nil "{\"hash\": \"0x~64,'0x\"}" 255))))
      (is (search "success in block 4000" answer) "the receipt's status and block")
      (is (search "ERC-20 USDC 5.000000" answer) "the token movement, decoded from the log")
      (is (search "from 0x1111" answer) "its sender")
      (is (search "fee 0.000001312 ETH" answer) "the fee in ETH"))))

(deftest basekit-cell-token-facts-name-the-proxy-and-the-hints (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (let ((answer (basekit-call "base_token"
                                 (format nil "{\"address\": \"~a\"}" +usdc+))))
      (is (search "USDC (USD Coin), 6 decimals" answer) "name, symbol and decimals")
      (is (search "ERC-20" answer) "and the standard it answers")
      (is (search "proxy: eip1967" answer) "the implementation read from the EIP-1967 slot")
      (is (search "implementation 0x2222" answer) "and the implementation address")
      (is (search "bytecode hints" answer) "the selector hints, labelled")
      (is (search "mint(address,uint256)" answer) "including the ones the code carries")
      (is (search "source code: not_configured" answer) "and no claim without a Basescan key"))))

(deftest basekit-cell-activity-and-allowances-state-their-window (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (let ((activity (basekit-call "base_activity"
                                   (format nil "{\"address\": \"~a\", \"token\": \"~a\", \"blocks\": 5000}"
                                           +owner+ +usdc+))))
      (is (search "blocks 0 to 4096" activity) "the window read, from the head it fetched")
      (is (search "out 5.000000" activity) "the outgoing transfer, newest first")
      (is (search "in 2.000000" activity) "and the incoming one")
      (is (search "tx 0xaaaa" activity) "with the hash to look up"))
    (let ((approvals (basekit-call "base_allowances"
                                    (format nil "{\"owner\": \"~a\", \"token\": \"~a\"}"
                                            +owner+ +usdc+))))
      (is (search "1 active" approvals) "only the allowance still live")
      (is (search "unlimited" approvals) "and that it is unlimited")
      (is (search "0x2222222222222222222222222222222222222222" approvals) "the spender named")
      (is (null (search "0x3333333333333333333333333333333333333333" approvals)) "the revoked one dropped, because the allowance was re-read"))))

(deftest basekit-cell-a-dead-endpoint-is-one-error-line (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (setf *rpc-status* 503)
    (let ((answer (basekit-call "base_network" "{}")))
      (is (search "ERROR: " answer) "a failure is the tool result")
      (is (search "HTTP 503" answer) "naming what every endpoint answered")
      (is (search "mainnet.base.org" answer) "and that the fallbacks were tried")
      (is (= 4 (length (basekit-asked))) "one POST per endpoint in the list"))))

;;; --- the operator's command ------------------------------------------------------

(deftest basekit-cell-the-command-reads-status-and-a-wallet (with-basekit-chain)
  (with-cell-stop ((basekit-start "watch_address" +owner+))
    (let ((status (nodecode-basekit::run-command "" nil)))
      (is (search "Base (chain 8453) at block 4096" status) "no argument reads the network summary")
      (is (search "gas 0.062500 gwei" status) "with the gas price")
      (is (null (search "rpc " status)) "and not the endpoint, which full answers"))
    (let ((full (nodecode-basekit::run-command "full" nil)))
      (is (search "rpc base-rpc.publicnode.com" full) "full names the endpoint"))
    (let ((wallet (nodecode-basekit::run-command "wallet" nil)))
      (is (search "USDC: 25.000000" wallet) "a bare wallet reads the section's watch_address"))
    (is (search "usage" (nodecode-basekit::run-command "nonsense" nil)) "an unknown sub-command says how to use it")))

(deftest basekit-cell-the-command-wallet-with-no-address-says-what-to-type (with-basekit-chain)
  (with-cell-stop ((basekit-start))
    (let ((wallet (nodecode-basekit::run-command "wallet" nil)))
      (is (search "watch_address" wallet) "no address and no watch address names the setting")
      (is (search "0x..." wallet) "and shows how to type one")
      (is (null (basekit-asked)) "without reading the chain"))
    (let ((wallet (nodecode-basekit::run-command "wallet 0x1111111111111111111111111111111111111111" nil)))
      (is (search "wallet 0x1111111111111111111111111111111111111111" wallet) "a typed address reads that wallet"))))
