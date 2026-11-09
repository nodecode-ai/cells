;;;; tools.lisp --- the ten verbs, each one or two mm commands, and their checks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every verb answers the CLI's own JSON, so the model reads what MetaMask
;;;; said rather than a retelling of it. What is checked here is what the
;;;; plugin checked before it ran anything: an address is 0x and 40 hex, an
;;;; amount is a positive decimal, a transaction hash is 0x and 64 hex, a chain
;;;; id is a positive integer. A verb that moves funds or signs says what it
;;;; did and, while MetaMask waits on the operator's 2FA, which request id to
;;;; watch.

(in-package #:nodecode-metamask)

;;; --- what the checks read ----------------------------------------------------------

(defun nonblank (text)
  "TEXT without its surrounding whitespace, or NIL when nothing is left."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun ascii-alnum-p (character)
  (and (< (char-code character) 128) (alphanumericp character)))

(defun hex-text-p (text)
  "TEXT as \"0x\" and then only hex digits."
  (and (stringp text)
       (>= (length text) 2)
       (uiop:string-prefix-p "0x" text)
       (every (lambda (character) (digit-char-p character 16)) (subseq text 2))))

(defun address-p (text)
  "TEXT as an EVM address: 0x and 40 hex."
  (and (hex-text-p text) (= (length text) 42)))

(defun tx-hash-p (text)
  "TEXT as a transaction hash: 0x and 64 hex."
  (and (hex-text-p text) (= (length text) 66)))

(defun symbol-p (text)
  "TEXT as a token symbol: up to 24 letters, digits, . _ and -."
  (and (stringp text)
       (<= 1 (length text) 24)
       (every (lambda (character)
                (or (ascii-alnum-p character) (find character "._-")))
              text)
       (ascii-alnum-p (char text 0))))

(defun currency-p (text)
  "TEXT as a fiat code: three to five lowercase letters."
  (and (stringp text)
       (<= 3 (length text) 5)
       (every (lambda (character) (and (<= (char-code #\a) (char-code character))
                                       (<= (char-code character) (char-code #\z))))
              text)))

(defun amount-p (text)
  "TEXT as a positive decimal amount: digits with at most one point, and not
all zero."
  (and (stringp text)
       (<= 1 (length text) 78)
       (let ((points 0) (digits 0) (nonzero nil))
         (loop for character across text
               do (cond ((digit-char-p character)
                         (incf digits)
                         (unless (char= character #\0) (setf nonzero t)))
                        ((char= character #\.) (incf points))
                        (t (return-from amount-p nil))))
         (and (plusp digits) (<= points 1) nonzero))))

(defun chain-id (value &key (default t))
  "VALUE as a chain id: the call's own, else the section's default; NIL when
the call names none and DEFAULT is NIL. \"8453\" reads as 8453; junk is NIL."
  (let ((value (cond ((or (null value) (equal value "") (equal value :null))
                      (and default (setting :default-chain-id)))
                     (t value))))
    (typecase value
      (integer (when (plusp value) value))
      (string (let ((number (ignore-errors (parse-integer (nlk:trimmed value)))))
                (and number (plusp number) number)))
      (t nil))))

(defun chain-args (chain-ids)
  "--chain-ids with CHAIN-IDS joined by commas, or nothing when none are named."
  (let ((ids (remove nil (mapcar #'nonblank (coerce (or chain-ids '()) 'list)))))
    (when ids (list "--chain-ids" (format nil "~{~a~^,~}" ids)))))

(defun number-text (value)
  "VALUE as the decimal the CLI reads: 0.5 stays 0.5, 5 stays 5."
  (if (integerp value)
      (princ-to-string value)
      (let ((text (format nil "~a" value)))
        (if (find #\e text :test #'char-equal)
            (format nil "~,9f" value)
            text))))

;;; --- metamask_status ---------------------------------------------------------------

(defun wallet-address ()
  "The active wallet's address as the CLI reports it, or NIL."
  (let ((body (nlk:json-value (mm-envelope '("wallet" "address")) :any "data")))
    (if (stringp body) body (nlk:json-value body :string "address"))))

(defun trading-mode ()
  "The wallet's trading mode -- guard or beast -- or NIL."
  (let ((body (nlk:json-value (mm-envelope '("wallet" "trading-mode" "get")) :any "data")))
    (if (stringp body) body (nlk:json-value body :string "mode"))))

(defun status (&key include-policy)
  "The wallet as the CLI sees it: signed in, set up, its address, its trading
mode, and with INCLUDE-POLICY the outflow policy too. Reads only; asks mm
doctor, then the wallet's own commands."
  (let* ((out (nlk:make-json-object))
         (doctor (mm-envelope '("doctor")))
         (data (nlk:json-value doctor :object "data"))
         (signed-in (nlk:json-value data :boolean "authenticated"))
         (initialized (nlk:json-value data :boolean "initialized")))
    (setf (gethash "authenticated" out) signed-in
          (gethash "initialized" out) initialized)
    (let ((hints (remove-if (lambda (hint) (search "skills add" hint))
                            (coerce (nlk:json-array data "hints") 'list))))
      (when hints (setf (gethash "cli_hints" out) (coerce hints 'vector))))
    (when signed-in
      (setf (gethash "address" out) (wallet-address)
            (gethash "trading_mode" out) (trading-mode))
      (let ((init (nlk:json-value (mm-envelope '("init" "show")) :any "data")))
        (when init (setf (gethash "init" out) init)))
      (when include-policy
        (setf (gethash "policy" out)
              (nlk:json-value (mm-envelope '("wallet" "policy" "get")) :any "data"))))
    (setf (gethash "next_step" out)
          (cond ((not signed-in)
                 "Not signed in: run `mm login` on this machine (or `mm login browser` for a sign-in link), then ask again.")
                ((not initialized)
                 "Signed in with no wallet yet: run `mm init --wallet server-wallet --mode guard` on this machine.")
                (t "Ready. Reads: metamask_balance, metamask_market, metamask_history, metamask_swap_quote, metamask_requests, metamask_policy get. Writes: metamask_transfer, metamask_swap_execute, metamask_sign.")))
    (rendered out)))

;;; --- metamask_balance --------------------------------------------------------------

(defun balance (&key chain-ids token currency address testnet token-contracts)
  "What the wallet holds. Mainnets by default; TESTNET reads the testnets mm
takes over RPC instead."
  (let ((currency (string-downcase (or (nonblank currency) (setting :default-currency))))
        (token (nonblank token))
        (address (nonblank address)))
    (unless (currency-p currency)
      (fail "currency must be a fiat code of three to five letters, like usd or eur; got ~s" currency))
    (when (and token (not (or (symbol-p token) (address-p token) (find #\: token))))
      (fail "token must be a symbol, a 0x contract address or a CAIP-19 id; got ~s" token))
    (when (and address (not (address-p address)))
      (fail "address must be a 0x-prefixed 40-hex EVM address; got ~s" address))
    (let ((contracts (remove nil (coerce (or token-contracts '()) 'list))))
      (unless (every #'address-p contracts)
        (fail "token_contracts must be 0x ERC-20 contract addresses"))
      (rendered
       (mm-envelope
        (append '("wallet" "balance")
                (unless testnet (chain-args chain-ids))
                (when token (list "--token" token))
                (list "--currency" currency)
                (when address (list "--address" address))
                (when testnet
                  (append '("--testnet")
                          (when contracts (list "--token-contracts"
                                                (format nil "~{~a~^,~}" contracts)))))))))))

;;; --- metamask_market ---------------------------------------------------------------

(defparameter *native-assets*
  '((1 "ETH" . "slip44:60") (8453 "ETH" . "slip44:60") (42161 "ETH" . "slip44:60")
    (10 "ETH" . "slip44:60") (59144 "ETH" . "slip44:60") (324 "ETH" . "slip44:60")
    (137 "POL" . "slip44:966") (56 "BNB" . "slip44:714") (43114 "AVAX" . "slip44:9000"))
  "Each chain's own native asset in CAIP-19's slip44 form: what the plugin knew
about ETH, POL, BNB and AVAX, so a price for the native coin needs no search.")

(defun native-asset-id (symbol chain)
  "SYMBOL on CHAIN as a CAIP-19 asset id when it is that chain's native asset."
  (let ((row (assoc chain *native-assets*)))
    (when (and row (string-equal symbol (second row)))
      (format nil "eip155:~a/~a" chain (cddr row)))))

(defun token-items (body)
  "The tokens a list answer carries, wherever the CLI nests them."
  (cond ((nlk:json-value body :array) (coerce (nlk:json-value body :array) 'list))
        ((hash-table-p body)
         (loop for key in '("tokens" "items" "results" "data")
               for value = (nlk:json-value body :array key)
               when value return (coerce value 'list)))
        (t '())))

(defun asset-id-of (item chain)
  "ITEM as a CAIP-19 asset id on CHAIN: the CLI's own when it gave one, else
its contract address."
  (let ((id (or (nlk:json-value item :string "assetId")
                (nlk:json-value item :string "caipAssetId"))))
    (if (and id (uiop:string-prefix-p "eip155:" id))
        id
        (let ((address (or (nlk:json-value item :string "address")
                           (nlk:json-value item :string "contractAddress"))))
          (when (address-p address)
            (format nil "eip155:~a/erc20:~a" chain (string-downcase address)))))))

(defun resolve-asset (symbol chain)
  "SYMBOL on CHAIN as a CAIP-19 asset id, by the chain's natives first and the
CLI's own token search second; NIL when it finds nothing."
  (let ((symbol (string-upcase symbol)))
    (or (native-asset-id symbol chain)
        (let ((found (nlk:json-value
                      (mm-envelope (list "token" "list" "search" symbol
                                         "--chain-ids" (princ-to-string chain)
                                         "--limit" "10"))
                      :any "data")))
          (loop for item in (token-items found)
                when (and (hash-table-p item)
                          (string-equal (or (nlk:json-value item :string "symbol") "") symbol))
                  return (asset-id-of item chain))))))

(defun market (&key action query symbol chain-id asset-ids limit vs market-data)
  "Prices, token search and the chains mm serves. One action per call."
  (let ((action (string-downcase (or (nonblank action) "price"))))
    (cond
      ((string= action "chains") (rendered (mm-envelope '("chains" "list"))))
      ((string= action "trending") (rendered (mm-envelope '("token" "list" "trending"))))
      ((string= action "popular") (rendered (mm-envelope '("token" "list" "popular"))))
      ((member action '("token_search" "search") :test #'string=)
       (let ((query (or (nonblank query) (nonblank symbol)
                        (fail "token_search needs a query: a token symbol or name")))
             (chain (chain-id chain-id)))
         (unless (<= (length query) 64)
           (fail "query is at most 64 characters; got ~d" (length query)))
         (rendered (mm-envelope
                    (append (list "token" "list" "search" query)
                            (when chain (chain-args (list (princ-to-string chain))))
                            (when limit (list "--limit" (princ-to-string (max 1 (min limit 100))))))))))
      ((string= action "price")
       (let* ((chain (or (chain-id chain-id) (fail "chain_id must be a positive integer")))
              (named (remove nil (mapcar (lambda (id)
                                           (and (stringp id)
                                                (uiop:string-prefix-p "eip155:" id)
                                                id))
                                         (coerce (or asset-ids '()) 'list))))
              (ids (or named
                       (let ((symbol (or (nonblank symbol)
                                         (fail "price needs a symbol (e.g. ETH) or asset_ids in CAIP-19 form"))))
                         (list (or (resolve-asset symbol chain)
                                   (fail "could not find ~a on chain ~a: try action \"search\" for its symbol or contract address"
                                         symbol chain))))))
              (vs (string-downcase (or (nonblank vs) (setting :default-currency)))))
         (rendered (mm-envelope
                    (append (list "price" "spot" "--asset-ids" (format nil "~{~a~^,~}" ids))
                            (when (currency-p vs) (list "--vs" vs))
                            (when market-data '("--market-data")))))))
      (t (fail "action must be price, search, chains, trending or popular; got ~s" action)))))

;;; --- metamask_history --------------------------------------------------------------

(defun history (&key action chain-ids limit type hash chain-id payload)
  "The wallet's transactions, one transaction, or calldata decoded."
  (let ((action (string-downcase (or (nonblank action) "list"))))
    (cond
      ((string= action "list")
       (let ((limit (or limit 20))
             (type (and (nonblank type) (string-downcase (nonblank type)))))
         (unless (<= 1 limit 50)
           (fail "limit must be 1 to 50; got ~a" limit))
         (when (and type (not (member type '("in" "out" "self") :test #'string=)))
           (fail "type must be in, out or self; got ~s" type))
         (rendered (mm-envelope (append '("tx" "history")
                                        (chain-args chain-ids)
                                        (list "--limit" (princ-to-string limit))
                                        (when type (list "--type" type)))))))
      ((string= action "tx")
       (unless (tx-hash-p hash)
         (fail "hash must be a 0x-prefixed 64-hex transaction hash; got ~s" hash))
       (let ((chain (chain-id chain-id :default nil)))
         (rendered (mm-envelope (append (list "tx" "--hash" hash)
                                        (when chain (list "--chain-id" (princ-to-string chain))))))))
      ((string= action "decode")
       (unless (and (hex-text-p payload) (>= (length payload) 10))
         (fail "payload must be hex calldata starting with 0x and at least a 4-byte selector; got ~s" payload))
       (rendered (mm-envelope (list "decode" "--payload" payload))))
      (t (fail "action must be list, tx or decode; got ~s" action)))))

;;; --- metamask_swap_quote and metamask_swap_execute ---------------------------------

(defun swap-args (from-token to-token amount from-chain-id to-chain-id slippage)
  "The --from/--to/--amount part of a swap command, checked."
  (unless (symbol-p from-token)
    (fail "from_token must be a token symbol like ETH or USDC; got ~s" from-token))
  (unless (symbol-p to-token)
    (fail "to_token must be a token symbol like ETH or USDC; got ~s" to-token))
  (unless (amount-p amount)
    (fail "amount must be a positive decimal number, e.g. \"0.5\" or \"100\"; got ~s" amount))
  (let ((chain (or (chain-id from-chain-id) (fail "from_chain_id must be a positive integer")))
        (to (chain-id to-chain-id :default nil)))
    (when (and slippage (not (<= 0 slippage 100)))
      (fail "slippage must be between 0 and 100 percent; got ~a" slippage))
    (append (list "--from" (string-upcase from-token) "--to" (string-upcase to-token)
                  "--amount" amount "--from-chain-id" (princ-to-string chain))
            (when (and to (/= to chain)) (list "--to-chain-id" (princ-to-string to)))
            (when slippage (list "--slippage" (number-text slippage))))))

(defun swap-quote (&key from-token to-token amount from-chain-id to-chain-id slippage)
  "Compare-only quote for a swap: --all-quotes, so nothing is executed."
  (rendered (mm-envelope (append (list "swap" "quote")
                                  (swap-args from-token to-token amount from-chain-id
                                             to-chain-id slippage)
                                  '("--all-quotes"))
                          :timeout (max (setting :timeout-seconds) 120))))

(defun swap-execute (&key quote-id from-token to-token amount from-chain-id to-chain-id slippage)
  "Execute a swap: by QUOTE-ID as metamask_swap_quote answered it, or by the
same arguments the quote took. MetaMask's policy and 2FA still apply."
  (let ((quote-id (nonblank quote-id)))
    (rendered
     (mm-envelope (if quote-id
                       (list "swap" "execute" "--quote-id" quote-id)
                       (append (list "swap" "execute")
                               (swap-args from-token to-token amount from-chain-id
                                          to-chain-id slippage)))
                   :timeout (max (setting :timeout-seconds) 180)))))

;;; --- metamask_transfer -------------------------------------------------------------

(defparameter *terminal-statuses* '("BROADCASTED" "CONFIRMED" "FAILED" "REJECTED" "CANCELLED")
  "The statuses a MetaMask request no longer waits past.")

(defun note-pending (envelope)
  "When the answer is a request MetaMask is still waiting on, say which one to
watch -- the plugin's own instruction to the model."
  (let* ((data (nlk:json-value envelope :object "data"))
         (polling (nlk:json-value data :string "pollingId"))
         (status (nlk:json-value data :string "status")))
    (when (and polling (not (member status *terminal-statuses* :test #'string=)))
      (setf (gethash "note" envelope)
            (format nil "MetaMask is waiting on the operator (~a): open its push or e-mail prompt, then check metamask_requests action=\"watch\" polling_id=\"~a\"."
                    (or status "AWAITING_MFA") polling))))
  envelope)

(defun transfer (&key to amount token chain-id)
  "Send TOKEN (ETH when unnamed) to TO, on CHAIN. A transfer is irreversible:
say the amount, the token, the chain and the address back before calling it."
  (unless (address-p to)
    (fail "to must be a 0x-prefixed 40-hex EVM address (ENS names are not supported); got ~s" to))
  (unless (amount-p amount)
    (fail "amount must be a positive decimal number, e.g. \"0.5\" or \"20\"; got ~s" amount))
  (let* ((token (or (nonblank token) "ETH"))
         (chain (or (chain-id chain-id) (fail "chain_id must be a positive integer"))))
    (unless (or (symbol-p token) (address-p token))
      (fail "token must be a symbol like ETH or USDC, or a 0x contract address; got ~s" token))
    (let ((answer (mm-envelope (list "transfer" "--to" to "--amount" amount
                                      "--chain-id" (princ-to-string chain)
                                      "--token" (if (symbol-p token)
                                                    (string-upcase token) token)))))
      (rendered (note-pending answer)))))

;;; --- metamask_sign -----------------------------------------------------------------

(defun typed-data (payload)
  "PAYLOAD as the EIP-712 object mm signs: a JSON object with types, primaryType,
domain and message."
  (let ((payload (if (stringp payload) (handler-case (nlk:decode-json payload) (error () nil)) payload)))
    (unless (hash-table-p payload)
      (fail "payload must be EIP-712 JSON: an object with types, primaryType, domain and message"))
    (dolist (key '("types" "primaryType" "domain" "message"))
      (unless (nth-value 1 (gethash key payload))
        (fail "payload needs ~a: it is EIP-712 JSON with types, primaryType, domain and message" key)))
    payload))

(defun sign (&key kind message payload chain-id intent)
  "A signature by the wallet: a plain message, or EIP-712 typed data. Treat a
signature as a credential."
  (let ((kind (string-downcase (or (nonblank kind) "message")))
        (chain (or (chain-id chain-id) (fail "chain_id must be a positive integer"))))
    (cond
      ((string= kind "message")
       (unless (and (stringp message) (<= 1 (length message) 4000)
                    (plusp (length (nlk:trimmed message))))
         (fail "message must be a non-empty string of at most 4000 characters"))
       (rendered (mm-envelope (list "wallet" "sign-message"
                                     (format nil "--message=~a" message)
                                     "--chain-id" (princ-to-string chain)))))
      ((string= kind "typed_data")
       (let* ((payload (typed-data (or payload (fail "typed_data needs a payload"))))
              (domain (nlk:json-value payload :object "domain"))
              (domain-chain (nlk:json-value domain :any "chainId"))
              (intent (nonblank intent)))
         (when (and domain-chain (not (equal (princ-to-string domain-chain)
                                             (princ-to-string chain))))
           (fail "payload.domain.chainId (~a) differs from chain_id (~a)"
                 domain-chain chain))
         (rendered (mm-envelope (append (list "wallet" "sign-typed-data"
                                               "--chain-id" (princ-to-string chain)
                                               (format nil "--payload=~a" (nlk:encode-json-object payload)))
                                         (when intent
                                           (list (format nil "--intent=~a" (subseq intent 0 (min 200 (length intent)))))))))))
      (t (fail "kind must be \"message\" or \"typed_data\"; got ~s" kind)))))

;;; --- metamask_requests -------------------------------------------------------------

(defun requests (&key action polling-id timeout)
  "What MetaMask is waiting on: the list, or one request watched to its end."
  (let ((action (string-downcase (or (nonblank action) "list"))))
    (cond
      ((string= action "list") (rendered (mm-envelope '("wallet" "requests" "list"))))
      ((string= action "watch")
       (let ((id (or (nonblank polling-id)
                     (fail "watch needs polling_id: it is what action list answers with")))
             (wait (or timeout 60)))
         (unless (and (integerp wait) (<= 5 wait 600))
           (fail "timeout must be an integer number of seconds, 5 to 600; got ~a" wait))
         (rendered (mm-envelope (list "wallet" "requests" "watch" id
                                       "--wallet-timeout" (princ-to-string wait))
                                 :timeout (+ wait 30)))))
      (t (fail "action must be list or watch; got ~s" action)))))

;;; --- metamask_policy ---------------------------------------------------------------

(defun policy (&key action policy)
  "The wallet's outflow policy: read it, take the template, or set it from YAML.
A set asks MetaMask for one 2FA and stops policy violations asking again."
  (let ((action (string-downcase (or (nonblank action) "get"))))
    (cond
      ((string= action "get") (rendered (mm-envelope '("wallet" "policy" "get"))))
      ((string= action "template") (rendered (mm-envelope '("wallet" "policy" "template"))))
      ((string= action "set")
       (let ((yaml (nonblank policy)))
         (unless yaml
           (fail "set needs policy: the YAML `mm wallet policy template` shows, with your limit and allowlist"))
         (rendered (mm-envelope (list "wallet" "policy" "set"
                                       (format nil "--policy=~a" yaml) "--no-wait")))))
      (t (fail "action must be get, template or set; got ~s" action)))))

;;; --- the boundaries the model crosses -----------------------------------------------

(nlk:define-contract status-call
  (:doc "The MetaMask wallet as the CLI sees it: whether the machine is signed in, whether a wallet is set up, its address and trading mode. Reads only. Call it first when a wallet call refuses, and after a setup step, to see what is left to do.")
  ("include_policy" :boolean
   :doc "carry the outflow policy too -- the 24h limit and the allowlist, one extra read"))

(nlk:define-contract balance-call
  (:doc "What the wallet holds, in fiat: every mainnet MetaMask indexes, or one token, one chain, one address. Reads only.")
  ("chain_ids" :array :of :string
   :doc "chain ids to read, as strings, e.g. [\"8453\"]; every mainnet MetaMask indexes when omitted")
  ("token" :string
   :doc "one token: a symbol (USDC), a 0x contract address or a CAIP-19 id")
  ("currency" :string
   :doc "the fiat to convert to, a three-to-five-letter code; the section's default_currency when omitted")
  ("address" :string
   :doc "an address to read instead of the active wallet's, 0x and 40 hex")
  ("testnet" :boolean
   :doc "read the testnets mm takes over RPC -- Sepolia, Arbitrum Sepolia, Polygon Amoy -- instead of the mainnets")
  ("token_contracts" :array :of :string
   :doc "with testnet: the ERC-20 contracts to read; Circle's USDC on those three when omitted"))

(nlk:define-contract market-call
  (:doc "Prices and tokens: the spot price of a token, a token search, the chains mm serves, or what is trending. Reads only, and keyless on mm's side.")
  ("action" :string :options '("price" "search" "chains" "trending" "popular")
   :doc "price, search, chains, trending or popular; price when omitted")
  ("symbol" :string :slot market-symbol
   :doc "with price: the token's symbol, e.g. ETH; the chain's native coin needs no search")
  ("query" :string
   :doc "with search: a symbol or name to search for")
  ("asset_ids" :array :of :string
   :doc "with price: CAIP-19 asset ids (eip155:8453/slip44:60) instead of a symbol")
  ("chain_id" :integer
   :doc "the chain the symbol is on; the section's default_chain_id when omitted")
  ("limit" :integer
   :doc "with search: how many tokens, 1 to 100")
  ("vs" :string
   :doc "the fiat to price against; the section's default_currency when omitted")
  ("market_data" :boolean
   :doc "with price: carry the 24h market data too"))

(nlk:define-contract history-call
  (:doc "The wallet's transactions, one transaction, or calldata decoded. Reads only.")
  ("action" :string :options '("list" "tx" "decode")
   :doc "list, tx or decode; list when omitted")
  ("chain_ids" :array :of :string
   :doc "with list: chain ids to read, as strings; every chain MetaMask indexes when omitted")
  ("limit" :integer
   :doc "with list: how many transactions, 1 to 50; 20 when omitted")
  ("type" :string :slot tx-type :options '("in" "out" "self")
   :doc "with list: only incoming, outgoing or self transfers")
  ("hash" :text
   :doc "with tx: the transaction hash, 0x and 64 hex")
  ("chain_id" :integer
   :doc "with tx: the chain the hash is on, when it is not the section's default")
  ("payload" :text
   :doc "with decode: the calldata, 0x and hex, at least a 4-byte selector"))

(nlk:define-contract swap-quote-call
  (:doc "A compare-only swap quote: every route mm finds for the pair, with its quoteId, and nothing executed. Reads only.")
  ("from_token" :text :required t
   :doc "the token sold, a symbol like ETH")
  ("to_token" :text :required t
   :doc "the token bought, a symbol like USDC")
  ("amount" :text :required t
   :doc "how much of from_token, a positive decimal like \"0.1\"")
  ("from_chain_id" :integer
   :doc "the chain the swap starts on; the section's default_chain_id when omitted")
  ("to_chain_id" :integer
   :doc "the chain it ends on, for a cross-chain swap; from_chain_id when omitted")
  ("slippage" :number
   :doc "slippage tolerance in percent, 0 to 100"))

(nlk:define-contract swap-execute-call
  (:doc "Execute a swap: irreversible, and MetaMask's policy, Blockaid scan and 2FA still apply. Give quote_id from metamask_swap_quote, or the same arguments the quote took. Say the pair, the amount and the chain before calling it.")
  ("quote_id" :string
   :doc "the quoteId metamask_swap_quote answered with; takes the place of the arguments below")
  ("from_token" :string :doc "the token sold, a symbol; only without quote_id")
  ("to_token" :string :doc "the token bought, a symbol; only without quote_id")
  ("amount" :string :doc "how much of from_token, a positive decimal; only without quote_id")
  ("from_chain_id" :integer :doc "the chain the swap starts on; only without quote_id")
  ("to_chain_id" :integer :doc "the chain it ends on, for a cross-chain swap; only without quote_id")
  ("slippage" :number :doc "slippage tolerance in percent, 0 to 100; only without quote_id"))

(nlk:define-contract transfer-call
  (:doc "Send a token or coin from the wallet. Irreversible once MetaMask broadcasts it: read the amount, the token, the chain and the destination back to the operator first, then call it once. MetaMask's Guard Mode policy, Blockaid scan and 2FA decide whether it goes through, or waits on the operator's phone.")
  ("to" :text :required t
   :doc "the destination address, 0x and 40 hex, exactly as the operator gave it; no ENS names")
  ("amount" :text :required t
   :doc "how much to send, a positive decimal like \"20\" or \"0.5\"")
  ("token" :text
   :doc "the token sent: a symbol like USDC, or a 0x contract address; ETH when omitted")
  ("chain_id" :integer
   :doc "the chain it goes on; the section's default_chain_id when omitted"))

(nlk:define-contract sign-call
  (:doc "A signature by the wallet: a plain message, or EIP-712 typed data such as a permit. MetaMask's policy and 2FA apply, and a signature is a credential -- whoever holds it can present it wherever that exact message is accepted.")
  ("kind" :string :options '("message" "typed_data")
   :doc "message for a plain string, typed_data for EIP-712; message when omitted")
  ("message" :text
   :doc "with message: the words to sign, at most 4000 characters")
  ("payload" :object
   :doc "with typed_data: the EIP-712 object, with types, primaryType, domain and message")
  ("chain_id" :integer
   :doc "the chain it is signed for; the section's default_chain_id when omitted")
  ("intent" :string
   :doc "with typed_data: one line on what the signature is for, shown in MetaMask"))

(nlk:define-contract requests-call
  (:doc "What MetaMask is waiting on: the pending requests with their intent and status, or one request watched until it settles. Use it after a transfer, swap or signature answered with a pollingId.")
  ("action" :string :options '("list" "watch")
   :doc "list, or watch one request; list when omitted")
  ("polling_id" :text
   :doc "with watch: the pollingId a write answered with")
  ("timeout" :integer
   :doc "with watch: how many seconds to wait for it, 5 to 600; 60 when omitted"))

(nlk:define-contract policy-call
  (:doc "The wallet's outflow policy: the rolling 24h limit and the allowlist that decide which transfers need a 2FA. get and template read; set changes it and asks MetaMask for one 2FA, after which routine transfers under that policy stop asking.")
  ("action" :string :options '("get" "template" "set")
   :doc "get, template or set; get when omitted")
  ("policy" :text
   :doc "with set: the policy as YAML -- start from action template, raise the limit and add the recipients the operator named"))

;;; --- the tools the model calls ------------------------------------------------------

(defun status-tool (call)
  (status :include-policy (status-call-include-policy call)))

(defun balance-tool (call)
  (balance :chain-ids (balance-call-chain-ids call)
           :token (balance-call-token call)
           :currency (balance-call-currency call)
           :address (balance-call-address call)
           :testnet (balance-call-testnet call)
           :token-contracts (balance-call-token-contracts call)))

(defun market-tool (call)
  (market :action (market-call-action call)
          :symbol (market-call-market-symbol call)
          :query (market-call-query call)
          :asset-ids (market-call-asset-ids call)
          :chain-id (market-call-chain-id call)
          :limit (market-call-limit call)
          :vs (market-call-vs call)
          :market-data (market-call-market-data call)))

(defun history-tool (call)
  (history :action (history-call-action call)
           :chain-ids (history-call-chain-ids call)
           :limit (history-call-limit call)
           :type (history-call-tx-type call)
           :hash (history-call-hash call)
           :chain-id (history-call-chain-id call)
           :payload (history-call-payload call)))

(defun swap-quote-tool (call)
  (swap-quote :from-token (swap-quote-call-from-token call)
              :to-token (swap-quote-call-to-token call)
              :amount (swap-quote-call-amount call)
              :from-chain-id (swap-quote-call-from-chain-id call)
              :to-chain-id (swap-quote-call-to-chain-id call)
              :slippage (swap-quote-call-slippage call)))

(defun swap-execute-tool (call)
  (swap-execute :quote-id (swap-execute-call-quote-id call)
                 :from-token (swap-execute-call-from-token call)
                 :to-token (swap-execute-call-to-token call)
                 :amount (swap-execute-call-amount call)
                 :from-chain-id (swap-execute-call-from-chain-id call)
                 :to-chain-id (swap-execute-call-to-chain-id call)
                 :slippage (swap-execute-call-slippage call)))

(defun transfer-tool (call)
  (transfer :to (transfer-call-to call)
            :amount (transfer-call-amount call)
            :token (transfer-call-token call)
            :chain-id (transfer-call-chain-id call)))

(defun sign-tool (call)
  (sign :kind (sign-call-kind call)
        :message (sign-call-message call)
        :payload (sign-call-payload call)
        :chain-id (sign-call-chain-id call)
        :intent (sign-call-intent call)))

(defun requests-tool (call)
  (requests :action (requests-call-action call)
            :polling-id (requests-call-polling-id call)
            :timeout (requests-call-timeout call)))

(defun policy-tool (call)
  (policy :action (policy-call-action call)
          :policy (policy-call-policy call)))
