;;;; chain.lisp --- the six reads: network, portfolio, receipt, token, transfers, approvals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every function here answers a plist the tools render, and every one reads
;;;; only. The selectors and topic hashes are the plugin's own verified
;;;; constants, re-used unchanged (basekit_core.py:62-80). A tool's batch is
;;;; one JSON-RPC POST (RPC-BATCH in wire.lisp), and the answers state the
;;;; block they were read at, as the original's do.

(in-package #:nodecode-basekit)

;;; --- the constants the chain answers to -------------------------------------------

(defparameter +topic-transfer+ "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
  "keccak of Transfer(address,address,uint256).")
(defparameter +topic-approval+ "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"
  "keccak of Approval(address,address,uint256).")

(defparameter +sel-balance-of+ "70a08231")
(defparameter +sel-decimals+ "313ce567")
(defparameter +sel-symbol+ "95d89b41")
(defparameter +sel-name+ "06fdde03")
(defparameter +sel-total-supply+ "18160ddd")
(defparameter +sel-owner+ "8da5cb5b")
(defparameter +sel-implementation+ "5c60da1b")
(defparameter +sel-allowance+ "dd62ed3e")

(defparameter +eip1967-implementation+ "0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc")
(defparameter +eip1967-beacon+ "0xa3f0ad74e5423aebfd80d3ef4346578335a9a72aeaee59ff6cb3582b35133d50")
(defparameter +eip1967-admin+ "0xb53127684a568b3173ae13b9f8a6016e243e63b6e8ee1178d6a717850b5d6103")

(defparameter +bytecode-probes+
  '(("40c10f19" . "mint(address,uint256)")
    ("8456cb59" . "pause()")
    ("5c975abb" . "paused()")
    ("f9f92be4" . "blacklist(address)")
    ("69fe0e2d" . "setFee(uint256)")
    ("715018a6" . "renounceOwnership()")
    ("f2fde38b" . "transferOwnership(address)")
    ("8da5cb5b" . "owner()"))
  "Selectors whose presence in the deployed code is a capability hint, never a verdict.")

(defparameter +token-set+
  '(("USDC"  "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913" 6)
    ("USDbC" "0xd9aAEc86B65D86f6A7B5B1b0c42FFA531710b6CA" 6)
    ("DAI"   "0x50c5725949A6F0c72E6C4a641F24049A917DB0Cb" 18)
    ("WETH"  "0x4200000000000000000000000000000000000006" 18)
    ("cbBTC" "0xcbB7C0000aB88B473b1f5aFd9ef808440eed33Bf" 8)
    ("cbETH" "0x2Ae3F1Ec7F1F5012CFEab0185bfc7aa3cf0DEc22" 18)
    ("wstETH" "0xc1CBa3fCea344f92D9239c08C0568f6F2F0ee452" 18)
    ("AERO"  "0x940181a94A35A4569E4529A3CDfB74e38FD98631" 18)
    ("DEGEN" "0x4ed4E862860beD51a9570b96d89aF5E1B0Efefed" 18)
    ("TOSHI" "0xAc1Bd2486aAf3B5C0fc3Fd868558b082a531B2B4" 18)
    ("BRETT" "0x532f27101965dd16442E59d40670FaF5eBB142E4" 18)
    ("WELL"  "0xA88594D404727625A9437C3f886C7643872296AE" 18))
  "The plugin's curated Base token set (tokens.json), unchanged: symbol, address, decimals.")

;;; --- one string for a time ---------------------------------------------------------

(defun utc-text (universal-time)
  "UNIVERSAL-TIME as YYYY-MM-DDTHH:MM:SSZ, in UTC."
  (when universal-time
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time universal-time 0)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
              year month day hour minute second))))

(defun now-text ()
  (utc-text (get-universal-time)))

(defun timestamp-text (seconds)
  "Unix SECONDS as the ISO text every answer carries."
  (and (integerp seconds) (plusp seconds) (utc-text (+ 2208988800 seconds))))

;;; --- metadata and balances ---------------------------------------------------------

(defun erc20-metadata (addresses)
  "ADDRESSES - a list - as symbol/name/decimals, in one batch.
=> a hash table address -> (:symbol .. :name .. :decimals ..)."
  (let ((addresses (remove-duplicates (mapcar #'string-downcase addresses) :test #'equal))
        (table (make-hash-table :test #'equal)))
    (when addresses
      (let ((results (eth-calls (loop for address in addresses
                                       append (list (cons address (concatenate 'string "0x" +sel-symbol+))
                                                    (cons address (concatenate 'string "0x" +sel-name+))
                                                    (cons address (concatenate 'string "0x" +sel-decimals+)))))))
        (loop for address in addresses
              for (symbol-raw name-raw decimals-raw) on results by #'cdddr
              do (let ((entry '()))
                   (let ((symbol (and symbol-raw (abi-string symbol-raw))))
                     (when (and symbol (plusp (length symbol)))
                       (setf (getf entry :symbol) symbol)))
                   (let ((name (and name-raw (abi-string name-raw))))
                     (when (and name (plusp (length name)))
                       (setf (getf entry :name) name)))
                   (when decimals-raw
                     (let ((decimals (uint-at decimals-raw)))
                       (when (<= 0 decimals 36) (setf (getf entry :decimals) decimals))))
                   (when entry (setf (gethash address table) entry))))))
    table))

;;; --- base_network -----------------------------------------------------------------

(defun block-fields (number)
  "NUMBER's base fee, timestamp and transaction count, or NIL."
  (let* ((reply (first (rpc-batch (list (cons "eth_getBlockByNumber"
                                               (format nil "[\"0x~x\",false]" number))))))
         (block (reply-value reply)))
    (when (hash-table-p block)
      (let ((base-fee (nlk:json-value block :string "baseFeePerGas"))
            (txs (nlk:json-value block :array "transactions")))
        (list :base-fee (and base-fee (hex-int base-fee))
              :timestamp (hex-int (nlk:json-value block :string "timestamp"))
              :txs (and txs (length txs)))))))

(defun network-state ()
  "Live Base state: head block, gas, base fee, ETH/USD and what three common
actions cost in dollars. => a plist."
  (let* ((replies (rpc-batch (list (cons "eth_blockNumber" "[]")
                                    (cons "eth_gasPrice" "[]")
                                    (cons "eth_chainId" "[]"))))
         (head (hex-int (reply-value (first replies))))
         (gas (hex-int (reply-value (second replies))))
         (chain (hex-int (reply-value (third replies))))
         (block (and (plusp head) (block-fields head)))
         (base-fee (getf block :base-fee))
         (price (eth-price-usd))
         (swap-gas (or base-fee gas)))
    (list :chain-id (if (plusp chain) chain +chain-id+)
          :head head
          :block-time (timestamp-text (getf block :timestamp))
          :txs (getf block :txs)
          :gas-gwei (gwei gas)
          :base-fee-gwei (and base-fee (gwei base-fee))
          :priority-gwei (gwei (max 0 (- gas (or base-fee 0))))
          :eth-price price
          :cost-transfer (usd-of-wei (* 21000 gas) price)
          :cost-erc20 (usd-of-wei (* 65000 gas) price)
          :cost-swap (usd-of-wei (* 150000 swap-gas) price)
          :block-read (plusp head)
          :endpoint (and *last-endpoint* (endpoint-name *last-endpoint*))
          :read-only t)))

;;; --- base_portfolio ---------------------------------------------------------------

(defun portfolio (address &key extras include-zero)
  "Native ETH and ERC-20 balances with USD values for ADDRESS."
  (unless (address-p address)
    (fail "basekit: ~s is not a 0x address of 40 hex digits" address))
  (let* ((owner (string-downcase address))
         (tokens (loop for entry in +token-set+
                       collect (list :symbol (first entry)
                                     :address (string-downcase (second entry))
                                     :decimals (third entry))))
         (known (mapcar (lambda (token) (getf token :address)) tokens)))
    (dolist (extra (remove-if-not #'address-p extras))
      (let ((lowered (string-downcase extra)))
        (unless (member lowered known :test #'string=)
          (push lowered known)
          (setf tokens (append tokens (list (list :symbol nil :address lowered :decimals nil)))))))
    (let* ((calls (cons (cons "eth_getBalance" (format nil "[~s,\"latest\"]" owner))
                        (loop for token in tokens
                              collect (cons "eth_call"
                                            (call-params (getf token :address)
                                                         (concatenate 'string "0x" +sel-balance-of+
                                                                      (address-word owner)))))))
           (replies (rpc-batch calls))
           (native (hex-int (reply-value (first replies))))
           (balances (loop for token in tokens
                           for reply in (rest replies)
                           collect (cons (getf token :address)
                                         (let ((body (safe-call-value reply)))
                                           (if body (uint-at body) 0)))))
           (unknown (remove-if (lambda (token) (getf token :decimals))
                               (loop for (address . raw) in balances
                                     when (plusp raw)
                                       collect (find address tokens :key (lambda (token) (getf token :address))
                                                     :test #'string=))))
           (meta (and unknown (erc20-metadata (mapcar (lambda (token) (getf token :address)) unknown)))))
      (dolist (token unknown)
        (let ((entry (gethash (getf token :address) meta)))
          (when entry
            (setf (getf token :symbol) (getf entry :symbol)
                  (getf token :decimals) (or (getf entry :decimals) 18)))))
      (let* ((held (remove-if (lambda (pair) (and (zerop (cdr pair)) (not include-zero))) balances))
             (table (make-hash-table :test #'equal))
             (sources nil)
             (native-price nil)
             (rows '())
             (total 0d0))
        (multiple-value-setq (table sources) (prices (mapcar #'car held)))
        (setf native-price (gethash "eth" table))
        (when (or (plusp native) include-zero)
          (let ((value (* (coerce (eth-of-wei native) 'double-float) (or native-price 0d0))))
            (incf total value)
            (push (list :symbol "ETH" :name "Ether (native)" :address nil
                        :balance (eth-of-wei native)
                        :price (and native-price (round-to native-price 4))
                        :value (round-to value 2))
                  rows)))
        (loop for token in tokens
              for raw = (cdr (assoc (getf token :address) balances :test #'string=))
              when (or (plusp raw) include-zero)
                do (let* ((decimals (or (getf token :decimals) 18))
                          (amount (units raw decimals))
                          (price (gethash (getf token :address) table))
                          (value (* (coerce amount 'double-float) (or price 0d0))))
                     (incf total value)
                     (push (list :symbol (getf token :symbol)
                                 :name nil
                                 :address (getf token :address)
                                 :balance amount
                                 :price (and price (round-to price 6))
                                 :value (and price (round-to value 2)))
                           rows)))
        (list :address owner
              :holdings (sort rows #'> :key (lambda (row) (or (getf row :value) 0)))
              :token-count (length rows)
              :total (round-to total 2)
              :scanned (length tokens)
              :price-sources sources
              :as-of (now-text))))))

;;; --- base_tx ----------------------------------------------------------------------

(defun log-topics (log)
  "LOG's topics as a list of lowercase hex strings."
  (let ((topics (nlk:json-value log :array "topics")))
    (loop for topic across (or topics #()) when (stringp topic) collect (string-downcase (strip-0x topic)))))

(defun tx-receipt (hash)
  "One transaction, human-readable: status, block, time, from/to, fee, and
every token movement and approval it emitted."
  (unless (tx-hash-p hash)
    (fail "basekit: ~s is not a 0x transaction hash of 64 hex digits" hash))
  (let* ((hash (concatenate 'string "0x" (string-downcase (strip-0x hash))))
         (replies (rpc-batch (list (cons "eth_getTransactionByHash" (format nil "[~s]" hash))
                                    (cons "eth_getTransactionReceipt" (format nil "[~s]" hash)))))
         (tx (reply-value (first replies)))
         (receipt (reply-value (second replies))))
    (unless (hash-table-p tx)
      (fail "basekit: no transaction found for ~a" hash))
    (unless (hash-table-p receipt)
      (return-from tx-receipt
        (list :hash hash :status "pending" :from (nlk:json-value tx :string "from")
              :to (nlk:json-value tx :string "to") :read-only t)))
    (let* ((block-number (hex-int (nlk:json-value receipt :string "blockNumber")))
           (block (and (plusp block-number) (block-fields block-number)))
           (gas-used (hex-int (nlk:json-value receipt :string "gasUsed")))
           (gas-price (hex-int (or (nlk:json-value receipt :string "effectiveGasPrice")
                                   (nlk:json-value tx :string "gasPrice"))))
           (fee (* gas-used gas-price))
           (price (eth-price-usd))
           (logs (nlk:json-value receipt :array "logs"))
           (transfers '())
           (approvals '())
           (tokens '()))
      (loop for log across (or logs #())
            when (hash-table-p log)
              do (let* ((topics (log-topics log))
                        (token (string-downcase (or (nlk:json-value log :string "address") "")))
                        (first-topic (first topics)))
                   (when (and first-topic (string= first-topic +topic-transfer+) (>= (length topics) 3))
                     (pushnew token tokens :test #'string=)
                     (push (list :token token
                                 :from (concatenate 'string "0x" (subseq (second topics) 24))
                                 :to (concatenate 'string "0x" (subseq (third topics) 24))
                                 :standard (if (>= (length topics) 4) "ERC-721" "ERC-20")
                                 :token-id (and (>= (length topics) 4) (hex-int (fourth topics)))
                                 :raw (if (< (length topics) 4)
                                          (uint-at (strip-0x (or (nlk:json-value log :string "data") "0x")))
                                          0))
                           transfers)
                   (when (and first-topic (string= first-topic +topic-approval+) (>= (length topics) 3))
                     (pushnew token tokens :test #'string=)
                     (push (list :token token
                                 :owner (concatenate 'string "0x" (subseq (second topics) 24))
                                 :spender (concatenate 'string "0x" (subseq (third topics) 24))
                                 :raw (uint-at (strip-0x (or (nlk:json-value log :string "data") "0x"))))
                           approvals)))))
      (let ((meta (and tokens (erc20-metadata tokens))))
        (multiple-value-bind (table sources) (prices tokens)
          (declare (ignore sources))
          (list :hash hash
                :status (if (eql 1 (hex-int (nlk:json-value receipt :string "status"))) "success" "reverted")
                :block block-number
                :timestamp (and block (getf block :timestamp) (timestamp-text (getf block :timestamp)))
                :from (nlk:json-value tx :string "from")
                :to (nlk:json-value tx :string "to")
                :created (nlk:json-value receipt :string "contractAddress")
                :value (eth-of-wei (hex-int (nlk:json-value tx :string "value")))
                :gas-used gas-used
                :gas-gwei (gwei gas-price)
                :fee-eth (eth-of-wei fee)
                :fee-usd (usd-of-wei fee price)
                :eth-price price
                :log-count (if logs (length logs) 0)
                :transfers (loop for entry in (nreverse transfers)
                                 collect (let* ((info (gethash (getf entry :token) meta))
                                                (decimals (or (getf info :decimals) 18))
                                                (amount (units (getf entry :raw) decimals))
                                                (token-price (gethash (getf entry :token) table)))
                                           (list :symbol (getf info :symbol)
                                                 :token (getf entry :token)
                                                 :standard (getf entry :standard)
                                                 :from (getf entry :from) :to (getf entry :to)
                                                 :token-id (getf entry :token-id)
                                                 :amount amount
                                                 :value (and token-price (round-to (* (coerce amount 'double-float) token-price) 2)))))
                :approvals (loop for entry in (nreverse approvals)
                                 collect (let* ((info (gethash (getf entry :token) meta))
                                                (decimals (or (getf info :decimals) 18))
                                                (raw (getf entry :raw)))
                                           (list :symbol (getf info :symbol)
                                                 :token (getf entry :token)
                                                 :owner (getf entry :owner) :spender (getf entry :spender)
                                                 :amount (units raw decimals)
                                                 :unlimited (>= raw (ash 1 255)))))
                :read-only t))))))

;;; --- base_token -------------------------------------------------------------------

(defun storage-word (address slot)
  "The 32-byte word ADDRESS holds in SLOT, as an integer."
  (let* ((reply (first (rpc-batch (list (cons "eth_getStorageAt"
                                               (format nil "[~s,~s,\"latest\"]" address slot))))))
         (value (reply-value reply)))
    (if (stringp value) (hex-int (strip-0x value)) 0)))

(defun verify-source (address)
  "The optional Etherscan V2 source check (chain 8453); without a key it says so
rather than guessing."
  (let ((key (nonempty (setting :basescan-api-key))))
    (unless key
      (return-from verify-source (list :status "not_configured")))
    (handler-case
        (let* ((url (format nil "https://api.etherscan.io/v2/api?chainid=8453&module=contract&action=getsourcecode&address=~a&apikey=~a"
                             address key))
               (body (http-json url :timeout 15))
               (result (nlk:json-value body :array "result"))
               (first (and result (plusp (length result)) (aref result 0))))
          (if (and first (nlk:json-value first :text "SourceCode"))
              (list :status "verified"
                    :contract (nlk:json-value first :string "ContractName")
                    :compiler (nlk:json-value first :string "CompilerVersion"))
              (list :status "unverified")))
      (error (condition)
        (list :status "error" :because (princ-to-string condition))))))

(defun token-facts (address)
  "Contract-level facts for ADDRESS: name, symbol, decimals, supply, owner,
proxy detection and labelled bytecode hints."
  (unless (address-p address)
    (fail "basekit: ~s is not a 0x address of 40 hex digits" address))
  (let* ((address (string-downcase address))
         (code (strip-0x (or (rpc "eth_getCode" (format nil "[~s,\"latest\"]" address)) "")))
         (facts (list :address address :contract (plusp (length code)) :code-bytes (floor (length code) 2)
                      :verification (verify-source address) :read-only t)))
    (unless (plusp (length code))
      (return-from token-facts
        (append facts (list :note "no bytecode at this address on Base - it is an account or an undeployed address"))))
    (let* ((requests (list (cons address (concatenate 'string "0x" +sel-name+))
                           (cons address (concatenate 'string "0x" +sel-symbol+))
                           (cons address (concatenate 'string "0x" +sel-decimals+))
                           (cons address (concatenate 'string "0x" +sel-total-supply+))
                           (cons address (concatenate 'string "0x" +sel-owner+))))
           (results (eth-calls requests))
           (name-raw (first results))
           (symbol-raw (second results))
           (decimals-raw (third results))
           (supply-raw (fourth results))
           (owner-raw (fifth results))
           (decimals (and decimals-raw (uint-at decimals-raw))))
      (when name-raw (setf (getf facts :name) (abi-string name-raw)))
      (when symbol-raw (setf (getf facts :symbol) (abi-string symbol-raw)))
      (when decimals-raw (setf (getf facts :decimals) (and (<= 0 decimals 36) decimals)))
      (when supply-raw
        (let ((raw (uint-at supply-raw)))
          (setf (getf facts :supply-raw) raw)
          (when (getf facts :decimals)
            (setf (getf facts :supply) (units raw (getf facts :decimals))))))
      (when owner-raw
        (let ((owner (address-at owner-raw)))
          (setf (getf facts :owner) (and owner (plusp (hex-int owner)) owner))))
      (setf (getf facts :standards)
            (cond ((and symbol-raw decimals-raw) '("ERC-20"))
                  (symbol-raw '("symbol() answers, decimals() does not"))
                  (t '("unknown"))))
      (let ((proxy nil))
        (dolist (slot (list (cons "eip1967" +eip1967-implementation+)
                            (cons "eip1967-beacon" +eip1967-beacon+)))
          (unless proxy
            (let ((value (storage-word address (cdr slot))))
              (when (plusp value)
                (setf proxy (list :proxy t :pattern (car slot)
                                  :implementation (format nil "0x~40,'0x" (mod value (expt 16 40)))
                                  :admin (format nil "0x~40,'0x"
                                                 (mod (storage-word address +eip1967-admin+) (expt 16 40)))))))))
        (unless proxy
          (cond ((and (eql 0 (search "363d3d373d3d3d363d73" code))
                      (search "5af43d82803e903d91602b57fd5bf3" code))
                 (setf proxy (list :proxy t :pattern "eip1167-minimal-clone"
                                   :implementation (concatenate 'string "0x" (subseq code 20 60)))))
                ((search +sel-implementation+ code)
                 (setf proxy (list :proxy t :pattern "custom (exposes implementation())" :implementation nil)))
                (t (setf proxy (list :proxy nil)))))
        (when (and (getf proxy :proxy) (null (getf proxy :implementation)))
          (let ((body (safe-eth-call address (concatenate 'string "0x" +sel-implementation+))))
            (when body (setf (getf proxy :implementation) (address-at body)))))
        (setf (getf facts :proxy) proxy))
      (setf (getf facts :bytecode-flags)
            (loop for (selector . signature) in +bytecode-probes+
                  when (search selector code) collect signature))
      facts)))

;;; --- base_activity ----------------------------------------------------------------

(defun bounded-blocks (blocks)
  "BLOCKS clamped to the 500..50000 window public endpoints accept."
  (max 500 (min (or blocks (setting :log-window)) 50000)))

(defun token-activity (address token &key blocks limit)
  "Recent ERC-20 transfers in and out of ADDRESS for TOKEN, newest first,
inside a stated block window."
  (unless (address-p address) (fail "basekit: ~s is not a 0x address of 40 hex digits" address))
  (unless (address-p token) (fail "basekit: ~s is not a 0x address of 40 hex digits" token))
  (let* ((owner (string-downcase address))
         (token (string-downcase token))
         (blocks (bounded-blocks blocks))
         (limit (max 1 (min (or limit 25) 200)))
         (head (head-block))
         (from (max 0 (- head blocks)))
         (padded (address-word owner))
         (outgoing (get-logs token (list +topic-transfer+ padded) from head :limit limit))
         (incoming (get-logs token (list +topic-transfer+ nil padded) from head :limit limit))
         (info (gethash token (erc20-metadata (list token))))
         (decimals (or (getf info :decimals) 18)))
    (multiple-value-bind (table sources) (prices (list token))
      (declare (ignore sources))
      (let ((price (gethash token table))
            (rows '()))
        (flet ((collect-logs (logs direction)
                 (dolist (log logs)
                   (let ((topics (log-topics log)))
                     (when (>= (length topics) 3)
                       (let* ((raw (uint-at (strip-0x (or (nlk:json-value log :string "data") "0x"))))
                              (amount (units raw decimals))
                              (counterparty (concatenate 'string "0x"
                                                         (subseq (if (string= direction "out")
                                                                     (third topics) (second topics))
                                                                 24))))
                         (push (list :direction direction
                                     :amount amount
                                     :value (and price (round-to (* (coerce amount 'double-float) price) 2))
                                     :counterparty counterparty
                                     :block (hex-int (nlk:json-value log :string "blockNumber"))
                                     :tx (nlk:json-value log :string "transactionHash"))
                               rows)))))))
          (collect-logs outgoing "out")
          (collect-logs incoming "in"))
        (setf rows (sort rows #'> :key (lambda (row) (getf row :block))))
        (when (> (length rows) limit) (setf rows (subseq rows 0 limit)))
        (list :address owner
              :token (list :address token :symbol (getf info :symbol) :decimals decimals)
              :window (list :from from :to head :blocks (- head from) :requested blocks)
              :transfers rows
              :count (length rows)
              :read-only t)))))

;;; --- base_allowances --------------------------------------------------------------

(defun token-allowances (owner token &key blocks)
  "The approvals OWNER granted on TOKEN, each re-read on-chain so an already
revoked one is dropped, with the dollar value still at risk."
  (unless (address-p owner) (fail "basekit: ~s is not a 0x address of 40 hex digits" owner))
  (unless (address-p token) (fail "basekit: ~s is not a 0x address of 40 hex digits" token))
  (let* ((owner (string-downcase owner))
         (token (string-downcase token))
         (blocks (bounded-blocks blocks))
         (head (head-block))
         (from (max 0 (- head blocks)))
         (logs (get-logs token (list +topic-approval+ (address-word owner)) from head :limit 400))
         (spenders '()))
    (dolist (log logs)
      (let ((topics (log-topics log)))
        (when (>= (length topics) 3)
          (pushnew (concatenate 'string "0x" (subseq (third topics) 24)) spenders :test #'string=))))
    (setf spenders (subseq spenders 0 (min 25 (length spenders))))
    (let* ((info (gethash token (erc20-metadata (list token))))
           (decimals (or (getf info :decimals) 18))
           (results (and spenders
                         (eth-calls (loop for spender in spenders
                                          collect (cons token
                                                        (concatenate 'string "0x" +sel-allowance+
                                                                     (address-word owner)
                                                                     (address-word spender)))))))
           (rows '()))
      (multiple-value-bind (table sources) (prices (list token))
        (declare (ignore sources))
        (let ((price (gethash token table))
              (unlimited 0))
          (loop for spender in spenders
                for result in results
                when result
                  do (let* ((raw (uint-at result))
                            (amount (units raw decimals)))
                       (when (plusp raw)
                         (when (>= raw (ash 1 255)) (incf unlimited))
                         (push (list :spender spender
                                     :allowance amount
                                     :unlimited (and (>= raw (ash 1 255)) t)
                                     :value (and price (round-to (* (coerce amount 'double-float) price) 2)))
                               rows))))
          (setf rows (sort rows #'> :key (lambda (row) (or (getf row :value) 0))))
          (list :owner owner
                :token (list :address token :symbol (getf info :symbol) :decimals decimals)
                :window (list :from from :to head :blocks (- head from) :requested blocks)
                :allowances rows
                :active-count (length rows)
                :unlimited-count unlimited
                :read-only t))))))
