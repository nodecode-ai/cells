;;;; tools.lisp --- the six tools, their arguments, and how each answer reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's six tools keep their names and their arguments. Each is one
;;;; NLK:DEFINE-CONTRACT, so the shape the model is shown and the shape the
;;;; body reads are one declaration. Every answer is text: the model reads it,
;;;; and the numbers and addresses stay where it can quote them. Every answer
;;;; names the block it was read at, as the original's do.

(in-package #:nodecode-basekit)

;;; --- rendering helpers ------------------------------------------------------------

(defun dollars (value)
  "VALUE as money, with enough decimals to keep a Base fee visible."
  (cond ((null value) "unknown")
        ((>= (abs (coerce value 'double-float)) 1d0) (format nil "$~,2f" value))
        ((>= (abs (coerce value 'double-float)) 1d-2) (format nil "$~,4f" value))
        (t (format nil "$~,6f" value))))

(defun amount (value)
  "VALUE as a token amount, or NIL."
  (and value (format nil "~,6f" value)))

(defun gwei-text (value)
  (if value (format nil "~,6f gwei" value) "unknown"))

(defun eth-text (value)
  (if value (format nil "~,9f ETH" value) "unknown"))

(defun unknown-price ()
  "The phrase an unpriced holding carries instead of a guess."
  "(no price)")

(defun address-line (address)
  (or address "none (a contract deployment)"))

;;; --- base_network -----------------------------------------------------------------

(nlk:define-contract network-call
  (:doc "Live Base (chain 8453) network state: head block, gas price and base fee in gwei,
ETH/USD, and the dollar cost of a plain ETH transfer, an ERC-20 transfer and a swap. Call this
before telling the user anything about current Base conditions or fees. Read-only: no key, no
transaction.")
  ("detail" :string :options '("summary" "full")
   :doc "summary (the default) answers the state and the cost table; full also names the RPC endpoint"))

(defun format-network (detail)
  "The network answer, summary or full."
  (let ((state (network-state)))
    (with-output-to-string (out)
      (format out "Base (chain ~a) at block ~a~@[ (block time ~a, ~a transactions)~]~%"
              (getf state :chain-id) (getf state :head)
              (getf state :block-time) (getf state :txs))
      (format out "gas ~a~@[ (base fee ~a)~], priority ~a~%"
              (gwei-text (getf state :gas-gwei))
              (and (getf state :base-fee-gwei) (gwei-text (getf state :base-fee-gwei)))
              (gwei-text (getf state :priority-gwei)))
      (format out "ETH ~a - a plain transfer ~a, an ERC-20 transfer ~a, a swap ~a~%"
              (dollars (getf state :eth-price))
              (dollars (getf state :cost-transfer))
              (dollars (getf state :cost-erc20))
              (dollars (getf state :cost-swap)))
      (when (getf state :endpoint)
        (when (string= detail "full")
          (format out "rpc ~a~%" (getf state :endpoint))))
      (format out "read-only: nothing signed, no transaction sent. State the block above with any answer about fees."))))

(defun tool-network (call)
  "base_network: chain state and what three actions cost in USD."
  (format-network (or (network-call-detail call) "summary")))

;;; --- base_portfolio ---------------------------------------------------------------

(nlk:define-contract portfolio-call
  (:doc "Native ETH plus ERC-20 balances with USD values for one Base address, from a curated
token set and priced with DefiLlama. Balances come straight from the chain; a token with no price
reports \"no price\" rather than a guess. Pass extra token addresses for anything outside the
set. Read-only.")
  ("address" :text :required t
   :doc "the wallet address to read, 0x-prefixed, 40 hex digits")
  ("extra_tokens" :array :of :string
   :doc "further token contract addresses to include beyond the built-in set (up to 25)")
  ("include_zero" :boolean
   :doc "include positions with a zero balance; useful to prove a wallet sold everything"))

(defun format-portfolio (address extras include-zero)
  "The portfolio answer."
  (let ((wallet (portfolio address :extras extras :include-zero (and include-zero t))))
    (with-output-to-string (out)
      (format out "wallet ~a - ~a across ~d position~:p (scanned ~d tokens, prices from ~{~a~^, ~})~%"
              (getf wallet :address) (dollars (getf wallet :total))
              (getf wallet :token-count) (getf wallet :scanned)
              (or (getf wallet :price-sources) '("none")))
      (if (null (getf wallet :holdings))
          (format out "  (every position is empty; pass include_zero to see them)~%")
          (loop for holding in (getf wallet :holdings)
                do (format out "  ~a: ~a ~@[~a ~]~a~%"
                           (or (getf holding :symbol) "unknown token")
                           (amount (getf holding :balance))
                           (when (getf holding :address) (format nil "~a" (getf holding :address)))
                           (if (getf holding :value) (dollars (getf holding :value)) (unknown-price)))))
      (format out "as of ~a; balances are on-chain, USD values are DefiLlama's. Read-only."
              (getf wallet :as-of)))))

(defun tool-portfolio (call)
  "base_portfolio: balances and their dollar value for one address."
  (format-portfolio (portfolio-call-address call)
                    (portfolio-call-extra-tokens call)
                    (portfolio-call-include-zero call)))

;;; --- base_tx ----------------------------------------------------------------------

(nlk:define-contract tx-call
  (:doc "Decode one Base transaction into a human-readable receipt: success or reverted, block
and UTC time, from/to, ETH value, gas used, the fee in ETH and USD, and every ERC-20 / ERC-721
movement and approval it emitted. Use it instead of guessing what a hash did. Read-only.")
  ("hash" :text :required t :slot tx-hash
   :doc "the transaction hash, 0x-prefixed, 64 hex digits"))

(defun format-receipt (hash)
  "The transaction answer."
  (let ((tx (tx-receipt hash)))
    (with-output-to-string (out)
      (cond
        ((string= (getf tx :status) "pending")
         (format out "tx ~a is known but not yet mined - no receipt to decode~%from ~a to ~a~%"
                 (getf tx :hash) (address-line (getf tx :from)) (address-line (getf tx :to))))
        (t
         (format out "tx ~a: ~a in block ~a~@[ at ~a~]~%"
                 (getf tx :hash) (getf tx :status) (getf tx :block) (getf tx :timestamp))
         (format out "from ~a to ~a, ~a~%"
                 (address-line (getf tx :from)) (address-line (getf tx :to))
                 (eth-text (getf tx :value)))
         (when (getf tx :created)
           (format out "deployed contract ~a~%" (getf tx :created)))
         (format out "gas ~a at ~a - fee ~a (~a, ETH ~a)~%"
                 (getf tx :gas-used) (gwei-text (getf tx :gas-gwei))
                 (eth-text (getf tx :fee-eth)) (dollars (getf tx :fee-usd))
                 (dollars (getf tx :eth-price)))
         (format out "~d log~:p, ~d token movement~:p, ~d approval~:p~%"
                 (getf tx :log-count) (length (getf tx :transfers)) (length (getf tx :approvals)))
         (dolist (transfer (getf tx :transfers))
           (format out "  ~a ~a ~a from ~a to ~a~@[ (~a)~]~@[ token #~a~]~%"
                   (getf transfer :standard)
                   (or (getf transfer :symbol) "token")
                   (amount (getf transfer :amount))
                   (getf transfer :from) (getf transfer :to)
                   (and (getf transfer :value) (dollars (getf transfer :value)))
                   (getf transfer :token-id)))
         (dolist (approval (getf tx :approvals))
           (format out "  approval: ~a ~a to spender ~a~@[ (unlimited)~]~%"
                   (or (getf approval :symbol) "token")
                   (amount (getf approval :amount)) (getf approval :spender)
                   (getf approval :unlimited)))
         (format out "read-only: the hash was read, never resent."))))))

(defun tool-tx (call)
  "base_tx: one transaction decoded."
  (format-receipt (tx-call-tx-hash call)))

;;; --- base_token -------------------------------------------------------------------

(nlk:define-contract token-call
  (:doc "Contract-level facts for one Base token: name, symbol, decimals, total supply, owner()
when exposed, proxy detection (EIP-1967 storage slot, EIP-1167 clone, or a contract that answers
implementation()), and capability hints from 4-byte selectors in the deployed code. The hints are
labelled as heuristics - this is not honeypot or rug detection. With a Basescan key in settings it
also reports whether the source is verified. Read-only.")
  ("address" :text :required t
   :doc "the token or contract address to inspect, 0x-prefixed, 40 hex digits"))

(defun format-token (address)
  "The token answer."
  (let ((facts (token-facts address)))
    (with-output-to-string (out)
      (format out "contract ~a~%" (getf facts :address))
      (unless (getf facts :contract)
        (format out "~a~%" (getf facts :note)))
      (when (getf facts :contract)
        (format out "~@[~a~]~@[ (~a)~]~@[, ~a decimals~]~@[, total supply ~a~] - ~{~a~^, ~}~%"
                (getf facts :symbol) (getf facts :name) (getf facts :decimals)
                (and (getf facts :supply) (amount (getf facts :supply)))
                (getf facts :standards))
        (when (getf facts :owner)
          (format out "owner() ~a~%" (getf facts :owner)))
        (let ((proxy (getf facts :proxy)))
          (format out "proxy: ~a~%"
                  (if (getf proxy :proxy)
                      (format nil "~a, implementation ~a~@[, admin ~a~]"
                              (getf proxy :pattern) (or (getf proxy :implementation) "unread")
                              (getf proxy :admin))
                      "none found in the EIP-1967 slots or the bytecode patterns")))
        (format out "bytecode hints (4-byte selector presence, not proof): ~{~a~^, ~}~%"
                (or (getf facts :bytecode-flags) '("none of the probed selectors")))
        (format out "source code: ~a~%" (getf (getf facts :verification) :status))
        (format out "read-only: nothing called that could change state.")))))

(defun tool-token (call)
  "base_token: contract facts, proxy detection and labelled hints."
  (format-token (token-call-address call)))

;;; --- base_activity ----------------------------------------------------------------

(nlk:define-contract activity-call
  (:doc "Recent ERC-20 transfers in and out of one address for ONE token, newest first, with USD
values. Public Base RPCs reject chain-wide log scans, so this takes a token address and a block
window and reports the window it actually read. Read-only.")
  ("address" :text :required t
   :doc "the wallet address whose transfers you want, 0x-prefixed, 40 hex digits")
  ("token" :text :required t
   :doc "the token contract to scan; naming it is what makes the log query legal on a public endpoint")
  ("blocks" :integer
   :doc "how many blocks back to scan (default the section's log_window, at most 50000; Base is about 2 seconds a block)")
  ("limit" :integer
   :doc "the most transfers to answer with (default 25, at most 200)"))

(defun format-activity (address token blocks limit)
  "The transfers answer."
  (let ((activity (token-activity address token :blocks blocks :limit limit))
        (window nil))
    (setf window (getf activity :window))
    (with-output-to-string (out)
      (format out "~a on ~a - blocks ~a to ~a (~a blocks, about ~,1f hours; ~a asked for)~%"
              (getf activity :address)
              (or (getf (getf activity :token) :symbol) (getf (getf activity :token) :address))
              (getf window :from) (getf window :to) (getf window :blocks)
              (/ (* 2 (getf window :blocks)) 3600.0) (getf window :requested))
      (if (zerop (getf activity :count))
          (format out "no transfers in that window - which proves nothing about earlier blocks~%")
          (loop for transfer in (getf activity :transfers)
                do (format out "  block ~a ~a ~a ~a ~a~@[ (~a)~] tx ~a~%"
                           (getf transfer :block)
                           (if (string= (getf transfer :direction) "out") "out" "in")
                           (amount (getf transfer :amount))
                           (if (string= (getf transfer :direction) "out") "to" "from")
                           (getf transfer :counterparty)
                           (getf transfer :value)
                           (getf transfer :tx))))
      (format out "the window is the claim: a transfer outside it is not in this answer. Read-only."))))

(defun tool-activity (call)
  "base_activity: transfers in and out of one address for one token."
  (format-activity (activity-call-address call) (activity-call-token call)
                   (activity-call-blocks call) (activity-call-limit call)))

;;; --- base_allowances --------------------------------------------------------------

(nlk:define-contract allowances-call
  (:doc "Approvals an owner granted on ONE token: which spenders they approved inside the scan
window, the CURRENT on-chain allowance for each (re-read live, so already-revoked ones are
dropped), whether it is unlimited, and the USD value at risk. Answers 'what can still drain this
wallet'. Window-bounded like base_activity, and it never revokes anything. Read-only.")
  ("owner" :text :required t
   :doc "the wallet that granted the approvals, 0x-prefixed, 40 hex digits")
  ("token" :text :required t
   :doc "the token contract whose approvals you want")
  ("blocks" :integer
   :doc "how many blocks back to look for Approval events (default the section's log_window, at most 50000)"))

(defun format-allowances (owner token blocks)
  "The approvals answer."
  (let ((report (token-allowances owner token :blocks blocks)))
    (with-output-to-string (out)
      (let ((window (getf report :window)))
        (format out "approvals by ~a on ~a - blocks ~a to ~a (~a blocks)~%"
                (getf report :owner)
                (or (getf (getf report :token) :symbol) (getf (getf report :token) :address))
                (getf window :from) (getf window :to) (getf window :blocks)))
      (if (zerop (getf report :active-count))
          (format out "no allowance is still active among the spenders of that window~%")
          (progn
            (format out "~d active~@[, ~a unlimited~]:~%"
                    (getf report :active-count)
                    (let ((unlimited (getf report :unlimited-count)))
                      (and (plusp unlimited) unlimited)))
            (loop for row in (getf report :allowances)
                  do (format out "  spender ~a allowance ~a~@[ (~a)~]~@[ (unlimited)~]~%"
                             (getf row :spender) (amount (getf row :allowance))
                             (and (getf row :value) (dollars (getf row :value)))
                             (getf row :unlimited)))))
      (format out "the window bounds which spenders were seen; each allowance is re-read on-chain.~%")
      (format out "basekit never revokes - it reports; the user decides. Read-only."))))

(defun tool-allowances (call)
  "base_allowances: approvals still active, with the value at risk."
  (format-allowances (allowances-call-owner call) (allowances-call-token call)
                     (allowances-call-blocks call)))
