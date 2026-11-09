;;;; cell.lisp --- the primer, the section, the /basekit command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin registered six tools, a /basekit command, a CLI command, a
;;;; skill and a prompt hook. The tools and the command are here; the skill is
;;;; +PRIMER+, which the model reads with (help :basekit) and whose one line
;;;; rides every request (see README, "Not ported").
;;;;
;;;; Config, a sibling top-level key:
;;;;   "basekit": {"rpc_url": "", "watch_address": "", "timeout_seconds": 20,
;;;;               "log_window": 20000, "basescan_api_key": ""}
;;;; Nothing in it is required: with no section at all the cell reads Base
;;;; through the keyless public endpoint. A vetoed section installs nothing.

(in-package #:nodecode-basekit)

(defparameter +primer+
  "basekit reads Base (chain 8453) and never writes: no key, no signing, no
transaction, nothing sent but a JSON-RPC read. Every answer names the block it
was read at, so an answer is only as fresh as that block. Six tools:
  base_network      chain state now: head block, gas and base fee in gwei,
                    ETH/USD, and what a plain transfer, an ERC-20 transfer and
                    a swap cost in dollars. Ask it before quoting any fee.
  base_portfolio    ETH plus ERC-20 balances with USD values for one address,
                    from a curated token set; extra_tokens adds contracts, and
                    a token with no price says so instead of guessing.
  base_tx           one transaction decoded: success or reverted, block and UTC
                    time, from/to, value, gas, the fee in ETH and USD, every
                    token movement and approval it emitted.
  base_token        contract facts: name, symbol, decimals, supply, owner(),
                    proxy detection (EIP-1967 slot, EIP-1167 clone,
                    implementation()) and selector hints labelled as hints.
  base_activity     transfers in and out of one address for ONE token inside a
                    stated block window (public endpoints cap eth_getLogs, so
                    the window is part of the answer).
  base_allowances   approvals an owner still has on one token, each re-read
                    on-chain (revoked ones dropped), with the USD at risk.
How to work: read before you claim. Quote the block with any number. A window
that read 20000 blocks says nothing about earlier ones, and a selector hint is
not a honeypot check. basekit cannot move funds; the user decides.
The operator runs a read from the keyboard with /basekit status, full,
wallet <address>, token <address> or activity <address> <token>."
  "What (help :basekit) answers while the cell runs: the plugin's skill as one topic.")

;;; --- the section's own checks ---------------------------------------------------------

(defun check-config ()
  "Refuse a section that could never reach Base, naming the member and what it
would accept. Nothing here is required: absent values read the public endpoint."
  (let ((url (nonempty (setting :rpc-url))))
    (when (and url (not (or (uiop:string-prefix-p "https://" url)
                            (uiop:string-prefix-p "http://" url))))
      (error 'nlk:config-refusal
             :detail (format nil "basekit: rpc_url is ~s, not an http or https endpoint; leave it empty for the keyless public Base RPC, or name the URL of your own node"
                              url))))
  (let ((watch (nonempty (setting :watch-address))))
    (when (and watch (not (address-p watch)))
      (error 'nlk:config-refusal
             :detail (format nil "basekit: watch_address is ~s, not a 0x address of 40 hex digits; /basekit wallet reads it when no address is typed"
                              watch)))))

;;; --- the operator's command -------------------------------------------------------

(defparameter +usage+
  "usage: /basekit status | full | wallet <address> | token <address> | activity <address> <token>")

(defun command-wallet (address)
  "The wallet view for ADDRESS, or the section's watch_address when none was typed."
  (let ((address (nonempty address)))
    (if address
        (format-portfolio address nil nil)
        (concatenate 'string
                     "basekit: no address given and no watch_address set; "
                     "run /basekit wallet 0x... or set watch_address in the basekit section"))))

(defun run-command (args session-id)
  "One /basekit invocation: with no argument the network summary, otherwise the
sub-command named."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (remove "" (uiop:split-string (nlk:trimmed (or args "")) :separator " ")
                            :test #'string=))
             (command (string-downcase (or (first words) "status"))))
        (cond
          ((member command '("status" "summary") :test #'string=) (format-network "summary"))
          ((string= command "full") (format-network "full"))
          ((string= command "wallet") (command-wallet (or (second words) (setting :watch-address))))
          ((string= command "token")
           (if (second words)
               (format-token (second words))
               "usage: /basekit token <address>"))
          ((string= command "activity")
           (if (and (second words) (third words))
               (format-activity (second words) (third words) nil 25)
               "usage: /basekit activity <address> <token>"))
          (t +usage+)))
    (basekit-error (condition)
      (format nil "basekit: ~a" condition))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell basekit
  (:section ("basekit")
    (:guide "nothing here is required: with no section at all the cell reads Base through a keyless public endpoint; rpc_url only for your own node or provider, watch_address for /basekit wallet with no argument, basescan_api_key only for source verification; next step: /basekit status")
    ("rpc_url" :string
     :doc "Base JSON-RPC endpoint; empty means the keyless public one (https://base-rpc.publicnode.com), and your own node or provider gives wider log windows and higher limits")
    ("watch_address" :string
     :doc "an optional wallet address for /basekit wallet with no argument")
    ("timeout_seconds" :integer :default 20 :min 2
     :doc "one RPC or price request's ceiling")
    ("log_window" :integer :default 20000 :min 500
     :doc "the default block window the transfer and approval reads scan (at most 50000; Base is about 2 seconds a block)")
    ("basescan_api_key" :secret
     :doc "optional Basescan/Etherscan key, used only by base_token's source-code check"))
  (:help :basekit
         "basekit: read-only Base (chain 8453) chain state, wallets, transactions, tokens and approvals"
         +primer+)
  (:command "basekit" 'run-command
            :description "Read-only Base: chain state, a wallet, a token, transfers, approvals"
            :argument-hint "[status|full|wallet <address>|token <address>|activity <address> <token>]"
            :session nil)
  (:tool "base_network" (nlk:contract 'network-call) #'tool-network)
  (:tool "base_portfolio" (nlk:contract 'portfolio-call) #'tool-portfolio)
  (:tool "base_tx" (nlk:contract 'tx-call) #'tool-tx)
  (:tool "base_token" (nlk:contract 'token-call) #'tool-token)
  (:tool "base_activity" (nlk:contract 'activity-call) #'tool-activity)
  (:tool "base_allowances" (nlk:contract 'allowances-call) #'tool-allowances)
  (:start (lambda ()
            (check-config))))
