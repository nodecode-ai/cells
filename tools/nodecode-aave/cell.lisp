;;;; cell.lisp --- Aave through its own MCP server: the tools, the section,
;;;; the primer (the workflow Aave's skills teach), and the /aave command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server serves its own tool set at run time, so the table below is
;;;; the everyday surface Aave's own guide names, and AAVE_CALL reaches the
;;;; rest: a tool the server grew after this cell was written is still one
;;;; call away, and /aave tools lists the drift.

(in-package #:nodecode-aave)

(nlk:define-peripheral aave :not-running t)

(defparameter +primer+
  "Aave V3 and V4 live through Aave's own MCP server, which serves every tool here at run time.
The chain is discover -> inspect -> simulate -> build, then hand over UNSIGNED:
  get_markets (symbols, no chainId: one call covers every chain) gives the selectors - v4 reserveId,
  v3 market + token + chainId - which are deployment-specific and cannot be recalled or constructed.
  get_user_summary and get_user_positions for the wallet, get_reserve_details for the reserve: an
  action sized without the position is a guess. preview_action with the exact parameters before
  building (mandatory before any borrow or withdraw): it returns healthFactorAfter, the build does
  not, and an error-level warning means the action cannot succeed - fix it rather than build it.
  prepare_action, or another prepare_* tool, returns an unsigned transaction and a plan; follow the
  plan in order - an approval may come first - and confirm with get_transaction_processed before
  building a step that depends on it. Nothing is ever signed here: signing is the user's move, so
  say what a transaction will do once signed and that nothing has been sent.
Amounts are main units (\"10.5\" = 10.5 USDC), never wei; a name ending Pct is a percent. Every
successful answer is {data, next_actions?, warnings?}: follow next_actions, and treat warnings by
level - error: stop; warning: reach the user; info: context. Health factors are per market (v3) or
per position (v4), never averaged; below 1.0 is liquidatable now, below 1.1 is urgent. A supply is
not collateral unless enabled (enableCollateral: true). History on v3 is one market on one chain per
call. When ranking yields, skip frozen, paused and cap-reached reserves, check capacity against the
size asked for, and say whether the answer is a sample or the population. Governance
(search_governance_proposals, get_governance_proposal) is the DAO's, not a market's: it takes no
version. Rewards are read with get_user_rewards and claimed with prepare_claim_rewards (v4).
AAVE_CALL (tool, arguments) reaches any tool the server serves, including one this cell does
not name; /aave tools lists what it serves now. A wallet's positions are public data the server reads; it holds no keys."
  "What (help :aave) answers while the cell runs.")

(defparameter *server-tools*
  '(("get_started"
     "Applies to both versions. What Aave's server can do: its tool families, ready-made prompts and
guide topics. Call it on a cold turn, before a chain of calls."
     ())
    ("get_markets"
     "Aave V3 and V4 markets and their rates: reserve selectors (v4 reserveId; v3 market + token +
chainId), supply and borrow APY, caps and liquidity. Omit chainId so one call covers every chain;
pass user to add what that wallet holds and can supply or borrow."
     (("symbols" "array" "asset symbols to keep, e.g. [\"USDC\",\"WETH\"]; a symbol filter is what makes v3 cross-chain")
      ("chainId" "integer" "one chain; omit for every chain the server serves")
      ("version" "string" "all, v3 or v4 - all by default")
      ("user" "string" "a 0x wallet address, to add what it holds and can supply or borrow")))
    ("get_user_summary"
     "A wallet's Aave position summary: total collateral, total debt, borrowing room and the health
factor. A health factor below 1.0 is liquidatable now, below 1.1 urgent."
     (("user" "string" "the wallet's 0x address" t)
      ("version" "string" "all, v3 or v4")
      ("chainId" "integer" "one chain")))
    ("get_user_positions"
     "Every reserve a wallet holds on Aave, by asset, with the balances behind the summary. Read it
before changing a position."
     (("user" "string" "the wallet's 0x address" t)
      ("version" "string" "all, v3 or v4")
      ("chainId" "integer" "one chain")))
    ("get_wallet_balances"
     "V4 only. The tokens a wallet actually holds that v4 can take, per token and summed across
chains, with perChain saying where each sits: what a repay, a supply or a collateral top-up can be
paid from."
     (("user" "string" "the wallet's 0x address" t)
      ("version" "string" "v4")
      ("chainId" "integer" "one chain")))
    ("get_reserve_details"
     "One reserve's risk parameters and rate curve: liquidation threshold, LTV, caps, e-mode, and
whether it is frozen, paused or at its cap."
     (("reserveId" "string" "the v4 selector get_markets returned")
      ("market" "string" "the v3 market address")
      ("token" "string" "the v3 asset")
      ("chainId" "integer" "the chain the reserve is on")
      ("version" "string" "v3, v4 or all")))
    ("get_apy_history"
     "Historical supply or borrow APY for a reserve, as a time series of {date, apyPct}: quote the
recent range beside today's spot rate before recommending one."
     (("reserveId" "string" "the v4 selector")
      ("market" "string" "the v3 market address")
      ("token" "string" "the v3 asset")
      ("chainId" "integer" "the chain")
      ("side" "string" "supply or borrow")
      ("window" "string" "how far back, e.g. 30d")
      ("version" "string" "v3, v4 or all")))
    ("get_user_activity"
     "A wallet's Aave event feed, newest first: supplies, borrows, repays, withdrawals and collateral
changes, as the protocol indexed them. V4 covers every chain; v3 reads one market on one chain per
call, so pass chainId and market. Pass a page's next cursor back as cursor."
     (("user" "string" "the wallet's 0x address" t)
      ("version" "string" "v3, v4 or all")
      ("chainId" "integer" "the chain (v3 needs it; v4 covers every chain)")
      ("market" "string" "the v3 market address")
      ("cursor" "string" "the pageInfo.next of the page before")))
    ("get_user_summary_history"
     "V4 only. How one wallet's net worth, supplied, debt and health factor moved over a window: how
close to liquidation it has been."
     (("user" "string" "the wallet's 0x address" t)
      ("version" "string" "v4")
      ("window" "string" "how far back, e.g. 30d")
      ("chainId" "integer" "one chain")))
    ("get_user_rewards"
     "A wallet's claimable Aave rewards on v3, v4 or both, including Merit programmes: rewards that
are claimable now. Claiming is prepare_claim_rewards."
     (("user" "string" "the wallet's 0x address" t)
      ("version" "string" "v3, v4 or all")
      ("chainId" "integer" "one chain")))
    ("preview_action"
     "Simulate a supply, borrow, withdraw or repay against the wallet's position without executing it
(v4 also simulates a quoted order that moves a position, by quoteId), and answer the resulting
health factor. Run it on the exact parameters before every borrow and withdraw. An error-level
warning means the action cannot succeed as specified."
     (("action" "string" "supply, borrow, withdraw or repay" t)
      ("sender" "string" "the wallet the action is for" t)
      ("amount" "string" "main units, e.g. \"10.5\" - never wei")
      ("version" "string" "v3 or v4")
      ("reserveId" "string" "the v4 selector")
      ("market" "string" "the v3 market address")
      ("token" "string" "the v3 asset")
      ("chainId" "integer" "the chain")
      ("max" "boolean" "the whole balance instead of amount")
      ("native" "boolean" "the chain's native token")
      ("enableCollateral" "boolean" "a supply that backs a borrow needs this")
      ("quoteId" "string" "v4: the quote an order was priced by")))
    ("prepare_action"
     "Build the unsigned transaction, or the approval-then-action plan, for one supply, borrow,
withdraw or repay. The user's own wallet signs and submits it; nothing is sent from here."
     (("action" "string" "supply, borrow, withdraw or repay" t)
      ("sender" "string" "the wallet the transaction is for" t)
      ("amount" "string" "main units, e.g. \"10.5\" - never wei")
      ("version" "string" "v3 or v4")
      ("reserveId" "string" "the v4 selector")
      ("market" "string" "the v3 market address")
      ("token" "string" "the v3 asset")
      ("chainId" "integer" "the chain")
      ("max" "boolean" "the whole balance instead of amount")
      ("native" "boolean" "the chain's native token")
      ("enableCollateral" "boolean" "a supply that backs a borrow needs this")
      ("permitSignature" "string" "an EIP-2612 permit to attach")
      ("permitDeadline" "integer" "that permit's deadline")))
    ("get_repay_with_supply_quote"
     "V4 only. Quote paying a borrow down with supplied collateral in one atomic step, without the
funds leaving Aave: the health factor never passes through a worse state. V3 has no atomic route."
     (("debtPosition" "string" "the borrow to reduce, as a position id" t)
      ("repayWithReserve" "string" "the collateral to pay it with" t)
      ("amount" "string" "main units, or \"max\"" t)
      ("user" "string" "the wallet's 0x address" t)
      ("slippagePct" "string" "tolerance in percent")
      ("version" "string" "v4")))
    ("prepare_claim_rewards"
     "V4 only. Build the unsigned transaction that claims a wallet's v4 rewards; with no ids, every
reward claimable now."
     (("user" "string" "the wallet's 0x address" t)
      ("version" "string" "v4")
      ("chainId" "integer" "the chain")
      ("ids" "array" "the reward ids to claim; omit for all claimable")))
    ("get_sgho_vault"
     "V3 only, Ethereum only. Savings GHO (sGHO): an ERC-4626 vault paying a governance-set target
rate on deposited GHO - savings, not a collateral position."
     (("user" "string" "a wallet's 0x address, to add what it has deposited")
      ("version" "string" "v3")))
    ("search_governance_proposals"
     "Aave DAO governance proposals, newest first, by state or a text search. Governance is the DAO's,
not a market's: it takes no version."
     (("state" "string" "open, executed, failed, ...")
      ("search" "string" "words to match")
      ("includeSummaries" "boolean" "attach each proposal's summary")
      ("limit" "integer" "how many")
      ("offset" "integer" "how many to skip")))
    ("get_governance_proposal"
     "One Aave DAO governance proposal: state, votes, payloads. Takes no version."
     (("proposalId" "string" "its id, from search_governance_proposals" t)
      ("includeDescription" "boolean" "attach the full description")))
    ("get_transaction_processed"
     "V4 only. Whether a transaction's operations were processed, by its hash and the operations the
build returned: what clears the next step of a plan."
     (("txHash" "string" "the transaction hash the user sent" t)
      ("operations" "array" "the operations array the build returned" t "object")
      ("version" "string" "v4")))))

(defparameter +call-tool+ "aave_call"
  "The generic verb: any tool the server serves, named at call time.")

(defun tool-schema (name)
  "The schema the model is shown for NAME, off *SERVER-TOOLS*: its parameters,
and the ones the server will not take the call without."
  (let ((entry (assoc name *server-tools* :test #'string=)))
    (unless entry (fail "no Aave tool named ~a in this cell: ~{~a~^, ~}" name (mapcar #'first *server-tools*)))
    (let ((properties (nlk:json-object))
          (required '()))
      (dolist (spec (third entry))
        (destructuring-bind (key type doc &optional required-p item-type) spec
          (let ((property (nlk:json-object "type" type "description" doc)))
            (when (string= type "array")
              (setf (gethash "items" property) (nlk:json-object "type" (or item-type "string"))))
            (setf (gethash key properties) property)
            (when required-p (push key required)))))
      (let ((parameters (nlk:json-object "type" "object"
                                          "properties" properties
                                          "additionalProperties" t)))
        (when required
          (setf (gethash "required" parameters) (coerce (nreverse required) 'vector)))
        (list :description (second entry) :parameters parameters)))))

(defun tool-verb (name)
  "The body of the tool for the server's NAME: its arguments go as given."
  (lambda (arguments) (call-tool name (arguments-object arguments))))

(defun call-tool-schema ()
  "The schema of AAVE_CALL, the one verb this cell names without the table."
  (list :description "Call any tool Aave's MCP server serves, by name: the reach behind the named tools,
for a tool the server grew after this cell was written. /aave tools lists what it serves now."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "tool" (nlk:json-object "type" "string"
                                                            "description" "the server tool's name, e.g. get_markets")
                                   "arguments" (nlk:json-object "type" "object"
                                                                 "description" "that tool's arguments"))
                     "required" (vector "tool")
                     "additionalProperties" t)))

(defun call-any-tool (arguments)
  "The body of AAVE_CALL: (tool, arguments) forwarded to the server."
  (let ((tool (and (hash-table-p arguments) (gethash "tool" arguments)))
        (fields (and (hash-table-p arguments) (gethash "arguments" arguments))))
    (unless (and (stringp tool) (plusp (length (string-trim '(#\Space) tool))))
      (fail "aave_call needs \"tool\": the server tool's name, as /aave tools lists it"))
    (call-tool tool (arguments-object fields))))

;;; --- /aave ---------------------------------------------------------------

(defun status-text ()
  "What /aave answers: where it talks, whether a session is open, what it offers."
  (let ((endpoint (or (setting :endpoint) "")))
    (format nil "aave: ~a~%~a~%~d tool~:p: ~{~a~^, ~}~%~
/aave tools asks the server what it serves now; /aave forget closes the session."
            (if (plusp (length (string-trim '(#\Space) endpoint)))
                endpoint
                "(no endpoint set: set \"endpoint\" to https://mcp.aave.com in the aave section)")
            (if (getf *session* :ready)
                "session open with Aave's MCP server"
                "no session open: the first tool call opens one")
            (length *server-tools*)
            (append (mapcar #'first *server-tools*) (list +call-tool+)))))

(defun tools-text ()
  "What /aave tools answers: the server's own list, and what this cell does
not name (the drift a run-time-served surface has)."
  (handler-case
      (let* ((served (tool-names))
             (named (mapcar #'first *server-tools*))
             (extra (set-difference (mapcar #'car served) named :test #'string=))
             (gone (set-difference named (mapcar #'car served) :test #'string=)))
        (format nil "aave server at ~a serves ~d tool~:p:~%~{  ~a - ~a~%~}~a~a"
                (server-url) (length served)
                (loop for (name . description) in served
                      collect name
                      collect (first (uiop:split-string description :separator (string #\Newline))))
                (if extra
                    (format nil "~%~d this cell does not name (~{~a~^, ~}): call one with ~a"
                            (length extra) extra +call-tool+)
                    "")
                (if gone
                    (format nil "~%~d this cell names but the server did not list: ~{~a~^, ~}"
                            (length gone) gone)
                    "")))
    (aave-error (condition) (format nil "ERROR: ~a" condition))))

(defun run-command (args session-id)
  "The /aave command: no argument reports; tools asks the server; forget
closes the session."
  (declare (ignore session-id))
  (let ((verb (string-downcase (string-trim '(#\Space #\Tab) (or args "")))))
    (cond ((string= verb "tools") (tools-text))
          ((string= verb "forget") (reset-session) "aave: session closed; the next call opens one")
          (t (status-text)))))

;;; --- the cell ------------------------------------------------------------

(nle:define-cell aave
  (:section ("aave")
    (:guide "nothing is required: endpoint already names Aave's public server, every tool is read-only except the two that relay an order the user signed, and no key is held here")
    ("endpoint" :string :default "https://mcp.aave.com"
     :doc "Aave's MCP server, an https URL speaking streamable HTTP")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one call's ceiling, in seconds"))
  (:help :aave "aave:get_markets and AAVE_CALL answer live Aave V3/V4 markets, positions, health factor and unsigned transactions" +primer+)
  (:command "aave" 'run-command
            :description "Aave: the server, and what it serves"
            :session nil)
  (:tool "get_started" (tool-schema "get_started") (tool-verb "get_started"))
  (:tool "get_markets" (tool-schema "get_markets") (tool-verb "get_markets"))
  (:tool "get_user_summary" (tool-schema "get_user_summary")
         (tool-verb "get_user_summary"))
  (:tool "get_user_positions" (tool-schema "get_user_positions")
         (tool-verb "get_user_positions"))
  (:tool "get_wallet_balances" (tool-schema "get_wallet_balances")
         (tool-verb "get_wallet_balances"))
  (:tool "get_reserve_details" (tool-schema "get_reserve_details")
         (tool-verb "get_reserve_details"))
  (:tool "get_apy_history" (tool-schema "get_apy_history")
         (tool-verb "get_apy_history"))
  (:tool "get_user_activity" (tool-schema "get_user_activity")
         (tool-verb "get_user_activity"))
  (:tool "get_user_summary_history" (tool-schema "get_user_summary_history")
         (tool-verb "get_user_summary_history"))
  (:tool "get_user_rewards" (tool-schema "get_user_rewards")
         (tool-verb "get_user_rewards"))
  (:tool "preview_action" (tool-schema "preview_action")
         (tool-verb "preview_action"))
  (:tool "prepare_action" (tool-schema "prepare_action")
         (tool-verb "prepare_action"))
  (:tool "get_repay_with_supply_quote" (tool-schema "get_repay_with_supply_quote")
         (tool-verb "get_repay_with_supply_quote"))
  (:tool "prepare_claim_rewards" (tool-schema "prepare_claim_rewards")
         (tool-verb "prepare_claim_rewards"))
  (:tool "get_sgho_vault" (tool-schema "get_sgho_vault")
         (tool-verb "get_sgho_vault"))
  (:tool "search_governance_proposals" (tool-schema "search_governance_proposals")
         (tool-verb "search_governance_proposals"))
  (:tool "get_governance_proposal" (tool-schema "get_governance_proposal")
         (tool-verb "get_governance_proposal"))
  (:tool "get_transaction_processed" (tool-schema "get_transaction_processed")
         (tool-verb "get_transaction_processed"))
  (:tool "aave_call" (call-tool-schema) #'call-any-tool)
  (:start (lambda ()
            (reset-session)
            (nle:on-stop #'reset-session))))
