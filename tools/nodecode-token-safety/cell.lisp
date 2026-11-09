;;;; cell.lisp --- two tools, one command, the manual, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original skill told an agent to run curl before a trade. Here the
;;;; model calls token_safety (or swap_gate for a sized decision), a person
;;;; types /token-safety, and an eval form reads (token-safety:report "0x..."):
;;;; all three are the same GET, through the same seam, answering the same
;;;; report.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "token-safety": {"api_base": "https://x402-data-api.sigrunner.workers.dev",
;;;;                    "timeout_seconds": 20, "max_result_chars": 6000}
;;;; No key, no token, no signup: the preview is free and metered per address.

(in-package #:nodecode-token-safety)

;;; --- the boundary: what a model call carries --------------------------------------
;;; The original's two curl lines, as two shapes: the wire keys are the query
;;; parameters it sent, member for member.

(nlk:define-contract token-check
  (:doc "Run a pre-trade safety check on one Base (chain 8453) ERC-20 token and
answer the report: whether a buy and a sell both go through (a honeypot reverts
the sell), the transfer tax measured on a plain transfer, whether the owner has
renounced, whether the contract can mint, pause, blacklist or be upgraded
through a proxy, the Uniswap v3 liquidity in USD, and one SAFE / CAUTION /
AVOID verdict with a risk score and its reasons. Use it before buying, swapping
or approving a token you do not know, and report the reasons to the user.")
  ("token" :text :required t
   :doc "the token's contract address on Base: 0x followed by 40 hexadecimal characters"))

(nlk:define-contract swap-gate
  (:doc "Size one swap of a Base ERC-20 token before it is made: the same
honeypot, tax and owner checks, answered as a go / hold / block decision for a
given side and dollar size, with the reasons. Prefer it over token_safety when a
particular trade is about to happen.")
  ("token" :text :required t
   :doc "the token's contract address on Base: 0x followed by 40 hexadecimal characters")
  ("side" :text :options '("buy" "sell") :default "buy"
   :doc "the side of the trade the decision is sized for")
  ("notional_usd" :integer
   :doc "the size of the trade in whole US dollars, e.g. 500; the section's default_notional_usd answers when this is absent"))

;;; --- the manual ------------------------------------------------------------------

(defparameter *primer*
  "A pre-trade safety check for Base (chain 8453) ERC-20 tokens is available through this cell.
Call token_safety {token} before buying, swapping or approving a token you do not
know: it answers whether the sell goes through (a honeypot reverts it), the tax
measured on a plain transfer, whether the owner renounced, whether the contract
can mint, pause, blacklist or be upgraded, the Uniswap v3 liquidity in dollars,
and one verdict with a risk score and its reasons. Call swap_gate {token, side,
notional_usd} when a particular trade is about to happen: the same checks, sized
into go, hold or block.
  verdict AVOID   do not trade; read the reasons to the user.
  verdict CAUTION tradeable with risk; show the reasons and let the user decide.
  verdict SAFE    no trap found by these checks; not a guarantee.
buyTaxBps and sellTaxBps are not measured (they read null); only transferTaxBps
is, and liquidity is the Uniswap v3 WETH pool only. The preview is free and
metered at three token checks a day per address; every answer carries the API's
paid route in paid_route when a heavier check is wanted. The report comes from a
third-party service and is a heuristic, not financial advice."
  "What (help :token-safety) answers while the cell runs.")

;;; --- answers and refusals ---------------------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text rather than an unhandled error."
  `(handler-case (progn ,@body)
     (token-safety-error (condition)
       (nle:failure (format nil "ERROR: ~a" (token-safety-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: token safety: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- the three doors ---------------------------------------------------------------

(define-verb report (token &key raw)
  "Check TOKEN, a Base ERC-20 address, before a trade. Answers the report as
JSON, or as an object when RAW is true."
  (let ((object (report-answer (checked-address token))))
    (if raw object (reply object))))

(define-verb gate (token &key side notional-usd)
  "Size one trade of TOKEN -- SIDE is \"buy\" or \"sell\", NOTIONAL-USD whole
dollars -- as a go, hold or block decision. Answers it as JSON, or as an object
when RAW is true."
  (let ((object (gate-answer (checked-address token)
                              (checked-side side)
                              (checked-notional (or notional-usd (setting :default-notional-usd))))))
    (reply object)))

(defun token-check-tool (request)
  "The token_safety tool: the one member a call carries."
  (answering (report (token-check-token request))))

(defun swap-gate-tool (request)
  "The swap_gate tool: the members a call carries, the section's size when it
names none."
  (answering (gate (swap-gate-token request)
                   :side (or (swap-gate-side request) "buy")
                   :notional-usd (swap-gate-notional-usd request))))

;;; --- the operator's own door -------------------------------------------------------

(defun reason-lines (object)
  "The report's reasons, one line each, as the API wrote them."
  (let ((reasons (nlk:json-array object "reasons")))
    (with-output-to-string (out)
      (loop for reason across (or reasons #())
            do (format out "  - ~a~%"
                       (if (stringp reason)
                           reason
                           (or (nlk:json-value reason :string "text")
                               (nlk:json-value reason :string "reason")
                               (nlk:encode-json-object reason))))))))

(defun summary-text (address object)
  "OBJECT -- one report -- as the lines a person reads in chat."
  (format nil "~a: ~a~@[ (risk ~a/100)~]~%~a~@[paid route: ~a~]"
          address
          (string-upcase (or (nlk:json-value object :string "verdict")
                             (nlk:json-value object :string "decision")
                             "unknown"))
          (nlk:json-value object :integer "risk_score")
          (reason-lines object)
          (nlk:json-value object :string "paid_route")))

(defun run-slash (args session-id)
  "/token-safety 0x...: the operator's own pre-trade check, answered in chat."
  (declare (ignore session-id))
  (let ((address (nonblank args)))
    (if (null address)
        (format nil "token-safety: type a Base token address, e.g. /token-safety ~a. The model has the ~
token_safety and swap_gate tools; the free preview is metered at three token checks a day per address."
                "0x4200000000000000000000000000000000000006")
        (handler-case (summary-text (checked-address address) (report address :raw t))
          (token-safety-error (condition)
            (format nil "token-safety: ~a" (token-safety-error-detail condition)))
          (error (condition)
            (format nil "token-safety: ~a" (nlk:one-line (princ-to-string condition))))))))

;;; --- the section, read once --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with api_base required here: the report is asked from a
third-party host, so the section names it rather than a default naming one for
the operator."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (unless (nonblank (getf plist :api-base))
      (nlk:config-error "no api_base: name the Grey Ridge x402 API root the report is asked from -- ~
~a for the free preview, which needs no key"
                        +default-base-url+))
    (setf (getf plist :api-base) (string-right-trim "/" (nonblank (getf plist :api-base))))
    plist))

(nle:define-cell token-safety
  (:section ("token-safety")
    (:guide "paste the API root into api_base -- https://x402-data-api.sigrunner.workers.dev is the free preview and needs no key, no token and no signup; the timeout and the answer's length are all that is left")
    ("api_base" :string
                :doc "the Grey Ridge x402 API root the report is asked from, e.g. https://x402-data-api.sigrunner.workers.dev; no key is needed")
    ("timeout_seconds" :integer :default 20 :min 1
                       :doc "seconds one report may take")
    ("default_notional_usd" :integer :default 500 :min 1
                            :doc "the trade size swap_gate sizes when a call names none")
    ("max_result_chars" :integer :default 6000 :min 200
                        :doc "the longest answer one call returns; a longer report is clipped with a note"))
  (:settings #'read-settings)
  (:tool "token_safety" (nlk:contract 'token-check) #'token-check-tool)
  (:tool "swap_gate" (nlk:contract 'swap-gate) #'swap-gate-tool)
  (:command "token-safety" 'run-slash
            :description "Check a Base token before trading it"
            :argument-hint "0xTOKEN")
  (:help :token-safety
   "token-safety: honeypot, transfer tax and owner risk for a Base ERC-20, as the token_safety and swap_gate tools and (token-safety:report \"0x...\")"
   *primer*))
