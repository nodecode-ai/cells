;;;; cell-test.lisp --- the token-safety cell against a stubbed API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every endpoint is a stubbed NLK:HTTP: nothing reaches the API, a file or
;;;; the environment. Each tool is called the way a model's call reaches it --
;;;; through the real dispatch, contract and all -- so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "token-safety" "TOKEN-SAFETY-CELL-" :start nodecode-token-safety:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter +base+ "https://preview.example"
  "The api_base every test names; the host is never dialled.")

(defparameter +token+ "0x4200000000000000000000000000000000000006"
  "A Base token address in the shape the guard wants.")

(defparameter +report-json+
  "{\"token\":\"0x4200000000000000000000000000000000000006\",\"verdict\":\"AVOID\",\"risk_score\":82,\"reasons\":[\"the sell simulation reverts: a honeypot\",\"the owner can still mint\"],\"security\":{\"isHoneypot\":true,\"sellable\":false,\"transferTaxBps\":500,\"ownerRenounced\":false,\"canMint\":true,\"canPause\":false,\"hasBlacklist\":true,\"upgradeableProxy\":false,\"buyTaxBps\":null},\"liquidity\":{\"wethPoolUsd\":12345},\"upgrade\":{\"paid_url\":\"https://paid.example/chain/token-report?token=0x4200000000000000000000000000000000000006\",\"price_usdc\":\"0.01\",\"pay_with\":\"npm i x402-fetch\"}}"
  "One report, as the free preview answers it.")

(defparameter +gate-json+
  "{\"token\":\"0x4200000000000000000000000000000000000006\",\"decision\":\"block\",\"risk_score\":82,\"reasons\":[\"a sell of this size would revert\"],\"checks\":{\"honeypot\":true,\"ownerRenounced\":false}}"
  "One sized decision, as the swap gate answers it.")

(defmacro with-token-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (declare (ignore options))
                                   (push (list method url) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun token-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun token-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (token-answer id tool arguments)))

(defmacro with-cell (&body body)
  "A started cell whose section names +BASE+, stopped afterwards."
  `(with-cell-stop ((token-safety-start "api_base" +base+)) ,@body))

;;; --- the lifecycle, from the one declaration -----------------------------------------

(define-cell-lifecycle-tests "token-safety"
  (:config "api_base" +base+)
  (:help :token-safety)
  (:command "token-safety")
  (:running
   (is (nle::find-tool "token_safety") "token_safety is registered")
   (is (nle::find-tool "swap_gate") "swap_gate is registered")
   (is (equal +base+ (nodecode-token-safety::setting :api-base)) "the section's api_base"))
  (:stopped
   (is (null (nle::find-tool "token_safety")) "stop unregisters token_safety")
   (is (null (nle::find-tool "swap_gate")) "and swap_gate"))
  (:refused
   ()
   ("api_base" "")
   ("api_base" +base+ "max_result_chars" 5)
   ("api_base" +base+ "timeout_seconds" 0))
  (:idle nodecode-token-safety:token-safety-error (token-safety:report +token+)))

(deftest token-safety-cell-a-missing-api-base-is-named ()
  (let ((condition (signals-error nlk:config-refusal (token-safety-start))))
    (is (cl:search "no api_base" (princ-to-string condition)) "the refusal says what is missing")
    (is (cl:search "x402-data-api" (princ-to-string condition)) "and names the host the free preview is on")
    (is (null (nle::find-tool "token_safety")) "with nothing installed")))

;;; --- token_safety --------------------------------------------------------------------

(deftest token-safety-cell-the-token-tool-answers-the-report ()
  (with-token-wire (asked ("/chain/token-report/preview" (values +report-json+ 200)))
    (with-cell
      (let* ((object (token-object "ts-1" "token_safety"
                                    (format nil "{\"token\":\"~a\"}" +token+)))
             (security (nlk:json-value object :object "security")))
        (is (equal "AVOID" (nlk:json-value object :string "verdict")) "the verdict comes through")
        (is (= 82 (nlk:json-value object :integer "risk_score")) "with its risk score")
        (is (equal +token+ (nlk:json-value object :string "token")) "and the token the answer is about")
        (is (eq t (nlk:json-value security :boolean "is_honeypot")) "the honeypot flag, in snake_case")
        (is (= 500 (nlk:json-value security :integer "transfer_tax_bps")) "the measured transfer tax")
        (is (= 12345 (nlk:json-value (nlk:json-value object :object "liquidity") :integer "weth_pool_usd")) "and the pool's liquidity")
        (is (equal 2 (length (nlk:json-array object "reasons"))) "both reasons, as written")
        (is (cl:search "0.01" (nlk:json-value object :string "paid_route")) "the paid route is lifted into one line")
        (is (null (nlk:json-value object :object "upgrade")) "and not repeated whole")
        (is (cl:search "free preview" (nlk:json-value object :string "tier")) "and the answer says what it cost")
        (is (= 1 (length asked)) "one request")
        (is (cl:search (format nil "token=~a" +token+) (second (first asked))) "carrying the address")
        (is (cl:search "preview.example" (second (first asked))) "on the section's api_base")))))

(deftest token-safety-cell-a-bad-address-refuses-without-a-request ()
  (with-token-wire (asked)
    (with-cell
      (let ((text (token-answer "ts-2" "token_safety" "{\"token\":\"0xnothex\"}")))
        (is (cl:search "40 hexadecimal" text) "the refusal names the shape")
        (is (cl:search "ERROR" text) "as a failed call, not an answer")
        (is (null asked) "and nothing was dialled")))))

(deftest token-safety-cell-a-rate-limited-preview-names-the-free-tier ()
  (with-token-wire (asked ("/chain/token-report/preview"
                            (values "{\"error\":\"daily cap reached\"}" 429)))
    (with-cell
      (let ((text (token-answer "ts-3" "token_safety"
                                 (format nil "{\"token\":\"~a\"}" +token+))))
        (is (cl:search "429" text) "the status is said")
        (is (cl:search "three real-token checks" text) "and what the free tier allows")
        (is (cl:search "daily cap reached" text) "with the API's own sentence")))))

(deftest token-safety-cell-the-report-is-clipped-to-the-section-s-length ()
  (with-token-wire (asked ("/chain/token-report/preview" (values +report-json+ 200)))
    (with-cell-stop ((token-safety-start "api_base" +base+ "max_result_chars" 200))
      (let ((text (token-answer "ts-4" "token_safety"
                                 (format nil "{\"token\":\"~a\"}" +token+))))
        (is (cl:search "clipped" text) "the answer says it was clipped")
        (is (< (length text) 400) "and stays near the section's length")))))

;;; --- swap_gate ------------------------------------------------------------------------

(deftest token-safety-cell-the-gate-sends-the-side-and-the-size ()
  (with-token-wire (asked ("/gate/swap/preview" (values +gate-json+ 200)))
    (with-cell
      (let* ((object (token-object "ts-5" "swap_gate"
                                    (format nil "{\"token\":\"~a\",\"side\":\"sell\",\"notional_usd\":250}" +token+)))
             (url (second (first asked))))
        (is (cl:search "side=sell" url) "the side a call names rides the query")
        (is (cl:search "notional_usd=250" url) "as does the size")
        (is (equal "block" (nlk:json-value object :string "decision")) "the decision comes through")
        (is (equal "sell" (nlk:json-value object :string "side")) "with the side it was sized for")
        (is (= 250 (nlk:json-value object :integer "notional_usd")) "and that size")
        (is (eq t (nlk:json-value (nlk:json-value object :object "checks") :boolean "honeypot")) "the checks are relabelled, not dropped")))))

(deftest token-safety-cell-the-gate-sizes-a-call-that-names-no-size ()
  (with-token-wire (asked ("/gate/swap/preview" (values +gate-json+ 200)))
    (with-cell-stop ((token-safety-start "api_base" +base+ "default_notional_usd" 300))
      (token-object "ts-6" "swap_gate" (format nil "{\"token\":\"~a\"}" +token+))
      (let ((url (second (first asked))))
        (is (cl:search "side=buy" url) "an absent side is a buy")
        (is (cl:search "notional_usd=300" url) "and an absent size is the section's")))))

;;; --- the command --------------------------------------------------------------------

(deftest token-safety-cell-the-command-answers-a-line-per-reason ()
  (with-token-wire (asked ("/chain/token-report/preview" (values +report-json+ 200)))
    (with-cell
      (let ((text (nodecode-token-safety::run-slash (concatenate 'string " " +token+) "s1")))
        (is (cl:search "AVOID" text) "the verdict stands at the head")
        (is (cl:search (format nil "risk 82") text) "with the score")
        (is (cl:search "a honeypot" text) "and the reasons as lines")))))

(deftest token-safety-cell-the-command-typed-alone-says-what-to-type ()
  (with-token-wire (asked)
    (with-cell
      (let ((text (nodecode-token-safety::run-slash "" "s1")))
        (is (cl:search "token_safety" text) "it names the tools")
        (is (cl:search "0x4200" text) "and an address to try")
        (is (null asked) "without asking anything")))))
