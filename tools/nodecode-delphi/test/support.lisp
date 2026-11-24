;;;; support.lisp --- the test slice: a stubbed Delphi API, subgraph and node.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; DELPHI-CELL- name prefix; RUN-DELPHI-TESTS runs exactly that slice.
;;;;
;;;; The two seams that reach outside are NODECODE-DELPHI::HTTP, the cell's
;;;; only way to the network, and UIOP:GETENV, the only way to the key.
;;;; WITH-DELPHI replaces both with canned answers and records every call,
;;;; so a test asserts on the request and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "delphi" "DELPHI-CELL-" :start nodecode-delphi:start-cell)

;;; --- the canned answers -----------------------------------------------------------

(defparameter +market-1+
  "{\"id\":\"0x1111111111111111111111111111111111111111\",\"appMarketId\":\"11111111-1111-4111-8111-111111111111\",\"marketUrl\":\"https://testnet.delphi.fyi/market/11111111-1111-4111-8111-111111111111\",\"status\":\"open\",\"category\":\"crypto\",\"metadata\":{\"question\":\"Will the network ship in May?\",\"outcomes\":[\"Yes\",\"No\"]},\"settlesAt\":\"2026-05-01T00:00:00Z\",\"tradingFee\":\"10000000000000000\",\"verifiable\":true}"
  "One open market: two outcomes, a 1% fee, a settle date.")

(defparameter +markets-page+
  (format nil "{\"markets\":[~a]}" +market-1+)
  "A page holding that one market.")

(defparameter +positions-page+
  "{\"positions\":[{\"marketProxy\":\"0x1111111111111111111111111111111111111111\",\"outcomeIdx\":\"1\",\"shares\":\"3500000000000000000\",\"redeemedOrLiquidated\":false,\"marketStatus\":\"open\"}]}"
  "One wallet holding 3.5 shares of outcome 1.")

(defparameter +absent+
  "{\"message\":\"Not Found\"}"
  "What an address the API does not list answers.")

(defparameter +meta-data+
  "{\"data\":{\"_meta\":{\"block\":{\"number\":18934221,\"timestamp\":1789000000,\"hash\":\"0xabc\"},\"deployment\":\"QmDeployment\",\"hasIndexingErrors\":false}}}"
  "The subgraph's own position in the chain.")

(defparameter +trades-data+
  "{\"data\":{\"gatewayBuys\":[{\"timestamp_\":\"1789000000\",\"outcomeIdx\":\"0\",\"tokensIn\":\"1500000000000000000\",\"sharesOut\":\"2500000000000000000\",\"buyer\":\"0xaaaa\"}],\"gatewaySells\":[]}}"
  "One buy, no sells.")

(defparameter +settlement-data+
  "{\"data\":{\"gatewayMarketSettleds\":[{\"timestamp_\":\"1789000000\",\"winningOutcomeIdx\":\"1\",\"marketCreatorReward\":\"1000\",\"refund\":null}],\"gatewayMarketFaileds\":[],\"marketResolutionRequesteds\":[]}}"
  "A market that resolved, outcome 1 winning.")

;;; --- the stub --------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers :content).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for the fixtures.")

(defvar *key* "key-1"
  "What the environment answers for the key variable, or NIL for no key.")

(defparameter +key-env+ "TEST_DELPHI_ACCESS_KEY"
  "The variable the tests' section names.")

(defparameter +word-zero+ (make-string 64 :initial-element #\0)
  "One empty 32-byte word.")

(defun word (number) (format nil "~64,'0x" number))

(defun stub-rpc (content)
  "The node's answer to CONTENT, chosen by the selector the call carries."
  (let ((mine (nodecode-delphi::selector "marketProxiesExist(address[])"))
        (buy (nodecode-delphi::selector "quoteBuyExactOut(address,uint256,uint256)"))
        (sell (nodecode-delphi::selector "quoteSellExactIn(address,uint256,uint256)"))
        (decimals (nodecode-delphi::selector "decimals()"))
        (spot (nodecode-delphi::selector "spotImpliedProbabilities(address,uint256[])")))
    (cond
      ((cl:search mine content)
       (values (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x~a~a~a\"}"
                       (word 32) (word 1) (word 1)) 200))
      ((cl:search buy content)
       (values (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x~a\"}" (word 420000000000000000)) 200))
      ((cl:search sell content)
       (values (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x~a\"}" (word 300000000000000000)) 200))
      ((cl:search decimals content)
       (values (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x~a\"}" (word 18)) 200))
      ((cl:search spot content)
       (values (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x~a~a~a~a\"}"
                       (word 32) (word 2) (word 400000000000000000) (word 600000000000000000)) 200))
      (t (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"message\":\"execution reverted\"}}" 200)))))

(defun stub-shape (call)
  "The fixture to answer CALL with; a POST is told apart by host and selector."
  (let ((url (getf call :url)) (content (getf call :content)))
    (cond
      ((cl:search "goldsky" url)
       (cond ((cl:search "_meta" content) (values +meta-data+ 200))
             ((cl:search "gatewayBuys" content) (values +trades-data+ 200))
             ((cl:search "gatewayMarketSettleds" content) (values +settlement-data+ 200))
             (t (values "{\"errors\":[{\"message\":\"unknown field\"}]}" 200))))
      ((cl:search "alchemy" url) (stub-rpc content))
      ((cl:search "0x2222" url) (values +absent+ 404))
      ((cl:search "/positions" url) (values +positions-page+ 200))
      ((cl:search "/markets/0x1111" url) (values +market-1+ 200))
      ((cl:search "/markets" url) (values +markets-page+ 200))
      (t (values +absent+ 404)))))

(defun stub-http (method url &key headers timeout content)
  (declare (ignore timeout))
  (push (list :method method :url url :headers headers :content content) *calls*)
  (if *answer* (funcall *answer* (first *calls*)) (stub-shape (first *calls*))))

(defmacro with-wire (&body body)
  "BODY with the cell's one HTTP seam stubbed, the environment answering
*KEY* for the key variable, and every call recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinitions
         ((nodecode-delphi::http (method url &key headers timeout content)
                                  (stub-http method url :headers headers :timeout timeout :content content))
          (uiop:getenv (name)
                       (and *key* (equal name +key-env+) *key*)))
       ,@body)))

(defmacro with-delphi ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS (the key variable always among them),
its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((delphi-start "api_key_env" +key-env+ ,@pairs))
       ,@body)))

;;; --- calling it ---------------------------------------------------------------

(defun delphi-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun call-urls ()
  "The url of every request the stub took, newest first."
  (mapcar (lambda (call) (getf call :url)) *calls*))

(defun call-header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun call-posts ()
  "Every POST the stub took, newest first."
  (remove-if-not (lambda (call) (eq (getf call :method) :post)) *calls*))
