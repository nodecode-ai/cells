;;;; cell-test.lisp --- lifecycle, the signature, the tools, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Sigrex: the config
;;;; surface (a section missing either credential refuses the start), the
;;;; signature against the published HMAC vector and the documented request
;;;; form, the three tools through the real dispatch seam against a stubbed
;;;; API and hook, and the refusals a model reads.

(in-package #:nodecode.test)

;;; --- the signature ---------------------------------------------------------------

(deftest sigrex-cell-the-hmac-matches-the-published-vector ()
  ;; HMAC-SHA256 keyed by "key" over the classic sentence: the one vector
  ;; every HMAC implementation is checked against.
  (is (string= "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"
               (nodecode-sigrex::hex (nodecode-sigrex::hmac-sha256 "key" "The quick brown fox jumps over the lazy dog"))) "the HMAC is HMAC-SHA256, lowercase hex"))

(deftest sigrex-cell-a-signature-over-a-documented-request-is-stable ()
  ;; The message the skills said to sign: method + path (with /api/v2) + the
  ;; body for POST and PUT + the timestamp, and nothing else.
  (is (string= "5553d6917f3fbb2e0350c83feb1481e0808c4cc5f9dbd49b211e6158c44af9a8"
               (nodecode-sigrex::signature "secret" "GET" "/api/v2/strategy/code" "" "1700000000")) "a GET signs the method, the path and the timestamp")
  (is (string= "9b5426930655b6ff9ebafb7b42deba9eeb074acb8ea3fb4833710fb031f9e0cb"
               (nodecode-sigrex::signature "secret" "POST" "/api/v2/strategy/code" "{\"status\":\"INACTIVE\"}" "1700000000")) "a POST signs the body it sends, as written"))

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "sigrex"
  (:config "api_key" +key+ "api_secret" +secret+)
  (:help :sigrex)
  (:command "sigrex")
  (:running
   (is (nle::find-tool "sigrex_strategies") "sigrex_strategies is registered")
   (is (nle::find-tool "sigrex_api") "sigrex_api is registered")
   (is (nle::find-tool "sigrex_signal") "sigrex_signal is registered")
   (is (equal +secret+ (nodecode-sigrex::setting :api-secret)) "the secret the section carries"))
  (:stopped
   (is (null (nle::find-tool "sigrex_strategies")) "stop unregisters the tools"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_SIGREX_KEY")
   ("api_key" +key+ "api_secret" "" "api_secret_env" "UNSET_SIGREX_SECRET")
   ("api_key" +key+ "api_secret" +secret+ "timeout_seconds" 0)))

(deftest sigrex-cell-without-a-credential-refuses-and-installs-nothing ()
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (sigrex-start "api_key" ""
                                                  "api_key_env" "UNSET_SIGREX_KEY" "api_secret" +secret+))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "sigrex_api")) "and installs nothing"))))

;;; --- the account, read -------------------------------------------------------------

(deftest sigrex-cell-strategies-are-signed-and-read-back (with-sigrex ()
  (let ((text (sigrex-call-tool "sigrex_strategies" "{}")))
    (is (cl:search "BTC momentum" text) "the first strategy's name")
    (is (cl:search "ACTIVE" text) "with its status")
    (is (cl:search "34" text) "and its id")
    (is (cl:search "ETH range" text) "the second one too"))
  (let ((call (first *calls*)))
    (is (= 1 (length *calls*)) "one request, not one a strategy")
    (is (equal :get (getf call :method)) "a list is a GET")
    (is (equal (concatenate 'string +api-host+ "/api/v2/strategy/code") (getf call :url)) "to the documented path, /api/v2 included")
    (is (equal +key+ (header call "api-key")) "with the section's key")
    (is (= 64 (length (header call "signature"))) "and a signature of sixty-four hex digits")
    (is (string= (string-downcase (header call "signature")) (header call "signature")) "written lowercase")
    (is (string= (nodecode-sigrex::signature +secret+ "GET" "/api/v2/strategy/code" ""
                                             (header call "timestamp"))
                 (header call "signature")) "over the method, the path and the timestamp this call sent"))))

(deftest sigrex-cell-the-llm-kind-reads-the-other-list (with-sigrex ()
  (sigrex-call-tool "sigrex_strategies" "{\"kind\":\"llm\"}")
  (is (equal (concatenate 'string +api-host+ "/api/v2/strategy/llm-session")
             (getf (first *calls*) :url)) "llm sessions are their own route")))

;;; --- the whole API, one call ---------------------------------------------------------

(deftest sigrex-cell-a-post-signs-the-body-it-sends (with-sigrex ()
  (sigrex-call-tool "sigrex_api" "{\"method\":\"PUT\",\"path\":\"/api/v2/bot/signal/cex/34/status\",\"body\":{\"status\":\"INACTIVE\"}}")
  (let ((call (first *calls*)))
    (is (equal :put (getf call :method)))
    (is (cl:search "/api/v2/bot/signal/cex/34/status" (getf call :url)))
    (is (cl:search "INACTIVE" (getf call :content)) "the body rides the request")
    (is (equal "application/json" (header call "content-type")))
    (is (string= (nodecode-sigrex::signature +secret+ "PUT"
                                             "/api/v2/bot/signal/cex/34/status"
                                             (getf call :content) (header call "timestamp"))
                 (header call "signature")) "and is signed exactly as it was sent"))))

(deftest sigrex-cell-a-path-without-the-prefix-gains-it (with-sigrex ()
  (nodecode-sigrex:api "GET" "/strategy/code")
  (is (cl:search "/api/v2/strategy/code" (getf (first *calls*) :url)) "a path written without /api/v2 is signed and sent with it")))

(deftest sigrex-cell-a-call-naming-no-path-never-reaches-the-wire (with-sigrex ()
  (let ((condition (signals-error nodecode-sigrex:sigrex-error
                                  (nodecode-sigrex:api "GET" "  "))))
    (is (cl:search "needs a path" (princ-to-string condition))))
  (is (null *calls*) "and nothing was dialled")))

(deftest sigrex-cell-a-refused-key-carries-the-api-s-own-sentence (with-sigrex ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +unauthorized+ 401)))
  (let ((condition (signals-error nodecode-sigrex:sigrex-error
                                  (nodecode-sigrex:strategies))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named")
    (is (cl:search "Invalid API key" (princ-to-string condition)) "and the API's own reason"))))

;;; --- one signal --------------------------------------------------------------------

(deftest sigrex-cell-a-signal-is-a-test-signal-unless-it-says-otherwise (with-sigrex ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +signal-answer+ 200)))
  (let ((text (sigrex-call-tool "sigrex_signal" "{\"hash\":\"bot-hash-1\",\"symbol\":\"BTCUSDT\",\"side\":\"BUY\"}")))
    (is (cl:search "TEST" text) "the answer says no trade was meant")
    (is (cl:search "HTTP 200" text) "and names the status"))
  (let ((call (first *calls*)))
    (is (equal :post (getf call :method)))
    (is (equal (concatenate 'string +hook-host+ "/bot/bot-hash-1") (getf call :url)) "to the bot hook the hash names")
    (is (cl:search "\"symbol\":\"BTCUSDT\"" (getf call :content)))
    (is (cl:search "\"flag\":[\"TEST\"]" (getf call :content)) "with the TEST flag")
    (is (null (header call "api-key")) "an unsigned hook takes no API key"))))

(deftest sigrex-cell-a-live-signal-drops-the-flag-and-says-so (with-sigrex ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +signal-answer+ 200)))
  (let ((text (sigrex-call-tool "sigrex_signal" "{\"hash\":\"bot-hash-1\",\"symbol\":\"BTCUSDT\",\"side\":\"SELL\",\"test\":false,\"key\":\"hook-secret\",\"size\":25}")))
    (is (cl:search "a live signal" text) "the answer says it could trade"))
  (let ((content (getf (first *calls*) :content)))
    (is (null (cl:search "TEST" content)) "no TEST flag rides a live signal")
    (is (cl:search "\"side\":\"SELL\"" content) "the side the call named")
    (is (cl:search "\"key\":\"hook-secret\"" content) "and the webhook's key rides the body")
    (is (cl:search "\"size\":25" content) "with the size it was given"))))

(deftest sigrex-cell-a-signal-naming-no-hash-never-reaches-the-wire (with-sigrex ()
  (let ((condition (signals-error nodecode-sigrex:sigrex-error
                                  (nodecode-sigrex:signal " " "BTCUSDT" "BUY"))))
    (is (cl:search "hash" (princ-to-string condition))))
  (is (null *calls*))))

;;; --- the command ------------------------------------------------------------------

(deftest sigrex-cell-the-command-says-what-it-gives-and-calls-for-you (with-sigrex ()
  (let ((bare (cell-entry "nodecode-sigrex" "sigrex" "" "sigrex-cell-1")))
    (is (cl:search "sigrex_api" bare) "with no argument it names its tools")
    (is (cl:search "api.sigrex.io" bare) "and the host it is pointed at")
    (is (null (cl:search +secret+ bare)) "and shows no secret"))
  (is (cl:search "BTC momentum"
                 (cell-entry "nodecode-sigrex" "sigrex" "GET /api/v2/strategy/code" "sigrex-cell-1")) "/sigrex METHOD PATH makes the call")
  (is (equal :get (getf (first *calls*) :method)))))
