;;;; cell-test.lisp --- lifecycle, the tool, the request it builds, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Cloudflare: the config
;;;; surface (a section naming no token, or an account id that is not 32 hex,
;;;; refuses the start), the tool through the real dispatch seam against a
;;;; stubbed API -- its url, its bearer header, the request object it posts --
;;;; the lines it answers, Cloudflare's own sentence on a refusal, and the
;;;; operator's command.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "cloudflare-web-search"
  (:fixture with-wire)
  (:config "api_token" +token+ "account_id" +account+)
  (:help :cloudflare-web-search)
  (:command "cloudflare-web-search")
  (:running
   (is (nle::find-tool "cloudflare_search") "cloudflare_search is registered")
   (is (equal +token+ (nodecode-cloudflare-web-search::setting :api-token)) "the token the section carries")
   (is (equal +account+ (nodecode-cloudflare-web-search::setting :account-id)) "and its account id"))
  (:stopped
   (is (null (nle::find-tool "cloudflare_search")) "stop unregisters the tool"))
  (:refused
   ("api_token" "" "api_token_env" "UNSET_CLOUDFLARE_WEB_SEARCH_TOKEN" "account_id" +account+)
   ("api_token" +token+ "account_id" "")
   ("api_token" +token+ "account_id" "abc")
   ("api_token" +token+ "account_id" +account+ "provider" "google")
   ("api_token" +token+ "account_id" +account+ "byok_alias" "bad alias!")
   ("api_token" +token+ "account_id" +account+ "max_results" 0)
   ("api_token" +token+ "account_id" +account+ "timeout_seconds" 0)))

(deftest cloudflare-web-search-cell-without-a-token-refuses-and-installs-nothing ()
  ;; The one setting no call can work without: a section naming no token, over
  ;; a variable that holds none, is refused at start.
  (with-wire
    (with-stubbed-fdefinition (nle::credential-env (name) (and name nil))
      (let ((text (refusal-text nlk:config-refusal
                                 (cloudflare-web-search-start "api_token" ""
                                                               "account_id" +account+))))
        (is (cl:search "api_token" text) "the refusal names the member to set")
        (is (null (nle::find-tool "cloudflare_search")) "and installs nothing")))))

(deftest cloudflare-web-search-cell-the-token-and-account-may-come-from-the-environment ()
  ;; The original read CLOUDFLARE_WEB_SEARCH_API_TOKEN from the process; a box
  ;; set up with nothing but the variables searches all the same.
  (with-wire
    (with-stubbed-fdefinition (nle::credential-env (name)
                                (cond ((equal name "CLOUDFLARE_WEB_SEARCH_API_TOKEN") "cf-env-token")
                                      ((equal name "CLOUDFLARE_ACCOUNT_ID") +account+)
                                      (t nil)))
      (with-cell-stop ((cloudflare-web-search-start "api_token" "" "account_id" ""))
        (cf-call-tool "cloudflare_search" "{\"query\":\"x\"}")
        (let ((call (first *calls*)))
          (is (equal "Bearer cf-env-token" (header call "authorization")) "the exported token is the one sent")
          (is (cl:search +account+ (getf call :url)) "and the exported account is in the path"))))))

;;; --- the tool ------------------------------------------------------------------

(deftest cloudflare-web-search-cell-the-tool-posts-the-query-and-answers-the-results (with-cloudflare ()
  (let ((text (cf-call-tool "cloudflare_search" "{\"query\":\"things to do in salt lake city\"}")))
    (is (cl:search "[1] Fall in Salt Lake City: A Local's Guide" text) "the first result, numbered and titled")
    (is (cl:search "https://example.test/salt-lake-fall" text) "with its url")
    (is (cl:search "From scenic drives up Big Cottonwood Canyon" text) "and its description")
    (is (cl:search "[2] Second result" text) "the second result too")
    (is (cl:search "provider ceramic" text) "the answer names the route that answered")
    (is (cl:search "gateway default" text) "and the gateway")
    (is (cl:search "req-cf-1" text) "and the request id, for a support question")
    (is (cl:search "612 ms" text) "and what the gateway said it took"))
  (let ((call (first *calls*)))
    (is (= 1 (length *calls*)) "one request, not one a result")
    (is (equal (format nil "https://api.cloudflare.com/client/v4/accounts/~a/ai/websearch/" +account+)
               (getf call :url)) "to the account's websearch endpoint")
    (is (equal "Bearer cf-test-token" (header call "authorization")) "with the section's token")
    (is (equal "application/json" (header call "content-type")) "and a JSON body")
    (is (equal "things to do in salt lake city" (body-value call "query")) "the query")
    (is (equal "ceramic" (body-value call "provider")) "the section's provider when the call names none")
    (is (= 5 (nlk:json-value (call-body call) :integer "limit")) "and its max_results")
    (is (equal "default" (body-value call "options" "gateway" "id")) "routed through the gateway's id")
    (is (null (body-value call "byokAlias")) "with no alias unless the section names one"))))

(deftest cloudflare-web-search-cell-a-call-s-own-provider-and-count-ride-the-request (with-cloudflare ()
  (cf-call-tool "cloudflare_search" "{\"query\":\"x\",\"max_results\":25,\"provider\":\"exa\"}")
  (let ((call (first *calls*)))
    (is (equal "exa" (body-value call "provider")) "a call's own provider wins over the section's")
    (is (= 10 (nlk:json-value (call-body call) :integer "limit")) "a count past ten is clamped, as the API's own limit says"))))

(deftest cloudflare-web-search-cell-a-byok-alias-rides-only-when-the-section-names-one (with-cloudflare ("byok_alias" "team-key")
  (cf-call-tool "cloudflare_search" "{\"query\":\"x\"}")
  (is (equal "team-key" (body-value (first *calls*) "byokAlias")) "the alias the original's settings carried")))

;;; --- refusals from the API --------------------------------------------------------

(deftest cloudflare-web-search-cell-a-refused-token-carries-cloudflare-s-own-sentence (with-cloudflare ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +refusal+ 403)))
  (let ((condition (signals-error nodecode-cloudflare-web-search:cloudflare-web-search-error
                                  (nodecode-cloudflare-web-search:search "x"))))
    (is (cl:search "HTTP 403" (princ-to-string condition)) "the status is named")
    (is (cl:search "Authentication error" (princ-to-string condition)) "and Cloudflare's own reason"))))

(deftest cloudflare-web-search-cell-an-answer-with-no-items-is-a-refusal (with-cloudflare ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +no-items+ 200)))
  (let ((condition (signals-error nodecode-cloudflare-web-search:cloudflare-web-search-error
                                  (nodecode-cloudflare-web-search:search "x"))))
    (is (cl:search "no items array" (princ-to-string condition)) "the original's one sentence for a response without items"))))

(deftest cloudflare-web-search-cell-a-search-with-no-results-says-so (with-cloudflare ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values "{\"items\":[]}" 200)))
  (let ((text (nodecode-cloudflare-web-search:search "nothing at all")))
    (is (cl:search "no results" text) "an empty items array is no results")
    (is (cl:search "\"nothing at all\"" text) "and names the query it found them for"))))

;;; --- the boundary -----------------------------------------------------------------

(deftest cloudflare-web-search-cell-a-blank-or-oversized-query-never-reaches-the-wire (with-cloudflare ()
  (let ((blank (signals-error nodecode-cloudflare-web-search:cloudflare-web-search-error
                               (nodecode-cloudflare-web-search:search "  ")))
        (long (signals-error nodecode-cloudflare-web-search:cloudflare-web-search-error
                              (nodecode-cloudflare-web-search:search
                               (make-string 1025 :initial-element #\x)))))
    (is (cl:search "needs a query" (princ-to-string blank)) "a blank query is refused")
    (is (cl:search "1025" (princ-to-string long)) "and one past 1024 characters names its length"))
  (is (null *calls*) "and nothing was dialled")))

;;; --- the command -------------------------------------------------------------------

(deftest cloudflare-web-search-cell-the-command-says-what-it-is-connected-to-and-searches (with-cloudflare ()
  (let ((bare (cell-entry "nodecode-cloudflare-web-search" "cloudflare-web-search" "" "cf-cell-1")))
    (is (cl:search "accounts/" bare) "with no query it names the endpoint")
    (is (cl:search "cloudflare_search" bare) "and the tool the model gets")
    (is (null (cl:search +token+ bare)) "without ever printing the token")
    (is (null *calls*) "and sends nothing"))
  (let ((answer (cell-entry "nodecode-cloudflare-web-search" "cloudflare-web-search"
                            "salt lake city" "cf-cell-1")))
    (is (cl:search "[1] Fall in Salt Lake City" answer) "an argument runs one search and answers its results")
    (is (equal "salt lake city" (body-value (first *calls*) "query")) "the query trimmed, as typed"))))
