;;;; cell-test.lisp --- lifecycle, the three tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring, not Parcel: the config
;;;; surface, the tools through the real dispatch seam against a stubbed API,
;;;; every member they send, what a delivery becomes for a model, and the
;;;; error a refusing or rate-limited Parcel answers.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "parcel"
  (:config "api_key" +parcel-key+ "api_base" +parcel-base+)
  (:fixture with-wire)
  (:help :parcel)
  (:command "parcel")
  (:running
   (is (nle::find-tool "parcel_deliveries") "parcel_deliveries is registered")
   (is (nle::find-tool "parcel_add_delivery") "parcel_add_delivery is registered")
   (is (nle::find-tool "parcel_carriers") "parcel_carriers is registered")
   (is (equal +parcel-key+ (nodecode-parcel::setting :api-key)) "the key the section carries")
   (is (equal +parcel-base+ (nodecode-parcel::setting :api-base)) "and the API base"))
  (:stopped
   (is (null (nle::find-tool "parcel_deliveries")) "stop unregisters the tools")
   (is (null (nle::find-tool "parcel_carriers")) "every one of them"))
  (:refused
   ("api_key" +parcel-key+ "api_base" "api.parcel.app/external")
   ("api_key" +parcel-key+ "cache_seconds" -5)
   ("api_key" +parcel-key+ "request_timeout" 0)
   ("api_key" "" "api_key_env" "")))

(deftest parcel-cell-a-missing-key-says-where-to-get-one ()
  ;; The one setting a call cannot work without: a section naming no key
  ;; refuses at start, in words that name where a key comes from.
  (let ((condition (signals-error nlk:config-refusal (parcel-start "api_key" "" "api_key_env" ""))))
    (is (search "web.parcelapp.net" (princ-to-string condition)) "the refusal names where a key comes from")
    (is (search "Parcel Premium" (princ-to-string condition)) "and why one is needed"))
  (is (null (nle::find-tool "parcel_deliveries")) "and nothing was installed"))

(deftest parcel-cell-a-schemeless-endpoint-says-what-to-set ()
  ;; An empty value reads as absent, so the declaration's default answers;
  ;; a base with no scheme is the one a call cannot use.
  (let ((condition (signals-error nlk:config-refusal
                                  (parcel-start "api_key" +parcel-key+
                                                "api_base" "api.parcel.app/external"))))
    (is (search "api_base" (princ-to-string condition)) "the refusal names the key to fix")
    (is (search "https://" (princ-to-string condition)) "and what a good one looks like")))

;;; --- the read tool ------------------------------------------------------------------

(deftest parcel-cell-a-list-names-what-is-coming (with-standard-parcel
  (let ((text (parcel-call-tool "parcel_deliveries" "{}")))
    (is (search "\"description\":\"Running shoes\"" text) "each delivery's description is there")
    (is (search "\"status\":\"out_for_delivery\"" text) "with its status in words")
    (is (search "\"status_text\":\"Out for delivery.\"" text) "and the sentence a model reads")
    (is (search "\"carrier\":\"UPS\"" text) "the carrier by name, not only by code")
    (is (search "\"date\":\"2026-11-11\"" text) "the date the carrier wrote")
    (is (search "\"latest_event\":{\"event\":\"Out for delivery\"" text) "the newest event")
    (is (null (search "Arrived at facility" text)) "and not the whole history by default")
    (is (search "\"count\":2" text) "both deliveries counted")
    (is (search "\"from_cache\":false" text) "a fresh read says so"))
  (let* ((call (parcel-call-of "/deliveries/"))
         (url (getf call :url))
         (active (concatenate 'string +parcel-base+ "/deliveries/?filter_mode=active")))
    (is (equal active url) "one request, the active list under the API base")
    (is (equal :get (getf call :method)) "a read is a GET")
    (is (equal +parcel-key+ (parcel-header call "api-key")) "the key rides it as api-key"))))

(deftest parcel-cell-one-tracking-number-and-its-history (with-standard-parcel
  (let ((text (parcel-call-tool
               "parcel_deliveries"
               "{\"tracking_number\":\"1z 999aa1 0123456784\", \"include_events\":true}")))
    (is (search "\"count\":1" text) "one delivery matches, spaces and case aside")
    (is (search "Arrived at facility" text) "the whole history when it is asked for")
    (is (search "1Z999AA10123456784" text) "the number as Parcel spells it"))))

(deftest parcel-cell-a-list-is-reused-before-it-is-read-again (with-standard-parcel
  (let ((first (parcel-call-tool "parcel_deliveries" "{}"))
        (second (parcel-call-tool "parcel_deliveries" "{}")))
    (is (search "\"from_cache\":false" first) "the first call reads Parcel")
    (is (search "\"from_cache\":true" second) "the second answers from the cache")
    (is (= 2 (parcel-member second "count")) "with the same list"))
  (is (= 1 (length (parcel-calls-of "/deliveries/"))) "one request, not two")))

(deftest parcel-cell-a-spent-read-limit-answers-the-last-list-stale (with-parcel ("reads_per_hour" 1)
  (parcel-answer "/deliveries/" +parcel-deliveries+)
  (parcel-answer "/supported_carriers.json" +parcel-carriers+)
  (parcel-call-tool "parcel_deliveries" "{}")
  (let ((text (parcel-call-tool "parcel_deliveries" "{\"refresh\":true}")))
    (is (search "\"stale\":true" text) "a spent limit answers the last list, marked stale")
    (is (search "\"count\":2" text) "with what it already knew")
    (is (search "used Parcel's limit" text) "and the warning names the limit"))
  (is (= 1 (length (parcel-calls-of "/deliveries/"))) "the refused read never left the machine")))

(deftest parcel-cell-a-rejected-key-says-what-to-check (with-parcel ()
  (parcel-answer "/deliveries/" "{\"success\": false, \"error_message\": \"invalid api key\"}")
  (let ((text (parcel-call-tool "parcel_deliveries" "{}")))
    (is (search "\"error\"" text) "the tool answers an error member, it does not throw")
    (is (search "invalid api key" text) "in Parcel's own words")
    (is (search "\"code\":\"api\"" text) "with a code a caller can branch on"))))

;;; --- carriers ------------------------------------------------------------------------

(deftest parcel-cell-carriers-match-by-name-and-cost-no-key (with-parcel ()
  (parcel-answer "/supported_carriers.json" +parcel-carriers+)
  (let ((text (parcel-call-tool "parcel_carriers" "{\"query\":\"Amazon\"}")))
    (is (search "\"carrier_code\":\"amzlus\"" text) "a name variation finds the code")
    (is (search "\"needs_postcode_or_email\":true" text) "and says the carrier wants more")
    (is (search "\"total_matches\":1" text) "nothing else matches"))
  (let ((call (parcel-call-of "/supported_carriers.json")))
    (is (equal :get (getf call :method)) "the carrier table is a GET")
    (is (null (parcel-header call "api-key")) "sent without the key"))))

;;; --- the add tool --------------------------------------------------------------------

(deftest parcel-cell-an-add-posts-what-it-says (with-parcel ()
  (parcel-answer "/supported_carriers.json" +parcel-carriers+)
  (parcel-answer "/add-delivery/" "{\"success\": true}")
  (let ((text (parcel-call-tool
               "parcel_add_delivery"
               "{\"tracking_number\":\"1Z999AA10123456785\",\"carrier_code\":\"UPS\",\"description\":\"Running shoes\"}")))
    (is (search "\"added\":true" text) "the add is reported done")
    (is (search "\"carrier\":\"UPS\"" text) "with the carrier by name")
    (is (search "\"adds_left_today\":19" text) "and the local count spent"))
  (let* ((call (parcel-call-of "/add-delivery/"))
         (body (getf call :body)))
    (is (equal :post (getf call :method)) "an add is a POST")
    (is (equal "1Z999AA10123456785" (nlk:json-value body :string "tracking_number")) "the number as typed")
    (is (equal "ups" (nlk:json-value body :string "carrier_code")) "the code lowercased")
    (is (equal "Running shoes" (nlk:json-value body :string "description")) "the label")
    (is (null (nlk:json-value body :boolean "send_push_confirmation")) "no push unless asked"))))

(deftest parcel-cell-an-add-is-checked-before-it-is-sent (with-parcel ()
  (parcel-answer "/supported_carriers.json" +parcel-carriers+)
  (let ((unknown (parcel-call-tool
                  "parcel_add_delivery"
                  "{\"tracking_number\":\"1Z999AA10123456785\",\"carrier_code\":\"nope\",\"description\":\"x\"}"))
        (short (parcel-call-tool
                "parcel_add_delivery"
                "{\"tracking_number\":\"12\",\"carrier_code\":\"ups\",\"description\":\"x\"}"))
        (needy (parcel-call-tool
                "parcel_add_delivery"
                "{\"tracking_number\":\"TBA123456789000\",\"carrier_code\":\"amzlus\",\"description\":\"x\"}")))
    (is (search "\"code\":\"unknown_carrier\"" unknown) "an unknown code is refused locally")
    (is (search "\"code\":\"invalid\"" short) "so is a tracking number of two characters")
    (is (search "\"code\":\"extra_required\"" needy) "and a carrier that needs a postcode or email"))
  (is (null (parcel-call-of "/add-delivery/")) "none of them spent a daily add")))

(deftest parcel-cell-an-already-tracked-number-sends-nothing (with-standard-parcel
  (parcel-answer "/add-delivery/" "{\"success\": true}")
  (parcel-call-tool "parcel_deliveries" "{}")
  (let ((text (parcel-call-tool
               "parcel_add_delivery"
               "{\"tracking_number\":\"1z999aa10123456784\",\"carrier_code\":\"ups\",\"description\":\"again\"}")))
    (is (search "\"already_tracked\":true" text) "a number the cached list already carries is not added")
    (is (search "\"added\":false" text) "and nothing is claimed"))
  (is (null (parcel-call-of "/add-delivery/")) "nothing was posted")))

;;; --- the command -----------------------------------------------------------------------

(deftest parcel-cell-the-command-lists-what-is-coming (with-standard-parcel
  (let ((text (cell-entry "nodecode-parcel" "parcel" "" "parcel-cell-1")))
    (is (search "Parcel: 2 on the way" text) "/parcel counts what is on the way")
    (is (search "- Running shoes (UPS): out for delivery" text) "one line a package")
    (is (search "expected 2026-11-11" text) "with the date the carrier wrote")
    (is (search "- Coffee filter (Amazon Logistics (US)): in transit" text) "the status in words"))
  (cell-entry "nodecode-parcel" "parcel" "recent" "parcel-cell-1")
  (let* ((call (parcel-call-of "/deliveries/"))
         (recent (concatenate 'string +parcel-base+ "/deliveries/?filter_mode=recent")))
    (is (equal recent (getf call :url)) "/parcel recent reads the other list"))))

(deftest parcel-cell-the-command-reuses-a-list (with-standard-parcel
  (let ((first (cell-entry "nodecode-parcel" "parcel" "" "parcel-cell-1"))
        (second (cell-entry "nodecode-parcel" "parcel" "" "parcel-cell-1")))
    (is (search "Parcel: 2 on the way" first) "the command lists what is on the way")
    (is (equal first second) "and the second answer is the same, from the cache"))
  (is (= 1 (length (parcel-calls-of "/deliveries/"))) "one read for both")))
