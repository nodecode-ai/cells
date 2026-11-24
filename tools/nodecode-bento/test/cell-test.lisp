;;;; cell-test.lisp --- lifecycle, the filaments, the tiers, the token, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Bento: the section
;;;; (no credentials refuses the start), the fifteen tools through the real
;;;; dispatch seam against a stubbed API, the request each one makes (Basic
;;;; auth, site_uuid, the body the original built), the five-minute cache, the
;;;; dry run and its confirm_token, and every guard that keeps a T3 call from
;;;; sending something nobody previewed.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "bento"
  (:help :bento)
  (:command "bento")
  (:running
   (is (nle::find-tool "bento_subscriber_get") "bento_subscriber_get is registered")
   (is (nle::find-tool "bento_email_send") "bento_email_send is registered")
   (is (= 15 (loop for name in '("bento_subscriber_get" "bento_subscriber_upsert"
                                 "bento_subscriber_tag" "bento_subscriber_consent"
                                 "bento_tag_list" "bento_tag_create"
                                 "bento_field_list" "bento_field_create"
                                 "bento_event_track" "bento_purchase_track"
                                 "bento_broadcast_list" "bento_broadcast_create"
                                 "bento_broadcast_schedule" "bento_email_send"
                                 "bento_stats")
                       count (and (nle::find-tool name) 1))) "the original's fifteen tools")
   (is (equal bento-test-site (nodecode-bento::setting :site-uuid)) "the section's site"))
  (:stopped
   (is (null (nle::find-tool "bento_email_send")) "stop unregisters the tools"))
  (:refused
   ("publishable_key" "" "secret_key" "" "site_uuid" "")
   ("base_url" "https://example.test/api")
   ("max_recipients_per_call" 0)
   ("environment" "sometimes")))

(deftest bento-cell-without-credentials-refuses-and-says-why ()
  ;; The original offered no tool without its three BENTO_* variables; here a
  ;; section naming no credentials refuses the start instead.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (bento-start "publishable_key" ""
                                                 "secret_key" ""
                                                 "site_uuid" ""))))
      (is (cl:search "credentials" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "bento_stats")) "and installs nothing"))))

;;; --- the reads -------------------------------------------------------------------

(deftest bento-cell-a-subscriber-read-is-authenticated-and-scoped (with-bento ()
  (setf bento-answer (lambda (call) (declare (ignore call)) (values bento-test-subscriber 200)))
  (let ((text (bento-call-tool "bento_subscriber_get" "{\"email\":\"ada@example.test\"}")))
    (is (cl:search "bento: GET /fetch/subscribers" text) "the call is named")
    (is (cl:search "HTTP 200" text) "and its status")
    (is (cl:search "untrusted" text) "Bento's text is marked as data")
    (is (cl:search "ada@example.test" text) "and its body carried through"))
  (let ((call (first bento-calls)))
    (is (= 1 (length bento-calls)) "one request")
    (is (cl:search "app.bentonow.com/api/v1/fetch/subscribers" (getf call :url)) "to Bento's endpoint")
    (is (equal "ada@example.test" (bento-param call "email")) "with the email")
    (is (equal bento-test-site (bento-param call "site_uuid")) "and site_uuid on every request")
    (is (equal (format nil "Basic ~a" (bento-auth)) (bento-header call "authorization")) "with HTTP Basic from the two keys")
    (is (equal "nodecode-bento/0.1.0" (bento-header call "user-agent")) "and a real User-Agent"))))

(deftest bento-cell-a-list-is-cached-for-five-minutes (with-bento ()
  (setf bento-answer (lambda (call) (declare (ignore call)) (values bento-test-tags 200)))
  (let ((first (bento-call-tool "bento_tag_list" "{}")))
    (declare (ignore first))
    (bento-call-tool "bento_tag_list" "{}")
    (is (= 1 (length bento-calls)) "the second call answers from the cache")
    (is (cl:search "cached" (bento-call-tool "bento_tag_list" "{}")) "and says it is cached"))))

(deftest bento-cell-a-broadcast-list-with-no-status-says-what-bento-hides (with-bento ()
  (setf bento-answer (lambda (call) (declare (ignore call)) (values "{\"data\":[]}" 200)))
  (let ((text (bento-call-tool "bento_broadcast_list" "{}")))
    (is (cl:search "only SENT" text) "no status is only sent broadcasts")
    (is (null (bento-param (first bento-calls) "status")) "and no status is sent"))))

;;; --- the guarded writes -------------------------------------------------------------

(deftest bento-cell-a-tag-change-posts-the-command-body (with-bento ()
  (let ((text (bento-call-tool "bento_subscriber_tag"
                                "{\"email\":\"ada@example.test\",\"action\":\"add\",\"tag\":\"vip\"}")))
    (is (cl:search "HTTP 200" text) "a Tier 1 write goes at once"))
  (let ((call (first bento-calls)))
    (is (cl:search "/fetch/commands" (getf call :url)) "to the commands endpoint")
    (is (cl:search "\"add_tag\"" (getf call :content)) "with the add_tag command")
    (is (cl:search "\"query\":\"vip\"" (getf call :content)) "and the tag")
    (is (equal "application/json" (bento-header call "content-type")) "as JSON"))))

(deftest bento-cell-a-small-import-goes-and-a-bulk-one-previews (with-bento ()
  (let ((text (bento-call-tool "bento_subscriber_upsert"
                                "{\"subscribers\":[{\"email\":\"ada@example.test\",\"tags\":[\"vip\"]}]}")))
    (is (cl:search "HTTP 200" text) "an import under the threshold is Tier 1")
    (is (cl:search "\"tags\":\"vip\"" (getf (first bento-calls) :content)) "tags go comma-joined, as Bento wants"))
  (setf bento-calls '())
  (let* ((many (format nil "~{{\"email\":\"p~d@example.test\"}~^,~}" (loop for i from 1 to 30 collect i)))
         (text (bento-call-tool "bento_subscriber_upsert"
                                 (format nil "{\"subscribers\":[~a]}" many))))
    (is (cl:search "dry run" text) (format nil "an import above bulk_threshold previews: ~a" text))
    (is (cl:search "confirm_token:" text) "with a confirm_token")
    (is (null bento-calls) "and sends nothing"))))

(deftest bento-cell-a-purchase-keeps-the-order-id-and-cents (with-bento ()
  (let ((body (bento-call-tool "bento_purchase_track"
                                "{\"email\":\"ada@example.test\",\"order_id\":\"A-1001\",\"amount_cents\":2500,\"currency\":\"usd\"}")))
    (is (cl:search "\"$purchase\"" body) (format nil "the event is a $purchase: ~a" body))
    (is (cl:search "\"unique\":{\"key\":\"A-1001\"}" body) "the order id is the dedupe key")
    (is (cl:search "\"amount\":2500" body) "the amount is integer cents"))))

(deftest bento-cell-an-event-that-is-a-purchase-is-sent-to-the-purchase-tool (with-bento ()
  (let ((condition (signals-error nodecode-bento:bento-error
                                  (nodecode-bento:track (list (list :type "$purchase"
                                                                    :email "ada@example.test"))))))
    (is (cl:search "bento_purchase_track" (princ-to-string condition)) "the refusal names the right tool"))))

;;; --- the dry run and the token ----------------------------------------------------

(deftest bento-cell-a-send-previews-then-posts-with-its-own-token (with-bento ("allowed_senders" (vector "news@brand.test"))
  (let* ((arguments "{\"from\":\"news@brand.test\",\"emails\":[{\"to\":\"ada@example.test\",\"subject\":\"Hi\",\"html_body\":\"<p>x</p>\"}]}")
         (draft (bento-call-tool "bento_email_send" arguments))
         (token (bento-token draft)))
    (is (cl:search "T3" draft) "a send is Tier 3")
    (is (stringp token) "the preview answers a confirm_token")
    (is (null bento-calls) "and a dry run sends nothing")
    (setf bento-answer (lambda (call) (declare (ignore call)) (values "{\"queued\":1}" 200)))
    (let ((real (bento-call-tool "bento_email_send"
                                  (format nil "{\"from\":\"news@brand.test\",\"dry_run\":false,\"confirm_token\":\"~a\",\"emails\":[{\"to\":\"ada@example.test\",\"subject\":\"Hi\",\"html_body\":\"<p>x</p>\"}]}"
                                          token))))
      (is (cl:search "HTTP 200" real) "the real call goes with the token")
      (is (= 1 (length bento-calls)) "once")
      (is (cl:search "/batch/emails" (getf (first bento-calls) :url)) "to the email endpoint")
      (is (cl:search "\"transactional\":false" (getf (first bento-calls) :content)) "transactional false is written, never omitted")))))

(deftest bento-cell-a-changed-payload-cannot-spend-the-token (with-bento ("allowed_senders" (vector "news@brand.test"))
  (let* ((draft (nodecode-bento:send-email
                 "news@brand.test"
                 (list (list :to "ada@example.test" :subject "Hi" :html_body "<p>x</p>"))))
         (token (bento-token draft)))
    (is (stringp token) "the preview answers a token")
    (let ((condition
            (signals-error nodecode-bento:bento-error
                           (nodecode-bento:send-email
                            "news@brand.test"
                            (list (list :to "ada@example.test" :subject "Changed" :html_body "<p>x</p>"))
                            :dry-run nil :confirm-token token))))
      (is (cl:search "payload changed" (princ-to-string condition)) "a changed body is refused"))
    (is (null bento-calls) "and nothing was sent"))))

(deftest bento-cell-a-send-outside-allowed-senders-is-refused (with-bento ("allowed_senders" (vector "news@brand.test"))
  (let ((condition (signals-error nodecode-bento:bento-error
                                  (nodecode-bento:send-email
                                   "other@brand.test"
                                   (list (list :to "ada@example.test" :subject "Hi" :html_body "<p>x</p>"))))))
    (is (cl:search "allowed_senders" (princ-to-string condition)) "the refusal names the setting"))
  (is (null bento-calls) "and nothing was dialled")))

(deftest bento-cell-a-schedule-needs-a-sender-a-lead-time-and-an-acknowledged-tag-audience
    (with-bento ("allowed_senders" (vector "news@brand.test"))
  (let ((later (bento-iso 3600)))
    (signals-error nodecode-bento:bento-error
                   (nodecode-bento:schedule "n" "s" "c" "Brand" "news@brand.test" later
                                             :inclusive-tags (list "vip") :batch-size-per-hour 100))
    (signals-error nodecode-bento:bento-error
                   (nodecode-bento:schedule "n" "s" "c" "Brand" "other@brand.test" later
                                             :inclusive-tags (list "vip")
                                             :acknowledge-unknown-audience t
                                             :batch-size-per-hour 100))
    (signals-error nodecode-bento:bento-error
                   (nodecode-bento:schedule "n" "s" "c" "Brand" "news@brand.test" (bento-iso 60)
                                             :inclusive-tags (list "vip")
                                             :acknowledge-unknown-audience t
                                             :batch-size-per-hour 100))
    (let ((draft (nodecode-bento:schedule "n" "s" "c" "Brand" "news@brand.test" later
                                           :inclusive-tags (list "vip")
                                           :acknowledge-unknown-audience t
                                           :batch-size-per-hour 100)))
      (is (cl:search "\"approved\":true" draft) "a schedule that passes every guard previews approved=true")
      (is (null bento-calls) "and still sends nothing")))))

