;;;; cell-test.lisp --- lifecycle, the seven tools, the verbs, /sequenzy.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, integer fields under their floors refused, a start with no key
;;;; refused), each tool through the seam the model calls it by -- its url,
;;;; its Bearer header, its x-company-id, its body and its answer -- the
;;;; verbs, and /sequenzy with and without a query. Every request is a
;;;; stubbed NLK:HTTP, so nothing reaches Sequenzy or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-sequenzy-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Sequenzy."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected sequenzy request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defmacro with-sequenzy-answers ((&rest routes) &body body)
  "BODY with the stubbed wire answering ROUTES, (FRAGMENT FORM): the first
whose FRAGMENT the url holds answers FORM's (values TEXT STATUS); a url no
route names is an error."
  `(with-sequenzy-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore method args))
                           (cond ,@(loop for (fragment form) in routes
                                         collect `((search ,fragment url) ,form))
                                 (t (error "unexpected request to ~a" url))))))
       (with-cell-stop ((sequenzy-start "api_key" "seq_user_test" "company_id" "comp_1"))
         ,@body))))

(defun sequenzy-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun sequenzy-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun sequenzy-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun sequenzy-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "sequenzy" "SEQUENZY-CELL-" :start nodecode-sequenzy:start-cell)

(define-cell-lifecycle-tests "sequenzy"
  (:fixture with-sequenzy-wire)
  (:config "api_key" "seq_user_test")
  (:help :sequenzy)
  (:command "sequenzy")
  (:running (is (nle::find-tool "sequenzy_account") "sequenzy_account is registered")
            (is (nle::find-tool "sequenzy_subscribers"))
            (is (nle::find-tool "sequenzy_add_subscriber"))
            (is (nle::find-tool "sequenzy_campaigns"))
            (is (nle::find-tool "sequenzy_stats"))
            (is (nle::find-tool "sequenzy_audience"))
            (is (nle::find-tool "sequenzy_send"))
            (is (equal "https://api.sequenzy.com" (nodecode-sequenzy::root)))
            (is (equal "seq_user_test" (nodecode-sequenzy::api-key)))
            (is (= 25 (nodecode-sequenzy::setting :max-rows)) "25 rows by default")
            (is (= 30 (nodecode-sequenzy::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "sequenzy_account")) "stop unregisters the tools")
            (is (null (nle::find-tool "sequenzy_send"))))
  (:refused ("max_rows" 0) ("timeout_seconds" 0))
  (:idle nodecode-sequenzy:sequenzy-error (sequenzy:account)))

(deftest sequenzy-cell-a-key-is-required ()
  (with-sequenzy-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignorable name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (sequenzy-start)))
        ("sequenzy: no API key") ("SEQUENZY_API_KEY"))
      (is (null (nle::find-tool "sequenzy_account")) "a refused start installed nothing"))))

;;; --- sequenzy_subscribers -------------------------------------------------------

(deftest sequenzy-cell-subscribers-list-the-audience ()
  (with-sequenzy-answers
      (("/api/v1/subscribers"
        (values (concatenate 'string
                             "{\"success\":true,\"subscribers\":[{\"id\":\"sub_1\",\"email\":\"user@example.com\","
                             "\"firstName\":\"Ana\",\"status\":\"active\",\"tags\":[\"vip\"],"
                             "\"createdAt\":\"2026-01-02T00:00:00Z\"}],"
                             "\"pagination\":{\"total\":150,\"hasMore\":false}}")
                200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_subscribers"
                                         (nlk:json-object "tag" "vip" "limit" 2)))
      ("\"email\":\"user@example.com\"") ("\"status\":\"active\"") ("\"total\":150")
      ("\"returned\":1"))
    (let ((request (first (sequenzy-asked))))
      (is (eq :get (first request)))
      (is (search "tags=vip" (second request)) "the tag rides as tags")
      (is (search "limit=2" (second request)) "the call's own limit")
      (is (equal "Bearer seq_user_test" (sequenzy-request-header request "authorization")) "the key rides the header")
      (is (equal "comp_1" (sequenzy-request-header request "x-company-id")) "the section's company pins the workspace"))))

(deftest sequenzy-cell-one-subscriber-answers-its-profile ()
  (with-sequenzy-answers
      (("/api/v1/subscribers/ana"
        (values (concatenate 'string
                             "{\"success\":true,\"subscriber\":{\"id\":\"sub_1\",\"email\":\"ana@example.com\","
                             "\"status\":\"active\",\"tags\":[\"vip\"],\"customAttributes\":{\"plan\":\"pro\"},"
                             "\"lists\":[{\"id\":\"list_1\",\"name\":\"Newsletter\"}],"
                             "\"notes\":[{\"id\":\"note_1\"}],\"createdAt\":\"2025-05-01T00:00:00Z\"}}")
                200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_subscribers"
                                         (nlk:json-object "email" "ana@example.com")))
      ("\"email\":\"ana@example.com\"") ("\"plan\":\"pro\"") ("\"Newsletter\"")
      ("\"notes\":1"))
    (is (search "/api/v1/subscribers/ana" (second (first (sequenzy-asked)))) "the address is the path, encoded")))

(deftest sequenzy-cell-a-stranger-is-named-not-guessed ()
  (with-sequenzy-answers
      (("/api/v1/subscribers/nobody" (values "{\"success\":true}" 200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_subscribers"
                                         (nlk:json-object "email" "nobody@example.com")))
      ("ERROR: sequenzy:") ("not a subscriber"))))

;;; --- sequenzy_add_subscriber ----------------------------------------------------

(deftest sequenzy-cell-add-a-subscriber-posts-the-contact ()
  (with-sequenzy-answers
      (("/api/v1/subscribers"
        (values (concatenate 'string
                             "{\"success\":true,\"subscriber\":{\"id\":\"sub_9\",\"email\":\"new@example.com\","
                             "\"status\":\"active\",\"tags\":[\"vip\"],\"created\":true,\"updated\":false}}")
                200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_add_subscriber"
                                         (nlk:json-object
                                          "email" "new@example.com"
                                          "first_name" "Ana"
                                          "tags" (vector "vip")
                                          "duplicate_strategy" "merge")))
      ("\"created\":true") ("new@example.com"))
    (let* ((request (first (sequenzy-asked)))
           (body (sequenzy-request-body request)))
      (is (eq :post (first request)))
      (is (equal "new@example.com" (nlk:json-value body :string "email")))
      (is (equal "Ana" (nlk:json-value body :string "firstName")) "a native name, not an attribute")
      (is (equal "merge" (nlk:json-value body :string "duplicateStrategy")))
      (is (equal "vip" (aref (nlk:json-value body :array "tags") 0))))))

(deftest sequenzy-cell-add-refuses-a-name-it-does-not-know ()
  (with-sequenzy-answers (("/api/v1/subscribers" (values "{}" 200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_add_subscriber"
                                         (nlk:json-object "email" "x@example.com"
                                                          "duplicate_strategy" "replace")))
      ("ERROR: sequenzy:") ("duplicate_strategy must be one of"))
    (is-carrying (answer (sequenzy-tool "sequenzy_add_subscriber" (nlk:json-object)))
      ("ERROR: sequenzy:") ("email is required"))))

;;; --- sequenzy_campaigns ---------------------------------------------------------

(deftest sequenzy-cell-campaigns-list-and-fetch-one ()
  (with-sequenzy-answers
      (("/api/v1/campaigns/camp_1"
        (values (concatenate 'string
                             "{\"success\":true,\"campaign\":{\"id\":\"camp_1\",\"name\":\"April Launch\","
                             "\"subject\":\"We shipped\",\"status\":\"draft\","
                             "\"previewUrl\":\"https://sequenzy.com/dashboard/campaign/camp_1?step=review\"}}")
                200))
       ("/api/v1/campaigns"
        (values (concatenate 'string
                             "{\"success\":true,\"campaigns\":[{\"id\":\"camp_1\",\"name\":\"April Launch\","
                             "\"status\":\"draft\",\"labels\":[\"edm\"],\"scheduledAt\":null}],"
                             "\"pagination\":{\"total\":1,\"hasMore\":false}}")
                200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_campaigns" (nlk:json-object)))
      ("April Launch") ("\"status\":\"draft\"") ("\"labels\":[\"edm\"]") ("\"total\":1"))
    (is (search "limit=25" (second (first (sequenzy-asked)))) "the section's max_rows bounds a list")
    (is-carrying (answer (sequenzy-tool "sequenzy_campaigns"
                                         (nlk:json-object "campaign_id" "camp_1")))
      ("\"id\":\"camp_1\"") ("\"preview_url\":\"https://sequenzy.com/dashboard/campaign/camp_1?step=review\""))))

;;; --- sequenzy_stats -------------------------------------------------------------

(deftest sequenzy-cell-stats-answer-the-funnel ()
  (with-sequenzy-answers
      (("/api/v1/metrics/campaigns/camp_1"
        (values (concatenate 'string
                             "{\"success\":true,\"campaignId\":\"camp_1\",\"stats\":{\"sent\":1200,"
                             "\"delivered\":1190,\"opened\":420,\"clicked\":85},\"clickedLinks\":[]}")
                200))
       ("/api/v1/metrics"
        (values (concatenate 'string
                             "{\"success\":true,\"period\":\"30d\",\"subscriberCount\":12400,"
                             "\"activeSubscriberCount\":11880,\"stats\":{\"sent\":3400,\"delivered\":3380,"
                             "\"opened\":1020,\"clicked\":340}}")
                200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_stats" (nlk:json-object "period" "30d")))
      ("\"period\":\"30d\"") ("\"sent\":3400") ("\"opened\":1020") ("\"subscriber_count\":12400"))
    (is (search "period=30d" (second (first (sequenzy-asked)))))
    (is-carrying (answer (sequenzy-tool "sequenzy_stats"
                                         (nlk:json-object "campaign_id" "camp_1")))
      ("\"campaign_id\":\"camp_1\"") ("\"sent\":1200"))
    (is-carrying (answer (sequenzy-tool "sequenzy_stats" (nlk:json-object "period" "1y")))
      ("ERROR: sequenzy:") ("period must be one of"))))

;;; --- sequenzy_audience ----------------------------------------------------------

(deftest sequenzy-cell-audience-reads-lists-tags-and-segments ()
  (with-sequenzy-answers
      (("/api/v1/lists"
        (values (concatenate 'string
                             "{\"success\":true,\"lists\":[{\"id\":\"list_1\",\"name\":\"Newsletter\","
                             "\"isPrivate\":false,\"subscriberCount\":1200,\"activeSubscriberCount\":1180}]}")
                200))
       ("/api/v1/tags"
        (values (concatenate 'string
                             "{\"success\":true,\"tags\":[{\"id\":\"tag_1\",\"name\":\"premium\","
                             "\"color\":\"orange\",\"isSystem\":false}]}")
                200))
       ("/api/v1/segments"
        (values (concatenate 'string
                             "{\"success\":true,\"segments\":[{\"id\":\"seg_1\",\"name\":\"Active buyers\","
                             "\"subscriberCount\":1200}]}")
                200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_audience" (nlk:json-object)))
      ("\"name\":\"Newsletter\"") ("\"subscribers\":1200") ("premium") ("Active buyers"))
    (is (= 3 (length (sequenzy-asked))) "three reads in one call")))

;;; --- sequenzy_send --------------------------------------------------------------

(deftest sequenzy-cell-send-answers-an-accepted-send ()
  (with-sequenzy-answers
      (("/api/v1/transactional/send"
        (values (concatenate 'string
                             "{\"success\":true,\"emailSendId\":\"send_1\",\"jobId\":\"job_1\","
                             "\"to\":\"reader@example.com\"}")
                200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_send"
                                         (nlk:json-object
                                          "to" "reader@example.com"
                                          "slug" "welcome"
                                          "variables" (nlk:json-object "firstName" "Ana")
                                          "idempotency_key" "key-1")))
      ("\"email_send_id\":\"send_1\"") ("\"job_id\":\"job_1\"") ("\"queued\":true"))
    (let* ((request (first (sequenzy-asked)))
           (body (sequenzy-request-body request)))
      (is (eq :post (first request)))
      (is (equal "key-1" (sequenzy-request-header request "idempotency-key")) "one key per logical email")
      (is (equal "welcome" (nlk:json-value body :string "slug")))
      (is (equal "Ana" (nlk:json-value body :string "variables" "firstName")))
      (is (equal "transactional" (nlk:json-value body :string "emailType"))))))

(deftest sequenzy-cell-send-needs-content ()
  (with-sequenzy-answers (("/api/v1/transactional/send" (values "{}" 200)))
    (is-carrying (answer (sequenzy-tool "sequenzy_send"
                                         (nlk:json-object "to" "reader@example.com")))
      ("ERROR: sequenzy:") ("a send needs 'slug'"))
    (is-carrying (answer (sequenzy-tool "sequenzy_send"
                                         (nlk:json-object "to" "reader@example.com"
                                                          "slug" "welcome" "subject" "Hi" "body" "<p>hi</p>")))
      ("ERROR: sequenzy:") ("not both"))))

;;; --- the verbs and /sequenzy ----------------------------------------------------

(deftest sequenzy-cell-the-command-shows-the-account-or-finds-people ()
  (with-sequenzy-answers
      (("/api/v1/account"
        (values (concatenate 'string
                             "{\"success\":true,\"currentCompanyId\":\"comp_1\","
                             "\"companies\":[{\"id\":\"comp_1\",\"name\":\"Acme Inc\",\"role\":\"owner\"}],"
                             "\"apiKeyPermissions\":{\"preset\":\"custom\",\"selectedScopeCount\":2,"
                             "\"currentScopeCount\":63,\"canSendLive\":false}}")
                200))
       ("/api/v1/metrics"
        (values (concatenate 'string
                             "{\"success\":true,\"period\":\"7d\",\"stats\":{\"sent\":3400,\"delivered\":3380,"
                             "\"opened\":1020,\"clicked\":340}}")
                200))
       ("/api/v1/subscribers"
        (values (concatenate 'string
                             "{\"success\":true,\"subscribers\":[{\"id\":\"sub_1\",\"email\":\"ana@example.com\","
                             "\"status\":\"active\",\"tags\":[\"vip\"]}],\"pagination\":{\"total\":1}}")
                200)))
    (is-carrying (answer (cell-entry "nodecode-sequenzy" "sequenzy" ""))
      ("Sequenzy: Acme Inc (comp_1, owner)") ("Key: custom preset, 2 of 63 scopes")
      ("live sends not allowed") ("sent 3400"))
    (is-carrying (answer (cell-entry "nodecode-sequenzy" "sequenzy" "ana"))
      ("subscribers matching ana") ("ana@example.com"))))

(deftest sequenzy-cell-a-verb-answers-one-line-each ()
  (with-sequenzy-answers
      (("/api/v1/campaigns"
        (values (concatenate 'string
                             "{\"success\":true,\"campaigns\":[{\"id\":\"camp_1\",\"name\":\"April Launch\","
                             "\"status\":\"draft\"}]}")
                200))
       ("/api/v1/lists"
        (values (concatenate 'string
                             "{\"success\":true,\"lists\":[{\"id\":\"list_1\",\"name\":\"Newsletter\","
                             "\"subscriberCount\":1200}]}")
                200))
       ("/api/v1/tags" (values "{\"success\":true,\"tags\":[{\"name\":\"vip\"}]}" 200))
       ("/api/v1/segments" (values "{\"success\":true,\"segments\":[]}" 200))
       ("/api/v1/subscribers/ana"
        (values (concatenate 'string
                             "{\"success\":true,\"subscriber\":{\"id\":\"sub_1\",\"email\":\"ana@example.com\","
                             "\"firstName\":\"Ana\",\"status\":\"active\",\"tags\":[\"vip\"],"
                             "\"lists\":[{\"name\":\"Newsletter\"}],\"notes\":[]}}")
                200)))
    (is-carrying (answer (sequenzy:campaigns :status "draft"))
      ("Sequenzy campaigns (draft)") ("camp_1  draft") ("April Launch"))
    (is-carrying (answer (sequenzy:audience))
      ("Sequenzy lists:") ("list_1  Newsletter (1200 subscribers)") ("Tags: vip"))
    (is-carrying (answer (sequenzy:subscriber "ana@example.com"))
      ("ana@example.com  active  Ana") ("Lists: Newsletter") ("Notes: 0"))))

;;; --- a refusal is a repair, never a crash ---------------------------------------

(deftest sequenzy-cell-a-refused-answer-is-the-model-repair ()
  (with-sequenzy-answers
      (("/api/v1/campaigns"
        (values "{\"success\":false,\"error\":\"Invalid status filter.\"}" 400)))
    (is-carrying (answer (sequenzy-tool "sequenzy_campaigns" (nlk:json-object "status" "nope")))
      ("ERROR: sequenzy:") ("HTTP 400") ("Invalid status filter"))))
