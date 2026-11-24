;;;; cell-test.lisp --- the nextdns cell against canned NextDNS answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one network seam, *HTTP*, is stubbed: nothing here dials NextDNS, and
;;;; no test needs the network. What is proved is what the plugin gave a user --
;;;; a profile's query log, its blocked queries with the blocklist that matched,
;;;; aggregate counts, and the profile's own settings -- plus the tool and the
;;;; command answering, a device name resolved to the id the API filters on, and
;;;; the section refusing to start without a key.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-test-slice "nextdns" "NEXTDNS-CELL-" :start nodecode-nextdns:start-cell
  :defaults (("api_key_env" "") ("api_key" "test-key-0123456789") ("profile_id" "abc123")))

(define-cell-lifecycle-tests "nextdns"
  (:help :nextdns)
  (:command "nextdns")
  (:running (is (equal "test-key-0123456789" (nodecode-nextdns::setting :api-key)) "the key the section holds")
            (is (equal "abc123" (nodecode-nextdns::setting :profile-id)) "the default profile")
            (is (= 200 (nodecode-nextdns::setting :max-rows)) "two hundred rows an answer by default")
            (is (equal "https://api.nextdns.io" (nodecode-nextdns::setting :base-url)) "the API's own base"))
  (:refused ("api_key_env" "NEXTDNS_TEST_UNSET_KEY" "api_key" "") ("timeout_seconds" 0) ("max_rows" 5))
  (:idle nextdns:nextdns-error (nodecode-nextdns::totals nil)))

;;; --- fixtures ----------------------------------------------------------------------

(defparameter +logs-fixture+
  "{\"data\":[{\"timestamp\":\"2026-01-04T10:00:00Z\",\"domain\":\"ads.example.com\",\"status\":\"blocked\",\"device\":{\"id\":\"dev1\",\"name\":\"laptop\"},\"reasons\":[{\"id\":\"nextdns-recommended\",\"name\":\"NextDNS Recommended\"}],\"protocol\":\"DOH\",\"encrypted\":true},{\"timestamp\":\"2026-01-04T10:00:01Z\",\"domain\":\"plex.tv\",\"device\":{\"id\":\"dev2\"},\"protocol\":\"UDP\",\"encrypted\":false}],\"meta\":{\"pagination\":{\"cursor\":\"c1\"}}}"
  "A log page shaped like the API's: one blocked query with its blocklist, one
default query from a device the log names only by id.")

(defparameter +devices-fixture+
  "{\"data\":[{\"id\":\"dev1\",\"name\":\"laptop\",\"queries\":10},{\"id\":\"dev2\",\"name\":\"phone\",\"queries\":4}]}"
  "The profile's devices, as the analytics endpoint lists them: names and ids.")

(defparameter +profile-fixture+
  "{\"data\":{\"name\":\"Home\",\"settings\":{\"logs\":{\"enabled\":true,\"retention\":604800,\"location\":\"ch\"}},\"privacy\":{\"blocklists\":[{\"id\":\"nextdns-recommended\"}]}}}"
  "One profile with its log on and a week of retention.")

(defvar *requests* '()
  "Every stubbed request of one test, (URL TIMEOUT), newest first.")

(defvar *reply* nil
  "What the stubbed seam answers: a function of URL => (values TEXT STATUS).")

(defmacro with-nextdns ((&rest config) &body body)
  "A started nextdns cell on CONFIG whose seam answers *REPLY* and logs what it
was asked, so no test leaves the machine."
  `(let ((*requests* '())
         (*reply* (lambda (url) (error "no nextdns fixture answered ~a" url))))
     (with-cell-stop ((nextdns-start ,@config))
       (let ((nodecode-nextdns::*http*
               (lambda (url timeout)
                 (push (list url timeout) *requests*)
                 (funcall *reply* url))))
         ,@body))))

(defun nextdns-reply (text &optional (status 200))
  "What the next stubbed request answers, whatever it asks."
  (setf *reply* (lambda (url)
                  (declare (ignore url))
                  (values text status))))

(defun nextdns-routes (routes)
  "What the next stubbed requests answer: ROUTES is a list of (MARKER TEXT
STATUS), the first whose MARKER the URL holds."
  (setf *reply*
        (lambda (url)
          (let ((hit (loop for (marker text status) in routes
                           when (search marker url) return (list text status))))
            (if hit
                (values (first hit) (second hit))
                (values "{\"errors\":[{\"detail\":\"no fixture for this address\"}]}" 404))))))

(defun nextdns-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun nextdns-url (&optional (index 0))
  "The URL of the INDEXth stubbed request, oldest first."
  (first (nth index (nextdns-asked))))

(defun nextdns-call (tool &rest pairs)
  "TOOL's body, given PAIRS as the JSON arguments a model's call carries."
  (let ((entry (nle::find-tool tool)))
    (assert entry () "~a is not registered" tool)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun nextdns-answer (tool &rest pairs)
  "TOOL's answer to PAIRS, decoded."
  (nlk:decode-json (apply #'nextdns-call tool pairs)))

;;; --- the log, through the tool ---------------------------------------------------

(deftest nextdns-cell-a-log-answer-carries-what-triage-reads ()
  (with-nextdns ()
    (nextdns-reply +logs-fixture+)
    (let* ((answer (nextdns-answer "nextdns_logs"))
           (logs (nlk:json-array answer "logs"))
           (blocked (aref logs 0))
           (plain (aref logs 1)))
      (is (search "https://api.nextdns.io/profiles/abc123/logs?" (nextdns-url)) "the one address read is the profile's log")
      (is (search "limit=10" (nextdns-url)) "a limit under the API's floor is raised, not refused")
      (is (search "raw=true" (nextdns-url)) "every query, not only navigational ones, by default")
      (is (equal "abc123" (nlk:json-value answer :string "profile")))
      (is (= 2 (nlk:json-value answer :integer "count")))
      (is (equal "c1" (nlk:json-value answer :string "cursor")) "the cursor to page with rides the answer")
      (is (equal "ads.example.com" (nlk:json-value blocked :string "domain")))
      (is (equal "blocked" (nlk:json-value blocked :string "status")))
      (is (equal "laptop" (nlk:json-value blocked :string "device")) "a device named in the log is named back")
      (is (equal "NextDNS Recommended" (aref (nlk:json-array blocked "reasons") 0)) "the blocklist that matched rides the row")
      (is (nlk:json-value blocked :boolean "encrypted") "and whether the query was encrypted")
      (is (equal "default" (nlk:json-value plain :string "status")) "an absent status reads as default")
      (is (equal "dev2" (nlk:json-value plain :string "device")) "a device the log names only by id falls back to its id"))))

(deftest nextdns-cell-blocked-asks-the-log-for-blocks-and-resolves-a-device-name ()
  (with-nextdns ()
    (nextdns-routes (list (list "/analytics/devices" +devices-fixture+ 200)
                           (list "/logs" +logs-fixture+ 200)))
    (let ((answer (nextdns-answer "nextdns_blocked" "device" "laptop" "limit" 25)))
      (is (= 2 (length (nextdns-asked))) "the device list is read once, then the log")
      (is (search "/profiles/abc123/analytics/devices" (nextdns-url 0)) "a device name is resolved through the profile's own list")
      (is (search "status=blocked" (nextdns-url 1)) "the log is asked for blocked queries only")
      (is (search "device=dev1" (nextdns-url 1)) "and filtered by the id, not the name")
      (is (search "limit=25" (nextdns-url 1)))
      (is (= 2 (nlk:json-value answer :integer "count"))))))

(deftest nextdns-cell-analytics-answers-one-metric-cheaply ()
  (with-nextdns ()
    (nextdns-reply "{\"data\":[{\"id\":\"ads.example.com\",\"queries\":42}]}")
    (let ((answer (nextdns-answer "nextdns_analytics" "metric" "domains" "limit" 5)))
      (is (search "/profiles/abc123/analytics/domains?" (nextdns-url)))
      (is (search "limit=10" (nextdns-url)) "the analytics floor is the same ten")
      (is (equal "domains" (nlk:json-value answer :string "metric")))
      (is (= 42 (nlk:json-value (aref (nlk:json-array answer "data") 0) :integer "queries"))))))

;;; --- the profile ------------------------------------------------------------------

(deftest nextdns-cell-a-profile-says-whether-its-log-is-on ()
  (with-nextdns ()
    (nextdns-reply +profile-fixture+)
    (let ((answer (nextdns-answer "nextdns_profile")))
      (is (equal "https://api.nextdns.io/profiles/abc123" (nextdns-url)))
      (is (equal "Home" (nlk:json-value answer :string "name")))
      (is (nlk:json-value answer :boolean "logging_enabled"))
      (is (= 604800 (nlk:json-value answer :integer "retention_seconds")))
      (is (= 7.0 (nlk:json-value answer :number "retention_days")) "a week, in days")
      (is (equal "ch" (nlk:json-value answer :string "log_location")))
      (is (equal "nextdns-recommended" (aref (nlk:json-array answer "blocklists") 0)))
      (is (null (nlk:json-value answer :string "note")) "a clean profile carries no note"))))

(deftest nextdns-cell-with-no-profile-the-key-s-profiles-are-listed ()
  (with-nextdns ("profile_id" "")
    (nextdns-reply "{\"data\":[{\"id\":\"abc123\",\"name\":\"Home\"}]}")
    (let ((answer (nextdns-answer "nextdns_profile")))
      (is (equal "https://api.nextdns.io/profiles" (nextdns-url)) "the profiles the key can see")
      (is (search "No default profile configured" (nlk:json-value answer :string "note")))
      (is (equal "Home" (nlk:json-value (aref (nlk:json-array answer "profiles") 0) :string "name"))))))

(deftest nextdns-cell-a-log-with-no-profile-says-what-to-set ()
  (with-nextdns ("profile_id" "")
    (is (search "no profile" (nextdns-call "nextdns_logs")) "the refusal names the member")
    (is (null (nextdns-asked)) "and nothing was dialled")))

;;; --- what can go wrong -------------------------------------------------------------

(deftest nextdns-cell-a-refusal-is-said-in-the-api-s-own-words ()
  (with-nextdns ()
    (nextdns-reply "{\"errors\":[{\"source\":{\"parameter\":\"limit\"},\"detail\":\"must be >= 10\"}]}" 400)
    (is (search "HTTP 400" (nextdns-call "nextdns_logs")))
    (is (search "limit must be >= 10" (nextdns-call "nextdns_logs")) "the API's own detail, not a bare status")
    (nextdns-reply "{\"errors\":[{\"detail\":\"profile not found\"}]}")
    (is (search "profile not found" (nextdns-call "nextdns_logs")) "a 200 that carries errors is a refusal too")
    (nextdns-reply "not json at all")
    (is (search "not JSON" (nextdns-call "nextdns_logs")) "an answer that is not JSON is refused, not half-read")
    (setf *reply* (lambda (url)
                    (declare (ignore url))
                    (error "no route to host")))
    (is (search "could not reach" (nextdns-call "nextdns_logs")) "a dial that failed says so")))

;;; --- the command -----------------------------------------------------------------

(deftest nextdns-cell-the-command-answers-the-status-totals ()
  (with-nextdns ()
    (nextdns-reply "{\"data\":[{\"id\":\"default\",\"queries\":1234},{\"id\":\"blocked\",\"queries\":210}]}")
    (let ((text (cell-entry "nodecode-nextdns" "nextdns" "")))
      (is (equal "https://api.nextdns.io/profiles/abc123/analytics/status?limit=100" (nextdns-url)) "the command reads the same API as the tools")
      (is (search "profile abc123" text))
      (is (search "default: 1234 queries" text))
      (is (search "blocked: 210 queries" text))
      (is (search "total: 1444 queries" text) "and the total the operator asked for"))))

;;; --- the request itself -----------------------------------------------------------

(deftest nextdns-cell-the-request-carries-the-key-and-the-name-the-section-set ()
  (with-nextdns ("timeout_seconds" 7 "user_agent" "nodecode-nextdns-test/0.1")
    (let ((asked '()))
      (let ((nodecode-nextdns::*http* #'nodecode-nextdns::default-http))
        (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                   (push (list* method url args) asked)
                                   (values +logs-fixture+ 200))
          (nextdns-call "nextdns_logs")))
      (let ((request (first asked)))
        (is (equal :get (first request)) "the log is read with a GET")
        (is (search "https://api.nextdns.io/profiles/abc123/logs?" (second request)))
        (is (equal "test-key-0123456789" (cdr (assoc "x-api-key" (getf (cddr request) :headers) :test #'equal))) "the key rides a header")
        (is (null (search "test-key" (second request))) "and never the URL")
        (is (equal "nodecode-nextdns-test/0.1" (cdr (assoc "user-agent" (getf (cddr request) :headers) :test #'equal))) "the name the section set, which NextDNS requires")
        (is (= 7 (getf (cddr request) :timeout)) "the section's timeout")
        (is (= (* 2 1024 1024) (getf (cddr request) :max-bytes)) "and a cap on the answer's size")))))

(deftest nextdns-cell-a-query-string-leaves-out-what-was-not-set ()
  (is (equal "limit=10&raw=true" (nodecode-nextdns:query-string '(("limit" . "10") ("search" . "") ("status" . nil) ("raw" . "true")))) "an unset filter stays unset")
  (is (equal "search=a%20b%26c" (nodecode-nextdns:query-string '(("search" . "a b&c")))) "a value is percent-encoded, so a domain with a space or an ampersand survives"))
