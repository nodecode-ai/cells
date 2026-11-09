;;;; cell-test.lisp --- the hass-statistics cell against a stubbed instance.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Home Assistant exchange is a stubbed wire -- the greeting, the
;;;; token's answer and the Recorder's result are canned JSON frames -- and the
;;;; token is a stubbed NLE::CREDENTIAL-ENV: nothing reaches an instance, the
;;;; environment or the operator's files. Each tool is called the way a model's
;;;; call reaches it, through the real dispatch, contract and all, so the schema
;;;; the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "hass-statistics" "HASS-STATISTICS-CELL-"
  :start nodecode-hass-statistics:start-cell
  :defaults (("url" "http://ha.test:8123") ("token_env" "HASS_TOKEN")))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-hass-token (&body body)
  "BODY with the token environment answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "HASS_TOKEN") "test-token"))
     ,@body))

(defmacro with-hass-wire ((sent &rest frames) &body body)
  "BODY with the cell's socket stubbed. FRAMES are the JSON strings the wire
answers, in order, one per read; SENT collects the JSON the cell sends, oldest
first, once BODY reads it."
  (let ((log (gensym "LOG"))
        (queue (gensym "QUEUE")))
    ;; A tool runs on the dispatch's own thread, where a LET of a special is not
    ;; seen: the seam is set globally and put back on unwind, test by test.
    `(let ((,log '())
           (,queue (list ,@frames))
           (previous nodecode-hass-statistics::*wire-maker*))
       (setf nodecode-hass-statistics::*wire-maker*
             (lambda (url)
               (declare (ignore url))
               (list :send (lambda (text) (push text ,log))
                     :recv (lambda (seconds)
                             (declare (ignore seconds))
                             (pop ,queue))
                     :close (lambda () t))))
       (unwind-protect
            (symbol-macrolet ((,sent (reverse ,log)))
              ,@body)
         (setf nodecode-hass-statistics::*wire-maker* previous)))))

(defparameter +greeting+ "{\"type\":\"auth_required\",\"ha_version\":\"2026.10.0\"}"
  "The first frame an instance sends.")

(defparameter +auth-ok+ "{\"type\":\"auth_ok\",\"ha_version\":\"2026.10.0\"}"
  "The answer to a good token.")

(defparameter +statistic-ids+
  (concatenate 'string
               "{\"id\":1,\"type\":\"result\",\"success\":true,\"result\":["
               "{\"statistic_id\":\"sensor.office_temperature\",\"unit_of_measurement\":\"C\","
               "\"has_mean\":true,\"has_sum\":false,\"source\":\"recorder\"},"
               "{\"statistic_id\":\"sensor.kitchen_energy\",\"unit_of_measurement\":\"kWh\","
               "\"has_mean\":false,\"has_sum\":true,\"source\":\"recorder\"}]}")
  "What recorder/list_statistic_ids answers.")

(defparameter +series+
  (concatenate 'string
               "{\"id\":1,\"type\":\"result\",\"success\":true,\"result\":{"
               "\"sensor.office_temperature\":["
               "{\"start\":1759276800000,\"end\":1759363200000,\"mean\":19.5,\"min\":18.1,\"max\":21.0},"
               "{\"start\":1759363200000,\"end\":1759449600000,\"mean\":20.25,\"min\":19.0,\"max\":21.5}]}}")
  "What recorder/statistics_during_period answers for one id.")

(defun hass-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun hass-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (hass-answer id tool arguments)))

(defun hass-fail (id tool arguments)
  "TOOL's failure text for ARGUMENTS, or NIL when it answered."
  (let ((text (hass-answer id tool arguments)))
    (and (search "ERROR:" text) text)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "hass-statistics"
  (:config "token_env" "HASS_TOKEN")
  (:help :hass-statistics)
  (:fixture with-hass-token)
  (:refused ("url" 5) ("max_ids" 0)))

(deftest hass-statistics-cell-without-a-token-refuses ()
  ;; The one setting no verb can work without: the section names a token, and a
  ;; section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (hass-statistics-start "token_env" ""))))
    (is (search "no token" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "ha_list_statistics")) "and installs nothing")))

;;; --- ha_list_statistics ---------------------------------------------------------------

(deftest hass-statistics-cell-lists-statistic-ids (with-hass-token)
  (with-cell-stop ((hass-statistics-start))
    (with-hass-wire (sent +greeting+ +auth-ok+ +statistic-ids+)
      (let* ((object (hass-object "hass-1" "ha_list_statistics" "{}"))
             (ids (nlk:json-array object "statistics")))
        (is (= 2 (nlk:json-value object :integer "count")) "both statistics are answered")
        (is (equal "sensor.office_temperature"
                   (nlk:json-value (aref ids 0) :string "statistic_id")))
        (is (equal "C" (nlk:json-value (aref ids 0) :string "unit_of_measurement")) "the unit rides along, as the instance sent it")
        (is (= 2 (length sent)) "the token and the one command are all it sends")
        (is (search "recorder/list_statistic_ids" (second sent)) "the discovery command is the one sent")
        (is (search "test-token" (first sent)) "the token is what the instance is given")))))

(deftest hass-statistics-cell-narrows-the-list (with-hass-token)
  (with-cell-stop ((hass-statistics-start))
    (with-hass-wire (sent +greeting+ +auth-ok+ +statistic-ids+)
      (let ((object (hass-object "hass-2" "ha_list_statistics" "{\"search\":\"ENERGY\"}")))
        (is (= 1 (nlk:json-value object :integer "count")) "search is case-insensitive and matches part of the id")))))

(deftest hass-statistics-cell-keeps-the-operators-deny-list (with-hass-token)
  (with-cell-stop ((hass-statistics-start "entity_denylist" #("sensor.kitchen*")))
    (with-hass-wire (sent +greeting+ +auth-ok+ +statistic-ids+)
      (let* ((object (hass-object "hass-3" "ha_list_statistics" "{}"))
             (ids (nlk:json-array object "statistics")))
        (is (= 1 (nlk:json-value object :integer "count")) "the denylisted statistic is gone")
        (is (equal "sensor.office_temperature"
                   (nlk:json-value (aref ids 0) :string "statistic_id")))))))

;;; --- ha_get_statistics ------------------------------------------------------------------

(defparameter +week+
  "{\"statistic_ids\":[\"sensor.office_temperature\"],\"start_time\":\"2026-10-01T00:00:00+02:00\",\"end_time\":\"2026-10-03T00:00:00+02:00\",\"period\":\"day\",\"types\":[\"mean\",\"min\",\"max\"]}"
  "One day-period query, as the model sends it.")

(deftest hass-statistics-cell-fetches-history (with-hass-token)
  (with-cell-stop ((hass-statistics-start))
    (with-hass-wire (sent +greeting+ +auth-ok+ +series+)
      (let* ((object (hass-object "hass-4" "ha_get_statistics" +week+))
             (result (nlk:json-value object :object "result"))
             (rows (nlk:json-array result "sensor.office_temperature")))
        (is (= 2 (length rows)) "both aggregated periods are answered")
        (is (= 195/10 (nlk:json-value (aref rows 0) :number "mean")) "the values are the instance's")
        (is (search "recorder/statistics_during_period" (second sent)) "the history command is the one sent")
        (is (search "\"period\":\"day\"" (second sent)) "the period is sent as asked")
        (is (search "\"end_time\"" (second sent)) "an end_time given is sent")))))

(deftest hass-statistics-cell-refuses-a-denied-id-before-dialling (with-hass-token)
  (with-cell-stop ((hass-statistics-start "entity_denylist" #("sensor.office_temperature")))
    (with-hass-wire (sent)
      (let ((failure (hass-fail "hass-5" "ha_get_statistics" +week+)))
        (is (search "denylist" failure) "the refusal names the list that excluded it")
        (is (null sent) "and nothing is sent: the socket is never opened")))))

(deftest hass-statistics-cell-refuses-a-timestamp-without-a-timezone (with-hass-token)
  (with-cell-stop ((hass-statistics-start))
    (with-hass-wire (sent)
      (let ((failure (hass-fail "hass-6" "ha_get_statistics"
                                 "{\"statistic_ids\":[\"sensor.office_temperature\"],\"start_time\":\"2026-10-01T00:00:00\"}")))
        (is (search "timezone" failure) "the refusal says the offset is required")
        (is (null sent) "and nothing is sent")))))

(deftest hass-statistics-cell-refuses-a-query-past-the-id-budget (with-hass-token)
  (with-cell-stop ((hass-statistics-start "max_ids" 1))
    (with-hass-wire (sent)
      (let ((failure (hass-fail "hass-7" "ha_get_statistics"
                                 "{\"statistic_ids\":[\"sensor.a\",\"sensor.b\"],\"start_time\":\"2026-10-01T00:00:00+02:00\"}")))
        (is (search "too many statistic ids" failure) "the budget is named")
        (is (null sent) "and nothing is sent")))))

(deftest hass-statistics-cell-refuses-a-rejected-token (with-hass-token)
  (with-cell-stop ((hass-statistics-start))
    (with-hass-wire (sent +greeting+ "{\"type\":\"auth_invalid\",\"message\":\"bad token\"}")
      (let ((failure (hass-fail "hass-8" "ha_list_statistics" "{}")))
        (is (search "refused the access token" failure) "the refusal says the token is the problem")
        (is (not (search "bad token" failure)) "and the instance's own words are not reflected back")))))
