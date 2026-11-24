;;;; cell-test.lisp --- the pull channel against a stubbed Watchline API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every /v1 route is a stubbed NLK:HTTP and the project key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Watchline or this machine's
;;;; environment. The poll thread's step is quieted so a pass is the test's
;;;; own, and what is asserted is what a pass leaves behind: the prompt in the
;;;; session, the one ack after it, and the cursor followed to the end of the
;;;; channel.

(in-package #:nodecode.test)

(define-test-slice "watchline" "WATCHLINE-" :start nodecode-watchline:start-cell)

;;; --- fixtures ----------------------------------------------------------------------

(defmacro with-watchline-key (&body body)
  "BODY with the project key's variable answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "WATCHLINE_API_KEY") "wl_test"))
     ,@body))

(defmacro with-watchline-api ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT) oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Watchline request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defmacro with-watchline ((&rest pairs) &body body)
  "BODY with a key, the poll thread's step quieted so a pass is the test's own,
and the cell started on PAIRS -- channel_id defaulted."
  `(with-watchline-key
     (with-stubbed-fdefinition (nodecode-watchline::poll-step () nil)
       (with-cell-stop ((watchline-start "channel_id" "ch_test" ,@pairs))
         ,@body))))

(defparameter +match-page+
  (concatenate 'string
               "{\"data\":[{\"type\":\"watchline.match\",\"delivery_id\":\"del_1\","
               "\"watch_id\":\"w_1\",\"user_id\":\"me\",\"intent\":\"urgent billing emails\","
               "\"event\":{\"from\":\"a customer\",\"subject\":\"Production invoice failure\","
               "\"snippet\":\"Our invoice payment is failing before renewal.\"}}],"
               "\"next_cursor\":null}")
  "What POST /v1/deliveries.pending answers: one matched event, one page.")

(defparameter +first-page+
  (concatenate 'string
               "{\"data\":[{\"type\":\"watchline.match\",\"delivery_id\":\"del_1\","
               "\"intent\":\"urgent billing emails\",\"event\":{\"subject\":\"first\"}}],"
               "\"next_cursor\":\"page-2\"}")
  "The first page of a channel that takes two reads.")

(defparameter +second-page+
  (concatenate 'string
               "{\"data\":[{\"type\":\"watchline.match\",\"delivery_id\":\"del_2\","
               "\"intent\":\"urgent billing emails\",\"event\":{\"subject\":\"second\"}}],"
               "\"next_cursor\":null}")
  "The second and last page.")

(defparameter +empty-page+ "{\"data\":[]}" "A channel with nothing on it.")

(defparameter +action-delivery+
  (concatenate 'string
               "{\"type\":\"watchline.needs_action\",\"delivery_id\":\"del_3\","
               "\"action\":\"reconnect gmail\",\"intent\":\"watch my inbox\","
               "\"connect_urls\":{\"gmail\":\"https://connect.example/1\"}}")
  "A delivery that is not a match: the watch needs the operator to reconnect.")

(defmacro with-submits ((var) &body body)
  "BODY with VAR collecting (SESSION PROMPT) from a stubbed NLE:SUBMIT."
  `(let ((,var '()))
     (with-stubbed-fdefinition (nle:submit (session prompt &key &allow-other-keys)
                                 (push (list session prompt) ,var) :started)
       ,@body)))

(defun watchline-tool (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun ack-ids-of (content)
  "The delivery ids one /v1/deliveries.ack request carried."
  (coerce (nlk:json-array (nlk:decode-json content) "delivery_ids") 'list))

;;; --- the lifecycle, from the one declaration ---------------------------------------

(define-cell-lifecycle-tests "watchline"
  (:config "channel_id" "ch_test")
  (:help :watchline)
  (:command "watchline")
  (:fixture with-watchline-key)
  (:running (is (equal "ch_test" (nodecode-watchline::setting :channel-id)))
            (is (equal "watchline" (nodecode-watchline::setting :session)) "a session of its own")
            (is (= 15 (nodecode-watchline::setting :poll-interval-seconds)))
            (is-present (tool (nle::find-tool "watchline_pending"))
              "the tool is on the catalog"))
  (:stopped (is (null nodecode-watchline::*worker*) "the poll thread is stopped"))
  (:refused ("channel_id" "")
            ("channel_id" "ch_test" "poll_interval_seconds" 1))
  (:idle watchline:watchline-error (watchline:pending :limit 5)))

(deftest watchline-cell-without-a-key-refuses ()
  ;; The original's normalize_config: no key, no start. The variable it names
  ;; is unset here, so the section has nothing to read.
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (let ((condition (signals-error nlk:config-refusal
                                    (watchline-start "channel_id" "ch_test"))))
      (is (search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "watchline_pending")) "and installs nothing"))))

;;; --- a delivery into the session ----------------------------------------------------

(deftest watchline-cell-a-match-becomes-a-prompt-in-its-session ()
  (with-temp-store ()
    (with-watchline-api (asked ("/v1/deliveries.ack" (values "{}" 200))
                                ("/v1/deliveries.pending" (values +match-page+ 200)))
      (with-watchline ()
          (with-submits (submitted)
            (is (= 1 (nodecode-watchline::poll-once :deliver t)) "one delivery read")
            (is (= 1 (length submitted)) "one prompt")
            (let ((session (first (first submitted)))
                  (prompt (second (first submitted))))
              (is (equal "watchline" session) "into the session the section names")
              (is (search "Matched event with user intent: urgent billing emails" prompt) "the intent")
              (is (search "Use this event as fresh context" prompt) "act only when asked")
              (is (search "Production invoice failure" prompt) "the event itself")
              (is (search "not a message a person typed" prompt) "framed as an event")))
          (is (nlk:session-exists-p "watchline") "the session is made on the first delivery")
          (is (= 2 (length asked)) "one poll, one ack")
          (let ((ack (second asked)))
            (is (equal :post (first ack)) "the ack is a POST")
            (is (equal '("del_1") (ack-ids-of (third ack))) "for the delivery the turn took")
            (is (equal "ch_test" (nlk:json-value (nlk:decode-json (third ack)) :string "channel_id")) "its channel"))))))

(deftest watchline-cell-follows-the-cursor-and-acks-once ()
  (with-temp-store ()
    (let ((page 0))
      (with-watchline-api (asked ("/v1/deliveries.ack" (values "{}" 200))
                                  ("/v1/deliveries.pending"
                                   (if (zerop page)
                                       (progn (incf page) (values +first-page+ 200))
                                       (values +second-page+ 200))))
        (with-watchline ()
          (with-submits (submitted)
            (is (= 2 (nodecode-watchline::poll-once :deliver t)) "both pages read")
            (is (= 2 (length submitted)) "and both delivered")))
        (is (= 3 (length asked)) "two polls, one ack for the whole pass")
        (is (equal '("del_2" "del_1") (ack-ids-of (third (third asked)))) "every id once")
        (is (search "\"cursor\":\"page-2\"" (third (second asked))) "past the first")))))

(deftest watchline-cell-nothing-waiting-asks-for-no-ack ()
  (with-temp-store ()
    (with-watchline-api (asked ("/v1/deliveries.pending" (values +empty-page+ 200)))
      (with-watchline ()
        (let ((answer (watchline-tool "c1" "watchline_pending" "{}")))
          (is (search "nothing waiting on channel ch_test" answer))
          (is (= 1 (length asked)) "one read of the channel and no ack"))))))

;;; --- the tool, and /watchline ------------------------------------------------------

(deftest watchline-cell-the-tool-reads-without-acking ()
  (with-temp-store ()
    (with-watchline-api (asked ("/v1/deliveries.pending" (values +match-page+ 200)))
      (with-watchline ()
        (let ((answer (watchline-tool "c1" "watchline_pending" "{\"limit\":5}")))
          (is (search "1 waiting on channel ch_test" answer) "what is waiting, counted")
          (is (search "urgent billing emails" answer) "with the intent the user set")
          (is (search "Production invoice failure" answer) "and the event")
          (is (search "nothing here is acknowledged" answer) "and says who delivers it")
          (is (= 1 (length asked)) "one read, and no ack: the worker still delivers it")
          (is (= 5 (nlk:json-value (nlk:decode-json (third (first asked))) :integer "limit")) "its own limit"))))))

(deftest watchline-cell-preview-shows-the-message-without-a-request ()
  (with-temp-store ()
    (with-watchline-api (asked)
      (with-watchline ()
        (let ((answer (nodecode-watchline::run-slash "preview" nil)))
          (is (search "Matched event with user intent: urgent billing emails" answer))
          (is (search "Production invoice failure" answer) "the original's own sample")
          (is (null asked) "nothing left the machine"))))))

(deftest watchline-cell-status-names-the-channel-and-the-last-poll ()
  (with-temp-store ()
    (with-watchline-api (asked)
      (with-watchline ("session" "watcher")
        (setf nodecode-watchline::*last-poll* "09:30: 2 deliveries")
        (let ((answer (nodecode-watchline::run-slash "" nil)))
          (is (search "channel ch_test" answer))
          (is (search "delivering into session watcher" answer))
          (is (search "last poll: 09:30: 2 deliveries" answer))
          (is (null asked) "status asks the API nothing"))))))

(deftest watchline-cell-a-refused-answer-says-what-the-api-said ()
  (with-temp-store ()
    (with-watchline-api (asked ("/v1/deliveries.pending"
                                 (values "{\"error\":\"invalid_api_key\"}" 401)))
      (with-watchline ()
        (let ((condition (signals-error watchline:watchline-error
                                        (nodecode-watchline::run-slash "check" nil))))
          (is (search "answered 401" (princ-to-string condition)))
          (is (search "invalid_api_key" (princ-to-string condition)) "its own words"))))))

;;; --- the two shapes of a delivery ---------------------------------------------------

(deftest watchline-cell-a-watch-that-needs-action-says-where-to-connect ()
  (let ((text (nodecode-watchline::delivery-text (nlk:decode-json +action-delivery+))))
    (is (search "Watch needs action: reconnect gmail" text))
    (is (search "User intent: watch my inbox" text))
    (is (search "https://connect.example/1" text) "the connection links it carries")
    (is (search "not a message a person typed" text) "under the same frame")))
