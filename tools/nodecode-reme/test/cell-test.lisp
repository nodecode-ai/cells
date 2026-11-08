;;;; cell-test.lisp --- the reme cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every action service is a stubbed NLE:HTTP: nothing reaches a ReMe, the
;;;; environment or the operator's files. The frames are the ones the gateway
;;;; publishes (CELL-FRAME-OP), so what the cell learns is what it would learn
;;;; in a turn.

(in-package #:nodecode.test)

(define-test-slice "reme" "REME-CELL-" :start nodecode-reme:start-cell
  :defaults (("endpoint" "http://127.0.0.1:2333")))

(define-cell-lifecycle-tests "reme"
  (:hooks :frame 'nle::read-harness-sections)
  (:help :reme)
  (:command "reme")
  (:running
   (is (nle::find-tool "reme_recall") "the recall tool is registered")
   (is (eq (nlk:contract 'nodecode-reme::recall-note)
           (second (nle::find-tool "reme_recall")))
       "registered against its declaration"))
  (:stopped
   (is (null (nle::find-tool "reme_recall")) "stop unregisters the tool"))
  (:refused ("endpoint" "") ("recall_limit" 0))
  (:idle nodecode-reme:reme-error (reme:health)))

;;; --- a stubbed action service --------------------------------------------------------

(defmacro with-reme-http ((log &rest routes) &body body)
  "BODY with NLE:HTTP answering by URL fragment: ROUTES are (FRAGMENT FORM),
FORM answering (values TEXT STATUS) for that request. LOG collects
(METHOD URL CONTENT), oldest first once BODY reads it."
  `(let ((,log '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list method url (getf args :content)) ,log)
                                (cond ,@(loop for (fragment form) in routes
                                              collect `((search ,fragment url) ,form))
                                      (t (error "unexpected request to ~a" url))))
       ,@body)))

(defparameter +reme-search+
  "{\"success\":true,\"answer\":\"NOTE one: the operator prefers tabs to spaces.\",\"metadata\":{\"counts\":{\"returned\":1}}}")

(defparameter +reme-recorded+
  "{\"success\":true,\"answer\":\"recorded\"}")

(defun reme-asked (session text)
  "The frame op that carries TEXT as SESSION's committed ask."
  (cell-frame-op "turn.input_committed" :session session
                                        :payload (nlk:json-object "message" text)))

(defun reme-answered (session text)
  "The frame op that carries TEXT as SESSION's assistant round."
  (cell-frame-op "turn.assistant_message_completed"
                  :session session
                  :payload (nlk:json-object "message" (nlk:json-object "role" "assistant"
                                                                   "content" text))))

;;; --- the tool --------------------------------------------------------------------------

(deftest reme-cell-tool-answers-with-what-the-service-holds ()
  (with-reme-http (log ("/search" (values +reme-search+ 200)))
    (with-cell-stop ((reme-start))
      (let ((note (nodecode-reme:decode-recall-note
                   (nlk:decode-json "{\"query\":\"indentation\",\"limit\":3}"))))
        (is (search "prefers tabs" (nodecode-reme::tool-recall note))))
      (is (= 1 (length log)) "one action, one request")
      (is (equal :post (first (first log))))
      (is (search "127.0.0.1:2333/search" (second (first log))))
      (is (search "\"query\":\"indentation\"" (third (first log))) "the query goes as the service wants it")
      (is (search "\"limit\":3" (third (first log))) "and so does the cap"))))

(deftest reme-cell-a-service-that-says-no-is-a-refusal ()
  (with-reme-http (log ("/search" (values "{\"success\":false,\"answer\":\"no workspace\"}" 200)))
    (with-cell-stop ((reme-start))
      (let ((text (handler-case (progn (reme:recall "anything") "")
                    (nodecode-reme:reme-error (condition) (princ-to-string condition)))))
        (is (search "no workspace" text) "the service's own reason reaches the caller")))))

;;; --- recall before a turn ----------------------------------------------------------------

(deftest reme-cell-recall-rides-the-request ()
  (with-reme-http (log ("/search" (values +reme-search+ 200)))
    (with-cell-stop ((reme-start))
      (cell-publish "nodecode-reme" (reme-asked "s1" "how does this operator indent"))
      (is (search "prefers tabs" (cell-section "s1" "reme")) "the notes are in the request")
      (is (= 1 (length log)) "one recall for one ask")
      (cell-section "s1" "reme")
      (is (= 1 (length log)) "the turn's later rounds read the one recall")
      (cell-publish "nodecode-reme" (reme-asked "s1" "and which editor"))
      (cell-section "s1" "reme")
      (is (= 2 (length log)) "a new ask is a new recall"))))

(deftest reme-cell-a-service-that-is-down-does-not-spoil-the-turn ()
  (with-reme-http (log ("/search" (error "connection refused")))
    (with-cell-stop ((reme-start))
      (cell-publish "nodecode-reme" (reme-asked "s1" "anything"))
      (is (null (cell-section "s1" "reme")) "no block when the service could not answer")
      (is (search "did not answer" (reme:status)) "and the cell says why in /reme"))))

;;; --- recording a completed turn ------------------------------------------------------------

(deftest reme-cell-records-a-completed-turn ()
  (let ((nodecode-reme::*use-worker* nil))
    (with-reme-http (log ("/auto_memory" (values +reme-recorded+ 200)))
      (with-cell-stop ((reme-start))
        (cell-publish "nodecode-reme" (reme-asked "s1" "ASK ONE"))
        (cell-publish "nodecode-reme" (reme-answered "s1" "ANSWER ONE"))
        (cell-publish "nodecode-reme" (cell-frame-op "turn.completed" :session "s1"))
        (is (zerop (length log)) "the publishing thread does no I/O")
        (is (zerop (nodecode-reme::flush-writes)) "the writer sends what was queued")
        (is (= 1 (length log)) "one completed turn, one record")
        (let ((body (third (first log))))
          (is (search "ASK ONE" body) "the ask goes back")
          (is (search "ANSWER ONE" body) "and the answer")
          (is (search "\"session_id\":\"nodecode-s1-" body) "scoped to this harness and session"))
        (nodecode-reme::flush-writes)
        (is (= 1 (length log)) "sent once, not again")))))

;;; --- /reme ---------------------------------------------------------------------------------

(deftest reme-cell-slash-answers-status-and-health ()
  (with-reme-http (log ("/health_check"
                         (values "{\"success\":true,\"metadata\":{\"health\":{\"healthy\":true}}}" 200)))
    (with-cell-stop ((reme-start))
      (multiple-value-bind (text dialog) (cell-entry "nodecode-reme" "reme" "")
        (is (search "http://127.0.0.1:2333" text) "the endpoint in use")
        (is (search "recall on" text))
        (is (not (null dialog)) "and a panel to act from"))
      (is (search "is healthy" (cell-entry "nodecode-reme" "reme" "health")))
      (is (equal :post (first (first log))) "a health check posts to the service"))))

;;; --- the one setting without which nothing is reachable ---------------------------------------

(deftest reme-cell-refuses-without-an-endpoint ()
  (is (signals-error nlk:config-refusal
        (nodecode-reme:start-cell (nlk:json-object "reme" (nlk:make-json-object))))
      "an absent endpoint is a config refusal, not a silent no-op")
  (is (null (nle::find-tool "reme_recall")) "and nothing was installed"))
