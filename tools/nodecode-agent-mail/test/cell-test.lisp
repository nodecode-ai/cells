;;;; cell-test.lisp --- the mailbox against a stubbed Agent Mail Gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every /v1 route is a stubbed NLK:HTTP and the mailbox key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a gateway, the environment or a
;;;; mailbox. Each tool is called the way a model's call reaches it -- through
;;;; the real dispatch, contract and all -- so the schema the provider is
;;;; shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "agent-mail" "AGENT-MAIL-" :start nodecode-agent-mail:start-cell)

;;; --- fixtures ----------------------------------------------------------------------

(defmacro with-mail-key (&body body)
  "BODY with the mailbox key's variable answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "AGENT_MAIL_API_KEY") "test-key"))
     ,@body))

(defmacro with-mail-gateway ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT) oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)
                                               (cdr (assoc "authorization" (getf options :headers)
                                                           :test #'string-equal)))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Agent Mail Gateway request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +mailbox+
  (concatenate 'string
               "{\"address\":\"agent@mail.test\","
               "\"allow_receive_from\":[\"boss@mail.test\"],"
               "\"allow_send_to\":[\"boss@mail.test\",\"*@partner.test\"],"
               "\"allow_delete\":false,\"max_sends_per_hour\":30,\"max_attachment_mb\":15,"
               "\"timezone\":\"Europe/Berlin\","
               "\"now\":\"2026-10-03T09:30:00.000Z\","
               "\"now_local\":\"Saturday, 3 October 2026 at 11:30 (Europe/Berlin)\","
               "\"review\":{\"rules\":\"block\",\"llm_review\":\"off\","
               "\"policies\":[{\"rule\":\"Never share financial information.\"}]}}")
  "What GET /v1/mailbox answers.")

(defparameter +list+
  (concatenate 'string
               "{\"messages\":[{\"id\":\"1712345678-42\",\"from\":\"boss@mail.test\","
               "\"from_name\":\"Boss\",\"to\":[\"agent@mail.test\"],\"cc\":[],"
               "\"subject\":\"Weekly numbers\",\"date\":\"2026-10-02T08:15:00.000Z\","
               "\"preview\":\"Please send the numbers\",\"unread\":true,"
               "\"has_attachments\":true}],\"next_cursor\":\"page-2\"}")
  "What GET /v1/messages answers.")

(defparameter +message+
  (concatenate 'string
               "{\"id\":\"1712345678-42\",\"from\":\"boss@mail.test\",\"from_name\":\"Boss\","
               "\"to\":[\"agent@mail.test\"],\"cc\":[],\"subject\":\"Weekly numbers\","
               "\"date\":\"2026-10-02T08:15:00.000Z\",\"unread\":true,\"has_attachments\":true,"
               "\"message_id\":\"<abc@mail.test>\",\"reply_to\":null,"
               "\"body_markdown\":\"Please send the **numbers**.\\n\\nThanks\","
               "\"attachments\":[{\"index\":0,\"filename\":\"numbers.csv\","
               "\"content_type\":\"text/csv\",\"size\":2048}],"
               "\"invitation\":null}")
  "What GET /v1/messages/{id} answers.")

(defparameter +event+
  (concatenate 'string
               "{\"id\":\"evt-1\",\"title\":\"Review\",\"start\":\"2026-10-05T12:00:00.000Z\","
               "\"end\":\"2026-10-05T13:00:00.000Z\",\"sequence\":0,\"status\":\"confirmed\","
               "\"start_local\":\"Monday, 5 October 2026 at 14:00 (Europe/Berlin)\","
               "\"end_local\":\"Monday, 5 October 2026 at 15:00 (Europe/Berlin)\","
               "\"attendees\":[\"boss@mail.test\"],"
               "\"responses\":{\"boss@mail.test\":\"accepted\"}}")
  "What POST /v1/events and GET /v1/events/{id} answer.")

(defun mail-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun mail-start (&rest pairs)
  "START-CELL against a literal config, the gateway's address defaulted."
  (apply #'agent-mail-start
         (append (unless (member "url" pairs :test #'string=)
                   (list "url" "http://mail.test:8080"))
                 (copy-list pairs))))

;;; --- the lifecycle, from the one declaration ---------------------------------------

(define-cell-lifecycle-tests "agent-mail"
  (:config "url" "http://mail.test:8080")
  (:help :agent-mail)
  (:command "mail")
  (:fixture with-mail-key)
  (:refused ("url" "mail.test:8080")
            ("url" "http://mail.test:8080" "api_key_env" "UNSET_MAIL_KEY")))

(deftest agent-mail-cell-without-a-key-refuses (with-mail-key)
  ;; The one setting no verb can work without: a section that names no key,
  ;; and a variable that holds none, is refused at start.
  (let ((condition (signals-error nlk:config-refusal
                                  (mail-start "api_key" "" "api_key_env" "UNSET_MAIL_KEY"))))
    (is (search "no key" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "mail_list")) "and installs nothing")))

;;; --- mail_mailbox ------------------------------------------------------------------

(deftest agent-mail-cell-describes-the-mailbox (with-mail-key)
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked ("/v1/mailbox" (values +mailbox+ 200)))
      (let ((answer (mail-answer "am-1" "mail_mailbox" "{}")))
        (is (search "mailbox: agent@mail.test  (Europe/Berlin)" answer) "who the agent is")
        (is (search "may write to: boss@mail.test, *@partner.test" answer) "the policy it plans around")
        (is (search "delete allowed: no" answer))
        (is (search "Never share financial information." answer) "the operator's own review rule")
        (is (= 1 (length asked)) "one call")
        (is (equal :get (first (first asked))))
        (is (equal "Bearer test-key" (fourth (first asked))) "the mailbox key rides every call")))))

;;; --- mail_list ---------------------------------------------------------------------

(deftest agent-mail-cell-lists-and-narrows-mail (with-mail-key)
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked ("/v1/messages" (values +list+ 200)))
      (let ((answer (mail-answer "am-2" "mail_list" "{\"unread\":true,\"limit\":5}")))
        (is (search "1 message" answer))
        (is (search "unread  " answer) "an unread message says so")
        (is (search "Boss <boss@mail.test>" answer))
        (is (search "Weekly numbers" answer))
        (is (search "[attachments]" answer))
        (is (search "next_cursor: page-2" answer) "the cursor for older mail is offered")
        (let ((url (second (first asked))))
          (is (search "unread=true" url) "the filter is in the query")
          (is (search "limit=5" url))
          (is (not (search "from=" url)) "and nothing unset is sent"))))))

;;; --- mail_read ---------------------------------------------------------------------

(deftest agent-mail-cell-reads-a-message-with-its-parts (with-mail-key)
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked ("/v1/messages/1712345678-42" (values +message+ 200)))
      (let ((answer (mail-answer "am-3" "mail_read" "{\"id\":\"1712345678-42\"}")))
        (is (search "subject: Weekly numbers" answer))
        (is (search "Please send the **numbers**." answer) "the body as Markdown")
        (is (search "[0] numbers.csv" answer) "the attachment and its index")
        (is (search "2.0 KB" answer))
        (is (search "mark_read=true" (second (first asked))) "reading marks it read unless told not to")))))

;;; --- mail_send, and a refused send --------------------------------------------------

(deftest agent-mail-cell-sends-and-reports-the-id (with-mail-key)
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked ("/v1/messages" (values "{\"message_id\":\"<sent@mail.test>\",\"warnings\":[]}" 200)))
      (let ((answer (mail-answer "am-4" "mail_send"
                                  "{\"to\":[\"boss@mail.test\"],\"subject\":\"Numbers\",\"body_markdown\":\"Here they are\"}")))
        (is (search "sent: <sent@mail.test>" answer) "the id the send answered")
        (is (equal :post (first (first asked))))
        (let ((body (third (first asked))))
          (is (search "\"to\":[\"boss@mail.test\"]" body))
          (is (search "\"body_markdown\":\"Here they are\"" body))
          (is (not (search "attachments" body)) "an unset member is not sent at all"))))))

(deftest agent-mail-cell-reports-a-refused-send (with-mail-key)
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked ("/v1/messages" (values "{\"error\":\"review_rejected\",\"message\":\"The pre-send review stopped the message\",\"details\":{\"reviewer\":\"policy\",\"reasons\":[{\"rule\":\"policy\",\"message\":\"Never share financial information.\"}]}}" 422)))
      (let ((answer (mail-answer "am-5" "mail_send"
                                  "{\"to\":[\"boss@mail.test\"],\"subject\":\"Numbers\",\"body_markdown\":\"Revenue was 12\"}")))
        (is (search "ERROR: HTTP 422 review_rejected" answer) "the code the model repairs from")
        (is (search "Never share financial information." answer) "and the reason itself")))))

;;; --- the rest of the calls ----------------------------------------------------------

(deftest agent-mail-cell-covers-the-rest (with-mail-key)
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked
                        ("/attachments/" (values "a,b\n1,2" 200))
                        ("/reply" (values "{\"message_id\":\"<r@mail.test>\",\"warnings\":[]}" 200))
                        ("/forward" (values "{\"message_id\":\"<f@mail.test>\",\"warnings\":[]}" 200))
                        ("/rsvp" (values "{}" 200))
                        ("/v1/events/" (values +event+ 200))
                        ("/v1/events" (if (eq method :post)
                                            (values +event+ 201)
                                            (values (format nil "{\"events\":[~a]}" +event+) 200)))
                        ("/v1/messages/" (values "{}" 200))
                        ("/v1/messages" (values "{\"message_id\":\"<s@mail.test>\",\"warnings\":[\"copy_to_sent_failed\"]}" 200)))
      (is (search "unread again" (mail-answer "r1" "mail_mark" "{\"id\":\"1712345678-42\"}")))
      (is (search "Trash" (mail-answer "r2" "mail_delete" "{\"id\":\"1712345678-42\"}")))
      (is (equal "a,b\n1,2" (mail-answer "r3" "mail_attachment"
                                           "{\"id\":\"1712345678-42\",\"index\":0}"))
          "a text attachment comes back as its text")
      (is (search "sent: <r@mail.test>"
                  (mail-answer "r4" "mail_reply"
                                "{\"id\":\"1712345678-42\",\"body_markdown\":\"done\"}")))
      (is (search "sent: <f@mail.test>"
                  (mail-answer "r5" "mail_forward"
                                "{\"id\":\"1712345678-42\",\"to\":[\"boss@mail.test\"]}")))
      (is (search "answered accept"
                  (mail-answer "r6" "mail_rsvp"
                                "{\"id\":\"1712345678-42\",\"response\":\"accept\"}")))
      (is (search "evt-1" (mail-answer "r7" "mail_events" "{}")) "one call lists the calendar")
      (is (search "event created: evt-1"
                  (mail-answer "r8" "mail_create_event"
                                "{\"title\":\"Review\",\"start\":\"2026-10-05T14:00\",\"end\":\"2026-10-05T15:00\",\"attendees\":[\"boss@mail.test\"]}")))
      (is (search "evt-1 updated"
                  (mail-answer "r9" "mail_update_event" "{\"id\":\"evt-1\",\"title\":\"Review 2\"}")))
      (is (search "cancelled" (mail-answer "r10" "mail_cancel_event" "{\"id\":\"evt-1\"}"))))))

;;; --- the operator's command ---------------------------------------------------------

(deftest agent-mail-cell-mail-command-answers (with-mail-key)
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked ("/v1/mailbox" (values +mailbox+ 200))
                               ("/v1/messages" (values +list+ 200)))
      (let ((text (nodecode-agent-mail::run-slash "" "s1")))
        (is (search "mailbox: agent@mail.test" text))
        (is (search "1 unread message" text) "and what is waiting"))))
  (with-cell-stop ((mail-start))
    (with-mail-gateway (asked ("/v1/events" (values (format nil "{\"events\":[~a]}" +event+) 200)))
      (let ((text (nodecode-agent-mail::run-slash "events" "s1")))
        (is (search "evt-1" text) "the calendar and the ids a change takes")
        (is (search "accepted" text) "with who answered")))))
