;;;; cell-test.lisp --- the openmail cell against a stubbed service.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every OpenMail endpoint is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches the service, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it
;;;; -- through the real dispatch, contract and all -- so the schema the
;;;; provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "openmail" "OPENMAIL-CELL-")

(defun openmail-config (&rest pairs) "A whole shared config holding the `openmail' section PAIRS make, with its
key variable and one inbox defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "api_key_env" section "OPENMAIL_KEY")
    (alexandria:ensure-gethash "inbox_id" section "inb_1")
    (nlk:json-object "openmail" section)))

(defun openmail-start (&rest pairs) "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-openmail:start-cell (apply #'openmail-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-openmail-key (&body body) "BODY with the key environment answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "OPENMAIL_KEY") "test-key"))
     ,@body))

(defmacro with-openmail-wire ((asked &rest routes) &body body) "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order, so a longer path goes first; FORM runs with METHOD, URL and OPTIONS
bound and answers (values TEXT STATUS), as NLK:HTTP does. ASKED collects
(METHOD URL CONTENT HEADERS), oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url)
                                                           ,form))
                                         (t (error "unexpected OpenMail request: ~a ~a" method url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +inbox+
  "{\"id\":\"inb_1\",\"address\":\"assistant@omail.sh\",\"podId\":\"pod_1\"}"
  "One inbox, as GET /v1/inboxes answers it.")

(defparameter +one-inbox+
  (concatenate 'string "{\"data\":[" +inbox+ "]}")
  "One inbox under a data key, as the list endpoint answers it.")

(defparameter +two-inboxes+
  (concatenate 'string "{\"data\":[" +inbox+ ","
               "{\"id\":\"inb_2\",\"address\":\"sales@omail.sh\",\"podId\":\"pod_1\"}]}")
  "Two inboxes under a data key, as the list endpoint may answer them.")

(defparameter +messages+
  (concatenate
   'string
   "{\"data\":[{\"id\":\"msg_1\",\"threadId\":\"thr_1\",\"direction\":\"inbound\","
   "\"fromAddr\":\"dana@example.com\",\"inboxId\":\"inb_1\",\"subject\":\"Invoice 42\","
   "\"bodyText\":\"Could you resend the invoice?\"},"
   "{\"id\":\"msg_2\",\"threadId\":\"thr_1\",\"direction\":\"outbound\","
   "\"fromAddr\":\"assistant@omail.sh\",\"inboxId\":\"inb_1\",\"subject\":\"Re: Invoice 42\","
   "\"bodyText\":\"Sending it again.\"}]}")
  "A thread with one inbound and one outbound message.")

(defparameter +sent+
  "{\"id\":\"msg_9\",\"threadId\":\"thr_1\",\"direction\":\"outbound\"}"
  "What a send answers.")

(defun om-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun om-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (om-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "openmail"
  (:config "api_key_env" "OPENMAIL_KEY")
  (:help :openmail)
  (:fixture with-openmail-key)
  (:refused ("base_url" 5) ("timeout" 0) ("api_key_env" "")))

(deftest openmail-cell-without-a-key-refuses ()
  ;; The one setting no verb can work without: the section names a key, and
  ;; a section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (openmail-start "api_key_env" ""))))
    (is (search "no key" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "openmail_whoami")) "and installs nothing")))

;;; --- openmail_whoami / openmail_list_inboxes ------------------------------------------

(deftest openmail-cell-whoami-lists-inboxes (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/inboxes" (values +one-inbox+ 200)))
      (let ((object (om-object "om-who-1" "openmail_whoami" "{}")))
        (is (equal "inb_1" (nlk:json-value object :string "default_inbox_id")) "the section's inbox is the default")
        (is (equal "assistant@omail.sh"
                   (nlk:json-value (aref (nlk:json-array object "inboxes") 0) :string "address")) "the address is the service's own")
        (is (= 1 (length asked)) "whoami is one REST call")))))

(deftest openmail-cell-list-inboxes-answers-every-inbox (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/inboxes" (values +two-inboxes+ 200)))
      (is (= 2 (length (om-object "om-inb-1" "openmail_list_inboxes" "{}"))) "both inboxes come through, under the data key the service may use"))))

;;; --- openmail_send ---------------------------------------------------------------------

(deftest openmail-cell-send-posts-a-thread (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/inboxes/inb_1/send" (values +sent+ 200)))
      (let ((object (om-object "om-send-1" "openmail_send"
                                "{\"to\":\"dana@example.com\",\"subject\":\"Invoice 42\",\"body\":\"Here it is.\"}")))
        (is (equal "thr_1" (nlk:json-value object :string "threadId")) "the answer carries the thread id"))
      (let* ((call (first asked))
             (payload (nlk:decode-json (third call)))
             (headers (fourth call)))
        (is (equal :post (first call)))
        (is (equal "https://api.openmail.sh/v1/inboxes/inb_1/send" (second call)) "the default base URL and the section's inbox")
        (is (equal "dana@example.com" (nlk:json-value payload :string "to")))
        (is (equal "Here it is." (nlk:json-value payload :string "body")))
        (is (equal "Bearer test-key" (cdr (assoc "authorization" headers :test #'equal))) "the key rides the request, never the model's context")))))

(deftest openmail-cell-send-needs-a-body (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked)
      (let ((answer (om-answer "om-send-2" "openmail_send"
                                "{\"to\":\"dana@example.com\",\"subject\":\"Hi\"}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (search "body" answer))
        (is (null asked) "so nothing is sent")))))

(deftest openmail-cell-send-copies-cc (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/inboxes/inb_1/send" (values +sent+ 200)))
      (om-answer "om-send-3" "openmail_send"
                  "{\"to\":\"dana@example.com\",\"subject\":\"Hi\",\"body\":\"Hello\",\"cc\":[\"ops@example.com\"]}")
      (let ((payload (nlk:decode-json (third (first asked)))))
        (is (equal "ops@example.com"
                   (nlk:json-value (aref (nlk:json-array payload "cc") 0) :string)) "the copied recipient rides the payload")))))

;;; --- openmail_reply ----------------------------------------------------------------------

(deftest openmail-cell-reply-fills-the-sender-in (with-openmail-key)
  ;; The original's reply: with no `to`, answer the last inbound sender, from
  ;; the inbox that received it -- two calls, the read then the send.
  (with-cell-stop ((openmail-start "inbox_id" ""))
    (with-openmail-wire (asked
                         ("/v1/threads/thr_1/messages" (values +messages+ 200))
                         ("/v1/inboxes/inb_1/send" (values +sent+ 200)))
      (let ((answer (om-object "om-reply-1" "openmail_reply"
                                "{\"thread_id\":\"thr_1\",\"body\":\"Sending it again.\"}")))
        (is (equal "thr_1" (nlk:json-value answer :string "threadId"))))
      (let ((payload (nlk:decode-json (third (second asked)))))
        (is (equal "dana@example.com" (nlk:json-value payload :string "to")) "the recipient is the last inbound sender")
        (is (equal "thr_1" (nlk:json-value payload :string "threadId")) "it stays in the thread")
        (is (search "/v1/inboxes/inb_1/send" (second (second asked))) "and is sent from the inbox that received it")))))

(deftest openmail-cell-reply-quotes-unless-asked-not-to (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/inboxes/inb_1/send" (values +sent+ 200)))
      (om-answer "om-reply-2" "openmail_reply"
                  "{\"thread_id\":\"thr_1\",\"body\":\"Thanks\",\"to\":\"dana@example.com\",\"quote\":false}")
      (let ((payload (nlk:decode-json (third (first asked)))))
        (is (eq nil (nlk:json-value payload :boolean "includeQuote")) "quote:false rides the payload as a JSON false")
        (is (null (nlk:json-value payload :string "subject")) "and the subject is the thread's, not a new one")))))

;;; --- openmail_list_threads / openmail_read_thread --------------------------------------------

(deftest openmail-cell-unread-threads-ask-for-unread (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/inboxes/inb_1/threads"
                                 (values "{\"data\":[]}" 200)))
      (om-answer "om-thr-1" "openmail_list_threads" "{\"read_state\":\"unread\",\"limit\":5}")
      (let ((url (second (first asked))))
        (is (search "is_read=false" url) "unread is the service's is_read=false")
        (is (search "limit=5" url) "and the caller's cap rides the query")))))

(deftest openmail-cell-reading-a-thread-marks-it-read (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked
                         ("/v1/threads/thr_1/messages" (values +messages+ 200))
                         ("/v1/threads/thr_1" (values "{\"id\":\"thr_1\",\"isRead\":true}" 200)))
      (let ((object (om-object "om-read-1" "openmail_read_thread" "{\"thread_id\":\"thr_1\"}")))
        (is (= 2 (length (nlk:json-array object "messages"))) "both messages come through")
        (is (equal "Could you resend the invoice?"
                   (nlk:json-value (aref (nlk:json-array object "messages") 0)
                                   :string "bodyText")) "oldest first, bodyText as the sender wrote it"))
      (is (= 2 (length asked)) "a read, then the mark")
      (is (equal :patch (first (second asked))) "marking read is the service's PATCH"))))

(deftest openmail-cell-reading-without-marking-skips-the-patch (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/threads/thr_1/messages" (values +messages+ 200)))
      (om-answer "om-read-2" "openmail_read_thread"
                  "{\"thread_id\":\"thr_1\",\"mark_read\":false}")
      (is (= 1 (length asked)) "mark_read:false leaves the thread as it was"))))

;;; --- refusals from the service -------------------------------------------------------------

(deftest openmail-cell-carries-the-services-own-words (with-openmail-key)
  (with-cell-stop ((openmail-start))
    (with-openmail-wire (asked ("/v1/threads/thr_404/messages"
                                 (values "{\"message\":\"thread not found\",\"code\":\"not_found\"}" 404)))
      (let ((answer (om-answer "om-read-3" "openmail_read_thread" "{\"thread_id\":\"thr_404\"}")))
        (is (search "HTTP 404" answer) "the status is named")
        (is (search "thread not found" answer) "and the service's own message with it")))))

(deftest openmail-cell-refuses-when-the-key-sees-several-inboxes (with-openmail-key)
  (with-cell-stop ((openmail-start "inbox_id" ""))
    (with-openmail-wire (asked ("/v1/inboxes" (values +two-inboxes+ 200)))
      (let ((answer (om-answer "om-send-4" "openmail_send"
                                "{\"to\":\"dana@example.com\",\"subject\":\"Hi\",\"body\":\"Hello\"}")))
        (is (search "inbox_id is required" answer) "the call says which setting it needs")
        (is (search "openmail_list_inboxes" answer) "and how to find one")
        (is (= 1 (length asked)) "nothing is sent on a guess")))))
