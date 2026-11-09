;;;; cell-test.lisp --- lifecycle, the three tools, the verbs, /fmsg.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer field under its floor refused, a start with no key
;;;; refused), each tool through the seam the model calls it by -- its url,
;;;; its Bearer header, its body and its answer -- the verbs, the key
;;;; exchange and its 401 retry, and /fmsg with and without an id. Every
;;;; request is a stubbed NLK:HTTP, so nothing reaches fmsg or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defparameter +jwt+
  "header.eyJzdWIiOiJAYWdlbnRAZXhhbXBsZS5jb20ifQ.sig"
  "A token whose payload names the address @agent@example.com, as an fmsg host grants one.")

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-fmsg-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials fmsg."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected fmsg request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun fmsg-answer (&rest pairs)
  "The wire's answer for an object body built from PAIRS, HTTP 200."
  (values (nlk:encode-json-object (apply #'nlk:make-json-object pairs)) 200))

(defun fmsg-answer-list (&rest messages)
  "The wire's answer for MESSAGES as a JSON array, HTTP 200."
  (values (nlk:encode-json-object (coerce messages 'vector)) 200))

(defun fmsg-token-answer ()
  "The key exchange's answer: a token good until long after the test."
  (fmsg-answer "access_token" +jwt+ "expires_at" "2999-01-01T00:00:00Z"))

(defmacro with-fmsg-answers (routes &body body)
  "BODY with the stubbed wire answering ROUTES, (FRAGMENT FORM): the first
whose FRAGMENT the url holds answers FORM's (values TEXT STATUS); the key
exchange is answered first, so a route never swallows it; a url no route
names is an error."
  `(with-fmsg-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore method args))
                           (cond ((search "/fmsg/token" url) (fmsg-token-answer))
                                 ,@(loop for (fragment form) in routes
                                         collect `((search ,fragment url) ,form))
                                 (t (error "unexpected request to ~a" url))))))
       (with-cell-stop ((fmsg-start "api_key" "fmsgk_key_secret"))
         ,@body))))

(defun fmsg-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun fmsg-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun fmsg-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun fmsg-token-requests ()
  "The stubbed key exchanges."
  (remove-if-not (lambda (request) (search "/fmsg/token" (second request))) (fmsg-asked)))

(defun fmsg-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun fmsg-args (&rest pairs)
  "A tool call's argument object from PAIRS, as dispatch decodes one."
  (apply #'nlk:make-json-object pairs))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "fmsg" "FMSG-CELL-" :start nodecode-fmsg:start-cell)

(define-cell-lifecycle-tests "fmsg"
  (:fixture with-fmsg-wire)
  (:config "api_key" "fmsgk_key_secret")
  (:help :fmsg)
  (:command "fmsg")
  (:running (is (nle::find-tool "fmsg_send") "fmsg_send is registered")
            (is (nle::find-tool "fmsg_inbox"))
            (is (nle::find-tool "fmsg_thread"))
            (is (equal "https://api.fmsg.io" (nodecode-fmsg::root)))
            (is (= 10 (nodecode-fmsg::setting :inbox-limit)) "ten messages by default")
            (is (equal "Nodecode" (nodecode-fmsg::setting :default-topic))))
  (:stopped (is (null (nle::find-tool "fmsg_send")) "stop unregisters the tools")
            (is (null nodecode-fmsg::*token*) "and forgets the token"))
  (:refused ("inbox_limit" 0) ("timeout_seconds" 0))
  (:idle nodecode-fmsg:fmsg-error (fmsg:inbox)))

(deftest fmsg-cell-a-key-is-required ()
  (with-fmsg-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignorable name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (fmsg-start)))
                   "fmsg: no key"
                   "FMSG_API_KEY")
      (is (null (nle::find-tool "fmsg_send")) "a refused start installed nothing"))))

;;; --- the key, exchanged for a token --------------------------------------------

(deftest fmsg-cell-reads-its-address-from-the-token ()
  (is (equal "@agent@example.com" (nodecode-fmsg::jwt-address +jwt+))
      "the sub claim is the granted address")
  (is (null (nodecode-fmsg::jwt-address "not-a-jwt")))
  (is (equal "hi" (nlk:decode-octets (nodecode-fmsg::base64url-octets "aGk")))
      "base64url decodes without padding"))

(deftest fmsg-cell-exchanges-the-key-once-and-caches-it ()
  (with-fmsg-answers (("/fmsg?" (fmsg-answer-list)))
    (fmsg-tool "fmsg_inbox" (fmsg-args))
    (fmsg-tool "fmsg_inbox" (fmsg-args))
    (is (= 1 (length (fmsg-token-requests))) "one key exchange for two calls")
    (let ((exchange (first (fmsg-token-requests))))
      (is (equal "Bearer fmsgk_key_secret" (fmsg-request-header exchange "authorization"))
          "the key rides the exchange as a Bearer")
      (is (search "/fmsg/token" (second exchange))))))

(deftest fmsg-cell-exchanges-again-when-a-call-is-refused ()
  (with-fmsg-wire
    (let ((calls 0))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method args))
              (cond ((search "/fmsg/token" url) (fmsg-token-answer))
                    ((= (incf calls) 1)
                     (values (nlk:encode-json-object
                              (nlk:make-json-object "message" "token expired")) 401))
                    (t (fmsg-answer-list)))))
      (with-cell-stop ((fmsg-start "api_key" "fmsgk_key_secret"))
        (is-carrying (answer (fmsg-tool "fmsg_inbox" (fmsg-args))) "No inbox messages")
        (is (= 2 (length (fmsg-token-requests))) "one 401 re-exchanges the key once")))))

;;; --- fmsg_inbox ----------------------------------------------------------------

(deftest fmsg-cell-inbox-lists-the-newest-messages ()
  (with-fmsg-answers (("/fmsg?" (fmsg-answer-list
                                  (nlk:make-json-object "id" 42
                                                         "from" "@alice@example.com"
                                                         "topic" "deploy"
                                                         "short_text" "is it up?")
                                  (nlk:make-json-object "id" 41
                                                         "from" "@bob@example.com"
                                                         "short_text" "hello"
                                                         "read" t))))
    (is-carrying (answer (fmsg-tool "fmsg_inbox" (fmsg-args "limit" 2)))
                 "2 inbox messages"
                 "#42 @alice@example.com"
                 "topic=\"deploy\""
                 "[unread]"
                 "#41 @bob@example.com")
    (let ((request (second (fmsg-asked))))
      (is (search "limit=2" (second request)) "the limit rides the query")
      (is (search "offset=0" (second request)))
      (is (search "eyJzdWIi" (fmsg-request-header request "authorization"))
          "the call rides the token, not the key"))))

(deftest fmsg-cell-inbox-reads-sent-when-asked ()
  (with-fmsg-answers (("/fmsg/sent" (fmsg-answer-list)))
    (is-carrying (answer (fmsg-tool "fmsg_inbox" (fmsg-args "sent" t))) "No sent messages")
    (is (search "/fmsg/sent" (second (second (fmsg-asked)))) "the sent route")))

;;; --- fmsg_thread ---------------------------------------------------------------

(deftest fmsg-cell-thread-walks-the-ancestry-to-its-root ()
  (with-fmsg-answers (("/fmsg/42" (fmsg-answer "id" 42 "from" "@bob@example.com"
                                                 "pid" 41 "short_text" "and the third"))
                      ("/fmsg/41" (fmsg-answer "id" 41 "from" "@alice@example.com"
                                                "pid" 40 "short_text" "the second"))
                      ("/fmsg/40" (fmsg-answer "id" 40 "from" "@alice@example.com"
                                                "topic" "deploy" "short_text" "the first")))
    (let ((text (fmsg-tool "fmsg_thread" (fmsg-args "id" 42))))
      (is-carrying (answer text)
                   "thread of #40"
                   "topic=\"deploy\""
                   "3 messages, root first"
                   "re #41"
                   "re #40")
      (is (< (search "#40" text) (search "#42" text)) "the root is read first")
      (is (= 4 (length (fmsg-asked))) "one exchange and three reads, and no more"))))

;;; --- fmsg_send -----------------------------------------------------------------

(deftest fmsg-cell-send-starts-a-thread ()
  (with-fmsg-answers (("/fmsg/7/send" (fmsg-answer "id" 7))
                      ("/fmsg" (fmsg-answer "id" 7)))
    (let ((text (fmsg-tool "fmsg_send"
                            (fmsg-args "text" "the build is green"
                                       "to" "@alice@example.com, @bob@example.com"))))
      (is (search "sent #7 to @alice@example.com, @bob@example.com" text)
          "the answer names the id and every recipient")
      (is (search "as a new thread, topic \"Nodecode\"" text) "as a new thread, with the section's default topic"))
    (let* ((requests (fmsg-asked))
           (draft (second requests))
           (body (fmsg-request-body draft)))
      (is (equal "https://api.fmsg.io/fmsg" (second draft)) "the draft is one POST to /fmsg")
      (is (equal "Bearer " (subseq (fmsg-request-header draft "authorization") 0 7))
          "and rides the token")
      (is (equal "@agent@example.com" (gethash "from" body)) "it writes from its own address")
      (is (equal '("@alice@example.com" "@bob@example.com") (coerce (gethash "to" body) 'list)))
      (is (equal "Nodecode" (gethash "topic" body)) "the section's default topic")
      (is (equal "the build is green" (gethash "data" body)))
      (is (equal (length "the build is green") (gethash "size" body)) "the body size is sent")
      (is (null (gethash "pid" body)) "a root, so no parent")
      (is (search "/fmsg/7/send" (second (third requests))) "the draft is then sent"))))

(deftest fmsg-cell-a-reply-writes-the-parents-participants ()
  (with-fmsg-answers (("/fmsg/9/send" (fmsg-answer "id" 11))
                      ("/fmsg/9" (fmsg-answer "id" 9 "from" "@alice@example.com"
                                               "to" (vector "@agent@example.com"
                                                             "@carol@example.com")
                                               "topic" "deploy"))
                      ("/fmsg" (fmsg-answer "id" 11)))
    (is-carrying (answer (fmsg-tool "fmsg_send" (fmsg-args "text" "and the tests" "pid" 9)))
                 "sent #11 to @alice@example.com, @carol@example.com"
                 "in reply to #9")
    (let* ((draft (third (fmsg-asked)))
           (body (fmsg-request-body draft)))
      (is (= 9 (gethash "pid" body)) "the reply names its parent")
      (is (null (gethash "topic" body)) "and carries no topic of its own")
      (is (equal '("@alice@example.com" "@carol@example.com") (coerce (gethash "to" body) 'list))
          "every participant of the parent but this address"))))

;;; --- /fmsg --------------------------------------------------------------------

(deftest fmsg-cell-the-command-answers-the-address-and-the-inbox ()
  (with-fmsg-answers (("/fmsg?" (fmsg-answer-list)))
    (is-carrying (answer (nodecode-fmsg::run-command "" "s1"))
                 "writing as @agent@example.com"
                 "https://api.fmsg.io"
                 "No inbox messages")))

(deftest fmsg-cell-the-command-reads-one-thread ()
  (with-fmsg-answers (("/fmsg/40/data" (values "the first body" 200))
                      ("/fmsg/40" (fmsg-answer "id" 40 "from" "@alice@example.com"
                                                "to" (vector "@agent@example.com")
                                                "topic" "deploy" "size" 14)))
    (is-carrying (answer (nodecode-fmsg::run-command "40" "s1"))
                 "#40 @alice@example.com"
                 "topic=\"deploy\""
                 "the first body"
                 "thread of #40")))
