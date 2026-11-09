;;;; cell-test.lisp --- one skill request, down the same path a voice takes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials out and nothing here speaks: a request is a body and
;;;; a path handed to the route's own function, the injection is a stub that
;;;; records what the kernel would have been asked to run, and every answer
;;;; is read back off the response the gateway would write.

(in-package #:nodecode.test)

(define-test-slice "alice-voice" "ALICE-VOICE-CELL-" :recipe "alice-voice"
  :start nodecode-alice-voice:start-cell
  :defaults (("secret" "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
             ("skill_id" "skill-test")
             ("session" "alice-test")
             ("owner_user_id" "")))

;;; --- the wire -------------------------------------------------------------------

(defvar *injected* '()
  "What the stub injection was asked to put where, newest first.")

(defun alice-stub (session text command-id)
  "The injection a test runs on: records the call, claims the kernel took it."
  (push (list session text command-id) *injected*)
  t)

(defmacro with-alice (&body body)
  "BODY with the cell started on the test's defaults, an empty cache, every
injection recorded instead of run, and the cell stopped on unwind."
  `(with-cell-stop ((alice-voice-start))
     (nodecode-alice-voice:reset!)
     (let ((*injected* '())
           (nodecode-alice-voice:*inject* #'alice-stub))
       ,@body)))

(defun alice-env (body &key path (content-type "application/json") length)
  "A POST carrying BODY, as the server hands a route its clack environment."
  (let ((headers (make-hash-table :test #'equal)))
    (when content-type (setf (gethash "content-type" headers) content-type))
    (list :request-method :post
          :request-uri (or path (nodecode-alice-voice:route-path))
          :headers headers
          :content-length (or length (length body))
          :raw-body (make-string-input-stream body))))

(defun alice-post (body &rest keys)
  "The route's answer to BODY: (values STATUS ANSWER-TEXT ANSWER-OBJECT)."
  (destructuring-bind (status headers octets)
      (nodecode-alice-voice:route-answer (apply #'alice-env body keys))
    (declare (ignore headers))
    (let ((answer (nlk:decode-json octets)))
      (values status (nodecode-alice-voice:response-text answer) answer))))

(defun alice-body (&key (skill "skill-test") (account "alice-1") (session "s-alice")
                      (message 1) (command "") (utterance ""))
  "One skill request, as the private skill posts it."
  (nlk:encode-json-object
   (nlk:json-object
    "version" "1.0"
    "session" (nlk:json-object "skill_id" skill
                                "user" (nlk:json-object "user_id" account)
                                "session_id" session
                                "message_id" message
                                "new" nil)
    "request" (nlk:json-object "type" "SimpleUtterance"
                                "command" (if (plusp (length command)) command utterance)
                                "original_utterance" utterance))))

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "alice-voice"
  (:command "alice")
  (:help :alice-voice)
  (:refused ("secret" "")
            ("secret" "not-64-hex-0123456789abcdef0123456789abcdef0123456789abcdef012")
            ("skill_id" "")))

(deftest alice-voice-cell-answers-nothing-while-stopped ()
  ;; A route is on the gateway only while the cell runs, and a request that
  ;; still reaches its function answers the skill's own "not sent".
  (nodecode-alice-voice:reset!)
  (multiple-value-bind (status text) (alice-post (alice-body :utterance "привет")
                                              :path "/alice/0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    (is (= 503 status) "a stopped bridge schedules nothing")
    (is (search "не отправлен" text) text))
  (with-alice
    (multiple-value-bind (status text) (alice-post (alice-body :utterance "привет"))
      (is (= 200 status) "a started one does")
      (is (search "принят" text) text)))
  (multiple-value-bind (status)
      (alice-post (alice-body :utterance "привет")
                  :path "/alice/0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    (is (= 503 status) "and a stopped cell schedules nothing again")))

;;; --- one utterance: ack, one turn --------------------------------------------------

(deftest alice-voice-cell-turns-one-utterance-into-one-turn ()
  (with-alice
    (multiple-value-bind (status text)
        (alice-post (alice-body :utterance "какая погода в Осло" :command "погода в осло"))
      (is (= 200 status) "the skill's request is accepted")
      (is (search "принят для обработки" text) "and answered with the original's ack")
      (is (= 1 (length *injected*)) "one turn was scheduled")
      (is (equal '("alice-test" "какая погода в Осло" "alice-skill-test-alice-1-s-alice-1") (first *injected*)) "into the configured session, the spoken words, under a command id of the message")
      (is (string= "alice-skill-test-alice-1-s-alice-1" (third (first *injected*))) "the id names the skill, the account and the message, so the kernel's deduplication agrees"))))

(deftest alice-voice-cell-remembers-a-message-id ()
  (with-alice
    (let ((body (alice-body :utterance "напомни позвонить" :message 7)))
      (multiple-value-bind (first-status first-text) (alice-post body)
        (multiple-value-bind (second-status second-text) (alice-post body)
          (is (= 200 first-status))
          (is (= second-status first-status) "a retry is answered the same way")
          (is (equal second-text first-text) "with the same words")
          (is (= 1 (length *injected*)) "and scheduled only once"))))
    (alice-post (alice-body :utterance "напомни позвонить" :message 8))
    (is (= 2 (length *injected*)) "a new message id is a new turn")
    (let ((nodecode-alice-voice:*inject* (lambda (session text command-id)
                                            (declare (ignore session text command-id))
                                            nil)))
      (multiple-value-bind (status text) (alice-post (alice-body :utterance "ещё" :message 9))
        (is (= 503 status) "a turn the kernel would not take is not claimed")
        (is (search "Попробуйте позже" text) text))
      (is (= 2 (length nodecode-alice-voice:*cache*)) "and is not remembered, so a retry may land"))))

;;; --- the gates ---------------------------------------------------------------------

(deftest alice-voice-cell-keeps-the-secret-and-the-skill-gates ()
  (with-alice
    (multiple-value-bind (status) (alice-post (alice-body :utterance "привет")
                                              :path "/alice/ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
      (is (= 403 status) "a path that is not the secret is refused"))
    (multiple-value-bind (status) (alice-post (alice-body :utterance "привет" :skill "another-skill"))
      (is (= 403 status) "a request for another skill is refused"))
    (multiple-value-bind (status) (alice-post (alice-body :utterance "привет" :session "agent:main:telegram:dm:1"))
      (is (= 200 status) "the session in the payload only names the message, it does not route")
      (is (equal "alice-test" (first (first *injected*))) "the configured session is the destination"))
    (multiple-value-bind (status) (alice-post (alice-body :utterance "привет" :account "ол")
                                              :path "/alice/0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
      (is (= 200 status) "any account holding the path is accepted while owner_user_id is empty"))
    (is (= 2 (length *injected*)) "only the two requests it accepted were scheduled"))
  (with-alice
    ;; The one account the operator named, in a section that names one.
    (let ((nodecode-alice-voice:*inject* #'alice-stub))
      (with-cell-stop ((alice-voice-start "owner_user_id" "alice-1"))
        (nodecode-alice-voice:reset!)
        (multiple-value-bind (status) (alice-post (alice-body :utterance "привет" :account "alice-2"))
          (is (= 403 status) "an account the section does not name is refused"))
        (multiple-value-bind (status) (alice-post (alice-body :utterance "привет" :account "alice-1"))
          (is (= 200 status) "and the named one is accepted"))))))

(deftest alice-voice-cell-refuses-what-the-model-should-not-see ()
  (with-alice
    (multiple-value-bind (status text) (alice-post (alice-body :utterance "/cells" :command "/cells"))
      (is (= 403 status) "a slash command cannot be spoken")
      (is (search "отклонён" text) text))
    (multiple-value-bind (status) (alice-post (alice-body :utterance "две
строки"))
      (is (= 403 status) "a control character cannot be smuggled in the utterance"))
    (multiple-value-bind (status) (alice-post "{\"version\":1.0, unbalanced")
      (is (= 400 status) "a body that is not JSON is refused"))
    (multiple-value-bind (status) (alice-post (alice-body :utterance "привет") :content-type "text/plain")
      (is (= 415 status) "a body not declared JSON is not parsed"))
    (multiple-value-bind (status) (alice-post (alice-body :utterance "привет") :length 20000)
      (is (= 413 status) "and a body past the ceiling is refused before it is read"))
    (is (null *injected*) "none of them reached a session")))

;;; --- words the cell answers itself ----------------------------------------------------

(deftest alice-voice-cell-answers-the-control-words-itself ()
  (with-alice
    (multiple-value-bind (status text) (alice-post (alice-body :utterance "помощь" :command "помощь"))
      (is (= 200 status))
      (is (search "только в Nodecode" text) "help says where the controls are"))
    (multiple-value-bind (status text answer) (alice-post (alice-body :utterance "стоп" :command "стоп"))
      (is (= 200 status))
      (is (search "До встречи" text) "an exit is answered")
      (is (eq t (gethash "end_session" (gethash "response" answer))) "and ends the skill's session"))
    (multiple-value-bind (status text) (alice-post (alice-body :utterance "ответ готов?" :command "ответ готов?"))
      (is (= 200 status))
      (is (search "будет в Nodecode" text) "the reminder names where the answer is"))
    (multiple-value-bind (status text) (alice-post (alice-body :utterance "   " :command ""))
      (is (= 200 status))
      (is (search "Назовите задачу" text) "an empty utterance asks for one"))
    (is (null *injected*) "and not one of them became a turn")))

;;; --- the operator's /alice --------------------------------------------------------------

(deftest alice-voice-cell-slash-command-shows-the-endpoint-and-says ()
  (with-alice
    (let ((status (cell-entry "nodecode-alice-voice" "alice" "")))
      (is (search "/alice/0123456789abcdef" status) "the endpoint and its secret are shown")
      (is (search "alice-test" status) "with the session it feeds"))
    (let ((said (cell-entry "nodecode-alice-voice" "alice" "say какая погода")))
      (is (search "200" said) "one utterance goes down the same path")
      (is (search "принят" said) said)
      (is (equal '("alice-test" "какая погода") (list (first (first *injected*)) (second (first *injected*)))) "and it becomes a turn in the configured session"))
    (is (search "not a verb" (cell-entry "nodecode-alice-voice" "alice" "shout hi")) "an unknown verb is named")
    (is (search "say <what" (cell-entry "nodecode-alice-voice" "alice" "say")) "and say alone asks for the words")))
