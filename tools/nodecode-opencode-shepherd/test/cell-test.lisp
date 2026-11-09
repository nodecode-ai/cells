;;;; cell-test.lisp --- the shepherd against a stubbed opencode server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange is a stubbed NLK:HTTP answering the shapes an opencode v2
;;;; server answers, so nothing here reaches a server or a network. Each tool is
;;;; called the way a model's call reaches it, through the real dispatch,
;;;; contract and all, so the schema the provider is shown and the body that
;;;; runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "opencode-shepherd" "OPENCODE-SHEPHERD-" :recipe "opencode-shepherd"
  :start shepherd:start-cell
  :defaults (("url" "http://127.0.0.1:4096")
             ("poll_seconds" 0)))

;;; --- fixtures ----------------------------------------------------------------------

(defparameter +running-session+
  (concatenate 'string
               "{\"id\":\"ses_run1\",\"title\":\"Fix the CORS bug\","
               "\"location\":{\"directory\":\"/work/api\"},"
               "\"time\":{\"updated\":~a}}"))

(defun milliseconds-ago (seconds)
  "The epoch, in the milliseconds an opencode timestamp counts, SECONDS ago."
  (- (* 1000 (get-universal-time)) (* 1000 seconds)))

(defun sessions-json (&rest rows)
  "An /session answer carrying ROWS, each a JSON object text."
  (format nil "{\"data\":[~{~a~^,~}]}" rows))

(defun session-row (id title updated &key parent outcome)
  "One session object, as the server describes it."
  (format nil "{\"id\":~s,\"title\":~s,\"time\":{\"updated\":~a}~@[,\"parentID\":~s~]~@[,\"outcome\":~s~]}"
          id title updated (and parent parent) (and outcome outcome)))

(defun messages-json (&rest rows)
  (format nil "{\"data\":[~{~a~^,~}]}" rows))

(defun text-message (type text &key (seconds 30) outcome)
  "One message: what the agent said, or a turn that ended."
  (format nil "{\"type\":~s,\"time\":{\"created\":~a}~@[,\"outcome\":~s~],\"content\":[{\"type\":\"text\",\"text\":~s}]}"
          type (milliseconds-ago seconds) (and outcome outcome) text))

(defun idle-message (outcome seconds)
  "A turn that ended: the idle message opencode writes last."
  (format nil "{\"type\":\"idle\",\"outcome\":~s,\"time\":{\"created\":~a}}"
          outcome (milliseconds-ago seconds)))

(defparameter +question+
  (concatenate 'string
               "{\"id\":\"frm_7\",\"title\":\"Pick an origin\",\"fields\":["
               "{\"key\":\"q0\",\"type\":\"select\",\"title\":\"Which origin?\","
               "\"options\":[{\"value\":\"a\",\"label\":\"https://a.test\"},"
               "{\"value\":\"b\",\"label\":\"https://b.test\"}],\"default\":\"a\"},"
               "{\"key\":\"q1\",\"type\":\"string\",\"title\":\"Why?\"}]}"))

(defparameter +permission+
  "{\"id\":\"per_9\",\"action\":\"bash\",\"resources\":[\"git push origin main\"],\"message\":\"publish the fix\"}")

(defmacro with-opencode ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering the opencode API by path. ROUTES are
(FRAGMENT FORM), tried in order; FORM runs with METHOD, URL, OPTIONS bound and
answers (values TEXT STATUS), as NLK:HTTP does. ASKED collects (METHOD URL
CONTENT HEADERS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                   (push (list method url content headers) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (values "{\"error\":\"no such route\"}" 404))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun opencode-shepherd-standing ()
  "The text the shepherd's notice stands under, or NIL."
  (second (assoc "nodecode-opencode-shepherd" (nlk:standing-notices) :test #'string=)))

(defun asked-for (asked &rest fragments)
  "The first logged request whose URL carries every FRAGMENT."
  (find-if (lambda (request)
              (every (lambda (fragment) (search fragment (second request))) fragments))
            asked))

;;; --- the lifecycle, from the one declaration ------------------------------------------

(define-cell-lifecycle-tests "opencode-shepherd"
  (:config)
  (:help :opencode)
  (:command "opencode")
  (:refused ("url" nil) ("timeout_seconds" 0))
  (:idle nodecode-opencode-shepherd:opencode-shepherd-error
         (shepherd:list-sessions nil)
         (shepherd:waiting-report)))

(deftest opencode-shepherd-cell-refuses-a-section-with-no-address ()
  ;; The one setting no verb can work without: a section naming none is
  ;; refused at start, before anything installs, and the refusal names it.
  (let ((condition (signals-error nlk:config-refusal (opencode-shepherd-start "url" nil))))
    (is (search "url" (princ-to-string condition)) "the refusal names the member")
    (is (null (nle::find-tool "opencode_waiting")) "and installs no tool")))

;;; --- what waits ------------------------------------------------------------------------

(deftest opencode-shepherd-cell-says-what-waits ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/session/ses_run1/form" (values (format nil "{\"data\":[~a]}" +question+) 200))
                     ("/session/ses_run1/permission" (values (format nil "{\"data\":[~a]}" +permission+) 200))
                     ("/session/ses_run1/message" (values (messages-json) 200))
                     ("/session" (values (sessions-json
                                           (session-row "ses_run1" "Fix the CORS bug"
                                                        (milliseconds-ago 120)))
                                          200)))
      (let ((text (answer "shepherd-1" "opencode_waiting" "{}")))
        (is (search "2 waiting in opencode" text) "one question and one permission")
        (is (search "Fix the CORS bug" text))
        (is (search "q0" text) "the question's field key, so a reply needs no guessing")
        (is (search "https://a.test" text) "and its options")
        (is (search "git push origin main" text) "the permission's resource")
        (is (search "opencode_permission session=\"ses_run1\"" text) "and the call that answers it")
        (is (search "opencode_form session=\"ses_run1\"" text)))
      (is (asked-for asked "/api/session/active") "the server's own API prefix rides the URL"))))

(deftest opencode-shepherd-cell-reports-a-finished-turn-but-not-a-fresh-one ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/form" (values "{\"data\":[]}" 200))
                     ("/permission" (values "{\"data\":[]}" 200))
                     ("/message" (values (messages-json (idle-message "succeeded" 600)
                                                         (text-message "assistant" "done; run the tests next?"))
                                          200))
                     ("/session" (values (sessions-json
                                           (session-row "ses_done" "Docs sweep" (milliseconds-ago 600)))
                                          200)))
      (let ((text (answer "shepherd-2" "opencode_waiting" "{}")))
        (is (search "Docs sweep" text))
        (is (search "finished and waits for input" text))
        (is (search "run the tests next?" text) "with the last thing it said"))
      (let ((fresh (answer "shepherd-3" "opencode_tail"
                           "{\"session\":\"ses_done\",\"count\":5}")))
        (is (search "done; run the tests next?" fresh) "the tail shows the turn's last words")
        (is (search "turn ended" fresh) "and how the turn ended")))))

(deftest opencode-shepherd-cell-leaves-a-background-shell-alone ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/form" (values "{\"data\":[]}" 200))
                     ("/permission" (values "{\"data\":[]}" 200))
                     ("/shell/shl_1" (values "{\"data\":{\"status\":\"running\"}}" 200))
                     ("/message" (values (messages-json
                                           (idle-message "succeeded" 600)
                                           (format nil "{\"type\":\"assistant\",\"time\":{\"created\":~a},\"metadata\":{\"shellID\":\"shl_1\"},\"content\":[{\"type\":\"text\",\"text\":\"running the suite\"}]}"
                                                   (milliseconds-ago 700)))
                                          200))
                     ("/session" (values (sessions-json
                                           (session-row "ses_shell" "Nightly" (milliseconds-ago 600)))
                                          200)))
      (let ((text (answer "shepherd-4" "opencode_waiting" "{}")))
        (is (search "nothing in opencode waits" text) "a running shell will resume the turn"))
      (is (asked-for asked "/api/shell/shl_1") "the shell is asked about by id"))))

;;; --- reading and steering ----------------------------------------------------------

(deftest opencode-shepherd-cell-lists-and-tails ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{\"ses_run1\":{\"id\":\"ses_run1\"}}}" 200))
                     ("/form" (values "{\"data\":[]}" 200))
                     ("/permission" (values "{\"data\":[]}" 200))
                     ("/message" (values (messages-json (text-message "assistant" "writing the fix")
                                                         (text-message "user" "fix the CORS bug"))
                                          200))
                     ("/session" (values (sessions-json
                                           (session-row "ses_run1" "Fix the CORS bug"
                                                        (milliseconds-ago 60)))
                                          200)))
      (let ((text (answer "shepherd-5" "opencode_sessions" "{\"limit\":5}")))
        (is (search "1 opencode session" text) "the count says itself")
        (is (search "RUNNING" text) "a turn that runs is marked"))
      (let ((text (answer "shepherd-6" "opencode_tail" "{\"session\":\"CORS\",\"count\":4}")))
        (is (search "fix the CORS bug" text) "a fragment of the title resolves the session")
        (is (search "writing the fix" text) "and the messages read oldest first")
        (is (< (search "fix the CORS bug" text) (search "writing the fix" text))))
      (let ((text (answer "shepherd-7" "opencode_tail" "{\"session\":\"nothing-like-this\"}")))
        (is (search "no opencode session matches" text) "a name that matches nothing says so")))))

(deftest opencode-shepherd-cell-steers-a-session ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/prompt" (values "{\"data\":{\"id\":\"msg_1\",\"delivery\":\"queue\"}}" 200))
                     ("/permission/per_9/reply" (values "{\"data\":true}" 200))
                     ("/form/frm_7/reply" (values "{\"data\":true}" 200))
                     ("/interrupt" (values "" 200))
                     ("/form" (values (format nil "{\"data\":[~a]}" +question+) 200))
                     ("/permission" (values (format nil "{\"data\":[~a]}" +permission+) 200))
                     ("/session" (values (sessions-json
                                           (session-row "ses_run1" "Fix the CORS bug"
                                                        (milliseconds-ago 60)))
                                          200)))
      (let ((text (answer "shepherd-8" "opencode_send"
                          "{\"session\":\"ses_run1\",\"text\":\"use https://a.test\",\"queue\":true}")))
        (is (search "sent to ses_run1" text))
        (let ((sent (asked-for asked "/prompt")))
          (is (search "use https://a.test" (third sent)) "the message is the body")
          (is (search "queue" (third sent)) "and the delivery rides with it")))
      (let ((text (answer "shepherd-9" "opencode_permission"
                          "{\"session\":\"ses_run1\",\"decision\":\"once\"}")))
        (is (search "bash" text) "the request answered is named")
        (let ((reply (asked-for asked "/permission/per_9/reply")))
          (is (search "\"decision\":\"once\"" (third reply)) "the decision is what the server reads")))
      (let ((text (answer "shepherd-10" "opencode_form"
                          "{\"session\":\"ses_run1\",\"answers\":{\"q0\":\"b\"}}")))
        (is (search "answered the question" text))
        (is (search "\"q0\":\"b\"" (third (asked-for asked "/form/frm_7/reply")))))
      (is (search "interrupted ses_run1"
                  (answer "shepherd-11" "opencode_interrupt" "{\"session\":\"ses_run1\"}"))))))

(deftest opencode-shepherd-cell-answers-before-a-request-waits ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/form" (values "{\"data\":[]}" 200))
                     ("/permission" (values "{\"data\":[]}" 200))
                     ("/session" (values (sessions-json
                                           (session-row "ses_run1" "Fix the CORS bug"
                                                        (milliseconds-ago 60)))
                                          200)))
      (is (search "no permission request waits"
                  (answer "shepherd-12" "opencode_permission"
                          "{\"session\":\"ses_run1\",\"decision\":\"once\"}")))
      (is (null (asked-for asked "/reply")) "and nothing is posted to a request that is not there"))))

(deftest opencode-shepherd-cell-refuses-a-server-that-says-no ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session" (values "{\"error\":\"unauthorized\"}" 401)))
      (let ((text (answer "shepherd-13" "opencode_sessions" "{}")))
        (is (search "HTTP 401" text) "the status is said")
        (is (search "unauthorized" text) "and the server's own words")))))

(deftest opencode-shepherd-cell-signs-with-its-password ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/session" (values (sessions-json) 200)))
      (with-stubbed-fdefinition (uiop:getenv (name)
                                  (and (equal name "OPENCODE_SERVER_PASSWORD") "hunter2"))
        (answer "shepherd-14" "opencode_sessions" "{}")
        (let* ((request (asked-for asked "/api/session"))
               (headers (fourth request))
               (authorization (cdr (assoc "authorization" headers :test #'equal))))
          (is (string= "Basic b3BlbmNvZGU6aHVudGVyMg==" (or authorization "")) "basic auth"))))))

;;; --- the watch, and the command ----------------------------------------------------

(deftest opencode-shepherd-cell-watch-says-each-thing-once ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/form" (values (format nil "{\"data\":[~a]}" +question+) 200))
                     ("/permission" (values "{\"data\":[]}" 200))
                     ("/message" (values (messages-json) 200))
                     ("/session" (values (sessions-json
                                           (session-row "ses_run1" "Fix the CORS bug"
                                                        (milliseconds-ago 120)))
                                          200)))
      (let ((nodecode-opencode-shepherd::*watching* (nodecode-opencode-shepherd::make-watching)))
        (nodecode-opencode-shepherd::watch-step)
        (let ((said (opencode-shepherd-standing)))
          (is (search "Pick an origin" (or said "")) "a new question is said at once")
          (is (search "opencode_form session=\"ses_run1\"" (or said "")) "with the call that answers it")
          (nodecode-opencode-shepherd::watch-step)
          (is (equal said (opencode-shepherd-standing)) "and the next poll leaves it as it is"))))))

(deftest opencode-shepherd-cell-command-answers-what-waits ()
  (with-cell-stop ((opencode-shepherd-start))
    (with-opencode (asked
                     ("/session/active" (values "{\"data\":{}}" 200))
                     ("/form" (values "{\"data\":[]}" 200))
                     ("/permission" (values "{\"data\":[]}" 200))
                     ("/message" (values (messages-json) 200))
                     ("/session" (values (sessions-json) 200)))
      (let ((text (nodecode-opencode-shepherd::run-opencode nil nil)))
        (is (search "nothing in opencode waits" text) "/opencode says silence plainly")))))
