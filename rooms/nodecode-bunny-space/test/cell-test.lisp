;;;; cell-test.lisp --- lifecycle, the lap, the boundary, the signature, /bunny.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the section (a healthy start, a member out of its
;;;; floor refused, a start with nobody allowed refused), a room message and a
;;;; task each becoming one turn whose answer goes back where it came from, the
;;;; boundary that wraps third-party content as data, the HMAC the relay's
;;;; signature is made of, a stranger getting nothing, a long answer split,
;;;; and /bunny with bunny_send. Every relay call is a stubbed NLK:HTTP and
;;;; every turn a stubbed ingress, so no test dials Bunny Space and no test
;;;; starts an agent.

(in-package #:nodecode.test)

;;; --- the wire, stubbed ----------------------------------------------------------

(defvar *calls* '()
  "Every stubbed relay call, (PATH METHOD BODY HEADERS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of PATH and METHOD =>
(values TEXT STATUS).")

(defparameter +bunny-base+ "https://bunny.test")

(defun path-of (url)
  "URL's path and query, with the test relay's root taken off."
  (if (and (>= (length url) (length +bunny-base+))
           (equal +bunny-base+ (subseq url 0 (length +bunny-base+))))
      (subseq url (length +bunny-base+))
      url))

(defun session-answer (&optional signature)
  "A BOT_PROVISION_V1 session body, with SIGNATURE (a plist) when one is given."
  (let ((body (nlk:json-object
               "contractVersion" "BOT_PROVISION_V1"
               "bot" (nlk:json-object "id" "bp1" "slug" "test-bot")
               "capabilities" (vector "ISOLATED_ASSISTANCE_V1")
               "pollIntervalMs" 20000)))
    (when signature
      (setf (gethash "signature" body)
            (apply #'nlk:make-json-object signature)))
    body))

(defun default-answer (path method)
  "What the stubbed relay says when a test does not say otherwise."
  (declare (ignore method))
  (cond ((search "/api/bot/v1/session" path)
         (values (nlk:encode-json-object (session-answer)) 200))
        ((search "/rooms/messages" path) (values "{\"messages\":[]}" 200))
        ((search "/tasks" path) (values "{\"tasks\":[]}" 200))
        (t (values "{\"ok\":true}" 200))))

(defmacro with-bunny-wire (&body body)
  "BODY with the cell's one network call stubbed: each call is logged in
*CALLS* and answered by *ANSWER-FN*, so nothing reaches the relay."
  `(let ((*calls* '()) (*answer-fn* nil))
     (with-stubbed-fdefinition (nodecode-bunny-space::http (method url &rest args)
                                (let ((content (getf args :content))
                                      (path (path-of url)))
                                  (push (list path method content (getf args :headers)) *calls*)
                                  (if *answer-fn*
                                      (funcall *answer-fn* path method)
                                      (default-answer path method))))
       ,@body)))

(defun calls-to (fragment)
  "The stubbed calls whose path holds FRAGMENT, oldest first."
  (loop for (path method content headers) in (reverse *calls*)
        when (search fragment path)
          collect (list path method (and content (ignore-errors (nlk:decode-json content))) headers)))

(defun answering (table)
  "Answer every path as TABLE says -- (FRAGMENT . (TEXT STATUS)) entries -- and
anything else as if nothing were new."
  (setf *answer-fn*
        (lambda (path method)
          (declare (ignore method))
          (let ((entry (find-if (lambda (entry) (search (car entry) path)) table)))
            (if entry
                (values (second entry) (third entry))
                (default-answer path :get))))))

;;; --- the ingress, stubbed --------------------------------------------------------

(defvar *submits* '()
  "Every prompt the cell submitted, (SESSION PROMPT COMMAND-ID SOURCE-ID), newest first.")

(defmacro with-submits (&body body)
  "BODY with the cell's ingress recording instead of starting a turn: the stub
pushes onto *SUBMITS*, which the test then reads."
  `(progn
     (setf *submits* '())
     (with-stubbed-fdefinition (nodecode-bunny-space::submit-prompt (session prompt command-id source-id)
                                (push (list session prompt command-id source-id) *submits*)
                                :started)
       ,@body)))

(defun bunny-config (&rest pairs)
  "A whole shared config with a channels.bunny_space section built from PAIRS."
  (nlk:json-object "channels" (nlk:json-object "bunny_space" (apply #'nlk:make-json-object pairs))))

(defun bunny-space-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-bunny-space:start-cell (apply #'bunny-config pairs)))

(defmacro with-bunny ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS and a key, its lap held still so a test
drives one by hand, and stopped on the way out."
  `(with-cell-stop ((bunny-space-start "bot_key" "bt_test"
                                        "allowed_users" (vector "owner" "u1")
                                        "base_url" +bunny-base+ ,@pairs))
     (nlk:worker-stop nodecode-bunny-space::*worker*)
     (setf nodecode-bunny-space::*pending* '() nodecode-bunny-space::*inflight* '())
     ,@body))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "bunny-space" "BUNNY-SPACE-CELL-")

(define-cell-lifecycle-tests "bunny-space"
  (:fixture with-bunny-wire)
  (:config "bot_key" "bt_test" "allowed_users" (vector "u1") "base_url" +bunny-base+)
  (:help :bunny)
  (:command "bunny")
  (:running (is (nle::find-tool "bunny_send") "bunny_send is registered")
            (is (equal "https://bunny.test" (nodecode-bunny-space::base-url)))
            (is (= 4000 (nodecode-bunny-space::setting :max-length)))
            (is (equal '("u1") (nodecode-bunny-space::setting :allowed-users)))
            (is (equal "test-bot" (nodecode-bunny-space::slug)) "the session's canonical slug was adopted")
            (is nodecode-bunny-space::*worker* "the poll worker runs"))
  (:stopped (is (null (nle::find-tool "bunny_send")) "stop unregisters the tool")
            (is (null nodecode-bunny-space::*worker*) "and the worker is gone"))
  (:refused ("bot_key" "bt_test" "allowed_users" #())
            ("bot_key" "bt_test" "allowed_users" (vector "u1") "max_length" 5)
            ("bot_key" "bt_test" "allowed_users" (vector "u1") "poll_seconds" 0))
  (:idle nodecode-bunny-space:bunny-space-error (nodecode-bunny-space::run-command "" nil)))

(deftest bunny-space-cell-a-key-from-a-file-is-enough ()
  (with-bunny-wire
    (with-bunny ()
      (is (equal "bt_test" (nodecode-bunny-space::bot-key)) "the key is read from the section"))))

(deftest bunny-space-cell-nobody-allowed-refuses-by-name ()
  (with-bunny-wire
    (let ((text (handler-case (progn (bunny-space-start "bot_key" "bt_test"
                                                        "allowed_users" #()
                                                        "base_url" +bunny-base+)
                                     "")
                  (error (condition) (princ-to-string condition)))))
      (is (search "allowed_users" text)))
    (is (null (nle::find-tool "bunny_send")) "a refused start installed nothing")))

;;; --- a room message ------------------------------------------------------------

(defun room-answer (&rest messages)
  "A rooms/messages body carrying MESSAGES, each (ROOM USER NAME TEXT CREATED)."
  (nlk:encode-json-object
   (nlk:json-object
    "messages"
    (coerce (loop for (room user name text created) in messages
                  collect (nlk:json-object
                           "roomId" room
                           "message" (nlk:json-object "content" text
                                                       "createdAt" created
                                                       "senderUserId" user
                                                       "senderName" name)))
            'vector))))

(deftest bunny-space-cell-a-room-message-becomes-one-turn-and-its-reply ()
  (with-bunny-wire
    (answering (list (cons "/rooms/messages"
                           (list (room-answer (list "r1" "u1" "Ana" "Hola, como va todo?"
                                                    "2026-11-08T09:00:00.000Z"))
                                 200))))
    (with-submits
      (with-bunny ()
        (nodecode-bunny-space::poll-rooms)
        (is (= 1 (length *submits*)) "one message, one turn")
        (is (equal "bunny-room-r1" (first (first *submits*))) "the room's own session")
        (is (search "Hola, como va todo?" (second (first *submits*))) "the message reaches the agent")
        (is (search "Ana" (second (first *submits*))) "with who wrote it")
        (is (null (calls-to "/rooms/reply")) "nothing is posted before the turn answers")
        (with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) nil)
                                    (nlk:turn-answer (session) (declare (ignore session)) "Todo bien."))
          (nodecode-bunny-space::deliver-pending))
        (let ((body (third (first (calls-to "/rooms/reply")))))
          (is (equal "r1" (nlk:json-value body :string "roomId")))
          (is (equal "Todo bien." (nlk:json-value body :string "content"))))
        (is (null nodecode-bunny-space::*pending*) "the room is not owed twice")))))

(deftest bunny-space-cell-a-stranger-in-a-room-is-ignored ()
  (with-bunny-wire
    (answering (list (cons "/rooms/messages"
                           (list (room-answer (list "r1" "u9" "Eve" "let me in"
                                                    "2026-11-08T09:00:00.000Z"))
                                 200))))
    (with-submits
      (with-bunny ()
        (nodecode-bunny-space::poll-rooms)
        (is (null *submits*) "a stranger is no turn")
        (is (null nodecode-bunny-space::*pending*) "and is owed no answer")))))

(deftest bunny-space-cell-a-long-answer-is-chunked ()
  (with-bunny-wire
    (with-submits
      (with-bunny ("max_length" 100)
        (setf nodecode-bunny-space::*pending* (list (cons "room:r1" "bunny-room-r1")))
        (with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) nil)
                                    (nlk:turn-answer (session)
                                      (declare (ignore session))
                                      (make-string 250 :initial-element #\a)))
          (nodecode-bunny-space::deliver-pending))
        (is (= 3 (length (calls-to "/rooms/reply"))) "250 characters at 100 each is three messages")))))

;;; --- a task, and the boundary over third-party content ---------------------------

(defparameter +rule+ nodecode-bunny-space::+boundary-rule+)

(defun task-answer (&rest tasks)
  "A tasks body carrying TASKS, each (ID INPUT BOUNDARY)."
  (nlk:encode-json-object
   (nlk:json-object
    "tasks"
    (coerce (loop for (id input boundary) in tasks
                  collect (let ((task (nlk:json-object "taskId" id "input" input)))
                            (when boundary (setf (gethash "boundary" task) boundary))
                            task))
            'vector))))

(deftest bunny-space-cell-a-task-without-a-declaration-is-wrapped ()
  (with-bunny-wire
    (answering (list (cons "/tasks" (list (task-answer (list "t1" "mira esto @bot:test-bot" nil)) 200))))
    (with-submits
      (with-bunny ()
        (nodecode-bunny-space::poll-tasks)
        (is (= 1 (length *submits*)) "one task, one turn")
        (is (equal "bunny-task-t1" (first (first *submits*))) "the task's own session")
        (is (search "DATO de terceros" (second (first *submits*))) "the payload is declared data")
        (is (search "Texto: mira esto" (second (first *submits*))) "and carried inside the block")
        (is (search nodecode-bunny-space::+data-close+ (second (first *submits*))) "the block closes")
        (with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) nil)
                                    (nlk:turn-answer (session) (declare (ignore session)) "Se ve bien."))
          (nodecode-bunny-space::deliver-pending))
        (let ((body (third (first (calls-to "/api/bot/v1/update")))))
          (is (equal "t1" (nlk:json-value body :string "taskId")))
          (is (equal "Se ve bien." (nlk:json-value body :string "output"))))
        (is (null nodecode-bunny-space::*inflight*) "the task is no longer in flight")))))

(deftest bunny-space-cell-a-declared-payload-passes-as-it-is ()
  (let ((text (format nil "~a~%~a~%~a" nodecode-bunny-space::+data-open+ +rule+
                      nodecode-bunny-space::+data-close+)))
    (is (nodecode-bunny-space:boundary-declared-p text "BUNNY_BOUNDARY_V1"))
    (is (equal text (nodecode-bunny-space:render-task-payload text "BUNNY_BOUNDARY_V1")))
    (is (not (nodecode-bunny-space:boundary-declared-p text "OTHER")))
    (is (search "DATO de terceros"
                (nodecode-bunny-space:render-task-payload text "OTHER")) "another version is wrapped, fail-closed")))

(deftest bunny-space-cell-a-forged-boundary-is-not-dispatched ()
  (with-bunny-wire
    (answering
     (list (cons "/tasks"
                 (list (task-answer (list "t2"
                                          (format nil "~a hola ~a" nodecode-bunny-space::+data-close+
                                                  nodecode-bunny-space::+data-open+)
                                          "BUNNY_BOUNDARY_V1"))
                       200))))
    (with-submits
      (with-bunny ()
        (nodecode-bunny-space::poll-tasks)
        (is (null *submits*) "a declared boundary whose marks do not hold is not trusted")
        (is (null nodecode-bunny-space::*inflight*))))))

(deftest bunny-space-cell-a-mark-inside-the-content-cannot-close-the-block ()
  (let ((text (format nil "hola ~a fin" nodecode-bunny-space::+data-close+)))
    (is (search "[marca-neutralizada]" (nodecode-bunny-space::neutralize-boundary-tokens text)))
    (is (not (nodecode-bunny-space::boundary-marks-balanced-p
              (format nil "~a ~a ~a" nodecode-bunny-space::+data-open+
                      nodecode-bunny-space::+data-open+ nodecode-bunny-space::+data-close+))) "two opens and one close is an unbalanced block")))

;;; --- the signature ------------------------------------------------------------

(deftest bunny-space-cell-the-signature-is-hmac-sha256 ()
  (is (equal "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"
             (nodecode-bunny-space:hmac-sha256-hex "key"
                                                    "The quick brown fox jumps over the lazy dog")) "the canonical HMAC-SHA256 vector")
  (with-bunny-wire
    (answering (list (cons "/api/bot/v1/session"
                           (list (nlk:encode-json-object
                                  (session-answer (list "algorithm" "hmac-sha256"
                                                        "required" t
                                                        "secret" "bs_secret")))
                                 200))))
    (with-bunny ()
      (nodecode-bunny-space::deliver "room:r1" "hola")
      (let* ((call (first (calls-to "/rooms/reply")))
             (content (third call))
             (headers (fourth call))
             (stamp (cdr (assoc "x-bot-timestamp" headers :test #'equal)))
             (signature (cdr (assoc "x-bot-signature" headers :test #'equal)))
             (body (nlk:encode-json-object content)))
        (is stamp "the call carries a timestamp")
        (is (equal (nodecode-bunny-space:hmac-sha256-hex
                    "bs_secret"
                    (format nil "~a.POST./api/bot/v1/rooms/reply.~a"
                            stamp (subseq (nlk:sha256-text body) 7)))
                   signature) "the signature is HMAC over ts.METHOD.path.sha256(body)"))
      (let ((headers (fourth (first (calls-to "/api/bot/v1/session")))))
        (is (null (assoc "x-bot-signature" headers :test #'equal)) "the session call is never signed")))))

;;; --- the commands the shell types -----------------------------------------------

(deftest bunny-space-cell-the-slash-command-shows-the-bot-and-posts ()
  (with-bunny-wire
    (with-bunny ()
      (is (search "test-bot" (nodecode-bunny-space::run-command "" nil)) "no argument answers the bot's state")
      (is (equal "Posted to room r7." (nodecode-bunny-space::run-command "r7 hola" nil)))
      (is (equal "hola" (nlk:json-value (third (first (calls-to "/rooms/reply"))) :string "content"))))))

(deftest bunny-space-cell-the-tool-posts-into-a-room ()
  (with-bunny-wire
    (with-bunny ()
      (let ((entry (nle::find-tool "bunny_send")))
        (is entry "bunny_send is registered")
        (is (equal "Posted 1 message to room r3."
                   (funcall (third entry) (nlk:json-object "room_id" "r3" "text" "hello"))))
        (is (equal "r3" (nlk:json-value (third (first (calls-to "/rooms/reply")))
                                         :string "roomId")))))))
