;;;; cell-test.lisp --- lifecycle, the two seams, the three tools, /protocol-chaos.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong choice refused, a start that could reach no server
;;;; refused), the :round point adding the overview and the matter a message
;;;; names while sending only ids to the server, the :frame point and the
;;;; worker recording the user's words verbatim, the three tools and
;;;; /protocol-chaos through the seam the model calls them by. Every request
;;;; is a stubbed NLK:HTTP and the recording model a stubbed NLK:COMPLETE, so
;;;; nothing reaches a server or a network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defvar *context-text* nil
  "The text the stubbed NLK:RECORD-TURN-CONTEXT-PROMPT was handed, and its turn.")

(defvar *context-turn* nil)

(defmacro with-chaos-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, and the round's context recorded in *CONTEXT-TEXT*."
  `(let ((*requests* '())
         (*context-text* nil)
         (*context-turn* nil)
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected protocol chaos request"))))
     (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &rest args)
                                (setf *context-turn* turn
                                      *context-text* (getf args :text)))
       (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                  (push (list* method url args) *requests*)
                                  (funcall *answer-fn* method url args))
         ,@body))))

(defun chaos-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun chaos-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun chaos-tool (request)
  "The MCP tool REQUEST calls."
  (nlk:json-value (chaos-body request) :string "params" "name"))

(defun chaos-arguments (request)
  "The arguments of the MCP tool REQUEST calls, decoded."
  (nlk:json-value (chaos-body request) :object "params" "arguments"))

(defun chaos-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defparameter +directory-answer+
  "{\"version\":\"v1\",\"events\":[{\"id\":\"m1\",\"title\":\"the router keeps dropping the link\",\"state\":\"active\",\"parents\":[]},{\"id\":\"m2\",\"title\":\"visit Lisbon in the spring\",\"state\":\"emerging\",\"parents\":[]},{\"id\":\"m3\",\"title\":\"the old flat\",\"state\":\"done\",\"parents\":[]}]}"
  "A directory the cell reads: three matters, one closed.")

(defparameter +matter-answer+
  "{\"event\":{\"id\":\"m1\",\"title\":\"the router keeps dropping the link\",\"state\":\"active\",\"version\":3,\"facts\":{\"goal\":\"a link that stays up all day\"},\"refs\":[],\"parents\":[]},\"accounts\":[{\"id\":\"a1\",\"text\":\"the router keeps dropping\",\"source\":\"user\",\"createdAt\":\"2026-03-01T10:00:00Z\"}],\"entities\":[]}"
  "One matter, as event_get_protocol answers it.")

(defun chaos-tool-result (payload)
  "An MCP tools/call answer carrying PAYLOAD's text as one content block."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 1
                     "result" (nlk:json-object "content"
                                                (vector (nlk:json-object "type" "text"
                                                                          "text" payload))))))

(defun chaos-route (note-answer &optional (directory +directory-answer+))
  "Answer each MCP tool as a Protocol Chaos server would: the directory, a
matter by id, and NOTE-ANSWER for event_note."
  (setf *answer-fn*
        (lambda (method url args)
          (declare (ignore method url args))
          (let* ((request (first *requests*))
                 (tool (chaos-tool request)))
            (values (chaos-tool-result (cond ((equal tool "context_pack") directory)
                                             ((equal tool "event_get_protocol") +matter-answer+)
                                             ((equal tool "event_note") note-answer)
                                             (t "{}")))
                    200)))))

(defun chaos-tool-call (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun chaos-turn-input (text &optional (turn "t1"))
  "Publish the frame a turn's user message arrives as."
  (cell-publish "nodecode-protocol-chaos"
                (cell-frame-op "turn.input_committed" :turn turn
                               :payload (nlk:json-object "role" "user" "message" text))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "protocol-chaos" "PROTOCOL-CHAOS-CELL-" :start nodecode-protocol-chaos:start-cell)

(define-cell-lifecycle-tests "protocol-chaos"
  (:fixture with-chaos-wire)
  (:config "url" "https://chaos.test/mcp" "token" "t-test")
  (:hooks :round :frame)
  (:help :protocol-chaos)
  (:command "protocol-chaos")
  (:running (is (nle::find-tool "protocol_chaos_matters") "protocol_chaos_matters is registered")
            (is (nle::find-tool "protocol_chaos_matter"))
            (is (nle::find-tool "protocol_chaos_record"))
            (is (equal "https://chaos.test/mcp" (nodecode-protocol-chaos::endpoint)))
            (is (equal "nodecode" (nodecode-protocol-chaos::setting :actor)) "the default client")
            (is nodecode-protocol-chaos::*worker* "the recording worker runs")
            (is (null (chaos-asked)) "a start asks the server nothing"))
  (:stopped (is (null (nle::find-tool "protocol_chaos_matters")) "stop unregisters the tools")
            (is (null nodecode-protocol-chaos::*worker*) "and the worker is gone"))
  (:refused ()
            ("capture" "carrier-pigeon")
            ("timeout_seconds" 0)))

(deftest protocol-chaos-cell-needs-a-token-too ()
  (with-chaos-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (protocol-chaos-start "url" "https://chaos.test/mcp")))
                   ("no token") ("PROTOCOL_CHAOS_TOKEN"))
      (is (null (nle::find-tool "protocol_chaos_matters")) "a refused start installed nothing"))))

;;; --- the round's context --------------------------------------------------------

(deftest protocol-chaos-cell-a-round-gets-the-overview-and-the-matter-its-message-names ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"))
      (chaos-route "{}")
      (chaos-turn-input "the router keeps dropping again")
      (nodecode-protocol-chaos::observe-round (list :session-id "s1" :turn "t1" :round 0))
      (let ((text *context-text*))
        (is (equal "t1" *context-turn*) "the context rides the turn")
        (is (search nodecode-protocol-chaos::+header+ text) "the client's header, never the user's words")
        (is (search "Protocol Chaos · the user's matters" text) "the overview, on the first round")
        (is (search "- the router keeps dropping the link · active" text) "with the open matters")
        (is (null (search "the old flat" text)) "and not the closed one")
        (is (= 3 (length (nodecode-protocol-chaos::matter-directory))) "though the directory holds all three")
        (is (search "Matters this message may be about" text) "then the related matters")
        (is (search "[id:m1]" text) "with the matter's id")
        (is (search "goal: a link that stays up all day" text) "and what it is for"))
      (let ((reads (chaos-asked)))
        (is (equal "context_pack" (chaos-tool (first reads))) "the directory, read once")
        (is (equal "event_get_protocol" (chaos-tool (second reads))) "then one matter by id")
        (is (equal '("accounts" "eventId")
                   (sort (loop for key being the hash-keys of (chaos-arguments (second reads))
                               collect key)
                         #'string<)) "only the id goes to the server: the message stays here")))))

(deftest protocol-chaos-cell-a-second-round-adds-nothing-it-added-before ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"))
      (chaos-route "{}")
      (chaos-turn-input "the router keeps dropping again")
      (nodecode-protocol-chaos::observe-round (list :session-id "s1" :turn "t1" :round 0))
      (setf *context-text* nil *requests* '())
      (nodecode-protocol-chaos::observe-round (list :session-id "s1" :turn "t1" :round 1))
      (is (null *context-text*) "no overview again, and the matter shown once stays shown"))))

(deftest protocol-chaos-cell-a-dead-server-adds-nothing ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"))
      (chaos-answer "{\"message\":\"boom\"}" 500)
      (chaos-turn-input "the router keeps dropping again")
      (nodecode-protocol-chaos::observe-round (list :session-id "s1" :turn "t1" :round 0))
      (is (null *context-text*) "a failed read is no context, not a broken turn"))))

;;; --- recording a finished turn ---------------------------------------------------

(deftest protocol-chaos-cell-a-finished-turn-is-recorded-verbatim-on-its-matter ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"
                                            "capture" "match" "actor" "laptop"))
      (chaos-route "{\"eventId\":\"m1\",\"accountId\":\"a2\",\"changed\":[\"words\"]}")
      (chaos-turn-input "the router keeps dropping the link in the evenings")
      (cell-publish "nodecode-protocol-chaos" (cell-frame-op "turn.completed" :turn "t1"))
      (nodecode-protocol-chaos::capture-step)
      (let* ((request (car (last (chaos-asked))))
             (arguments (chaos-arguments request)))
        (is (equal "event_note" (chaos-tool request)) "the words go to event_note")
        (is (equal "the router keeps dropping the link in the evenings"
                   (nlk:json-value arguments :string "text")) "copied character for character")
        (is (equal "m1" (nlk:json-value arguments :string "eventId")) "on the matter whose title shares words with it")
        (is (equal "user" (nlk:json-value arguments :string "accountSource")))
        (is (equal "laptop" (nlk:json-value arguments :string "actor" "id")) "stamped with the client")
        (is (equal "Bearer t-test"
                   (cdr (assoc "authorization" (getf (cddr request) :headers) :test #'equal))) "with the section's token")))))

(deftest protocol-chaos-cell-the-model-decides-what-to-record-and-where ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"))
      (chaos-route "{\"eventId\":\"m2\",\"accountId\":\"a9\",\"changed\":[\"words\"]}")
      (with-stubbed-fdefinition (nle:complete (system user &rest args)
                                 (declare (ignore args))
                                 (is (search "the router keeps dropping the link in the evenings" user) "the model reads the message")
                                 (is (search "m1 the router keeps dropping the link" user) "and the matters it may pick")
                                 (format nil "Here it is: {\"record\": true, \"ref\": \"m2\", ~
                                              \"words\": \"the router keeps dropping the link in the evenings\"}"))
        (chaos-turn-input "the router keeps dropping the link in the evenings")
        (cell-publish "nodecode-protocol-chaos" (cell-frame-op "turn.completed" :turn "t1"))
        (nodecode-protocol-chaos::capture-step)
        (let* ((request (car (last (chaos-asked))))
               (arguments (chaos-arguments request)))
          (is (equal "event_note" (chaos-tool request)))
          (is (equal "m2" (nlk:json-value arguments :string "eventId")) "the matter the model picked")
          (is (equal "the router keeps dropping the link in the evenings"
                     (nlk:json-value arguments :string "text")) "the words, still verbatim"))))))

(deftest protocol-chaos-cell-nothing-is-recorded-when-recording-is-off ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"
                                            "capture" "off"))
      (chaos-route "{}")
      (chaos-turn-input "the router keeps dropping the link in the evenings")
      (cell-publish "nodecode-protocol-chaos" (cell-frame-op "turn.completed" :turn "t1"))
      (nodecode-protocol-chaos::capture-step)
      (is (null (chaos-asked)) "not one request"))))

;;; --- the tools ------------------------------------------------------------------

(deftest protocol-chaos-cell-the-tools-answer-through-the-wire ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"))
      (chaos-route "{\"eventId\":\"m1\",\"accountId\":\"a2\",\"changed\":[\"words\"]}")
      (let ((answer (chaos-tool-call "protocol_chaos_matters" (nlk:json-object))))
        (is (search "- the router keeps dropping the link · active" answer) "the open matters, as a tree")
        (is (null (search "the old flat" answer)) "the closed one stays out"))
      (let ((answer (chaos-tool-call "protocol_chaos_matter" (nlk:json-object "id" "m1"))))
        (is (search "[id:m1]" answer))
        (is (search "goal: a link that stays up all day" answer) "its goal")
        (is (search "said 2026-03-01" answer) "and the last of its words")
        (is (equal "m1" (nlk:json-value (chaos-arguments (car (last (chaos-asked)))) :string "eventId"))))
      (let ((answer (chaos-tool-call "protocol_chaos_record"
                                      (nlk:json-object "text" "the router keeps dropping"
                                                       "matter_id" "m1"))))
        (is (search "Recorded on m1" answer) "the receipt names the matter")))))

(deftest protocol-chaos-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"))
      (chaos-answer "{\"message\":\"bad token\"}" 401)
      (is-carrying (answer (chaos-tool-call "protocol_chaos_matters" (nlk:json-object)))
                   ("ERROR: protocol-chaos:") ("HTTP 401")))))

(deftest protocol-chaos-cell-a-tool-refuses-while-the-cell-is-not-running ()
  (with-chaos-wire
    (is-carrying (answer (nodecode-protocol-chaos::tool-matters (nlk:json-object)))
                 ("not running") ("protocol-chaos"))))

;;; --- /protocol-chaos ---------------------------------------------------------------

(deftest protocol-chaos-cell-the-command-shows-the-server-or-finds-a-matter ()
  (with-chaos-wire
    (with-cell-stop ((protocol-chaos-start "url" "https://chaos.test/mcp" "token" "t-test"
                                            "actor" "laptop"))
      (chaos-route "{}")
      (is (search "Protocol Chaos at https://chaos.test/mcp as \"laptop\""
                  (cell-entry "nodecode-protocol-chaos" "protocol-chaos" "")) "the server and the client")
      (setf *requests* '())
      (chaos-turn-input "the router")
      (let ((answer (cell-entry "nodecode-protocol-chaos" "protocol-chaos" "router")))
        (is (search "- the router keeps dropping the link · active [id:m1]" answer) "the matter it finds"))
      (let ((answer (cell-entry "nodecode-protocol-chaos" "protocol-chaos" "the old flat")))
        (is (search "- the old flat · done [id:m3]" answer) "a closed one is still found by name")))))
