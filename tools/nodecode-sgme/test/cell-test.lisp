;;;; cell-test.lisp --- lifecycle, the tools, /sgme, the round block and the capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong choice and an integer under its floor refused, a start
;;;; that could reach no server refused), the tools through the seam the
;;;; model calls them by -- their URL, their body and their answer -- /sgme
;;;; with and without a query, the profile block recorded once per turn, and
;;;; a finished turn going from the :frame point to one append and one
;;;; refinement. Every request is a stubbed NLK:HTTP, so nothing reaches
;;;; SGME or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-sgme-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials SGME. The default answer is an empty object,
which is what starting the cell reads for its profile."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (values "{}" 200))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun sgme-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun sgme-request-to (path &optional (nth 0))
  "The (METHOD URL . ARGS) of the Nth request whose URL holds PATH, or NIL."
  (nth nth (remove-if-not (lambda (request) (search path (second request))) (sgme-asked))))

(defun sgme-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun sgme-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun sgme-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun sgme-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defparameter *inject-body*
  "{\"blocks\":[{\"title\":\"preferences\",\"items\":[{\"content\":\"drinks green tea\"}]}]}"
  "What /v1/inject answers in these tests.")

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "sgme" "SGME-CELL-" :start nodecode-sgme:start-cell)

(define-cell-lifecycle-tests "sgme"
  (:fixture with-sgme-wire)
  (:config "base_url" "https://sgme.test" "agent_key" "k-test" "admin_key" "a-test")
  (:hooks :frame :round)
  (:help :sgme)
  (:command "sgme")
  (:running (is (nle::find-tool "sgme_memory_search") "sgme_memory_search is registered")
            (is (nle::find-tool "sgme_conversation_search"))
            (is (nle::find-tool "sgme_skill_get"))
            (is (nle::find-tool "sgme_signal_pull"))
            (is (nle::find-tool "sgme_stats"))
            (is (equal "https://sgme.test" (nodecode-sgme::root)))
            (is (equal "nodecode" (nodecode-sgme::setting :agent-id)) "the default agent tag")
            (is (= 5 (nodecode-sgme::setting :search-limit)))
            (is nodecode-sgme::*worker* "the capture worker runs"))
  (:stopped (is (null (nle::find-tool "sgme_memory_search")) "stop unregisters the tools")
            (is (null nodecode-sgme::*worker*) "and the worker is gone"))
  (:refused ("base_url" "https://sgme.test" "timeout_seconds" 0)
            ("inject_mode" "poetic"))
  (:idle nodecode-sgme:sgme-error (nodecode-sgme::run-command "" nil)))

(deftest sgme-cell-a-remote-server-needs-an-agent-key ()
  (with-sgme-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (sgme-start "base_url" "https://sgme.test")))
                   ("needs an agent key" "SGME_AGENT_KEY"))
      (is (null (nle::find-tool "sgme_memory_search")) "a refused start installed nothing"))))

;;; --- the tools ------------------------------------------------------------------

(deftest sgme-cell-search-answers-the-nearest-memories ()
  (with-sgme-wire
    (setf *answer-fn*
          (lambda (method url args)
            (declare (ignore method url args))
            (values "{\"results\":[{\"id\":\"m1\",\"memory\":\"drinks green tea\",\"score\":0.9},{\"id\":\"m2\",\"memory\":\"lives in Oslo\"}]}" 200)))
    (with-cell-stop ((sgme-start "base_url" "https://sgme.test" "agent_key" "k-test"))
      (let ((answer (sgme-tool "sgme_memory_search"
                                (nlk:json-object "query" "what do I drink" "limit" 3))))
        (is (search "2 results for \"what do I drink\"" answer))
        (is (search "[m1] drinks green tea (score 0.90)" answer))
        (is (search "[m2] lives in Oslo" answer)))
      (let* ((request (sgme-request-to "/v1/search"))
             (body (sgme-request-body request)))
        (is (equal "https://sgme.test/v1/search" (second request)))
        (is (eq :post (first request)))
        (is (equal "k-test" (sgme-request-header request "x-api-key")))
        (is (equal "what do I drink" (nlk:json-value body :string "query")))
        (is (= 3 (nlk:json-value body :integer "limit")) "the call's limit")
        (is (equal '("memory" "skills")
                   (coerce (nlk:json-array body "scopes") 'list)) "the original's default scopes")))))

(deftest sgme-cell-conversation-and-wiki-read-their-own-layers ()
  (with-sgme-wire
    (setf *answer-fn*
          (lambda (method url args)
            (declare (ignore method url args))
            (values "{\"results\":[{\"id\":\"s1\",\"text\":\"the words of 2026-08-11\"}]}" 200)))
    (with-cell-stop ((sgme-start "base_url" "https://sgme.test" "agent_key" "k-test"))
      (is (search "[s1] the words of 2026-08-11"
                  (sgme-tool "sgme_conversation_search" (nlk:json-object "query" "Oslo"))))
      (let ((body (sgme-request-body (sgme-request-to "/v1/search"))))
        (is (equal '("sessions") (coerce (nlk:json-array body "scopes") 'list)) "the raw session layer only"))
      (sgme-tool "sgme_wiki_search" (nlk:json-object "query" "tea" "limit" 4))
      (let ((request (sgme-request-to "/v1/wiki/search")))
        (is (search "q=tea" (second request)) "the query rides the URL")
        (is (search "limit=4" (second request))))
      (sgme-tool "sgme_skill_get" (nlk:json-object "name" "adapter hermes" "section" "install"))
      (let ((request (sgme-request-to "/v1/skills/")))
        (is (search "/v1/skills/adapter%20hermes" (second request)) "a name in a path is encoded")
        (is (search "section=install" (second request)))))))

(deftest sgme-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-sgme-wire
    (with-cell-stop ((sgme-start "base_url" "https://sgme.test" "agent_key" "k-test"))
      (sgme-answer "{\"message\":\"invalid API key\"}" 401)
      (is-carrying (answer (sgme-tool "sgme_memory_search" (nlk:json-object "query" "x")))
                   ("ERROR: sgme:") ("HTTP 401") ("invalid API key"))
      (sgme-answer "{\"results\":[]}")
      (is (search "Nothing in the memory pool matched"
                  (sgme-tool "sgme_memory_search" (nlk:json-object "query" "x"))))
      (setf *requests* '())
      (is-carrying (answer (sgme-tool "sgme_memory_search" (nlk:json-object)))
                   ("query is required"))
      (is (null (sgme-asked)) "a missing argument asks the server nothing"))))

(deftest sgme-cell-care-signals-pull-claim-and-ack ()
  (with-sgme-wire
    (with-cell-stop ((sgme-start "base_url" "https://sgme.test" "agent_key" "k-test"
                                  "admin_key" "a-test" "agent_id" "laptop"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore args))
              (case method
                (:get (if (search "/v1/events/pull" url)
                          (values "{\"events\":[{\"id\":\"e1\",\"type\":\"care_due\",\"ts\":\"2026-10-25T10:00:00Z\",\"payload\":{\"item\":\"water the plants\"}}],\"next_cursor\":7}" 200)
                          (values "{}" 200)))
                (:post (if (search "e2" url)
                           (values "{\"message\":\"conflict\"}" 409)
                           (values "{\"event_id\":\"e1\",\"status\":\"acked\"}" 200))))))
      (let ((answer (sgme-tool "sgme_signal_pull" (nlk:json-object))))
        (is (search "[e1] care_due" answer))
        (is (search "water the plants" answer)))
      (let ((request (sgme-request-to "/v1/events/pull")))
        (is (search "subscriber_id=laptop" (second request)) "the agent id subscribes"))
      (is (equal "Claimed signal e1."
                 (sgme-tool "sgme_signal_claim" (nlk:json-object "event_id" "e1"))))
      (is (search "/v1/admin/care/signals/e1/consume"
                  (second (sgme-request-to "/consume"))))
      (is (search "already claimed"
                  (sgme-tool "sgme_signal_claim" (nlk:json-object "event_id" "e2"))) "a 409 is a skip, not a failure")
      (is (equal "Acked signal e1 as acked."
                 (sgme-tool "sgme_signal_ack"
                            (nlk:json-object "event_id" "e1" "result" "watered them"))))
      (let ((body (sgme-request-body (sgme-request-to "/ack"))))
        (is (equal "acked" (nlk:json-value body :string "status")))
        (is (equal "watered them" (nlk:json-value body :string "result"))))
      (is-carrying (answer (sgme-tool "sgme_signal_ack"
                                       (nlk:json-object "event_id" "e3" "status" "shrug")))
                   ("claimed, acked or failed")))))

;;; --- /sgme ---------------------------------------------------------------------

(deftest sgme-cell-the-command-shows-the-server-or-searches-it ()
  (with-sgme-wire
    (with-cell-stop ((sgme-start "base_url" "https://sgme.test" "agent_key" "k-test"))
      (setf *requests* '())
      (let ((answer (cell-entry "nodecode-sgme" "sgme" "")))
        (is (search "SGME at https://sgme.test, user \"nodecode\"" answer))
        (is (null (sgme-asked)) "a status read asks the server nothing"))
      (sgme-answer "{\"results\":[{\"id\":\"m1\",\"memory\":\"lives in Oslo\"}]}")
      (is (search "[m1] lives in Oslo" (cell-entry "nodecode-sgme" "sgme" "where do I live")))
      (is (equal "where do I live"
                 (nlk:json-value (sgme-request-body (sgme-request-to "/v1/search")) :string "query"))))))

;;; --- the profile block ------------------------------------------------------------

(deftest sgme-cell-the-profile-rides-one-round-per-turn ()
  (with-sgme-wire
    (setf *answer-fn*
          (lambda (method url args)
            (declare (ignore method url args))
            (values *inject-body* 200)))
    (with-cell-stop ((sgme-start "base_url" "https://sgme.test" "agent_key" "k-test"))
      (is (search "drinks green tea" (nodecode-sgme::profile-block)) "the profile is composed")
      (let ((recorded '())
            (hook (cell-hook "nodecode-sgme" :round)))
        (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &key text &allow-other-keys)
                                   (push (cons turn text) recorded))
          (funcall hook '(:session-id "s1" :turn "t1" :round 1) (lambda (op) (declare (ignore op)) nil))
          (funcall hook '(:session-id "s1" :turn "t1" :round 2) (lambda (op) (declare (ignore op)) nil))
          (funcall hook '(:session-id "s1" :turn "t2" :round 1) (lambda (op) (declare (ignore op)) nil)))
        (is (= 2 (length recorded)) "one block per turn, not one per round")
        (is (search "drinks green tea" (cdar recorded)) "and it is the profile")
        (is (search "<system-reminder>" (cdar recorded)))))))

;;; --- the capture ------------------------------------------------------------------

(deftest sgme-cell-a-finished-turn-becomes-one-append-and-one-refinement ()
  (with-sgme-wire
    (setf *answer-fn*
          (lambda (method url args)
            (declare (ignore method url args))
            (values "{\"ok\":true}" 200)))
    (with-cell-stop ((sgme-start "base_url" "https://sgme.test" "agent_key" "k-test"
                                  "admin_key" "a-test" "agent_id" "laptop"))
      (setf *requests* '())
      (cell-publish "nodecode-sgme"
                    (cell-frame-op "turn.input_committed" :session "s-42" :turn "t9"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "My name is Ada Lovelace.")))
      (cell-publish "nodecode-sgme"
                    (cell-frame-op "turn.assistant_message_completed" :session "s-42" :turn "t9"
                                   :payload (nlk:json-object "message" "Noted.")))
      (cell-publish "nodecode-sgme" (cell-frame-op "turn.completed" :session "s-42" :turn "t9"))
      (nodecode-sgme::capture-step)
      (await (:timeout 5) (sgme-request-to "/v1/append"))
      (let* ((request (sgme-request-to "/v1/append"))
             (body (sgme-request-body request))
             (content (nlk:json-value body :string "content")))
        (is (equal "https://sgme.test/v1/append" (second request)))
        (is (equal "k-test" (sgme-request-header request "x-api-key")) "an append carries the agent key, as the original sends it")
        (is (equal "nodecode-s-42" (nlk:json-value body :string "session_key")))
        (is (equal "laptop" (nlk:json-value body :string "agent_id")))
        (is (search "Ada Lovelace" content))
        (is (search "Noted." content)))
      (await (:timeout 5) (sgme-request-to "/v1/admin/refine/trigger_async"))
      (is (equal "a-test"
                 (sgme-request-header (sgme-request-to "/v1/admin/refine/trigger_async") "x-api-key")) "a refinement is admin work"))))
