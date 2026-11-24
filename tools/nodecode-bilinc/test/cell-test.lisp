;;;; cell-test.lisp --- lifecycle, the four tools, /bilinc, recall and capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a base_url that is not https and an integer under its floor
;;;; refused, a start with no key refused), the four tools through the
;;;; dispatch seam the model calls them by -- their URL, their headers, their
;;;; body and their answer -- /bilinc with and without a query, the block an
;;;; automatic recall injects, and a finished turn captured as one episodic
;;;; record. Every request is a stubbed NLK:HTTP, so nothing reaches Bilinc.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defvar *calls* 0
  "How many tool calls this test has made, so each has its own call id.")

(defmacro with-bilinc-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Bilinc."
  `(let ((*requests* '())
         (*calls* 0)
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected bilinc request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun bilinc-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun bilinc-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun bilinc-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun bilinc-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun bilinc-tool (name arguments)
  "Run the registered tool NAME over ARGUMENTS, its JSON text, through the
real dispatch seam -- arguments decoded by the tool's contract -- and answer
the text the model reads."
  (nle::message-content
   (execute-wire-call (format nil "bilinc-call-~d" (incf *calls*)) name arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "bilinc" "BILINC-CELL-" :start nodecode-bilinc:start-cell)

(define-cell-lifecycle-tests "bilinc"
  (:fixture with-bilinc-wire)
  (:config "api_key" "k-test" "base_url" "https://bilinc.test")
  (:hooks :round :frame)
  (:help :bilinc)
  (:command "bilinc")
  (:running (is (nle::find-tool "bilinc_recall") "bilinc_recall is registered")
            (is (nle::find-tool "bilinc_remember"))
            (is (nle::find-tool "bilinc_revise"))
            (is (nle::find-tool "bilinc_forget"))
            (is (equal "https://bilinc.test" (nodecode-bilinc::root)))
            (is (equal "k-test" (nodecode-bilinc::api-key)) "the key the section names")
            (is (= 6 (nodecode-bilinc::setting :recall-limit)) "the default recall limit")
            (is nodecode-bilinc::*worker* "the capture worker runs"))
  (:stopped (is (null (nle::find-tool "bilinc_recall")) "stop unregisters the tools")
            (is (null nodecode-bilinc::*worker*) "and the worker is gone"))
  (:refused ("api_key" "k-test" "base_url" "http://bilinc.example")
            ("api_key" "k-test" "recall_limit" 0)
            ("api_key" "k-test" "timeout_seconds" 0))
  (:idle nodecode-bilinc:bilinc-error (nodecode-bilinc::run-command "" nil)))

(deftest bilinc-cell-needs-a-key ()
  (with-bilinc-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (bilinc-start "base_url" "https://bilinc.test")))
                   ("no API key" "BILINC_API_KEY"))
      (is (null (nle::find-tool "bilinc_recall")) "a refused start installed nothing"))))

;;; --- the tools ------------------------------------------------------------------

(deftest bilinc-cell-recall-answers-keys-and-scores ()
  (with-bilinc-wire
    (with-cell-stop ((bilinc-start "api_key" "k-test" "base_url" "https://bilinc.test"))
      (bilinc-answer "{\"results\":[{\"key\":\"pref:editor\",\"value\":\"uses emacs\",\"score\":0.912,\"entry_version\":3},{\"key\":\"pref:city\",\"value\":\"lives in Oslo\"}]}")
      (let ((answer (bilinc-tool "bilinc_recall" "{\"query\":\"which editor\",\"limit\":3}")))
        (is (search "2 memories for \"which editor\"" answer))
        (is (search "[pref:editor] uses emacs (score 0.91), v3" answer))
        (is (search "[pref:city] lives in Oslo" answer) "a memory without a score still lists"))
      (let* ((request (first (bilinc-asked)))
             (body (bilinc-request-body request)))
        (is (= 1 (length (bilinc-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://bilinc.test/api/cloud/memory/recall" (second request)))
        (is (equal "Bearer k-test" (bilinc-request-header request "authorization")))
        (is (equal "nodecode-plugin" (bilinc-request-header request "x-bilinc-client")))
        (is (equal "which editor" (nlk:json-value body :string "query")))
        (is (= 3 (nlk:json-value body :integer "limit")) "the call's limit")
        (is (equal "balanced" (nlk:json-value body :string "profile")))))))

(deftest bilinc-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-bilinc-wire
    (with-cell-stop ((bilinc-start "api_key" "k-test" "base_url" "https://bilinc.test"))
      (bilinc-answer "{\"error\":\"invalid_api_key\",\"message\":\"Invalid API key\"}" 401)
      (is-carrying (answer (bilinc-tool "bilinc_recall" "{\"query\":\"x\"}"))
                   ("ERROR: bilinc:" "the refusal is the model's repair") ("HTTP 401")
                   ("Invalid API key"))
      (bilinc-answer "{\"results\":[]}")
      (is (equal "No memory matched \"x\"." (bilinc-tool "bilinc_recall" "{\"query\":\"x\"}")))
      (is-carrying (answer (bilinc-tool "bilinc_forget" "{\"key\":\"pref:city\"}"))
                   ("reason" "the contract names the missing member")))))

(deftest bilinc-cell-remember-commits-one-redacted-fact ()
  (with-bilinc-wire
    (with-cell-stop ((bilinc-start "api_key" "k-test" "base_url" "https://bilinc.test"))
      (bilinc-answer "{\"success\":true,\"entryVersion\":1}")
      (is (equal "Stored pref:editor, version 1."
                 (bilinc-tool "bilinc_remember"
                              "{\"content\":\"the editor is emacs, token=abc123secret\",\"key\":\"pref:editor\",\"memory_type\":\"procedural\"}")))
      (let* ((request (first (bilinc-asked)))
             (body (bilinc-request-body request)))
        (is (equal "https://bilinc.test/api/cloud/memory/commit" (second request)))
        (is (equal "pref:editor" (nlk:json-value body :string "key")))
        (is (equal "procedural" (nlk:json-value body :string "memoryType")))
        (is (eq t (nlk:json-value body :boolean "ifAbsent")) "never overwrites")
        (is (search "[REDACTED]" (nlk:json-value body :string "value")) "a secret-shaped value is replaced before it is sent")
        (is (stringp (bilinc-request-header request "idempotency-key")) "a write carries a fresh idempotency key"))
      (setf *requests* '())
      (bilinc-answer "{\"success\":true}")
      (bilinc-tool "bilinc_remember" "{\"content\":\"likes tea\"}")
      (is (search "nodecode_note::"
                  (nlk:json-value (bilinc-request-body (first (bilinc-asked))) :string "key")) "a key is generated when the call gives none")
      (bilinc-answer "{\"success\":false}")
      (is (search "already exists"
                  (bilinc-tool "bilinc_remember" "{\"content\":\"x\",\"key\":\"pref:editor\"}"))))))

(deftest bilinc-cell-revise-and-forget-keep-the-reason ()
  (with-bilinc-wire
    (with-cell-stop ((bilinc-start "api_key" "k-test" "base_url" "https://bilinc.test"))
      (bilinc-answer "{\"success\":true,\"entryVersion\":2}")
      (is (equal "Revised pref:city, version 2."
                 (bilinc-tool "bilinc_revise"
                              "{\"key\":\"pref:city\",\"content\":\"lives in Bergen\",\"reason\":\"moved\"}")))
      (let* ((request (first (bilinc-asked)))
             (body (bilinc-request-body request)))
        (is (equal "https://bilinc.test/api/cloud/memory/revise" (second request)))
        (is (equal "recency" (nlk:json-value body :string "strategy")))
        (is (equal "moved" (nlk:json-value body :string "reason")))
        (is (equal "lives in Bergen" (nlk:json-value body :string "value"))))
      (bilinc-answer "{\"success\":true}")
      (is (equal "Forgot pref:city."
                 (bilinc-tool "bilinc_forget" "{\"key\":\"pref:city\",\"reason\":\"wrong\"}")))
      (let* ((request (car (last (bilinc-asked))))
             (body (bilinc-request-body request)))
        (is (equal "https://bilinc.test/api/cloud/memory/forget" (second request)))
        (is (equal "wrong" (nlk:json-value body :string "reason")) "the reason rides the forget, for the history")))))

;;; --- /bilinc --------------------------------------------------------------------

(deftest bilinc-cell-the-command-shows-the-state-or-searches ()
  (with-bilinc-wire
    (with-cell-stop ((bilinc-start "api_key" "k-test" "base_url" "https://bilinc.test"))
      (let ((answer (cell-entry "nodecode-bilinc" "bilinc" "")))
        (is (search "Bilinc memory: key set at https://bilinc.test" answer))
        (is (null (bilinc-asked)) "a status read asks the API nothing"))
      (bilinc-answer "{\"results\":[{\"key\":\"pref:city\",\"value\":\"lives in Oslo\"}]}")
      (is (search "[pref:city] lives in Oslo"
                  (cell-entry "nodecode-bilinc" "bilinc" "where do I live")))
      (is (equal "where do I live"
                 (nlk:json-value (bilinc-request-body (first (bilinc-asked))) :string "query"))))))

;;; --- recall before a turn, and the capture of a finished one -----------------------

(deftest bilinc-cell-auto-recall-injects-a-block ()
  (with-bilinc-wire
    (with-cell-stop ((bilinc-start "api_key" "k-test" "base_url" "https://bilinc.test"))
      (bilinc-answer "{\"results\":[{\"key\":\"pref:editor\",\"value\":\"uses emacs\"}]}")
      (let ((injected '())
            (hook (cell-hook "nodecode-bilinc" :round)))
        (with-stubbed-fdefinitions
            ((nlk:events (&rest args)
                         (nlk:json-object "input" "which editor do I use"))
             (nlk:record-turn-context-prompt (turn &key text &allow-other-keys)
                                              (push (cons turn text) injected)))
          (funcall hook (list :turn "t1" :session-id "s1") #'identity)
          (is (= 1 (length injected)) "one block rides this request")
          (is (search "[Bilinc memory - recalled for this turn" (cdr (first injected))))
          (is (search "- pref:editor: uses emacs" (cdr (first injected))))
          (is (equal "fast"
                     (nlk:json-value (bilinc-request-body (first (bilinc-asked))) :string "profile")) "a recall before a turn asks for the fast profile")
          (funcall hook (list :turn "t2" :session-id "s1") #'identity)
          (is (= 1 (length (bilinc-asked))) "the same query again does not dial twice")
          (funcall hook (list :turn "t3" :session-id "s2") #'identity)
          (is (= 2 (length (bilinc-asked))) "another session recalls again"))))))

(deftest bilinc-cell-a-finished-turn-is-one-episodic-record ()
  (with-bilinc-wire
    (with-cell-stop ((bilinc-start "api_key" "k-test" "base_url" "https://bilinc.test"
                                    "capture_turns" t "capture_ttl_days" 7))
      (bilinc-answer "{\"success\":true}")
      (cell-publish "nodecode-bilinc"
                    (cell-frame-op "turn.input_committed" :turn "t9" :session "s1"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "I live in Oslo and drink green tea.")))
      (cell-publish "nodecode-bilinc"
                    (cell-frame-op "turn.assistant_message_completed" :turn "t9" :session "s1"
                                   :payload (nlk:json-object "message" "Noted: Oslo and green tea.")))
      (cell-publish "nodecode-bilinc" (cell-frame-op "turn.completed" :turn "t9" :session "s1"))
      (nodecode-bilinc::capture-step)
      (await (:timeout 5) (= 1 (length (bilinc-asked))))
      (let* ((request (first (bilinc-asked)))
             (body (bilinc-request-body request)))
        (is (equal "https://bilinc.test/api/cloud/memory/commit" (second request)))
        (is (equal "episodic" (nlk:json-value body :string "memoryType")))
        (is (equal "nodecode_session_summary" (nlk:json-value body :string "source")))
        (is (equal "I live in Oslo and drink green tea."
                   (nlk:json-value body :string "value" "user")) "the operator's words")
        (is (equal "Noted: Oslo and green tea."
                   (nlk:json-value body :string "value" "assistant")))
        (is (search "nodecode_turn::s1::" (nlk:json-value body :string "key")))
        (is (= (* 7 86400) (nlk:json-value body :number "ttl")) "the section's ttl, in seconds")))))
