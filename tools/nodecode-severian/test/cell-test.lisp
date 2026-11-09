;;;; cell-test.lisp --- lifecycle, the five tools, /severian, and the turn write.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a server address that is missing or is not an address, an integer
;;;; under its floor), the five tools through the seam the model calls them by
;;;; -- their URL, their body and their answer -- /severian with and without a
;;;; query, and a finished turn going from the :frame point to one
;;;; observation. Every request is a stubbed NLK:HTTP, so nothing reaches a
;;;; Severian server or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-severian-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials a server."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected severian request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun severian-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun severian-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun severian-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun severian-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun severian-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "severian" "SEVERIAN-CELL-" :start nodecode-severian:start-cell)

(define-cell-lifecycle-tests "severian"
  (:fixture with-severian-wire)
  (:config "base_url" "http://severian.test")
  (:help :severian)
  (:command "severian")
  (:running (is (nle::find-tool "severian_search") "severian_search is registered")
            (is (nle::find-tool "severian_ingest"))
            (is (nle::find-tool "severian_get_observation"))
            (is (nle::find-tool "severian_get_memory"))
            (is (nle::find-tool "severian_search_shared"))
            (is (equal "http://severian.test" (nodecode-severian::root)))
            (is (equal "nodecode" (nodecode-severian::setting :tenant-id)) "the default tenant")
            (is (equal "profile" (nodecode-severian::setting :collection-id)) "the profile-wide collection")
            (is (= 10 (nodecode-severian::setting :search-limit)))
            (is nodecode-severian::*worker* "the capture worker runs"))
  (:stopped (is (null (nle::find-tool "severian_search")) "stop unregisters the tools")
            (is (null nodecode-severian::*worker*) "and the worker is gone"))
  (:refused ("search_limit" 0) ("max_chars" 5) ("timeout_seconds" 0))
  (:idle nodecode-severian:severian-error (nodecode-severian::run-command "" nil)))

(deftest severian-cell-the-server-address-is-required ()
  (with-severian-wire
    (is-carrying (text (refusal-text nlk:config-refusal (severian-start)))
                 "base_url" "required" "severian serve")
    (is-carrying (text (refusal-text nlk:config-refusal (severian-start "base_url" "127.0.0.1:8000")))
                 "base_url" "http://")
    (is (null (nle::find-tool "severian_search")) "a refused start installed nothing")))

;;; --- the tools ------------------------------------------------------------------

(deftest severian-cell-search-answers-cited-memories ()
  (with-severian-wire
    (with-cell-stop ((severian-start "base_url" "http://severian.test" "agent_id" "laptop"))
      (severian-answer
       "{\"hits\":[{\"memory_id\":\"m1\",\"content\":\"drinks green tea, never coffee\",\"source_observation_ids\":[\"o1\",\"o2\"],\"fused_score\":0.912},{\"memory_id\":\"m2\",\"content\":\"lives in Oslo\",\"source_observation_ids\":[],\"fused_score\":0.4}]}")
      (let ((answer (severian-tool "severian_search"
                                    (nlk:json-object "query" "what do I drink" "limit" 3))))
        (is (search "2 memories for \"what do I drink\"" answer))
        (is (search "[m1] drinks green tea, never coffee (score 0.91) <- obs o1, o2" answer))
        (is (search "[m2] lives in Oslo (score 0.40)" answer)))
      (let* ((request (first (severian-asked)))
             (body (severian-request-body request)))
        (is (= 1 (length (severian-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "http://severian.test/api/v1/search" (second request)))
        (is (equal "what do I drink" (nlk:json-value body :string "query")))
        (is (= 3 (nlk:json-value body :integer "limit")) "the call's limit")
        (is (null (nlk:json-value body :boolean "derive")) "derive is off unless the section says so")
        (is (equal "laptop" (nlk:json-value body :string "scope" "agent_id")))
        (is (equal "nodecode" (nlk:json-value body :string "scope" "tenant_id")))
        (is (equal "shared" (nlk:json-value body :string "scope" "session_id")))))))

(deftest severian-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-severian-wire
    (with-cell-stop ((severian-start "base_url" "http://severian.test"))
      (severian-answer "{\"detail\":\"observation not found in scope\"}" 404)
      (is-carrying (text (severian-tool "severian_get_observation"
                                         (nlk:json-object "observation_id" "o9")))
                   "ERROR: severian:" "HTTP 404")
      (severian-answer "{\"detail\":\"internal error\"}" 500)
      (is-carrying (text (severian-tool "severian_search" (nlk:json-object "query" "x")))
                   "ERROR: severian:" "HTTP 500" "internal error")
      (severian-answer "{\"hits\":[]}")
      (is (equal "No memory in this scope matched \"x\"."
                 (severian-tool "severian_search" (nlk:json-object "query" "x")))))))

(deftest severian-cell-ingest-writes-one-observation ()
  (with-severian-wire
    (with-cell-stop ((severian-start "base_url" "http://severian.test" "agent_id" "laptop"))
      (severian-answer "{\"observation_id\":\"obs-1\",\"job_id\":\"derive-obs-1\",\"job_kind\":\"derive\"}" 201)
      (is (equal "Stored observation obs-1 (derive job derive-obs-1)."
                 (severian-tool "severian_ingest"
                                (nlk:json-object "content" "prefers window seats"))))
      (let* ((request (first (severian-asked)))
             (body (severian-request-body request)))
        (is (eq :post (first request)))
        (is (equal "http://severian.test/api/v1/observations" (second request)))
        (is (equal "prefers window seats" (nlk:json-value body :string "content")))
        (is (equal "laptop" (nlk:json-value body :string "scope" "agent_id")))
        (is (hash-table-p (nlk:json-value body :object "source")) "an empty source object")
        (is (equal "application/json" (severian-request-header request "content-type")))
        (is (null (severian-request-header request "authorization"))
            "no key, no header")))))

(deftest severian-cell-reads-back-an-observation-and-a-memory ()
  (with-severian-wire
    (with-cell-stop ((severian-start "base_url" "http://severian.test" "api_key" "k-test"))
      (severian-answer
       "{\"observation_id\":\"obs-1\",\"content\":\"user: I live in Oslo\",\"content_hash\":\"sha256:abc\",\"created_at\":\"2026-11-08T10:00:00Z\",\"scope\":{\"tenant_id\":\"nodecode\",\"profile_id\":\"default\",\"collection_id\":\"profile\",\"agent_id\":\"nodecode\",\"session_id\":\"shared\"}}")
      (let ((answer (severian-tool "severian_get_observation"
                                    (nlk:json-object "observation_id" "obs-1"))))
        (is (search "Observation obs-1 (2026-11-08T10:00:00Z, hash sha256:abc)" answer))
        (is (search "user: I live in Oslo" answer)))
      (let ((request (first (severian-asked))))
        (is (eq :get (first request)))
        (is-carrying (url (second request))
                     "http://severian.test/api/v1/observations/obs-1?"
                     "tenant_id=nodecode" "profile_id=default" "collection_id=profile"
                     "agent_id=nodecode" "session_id=shared")
        (is (equal "Bearer k-test" (severian-request-header request "authorization"))
            "the section's key rides every request"))
      (severian-answer
       "{\"memory_id\":\"m1\",\"content\":\"lives in Oslo\",\"kind\":\"fact\",\"source_observation_ids\":[\"obs-1\"],\"created_at\":\"2026-11-08T10:01:00Z\"}")
      (let ((answer (severian-tool "severian_get_memory" (nlk:json-object "memory_id" "m1"))))
        (is (search "Memory m1 (fact, 2026-11-08T10:01:00Z) <- obs obs-1" answer))
        (is (search "lives in Oslo" answer)))
      (is (equal "http://severian.test/api/v1/memories/m1?tenant_id=nodecode&profile_id=default&collection_id=profile&agent_id=nodecode&session_id=shared"
                 (second (second (severian-asked))))))))

(deftest severian-cell-shared-search-queries-each-agent-and-merges ()
  (with-severian-wire
    (with-cell-stop ((severian-start "base_url" "http://severian.test" "agent_id" "laptop"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method url args))
              (let ((body (severian-request-body (first *requests*)))
                    (agent (nlk:json-value (severian-request-body (first *requests*))
                                            :string "scope" "agent_id")))
                (declare (ignore body))
                (cond ((equal agent "laptop")
                       (values "{\"hits\":[{\"memory_id\":\"m1\",\"content\":\"writes Lisp\",\"fused_score\":0.9,\"source_observation_ids\":[\"o1\"]}]}" 200))
                      ((equal agent "desktop")
                       (values "{\"hits\":[{\"memory_id\":\"m1\",\"content\":\"writes Lisp\",\"fused_score\":0.9,\"source_observation_ids\":[\"o1\"]},{\"memory_id\":\"m2\",\"content\":\"prefers dark themes\",\"fused_score\":0.5}]}" 200))
                      (t (values "{\"hits\":[{\"memory_id\":\"m3\",\"content\":\"runs the morning triage\",\"fused_score\":0.7}]}" 200))))))
      (let ((answer (severian-tool "severian_search_shared"
                                    (nlk:json-object "agent_ids" (vector "desktop" "phone")
                                                     "query" "what do we prefer" "limit" 2))))
        (is (= 3 (length (severian-asked))) "our scope and each listed agent, once")
        (is (equal '("laptop" "desktop" "phone")
                   (mapcar (lambda (request)
                             (nlk:json-value (severian-request-body request) :string "scope" "agent_id"))
                           (severian-asked)))
            "each scope queried on its own, never a wildcard")
        (is (every (lambda (request)
                     (= 50 (nlk:json-value (severian-request-body request) :integer "limit")))
                   (severian-asked))
            "each read asks for the merge's own 50")
        (is (search "2 memories for \"what do we prefer\" (this agent and desktop, phone)" answer))
        (is (search "[m1] writes Lisp (score 0.90) <- obs o1" answer)
            "a memory seen twice is listed once, at its best score")
        (is (search "[m3] runs the morning triage" answer))
        (is (null (search "prefers dark themes" answer)) "the limit cuts the rest")
        (is (null (search "[m2]" answer)))))))

;;; --- /severian -----------------------------------------------------------------

(deftest severian-cell-the-command-shows-the-scope-or-searches-it ()
  (with-severian-wire
    (with-cell-stop ((severian-start "base_url" "http://severian.test" "profile_id" "work"))
      (let ((answer (cell-entry "nodecode-severian" "severian" "")))
        (is (search "Severian memory: http://severian.test, tenant \"nodecode\", profile \"work\"" answer))
        (is (search "capture is on" answer))
        (is (null (severian-asked)) "a status read asks the server nothing"))
      (severian-answer "{\"hits\":[{\"memory_id\":\"m2\",\"content\":\"lives in Oslo\",\"fused_score\":0.4}]}")
      (is (search "[m2] lives in Oslo"
                  (cell-entry "nodecode-severian" "severian" "where do I live")))
      (is (equal "where do I live"
                 (nlk:json-value (severian-request-body (first (severian-asked))) :string "query"))))))

;;; --- the turn written as one observation ---------------------------------------

(deftest severian-cell-a-finished-turn-becomes-one-observation ()
  (with-severian-wire
    (with-cell-stop ((severian-start "base_url" "http://severian.test"
                                      "max_chars" 50 "agent_id" "laptop"))
      (severian-answer "{\"observation_id\":\"obs-9\"}" 201)
      (cell-publish "nodecode-severian"
                    (cell-frame-op "turn.input_committed" :turn "t9"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "My name is Ada Lovelace. I live in Oslo. I drink green tea.")))
      (cell-publish "nodecode-severian"
                    (cell-frame-op "turn.assistant_message_completed" :turn "t9"
                                   :payload (nlk:json-object "message" "Noted, Oslo and tea.")))
      (cell-publish "nodecode-severian" (cell-frame-op "turn.completed" :turn "t9"))
      (nodecode-severian::capture-step)
      (await (:timeout 5) (= 1 (length (severian-asked))))
      (let* ((request (first (severian-asked)))
             (body (severian-request-body request)))
        (is (equal "http://severian.test/api/v1/observations" (second request))
            "the turn goes in as an observation")
        (is (equal (format nil "user: My name is Ada Lovelace. I live in Oslo.~%assistant: Noted, Oslo and tea.")
                   (nlk:json-value body :string "content"))
            "a long message is cut at a sentence end inside max_chars")
        (is (equal "nodecode" (nlk:json-value body :string "source" "provider")))
        (is (equal "t9" (nlk:json-value body :string "source" "turn")))))))
