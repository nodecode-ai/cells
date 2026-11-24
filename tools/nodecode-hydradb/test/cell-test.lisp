;;;; cell-test.lisp --- lifecycle, the three tools, /hydradb, and the turn capture.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong choice and an integer under its floor refused, a start
;;;; that could reach no backend refused), the three tools through the seam
;;;; the model calls them by -- their URL, their body and their answer --
;;;; /hydradb with and without a query, the chunk unwrap, and a finished turn
;;;; going from the :frame point to one ingest request. Every request is a
;;;; stubbed NLK:HTTP, so nothing reaches HydraDB or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-hydradb-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials HydraDB."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected hydradb request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun hydradb-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun hydradb-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun hydradb-request-part (request name)
  "REQUEST's multipart form field NAME, as its text."
  (let ((content (getf (cddr request) :content)))
    (and (listp content) (cdr (assoc name content :test #'equal)))))

(defun hydradb-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun hydradb-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun hydradb-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun hydradb-item (request)
  "REQUEST's one app_knowledge item, decoded: the note the ingest stores."
  (aref (nlk:decode-json (hydradb-request-part request "app_knowledge")) 0))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "hydradb" "HYDRADB-CELL-" :start nodecode-hydradb:start-cell)

(define-cell-lifecycle-tests "hydradb"
  (:fixture with-hydradb-wire)
  (:config "api_key" "k-test" "database" "db-test" "base_url" "https://hydra.test")
  (:help :hydradb)
  (:command "hydradb")
  (:running (is (nle::find-tool "hydradb_recall") "hydradb_recall is registered")
            (is (nle::find-tool "hydradb_remember"))
            (is (nle::find-tool "hydradb_forget"))
            (is (equal "https://hydra.test" (nodecode-hydradb::root)))
            (is (equal "db-test" (nodecode-hydradb::database)))
            (is (equal "k-test" (nodecode-hydradb::api-key)))
            (is (equal "knowledge" (nodecode-hydradb::setting :kind)) "the original's recall family")
            (is (= 6 (nodecode-hydradb::setting :search-limit)) "the original's max_results")
            (is nodecode-hydradb::*worker* "the capture worker runs"))
  (:stopped (is (null (nle::find-tool "hydradb_recall")) "stop unregisters the tools")
            (is (null nodecode-hydradb::*worker*) "and the worker is gone"))
  (:refused ("kind" "carrier-pigeon") ("search_limit" 0) ("timeout_seconds" 0))
  (:idle nodecode-hydradb:hydradb-error (nodecode-hydradb::run-command "" nil)))

(deftest hydradb-cell-a-start-that-could-reach-no-backend-refuses ()
  (with-hydradb-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                        (hydradb-start "database" "db-test"
                                                       "base_url" "https://hydra.test")))
                   ("a key is required" "HYDRADB_API_KEY"))
      (is (null (nle::find-tool "hydradb_recall")) "a refused start installed nothing")
      (is-carrying (answer (refusal-text nlk:config-refusal
                                        (hydradb-start "api_key" "k-test"
                                                       "base_url" "https://hydra.test")))
                   ("\"database\" is required" "HYDRADB_DATABASE")))))

;;; --- the tools ------------------------------------------------------------------

(deftest hydradb-cell-recall-prefers-the-server-prompt ()
  (with-hydradb-wire
    (with-cell-stop ((hydradb-start "api_key" "k-test" "database" "db-test"
                                     "collection" "col-1" "base_url" "https://hydra.test"))
      (hydradb-answer "{\"success\":true,\"data\":{\"llm_prompt\":\"HydraDB memory:\\n- drinks green tea\",\"chunks\":[]}}")
      (is (equal "HydraDB memory:
- drinks green tea"
                 (hydradb-tool "hydradb_recall"
                               (nlk:json-object "query" "what do I drink" "max_results" 3))) "the server's own prompt is the answer")
      (let ((request (first (hydradb-asked))))
        (is (= 1 (length (hydradb-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://hydra.test/query" (second request)))
        (is (equal "Bearer k-test" (hydradb-request-header request "authorization")))
        (is (equal "application/json" (hydradb-request-header request "content-type")))
        (let ((body (hydradb-request-body request)))
          (is (equal "db-test" (nlk:json-value body :string "tenant_id")))
          (is (equal "col-1" (nlk:json-value body :string "sub_tenant_id")))
          (is (equal "what do I drink" (nlk:json-value body :string "query")))
          (is (equal "knowledge" (nlk:json-value body :string "kind")))
          (is (equal "fast" (nlk:json-value body :string "mode")))
          (is (= 3 (nlk:json-value body :integer "max_results")) "the call's max_results"))))))

(deftest hydradb-cell-recall-unwraps-a-stored-envelope ()
  (with-hydradb-wire
    (with-cell-stop ((hydradb-start "api_key" "k-test" "database" "db-test"
                                     "base_url" "https://hydra.test"))
      (hydradb-answer "{\"success\":true,\"data\":{\"chunks\":[{\"chunk_uuid\":\"c1\",\"chunk_content\":\"{\\\"content\\\":{\\\"text\\\":\\\"drinks green tea\\\"}}\\n\\nExtra context: sister drinks coffee\"},{\"id\":\"c2\",\"text\":\"lives in Oslo\",\"score\":0.8749}]}}")
      (let ((answer (hydradb-tool "hydradb_recall" (nlk:json-object "query" "tea"))))
        (is (search "2 passages for \"tea\":" answer))
        (is (search "- [c1] drinks green tea" answer) "the envelope is prose")
        (is (search "Extra context: sister drinks coffee" answer) "and the enrichment is kept")
        (is (search "- [c2] lives in Oslo (score 0.87)" answer))
        (is (not (search "{\"content\"" answer)) "the JSON scaffolding is gone"))
      (hydradb-answer "{\"success\":true,\"data\":{\"chunks\":[]}}")
      (is (equal "No HydraDB context matched \"x\"."
                 (hydradb-tool "hydradb_recall" (nlk:json-object "query" "x")))))))

(deftest hydradb-cell-remember-ingests-one-note ()
  (with-hydradb-wire
    (with-cell-stop ((hydradb-start "api_key" "k-test" "database" "db-test"
                                     "base_url" "https://hydra.test"))
      (hydradb-answer "{\"success\":true,\"data\":{\"accepted\":1}}")
      (is (equal "Queued for storage: seat; HydraDB indexes it in the background."
                 (hydradb-tool "hydradb_remember"
                               (nlk:json-object "text" "prefers a window seat" "title" "seat"))))
      (let* ((request (first (hydradb-asked)))
             (item (hydradb-item request)))
        (is (eq :post (first request)))
        (is (equal "https://hydra.test/context/ingest" (second request)))
        (is (equal "Bearer k-test" (hydradb-request-header request "authorization")))
        (is (equal "db-test" (hydradb-request-part request "tenant_id")))
        (is (equal "knowledge" (hydradb-request-part request "kind")))
        (is (equal "seat" (nlk:json-value item :string "title")))
        (is (equal "prefers a window seat" (nlk:json-value item :string "content" "text")))
        (is (eq t (nlk:json-value item :boolean "infer")) "the section's infer is on by default")
        (is (search "note-" (nlk:json-value item :string "id")) "the original's note id"))
      ;; JSON false decodes to a present NIL, as dispatch hands it over
      (hydradb-tool "hydradb_remember" (nlk:make-json-object "text" "ships on Friday" "infer" nil))
      (is (null (nlk:json-value (hydradb-item (car (last (hydradb-asked)))) :boolean "infer")) "infer off stores it verbatim"))))

(deftest hydradb-cell-forget-deletes-the-ids-it-is-given ()
  (with-hydradb-wire
    (with-cell-stop ((hydradb-start "api_key" "k-test" "database" "db-test"
                                     "base_url" "https://hydra.test"))
      (hydradb-answer "{\"success\":true,\"data\":{\"deleted_count\":2}}")
      (is (equal "Removed 2 contexts."
                 (hydradb-tool "hydradb_forget"
                               (nlk:json-object "ids" (vector "c1" "c2")))))
      (let* ((request (first (hydradb-asked)))
             (body (hydradb-request-body request)))
        (is (eq :delete (first request)))
        (is (equal "https://hydra.test/context" (second request)))
        (is (equal "db-test" (nlk:json-value body :string "tenant_id")))
        (is (equal "knowledge" (nlk:json-value body :string "kind")))
        (is (equal '("c1" "c2")
                   (coerce (nlk:json-value body :array "ids") 'list))))
      (setf *requests* '())
      (is-carrying (answer (hydradb-tool "hydradb_forget" (nlk:json-object)))
                   ("ids is required"))
      (is (null (hydradb-asked)) "nothing was sent"))))

(deftest hydradb-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-hydradb-wire
    (with-cell-stop ((hydradb-start "api_key" "k-test" "database" "db-test"
                                     "base_url" "https://hydra.test"))
      (hydradb-answer "{\"success\":false,\"error\":{\"code\":\"UNAUTHORIZED\",\"message\":\"Invalid API key\"}}")
      (is-carrying (answer (hydradb-tool "hydradb_recall" (nlk:json-object "query" "x")))
                   ("ERROR: hydradb:" "the refusal is the model's repair") ("Invalid API key"))
      (hydradb-answer "{\"success\":false,\"error\":{\"code\":\"UNAUTHORIZED\",\"message\":\"Invalid API key\"}}" 401)
      (is-carrying (answer (hydradb-tool "hydradb_recall" (nlk:json-object "query" "x")))
                   ("HTTP 401") ("Invalid API key")))))

;;; --- /hydradb -------------------------------------------------------------------

(deftest hydradb-cell-the-command-shows-the-backend-or-recalls ()
  (with-hydradb-wire
    (with-cell-stop ((hydradb-start "api_key" "k-test" "database" "db-test"
                                     "collection" "col-1" "base_url" "https://hydra.test"))
      (let ((answer (cell-entry "nodecode-hydradb" "hydradb" "")))
        (is (search "HydraDB memory: database \"db-test\", collection \"col-1\" at https://hydra.test"
                    answer))
        (is (search "capture on" answer))
        (is (null (hydradb-asked)) "a status read asks the backend nothing"))
      (hydradb-answer "{\"success\":true,\"data\":{\"chunks\":[{\"id\":\"c1\",\"text\":\"lives in Oslo\"}]}}")
      (is (search "[c1] lives in Oslo" (cell-entry "nodecode-hydradb" "hydradb" "where do I live")))
      (is (equal "where do I live"
                 (nlk:json-value (hydradb-request-body (first (hydradb-asked))) :string "query"))))))

;;; --- the turn capture ----------------------------------------------------------

(deftest hydradb-cell-a-finished-turn-becomes-one-ingest-request ()
  (with-hydradb-wire
    (with-cell-stop ((hydradb-start "api_key" "k-test" "database" "db-test"
                                     "base_url" "https://hydra.test"))
      (hydradb-answer "{\"success\":true,\"data\":{\"accepted\":1}}")
      ;; the turn's two texts arrive as their durable facts, as the gateway publishes them
      (cell-publish "nodecode-hydradb"
                    (cell-frame-op "turn.input_committed" :turn "t9"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "My name is Ada Lovelace. I drink green tea.")))
      (cell-publish "nodecode-hydradb"
                    (cell-frame-op "turn.assistant_message_completed" :turn "t9"
                                   :payload (nlk:json-object "message" "Noted, tea it is.")))
      (cell-publish "nodecode-hydradb" (cell-frame-op "turn.completed" :turn "t9"))
      (nodecode-hydradb::capture-step)
      (await (:timeout 5) (= 1 (length (hydradb-asked))))
      (let* ((request (first (hydradb-asked)))
             (item (hydradb-item request)))
        (is (eq :post (first request)) "the capture is one ingest")
        (is (equal "https://hydra.test/context/ingest" (second request)))
        (is (equal "db-test" (hydradb-request-part request "tenant_id")))
        (is (equal "knowledge" (hydradb-request-part request "kind")))
        (is (equal "conversation" (nlk:json-value item :string "title")))
        (is (equal "User: My name is Ada Lovelace. I drink green tea.
Assistant: Noted, tea it is."
                   (nlk:json-value item :string "content" "text")) "both texts, whole, as the original's sync_turn")
        (is (null (nlk:json-value item :boolean "infer")) "verbatim, as the original's sync_turn")))))
