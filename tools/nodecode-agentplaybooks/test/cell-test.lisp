;;;; cell-test.lisp --- lifecycle, the six tools, /agentplaybooks, and the recall.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong GUID, a non-https address and an integer under its floor
;;;; refused, a start with no key refused), the six tools through the seam the
;;;; model calls them by -- their URL, their headers, their body and their
;;;; answer -- /agentplaybooks with and without a query, the visibility check
;;;; that refuses a write to a playbook anyone may read, and the bounded block
;;;; a round carries. Every request is a stubbed NLK:HTTP, so nothing reaches
;;;; AgentPlaybooks or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defparameter +guid+ "4c1f9a2b-3d4e-4f60-8a71-9b2c3d4e5f60"
  "The playbook every test but the refusals runs against.")

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-apb-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials AgentPlaybooks."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected AgentPlaybooks request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun apb-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun apb-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun apb-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun apb-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun apb-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun apb-start (&rest pairs)
  "The cell started against the test playbook, with PAIRS over it."
  (apply #'agentplaybooks-start
         (append (list "playbook_guid" +guid+ "api_key" "k-test" "base_url" "https://apb.test")
                 pairs)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "agentplaybooks" "AGENTPLAYBOOKS-CELL-" :start nodecode-agentplaybooks:start-cell)

(define-cell-lifecycle-tests "agentplaybooks"
  (:fixture with-apb-wire)
  (:config "playbook_guid" +guid+ "api_key" "k-test" "base_url" "https://apb.test")
  (:hooks :round)
  (:help :agentplaybooks)
  (:command "agentplaybooks")
  (:running (is (nle::find-tool "apb_memory_search") "apb_memory_search is registered")
            (is (nle::find-tool "apb_memory_read"))
            (is (nle::find-tool "apb_memory_write"))
            (is (nle::find-tool "apb_memory_archive"))
            (is (nle::find-tool "apb_memory_delete"))
            (is (nle::find-tool "apb_memory_history"))
            (is (equal "https://apb.test" (nodecode-agentplaybooks::root)))
            (is (equal +guid+ (nodecode-agentplaybooks::source-guid
                                nodecode-agentplaybooks::*personal*)) "the personal playbook")
            (is (= 1 (length nodecode-agentplaybooks::*sources*)) "and no other source")
            (is (= 15 (nodecode-agentplaybooks::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "apb_memory_search")) "stop unregisters the tools")
            (is (null nodecode-agentplaybooks::*sources*) "and forgets the playbooks")
            (is (null nodecode-agentplaybooks::*personal*)))
  (:refused ("playbook_guid" +guid+)
            ("playbook_guid" "not-a-guid" "api_key" "k-test")
            ("playbook_guid" +guid+ "api_key" "k-test" "base_url" "http://agentplaybooks.example")
            ("playbook_guid" +guid+ "api_key" "k-test" "timeout_seconds" 0))
  (:idle nodecode-agentplaybooks:agentplaybooks-error
         (nodecode-agentplaybooks::run-command "" nil)))

(deftest agentplaybooks-cell-a-keyless-section-is-refused ()
  (with-apb-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (agentplaybooks-start "playbook_guid" +guid+)))
                   ("needs a playbook-scoped memory key") ("AGENTPLAYBOOKS_MEMORY_API_KEY"))
      (is (null (nle::find-tool "apb_memory_search")) "a refused start installed nothing"))))

;;; --- the tools ------------------------------------------------------------------

(deftest agentplaybooks-cell-search-answers-what-the-playbook-holds ()
  (with-apb-wire
    (with-cell-stop ((apb-start))
      (apb-answer "[{\"key\":\"notes.tea\",\"value\":\"drinks green tea\",\"tier\":\"longterm\",\"summary\":\"what the operator drinks\"},{\"key\":\"notes.oslo\",\"value\":\"lives in Oslo\"}]")
      (let ((answer (apb-tool "apb_memory_search" (nlk:json-object "query" "tea" "limit" 3))))
        (is (search "2 found for \"tea\"" answer))
        (is (search "notes.tea [longterm] — what the operator drinks" answer))
        (is (search "notes.oslo" answer)))
      (let ((request (first (apb-asked))))
        (is (= 1 (length (apb-asked))) "one request")
        (is (eq :get (first request)))
        (is (equal (format nil "https://apb.test/api/playbooks/~a/memory?search=tea&limit=3&offset=0" +guid+)
                   (second request)))
        (is (equal "k-test" (apb-request-header request "x-api-key")))
        (is (equal "application/json" (apb-request-header request "accept"))))
      (apb-answer "[]")
      (is (equal "No memory matched \"nothing\"."
                 (apb-tool "apb_memory_search" (nlk:json-object "query" "nothing")))))))

(deftest agentplaybooks-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-apb-wire
    (with-cell-stop ((apb-start))
      (apb-answer "{\"message\":\"Invalid API key\"}" 401)
      (is-carrying (answer (apb-tool "apb_memory_search" (nlk:json-object "query" "x")))
                   ("ERROR: agentplaybooks:") ("HTTP 401") ("Invalid API key")))))

(deftest agentplaybooks-cell-a-write-checks-visibility-first ()
  (with-apb-wire
    (with-cell-stop ((apb-start))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore args))
              (cond ((equal (format nil "https://apb.test/api/playbooks/~a" +guid+) url)
                     (values "" 404))
                    ((eq :put method) (values "{}" 200))
                    (t (values "[]" 200)))))
      (is (equal "Stored notes.deploy in personal memory; earlier contents stay in its history."
                 (apb-tool "apb_memory_write"
                           (nlk:json-object "key" "notes.deploy" "tier" "longterm"
                                            "summary" "how a deploy runs"
                                            "value" (nlk:json-object "steps" (vector "build" "ship"))))))
      (let ((asked (apb-asked)))
        (is (= 4 (length asked)) "visibility, the key check, the old entry, the write")
        (is (search (format nil "/api/playbooks/~a" +guid+) (second (first asked))))
        (is (null (apb-request-header (first asked) "x-api-key")) "the metadata read is anonymous")
        (is (eq :put (first (car (last asked))))))
      (let* ((put (car (last (apb-asked))))
             (body (apb-request-body put)))
        (is (equal (format nil "https://apb.test/api/playbooks/~a/memory/notes.deploy" +guid+)
                   (second put)))
        (is (equal "longterm" (nlk:json-value body :string "tier")))
        (is (equal "how a deploy runs" (nlk:json-value body :string "summary")))
        (is (equal "build" (aref (nlk:json-value body :array "value" "steps") 0)) "the value is the JSON it was given")
        (is (equal "nodecode" (nlk:json-value body :string "metadata" "source")))
        (is (not (nlk:json-value body :boolean "is_archived"))))
      ;; a playbook anyone may read is refused, and nothing is written
      (setf *requests* '())
      (apb-answer "{\"name\":\"a public playbook\"}")
      (is-carrying (answer (apb-tool "apb_memory_write"
                                      (nlk:json-object "key" "notes.tea" "value" "oolong")))
                   ("private playbook"))
      (is (null (find :put (apb-asked) :key #'first)) "only the visibility read happened"))))

(deftest agentplaybooks-cell-read-archive-delete-and-history ()
  (with-apb-wire
    (with-cell-stop ((apb-start))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore args))
              (cond ((equal (format nil "https://apb.test/api/playbooks/~a" +guid+) url)
                     (values "" 404))
                    ((search "history_key=" url)
                     (values "[{\"key\":\"notes.tea\",\"history_id\":\"h1\",\"value\":\"oolong\",\"summary\":\"before the correction\"}]" 200))
                    ((search "/memory/notes.tea" url)
                     (case method
                       (:get (values "[{\"key\":\"notes.tea\",\"value\":\"green\",\"tier\":\"longterm\",\"summary\":\"what the operator drinks\",\"memory_at\":\"2026-01-02T00:00:00Z\"}]" 200))
                       (:put (values "{}" 200))
                       (:delete (values "{}" 200))))
                    (t (values "[]" 200)))))
      (let ((answer (apb-tool "apb_memory_read" (nlk:json-object "key" "notes.tea"))))
        (is (search "key: notes.tea" answer))
        (is (search "value: green" answer))
        (is (search "memory_at: 2026-01-02T00:00:00Z" answer)))
      (is (search "1 earlier version of notes.tea" (apb-tool "apb_memory_history" (nlk:json-object "key" "notes.tea"))))
      (is (search "Archived notes.tea" (apb-tool "apb_memory_archive" (nlk:json-object "key" "notes.tea"))))
      (let ((put (car (last (apb-asked)))))
        (is (eq :put (first put)))
        (is (nlk:json-value (apb-request-body put) :boolean "is_archived") "archived, not deleted")
        (is (equal "green" (nlk:json-value (apb-request-body put) :string "value")) "the content is kept")
        (is (equal "2026-01-02T00:00:00Z" (nlk:json-value (apb-request-body put) :string "memory_at")) "and its memory time"))
      (is (search "Deleted notes.tea" (apb-tool "apb_memory_delete" (nlk:json-object "key" "notes.tea"))))
      (is (eq :delete (first (car (last (apb-asked)))))))))

;;; --- /agentplaybooks ---------------------------------------------------------------

(deftest agentplaybooks-cell-the-command-shows-the-playbook-or-searches-it ()
  (with-apb-wire
    (with-cell-stop ((apb-start))
      (let ((answer (cell-entry "nodecode-agentplaybooks" "agentplaybooks" "")))
        (is (search (format nil "private playbook ~a at https://apb.test" +guid+) answer))
        (is (search "/agentplaybooks QUERY searches" answer))
        (is (null (apb-asked)) "a status read asks the service nothing"))
      (apb-answer "[{\"key\":\"notes.tea\",\"value\":\"green\"}]")
      (is (search "notes.tea" (cell-entry "nodecode-agentplaybooks" "agentplaybooks" "tea")))
      (is (equal (format nil "/api/playbooks/~a/memory?search=tea&limit=20" +guid+)
                 (let ((url (second (first (apb-asked)))))
                   (subseq url (length "https://apb.test"))))))))

;;; --- the recall --------------------------------------------------------------------

(deftest agentplaybooks-cell-the-recall-is-bounded-and-framed ()
  (with-apb-wire
    (with-cell-stop ((apb-start))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method args))
              (cond ((search "search=deploy" url)
                     (values "[{\"key\":\"notes.deploy\",\"value\":\"the deploy runs on fridays\",\"summary\":\"deploy day\"},{\"key\":\"notes.old\",\"value\":\"deploy\",\"is_archived\":true}]" 200))
                    (t (values "[]" 200)))))
      (let ((block (nodecode-agentplaybooks::recall-block "when is the deploy window?")))
        (is (search "AgentPlaybooks recalled memory" block) "the block says what it is")
        (is (search "notes.deploy — deploy day" block))
        (is (null (search "notes.old" block)) "an archived entry is not injected"))
      (is (null (nodecode-agentplaybooks::recall-block "a is of the")) "a query of stop words recalls nothing")
      (is (null (nodecode-agentplaybooks::recall-block "nothing matches this")) "and so does a miss"))))
