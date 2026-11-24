;;;; cell-test.lisp --- lifecycle, the six tools, the recall, /dexio.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a member refused, a missing key refused), the six tools through
;;;; the seam the model calls them by -- their URL, their JSON-RPC body, the
;;;; answer they render -- the recall that files the wiki's pages as the
;;;; turn's context, and /dexio. Every request is a stubbed NLK:HTTP, so
;;;; nothing reaches a wiki or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-dexio-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials a wiki."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected dexio request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun dexio-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun dexio-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun dexio-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun dexio-call-name (request)
  "The MCP tool REQUEST calls, as its JSON-RPC body names it."
  (nlk:json-value (dexio-request-body request) :string "params" "name"))

(defun dexio-call-arguments (request)
  "The arguments REQUEST carries, decoded."
  (nlk:json-value (dexio-request-body request) :object "params" "arguments"))

(defun dexio-reply (text &optional page)
  "A JSON-RPC reply whose content carries TEXT, and PAGE as a second item when given."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 1
                     "result"
                     (nlk:json-object
                      "content" (coerce (append (list (nlk:json-object "type" "text" "text" text))
                                                 (when page
                                                   (list (nlk:json-object "type" "text" "text" page))))
                                        'vector)))))

(defun dexio-answer (text &optional page)
  "Answer every request with a reply carrying TEXT, and PAGE as the second item."
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values (dexio-reply text page) 200))))

(defun dexio-status (status &optional (text "{}"))
  "Answer every request with STATUS and TEXT."
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun dexio-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "dexio" "DEXIO-CELL-" :start nodecode-dexio:start-cell)

(define-cell-lifecycle-tests "dexio"
  (:fixture with-dexio-wire)
  (:config "api_key" "dxk-test" "url" "https://dexio.test")
  (:help :dexio)
  (:command "dexio")
  (:hooks :round 'nle::read-harness-sections)
  (:running (is (nle::find-tool "dexio_search") "dexio_search is registered")
            (is (nle::find-tool "dexio_read"))
            (is (nle::find-tool "dexio_list"))
            (is (nle::find-tool "dexio_write"))
            (is (nle::find-tool "dexio_edit"))
            (is (nle::find-tool "dexio_append"))
            (is (equal "https://dexio.test" (nodecode-dexio::root)))
            (is (equal "https://dexio.test/mcp" (nodecode-dexio::endpoint)))
            (is (equal "nodecode" (nodecode-dexio::agent-name)) "the default agent name")
            (is (= 4 (nodecode-dexio::result-limit))))
  (:stopped (is (null (nle::find-tool "dexio_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "dexio_append"))))
  (:refused ("max_results" 0) ("recall_chars" 5) ("timeout_seconds" 0))
  (:idle nodecode-dexio:dexio-error (nodecode-dexio::setting :timeout-seconds)))

(deftest dexio-cell-a-key-is-required ()
  (with-dexio-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (dexio-start "url" "https://dexio.test")))
                   ("needs an API key" "DEXIO_API_KEY"))
      (is (null (nle::find-tool "dexio_search")) "a refused start installed nothing"))))

;;; --- the tools ------------------------------------------------------------------

(deftest dexio-cell-search-asks-the-wiki-and-answers-its-lines ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"))
      (dexio-answer
       (nlk:encode-json-object
        (nlk:json-object
         "results"
         (vector (nlk:json-object
                  "path" "decisions/pricing"
                  "title" "Pricing decisions"
                  "matches" (vector (nlk:json-object "text" "title: Pricing decisions")
                                    (nlk:json-object "text" "we charge per seat")))))))
      (let ((answer (dexio-tool "dexio_search"
                                 (nlk:json-object "query" "what do we charge" "limit" 2))))
        (is (search "- Pricing decisions (`decisions/pricing`)" answer))
        (is (search "    we charge per seat" answer))
        (is (null (search "title: Pricing decisions" answer)) "the page's own title line is not repeated"))
      (let ((request (first (dexio-asked))))
        (is (= 1 (length (dexio-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://dexio.test/mcp" (second request)))
        (is (equal "Bearer dxk-test" (dexio-request-header request "authorization")))
        (is (equal "tools/call" (nlk:json-value (dexio-request-body request) :string "method")))
        (is (equal "search_pages" (dexio-call-name request)))
        (let ((arguments (dexio-call-arguments request)))
          (is (equal "what do we charge" (nlk:json-value arguments :string "query")))
          (is (= 2 (nlk:json-value arguments :integer "limit")) "the call's limit")
          (is (= 3 (nlk:json-value arguments :integer "matches_per_page"))))))))

(deftest dexio-cell-read-takes-the-page-from-the-second-item ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"))
      (dexio-answer "{\"path\":\"decisions/pricing\",\"title\":\"Pricing\",\"version\":\"v3\"}"
                    "# Pricing\n\nWe charge per seat.")
      (is-carrying (answer (dexio-tool "dexio_read"
                                        (nlk:json-object "path" "decisions/pricing")))
                   ("decisions/pricing (version v3)") ("We charge per seat."))
      (is (equal "read_page" (dexio-call-name (first (dexio-asked)))))
      ;; dexio_list answers one line a page
      (dexio-answer "{\"pages\":[{\"path\":\"decisions/pricing\",\"title\":\"Pricing\",\"description\":\"per seat\"}]}")
      (is-carrying (answer (dexio-tool "dexio_list" (nlk:json-object)))
                   ("1 page:") ("- Pricing (decisions/pricing) -- per seat")))))

(deftest dexio-cell-writes-carry-the-agent-and-the-note ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"
                                   "agent" "laptop"))
      (dexio-answer "{\"path\":\"decisions/pricing\",\"version\":\"4\"}")
      (is (equal "Wrote decisions/pricing (version 4)"
                 (dexio-tool "dexio_write" (nlk:json-object "path" "decisions/pricing"
                                                             "text" "# Pricing"
                                                             "note" "settled per seat"))))
      (let ((arguments (dexio-call-arguments (first (dexio-asked)))))
        (is (equal "write_page" (dexio-call-name (first (dexio-asked)))))
        (is (equal "laptop" (nlk:json-value arguments :string "agent")) "the section's agent name")
        (is (equal "settled per seat" (nlk:json-value arguments :string "note"))))
      ;; an edit whose new_text is absent is refused rather than treated as a deletion
      (is-carrying (answer (dexio-tool "dexio_edit" (nlk:json-object "path" "decisions/pricing"
                                                                      "old_text" "per seat")))
                   ("ERROR: dexio:" "new_text is required"))
      (is (= 1 (length (dexio-asked))) "the refused edit asked nothing")
      (is (equal "Edited decisions/pricing (version 4)"
                 (dexio-tool "dexio_edit" (nlk:json-object "path" "decisions/pricing"
                                                            "old_text" "per seat"
                                                            "new_text" ""))))
      (let ((arguments (dexio-call-arguments (second (dexio-asked)))))
        (is (equal "edit_page" (dexio-call-name (second (dexio-asked)))))
        (is (equal "" (nlk:json-value arguments :string "new_text")) "an empty replacement deletes")
        (is (null (nlk:json-value arguments :string "note")) "no note, no member"))
      (is (equal "Appended to log/decisions (version 5)"
                 (progn (dexio-answer "{\"path\":\"log/decisions\",\"version\":\"5\"}")
                        (dexio-tool "dexio_append" (nlk:json-object "path" "log/decisions"
                                                                     "text" "- moved to per seat")))))
      (is (equal "append_page" (dexio-call-name (car (last (dexio-asked)))))))))

(deftest dexio-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"))
      (dexio-status 401 "{\"error\":\"bad key\"}")
      (is-carrying (answer (dexio-tool "dexio_search" (nlk:json-object "query" "x")))
                   ("ERROR: dexio:" "the refusal is the model's repair") ("401"))
      (dexio-answer "{\"results\":[]}")
      (is (equal "No page of the wiki matched \"x\"."
                 (dexio-tool "dexio_search" (nlk:json-object "query" "x")))))))

;;; --- the recall ------------------------------------------------------------------

(deftest dexio-cell-recall-puts-the-matching-pages-in-front-of-the-request ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"))
      (dexio-answer
       (nlk:encode-json-object
        (nlk:json-object "results"
                         (vector (nlk:json-object "path" "decisions/pricing"
                                                   "title" "Pricing"
                                                   "matches" (vector (nlk:json-object
                                                                      "text" "we charge per seat")))))))
      (let ((recorded '()))
        (with-stubbed-fdefinitions
            ((nlk:session-source-kind (session) (declare (ignore session)) "chat")
             (nlk:store-open-p () t)
             (nodecode-dexio::turn-input (session)
               (declare (ignore session))
               "How do we price a seat, again?")
             (nlk:record-turn-context-prompt (turn &key text)
               (push (list turn text) recorded)))
          (nodecode-dexio::recall-at-round (list :session-id "s1" :turn "t1" :round 0))
          (is (= 1 (length recorded)) "the recall filed one block")
          (is (equal "t1" (first (first recorded))))
          (let ((filed (second (first recorded))))
            (is-carrying filed
                          ("## From your Dexio wiki") ("- Pricing (`decisions/pricing`)")
                          ("    we charge per seat")))
          (let ((arguments (dexio-call-arguments (first (dexio-asked)))))
            (is (equal "search_pages" (dexio-call-name (first (dexio-asked)))))
            (is (equal "How do we price a seat, again?" (nlk:json-value arguments :string "query")))
            (is (= 4 (nlk:json-value arguments :integer "limit")) "the section's max_results"))
          ;; the second request of the same turn asks the wiki once, not twice
          (nodecode-dexio::recall-at-round (list :session-id "s1" :turn "t1" :round 1))
          (is (= 1 (length (dexio-asked))) "a later round recalls nothing new"))))))

(deftest dexio-cell-recall-off-asks-the-wiki-nothing ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"
                                   "auto_recall" nil))
      (with-stubbed-fdefinitions
          ((nlk:session-source-kind (session) (declare (ignore session)) "chat")
           (nlk:store-open-p () t)
           (nodecode-dexio::turn-input (session)
             (declare (ignore session)) "How do we price a seat?")
           (nlk:record-turn-context-prompt (turn &key text)
             (declare (ignore turn text))
             (error "nothing should be filed")))
        (nodecode-dexio::recall-at-round (list :session-id "s1" :turn "t1" :round 0))
        (is (null (dexio-asked)) "auto_recall off leaves the recall to the tools")))))

;;; --- /dexio ---------------------------------------------------------------------

(deftest dexio-cell-the-command-shows-the-wiki-or-searches-it ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"))
      (dexio-answer "{\"total\":42}")
      (let ((status (cell-entry "nodecode-dexio" "dexio" "")))
        (is-carrying status ("Dexio wiki at https://dexio.test, 42 pages.")))
      (is (equal "list_pages" (dexio-call-name (first (dexio-asked)))) "a status read asks for one page")
      (setf *requests* '())
      (dexio-answer
       (nlk:encode-json-object
        (nlk:json-object "results" (vector (nlk:json-object "path" "a/b" "title" "B")))))
      (let ((found (cell-entry "nodecode-dexio" "dexio" "billing")))
        (is-carrying found ("- B (`a/b`)")))
      (is (equal "billing" (nlk:json-value (dexio-call-arguments (first (dexio-asked)))
                                            :string "query"))))))

(deftest dexio-cell-the-harness-section-names-the-wiki ()
  (with-dexio-wire
    (with-cell-stop ((dexio-start "api_key" "dxk-test" "url" "https://dexio.test"))
      (let ((sections (nle::read-harness-sections "s1"))
            (entry nil))
        (setf entry (assoc "dexio" (nle::kept-sections sections) :test #'equal))
        (is entry "Dexio's paragraph is in the harness sections")
        (is (search "https://dexio.test" (cdr entry)))
        (is (search "dexio_write" (cdr entry)))))))
