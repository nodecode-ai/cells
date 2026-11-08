;;;; cell-test.lisp --- lifecycle, the four tools, /mem0, and the turn capture.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong choice and an integer under its floor refused, a start
;;;; that could reach no backend refused), the four tools through the seam
;;;; the model calls them by -- their URL, their body and their answer --
;;;; /mem0 with and without a query, and a finished turn going from the
;;;; :frame point to one extraction request. Every request is a stubbed
;;;; NLK:HTTP, so nothing reaches Mem0 or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-mem0-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Mem0."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected mem0 request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun mem0-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun mem0-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun mem0-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun mem0-message (body index)
  "BODY's INDEXth wire message, decoded."
  (let ((messages (nlk:json-value body :array "messages")))
    (and messages (< index (length messages)) (aref messages index))))

(defun mem0-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun mem0-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "mem0" "MEM0-CELL-" :start nodecode-mem0:start-cell)

(define-cell-lifecycle-tests "mem0"
  (:fixture with-mem0-wire)
  (:config "api_key" "k-test" "base_url" "https://mem0.test")
  (:help :mem0)
  (:command "mem0")
  (:running (is (nle::find-tool "mem0_search") "mem0_search is registered")
            (is (nle::find-tool "mem0_add"))
            (is (nle::find-tool "mem0_update"))
            (is (nle::find-tool "mem0_delete"))
            (is (equal "https://mem0.test" (nodecode-mem0::root)))
            (is (equal "nodecode-user" (nodecode-mem0::setting :user-id)) "the default user")
            (is (= 10 (nodecode-mem0::setting :search-limit)))
            (is nodecode-mem0::*worker* "the capture worker runs"))
  (:stopped (is (null (nle::find-tool "mem0_search")) "stop unregisters the tools")
            (is (null nodecode-mem0::*worker*) "and the worker is gone"))
  (:refused ("mode" "carrier-pigeon") ("search_limit" 0) ("max_chars" 5) ("timeout_seconds" 0))
  (:idle nodecode-mem0:mem0-error (nodecode-mem0::run-command "" nil)))

(deftest mem0-cell-platform-mode-needs-a-key ()
  (with-mem0-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (mem0-start "base_url" "https://mem0.test")))
                   ("platform mode needs a key" "MEM0_API_KEY"))
      (is (null (nle::find-tool "mem0_search")) "a refused start installed nothing"))))

;;; --- the tools ------------------------------------------------------------------

(deftest mem0-cell-search-answers-the-nearest-facts ()
  (with-mem0-wire
    (with-cell-stop ((mem0-start "api_key" "k-test" "base_url" "https://mem0.test" "rerank" t))
      (mem0-answer "{\"results\":[{\"id\":\"m1\",\"memory\":\"drinks tea, never coffee\",\"score\":0.912},{\"id\":\"m2\",\"memory\":\"lives in Oslo\"}]}")
      (let ((answer (mem0-tool "mem0_search"
                                (nlk:json-object "query" "what do I drink" "top_k" 3))))
        (is (search "2 memories for \"what do I drink\"" answer))
        (is (search "[m1] drinks tea, never coffee (score 0.91)" answer))
        (is (search "[m2] lives in Oslo" answer)))
      (let ((request (first (mem0-asked))))
        (is (= 1 (length (mem0-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://mem0.test/v3/memories/search/" (second request)))
        (is (equal "Token k-test" (mem0-request-header request "authorization")))
        (let ((body (mem0-request-body request)))
          (is (equal "what do I drink" (nlk:json-value body :string "query")))
          (is (= 3 (nlk:json-value body :integer "top_k")) "the call's top_k")
          (is (eq t (nlk:json-value body :boolean "rerank")) "the section's rerank is the default")
          (is (equal "nodecode-user" (nlk:json-value body :string "filters" "user_id"))))))))

(deftest mem0-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-mem0-wire
    (with-cell-stop ((mem0-start "api_key" "k-test" "base_url" "https://mem0.test"))
      (mem0-answer "{\"message\":\"Invalid API key\"}" 401)
      (is-carrying (answer (mem0-tool "mem0_search" (nlk:json-object "query" "x")))
                   ("ERROR: mem0:" "the refusal is the model's repair") ("HTTP 401") ("Invalid API key"))
      (mem0-answer "{\"results\":[]}")
      (is (equal "No memory matched \"x\"."
                 (mem0-tool "mem0_search" (nlk:json-object "query" "x")))))))

(deftest mem0-cell-add-stores-one-fact-verbatim ()
  (with-mem0-wire
    (with-cell-stop ((mem0-start "api_key" "k-test" "base_url" "https://mem0.test"
                                  "user_id" "u-7" "agent_id" "laptop"))
      (mem0-answer "{\"event_id\":\"evt-1\"}")
      (is (equal "Queued for storage. (event evt-1)"
                 (mem0-tool "mem0_add" (nlk:json-object "content" "prefers window seats"))))
      (let* ((request (first (mem0-asked)))
             (body (mem0-request-body request))
             (message (mem0-message body 0)))
        (is (equal "https://mem0.test/v3/memories/add/" (second request)))
        (is (null (nlk:json-value body :boolean "infer")) "verbatim: no extraction")
        (is (equal "u-7" (nlk:json-value body :string "user_id")))
        (is (equal "laptop" (nlk:json-value body :string "agent_id")))
        (is (equal "prefers window seats" (nlk:json-value message :string "content")))
        (is (equal "nodecode" (nlk:json-value body :string "metadata" "channel")))))))

(deftest mem0-cell-update-and-delete-own-the-memory-they-touch ()
  (with-mem0-wire
    (with-cell-stop ((mem0-start "api_key" "k-test" "base_url" "https://mem0.test"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore args))
              (case method
                (:get (if (search "someone-else" url)
                          (values "{\"id\":\"someone-else\",\"user_id\":\"someone-else\"}" 200)
                          (values "{\"id\":\"m1\",\"user_id\":\"nodecode-user\"}" 200)))
                (:put (values "{}" 200))
                (:delete (if (search "m3" url) (values "{\"message\":\"boom\"}" 500) (values "{}" 200))))))
      (is (equal "Updated memory m1."
                 (mem0-tool "mem0_update"
                            (nlk:json-object "memory_id" "m1" "text" "drinks green tea"))))
      (let ((put (second (mem0-asked))))
        (is (eq :put (first put)))
        (is (equal "https://mem0.test/v1/memories/m1/" (second put)))
        (is (equal "drinks green tea" (nlk:json-value (mem0-request-body put) :string "text"))))
      (is (equal "Deleted memory m2."
                 (mem0-tool "mem0_delete" (nlk:json-object "memory_id" "m2"))))
      (is (eq :delete (first (car (last (mem0-asked))))) "the delete came last")
      (setf *requests* '())
      (is-carrying (answer (mem0-tool "mem0_update"
                                       (nlk:json-object "memory_id" "someone-else" "text" "mine now")))
                   ("belongs to \"someone-else\""))
      (is (= 1 (length (mem0-asked))) "only the ownership read: nothing was written")
      (setf *requests* '())
      (is-carrying (answer (mem0-tool "mem0_delete" (nlk:json-object "memory_id" "m3")))
                   ("HTTP 500" "boom")))))

;;; --- /mem0 ---------------------------------------------------------------------

(deftest mem0-cell-the-command-shows-the-backend-or-searches-it ()
  (with-mem0-wire
    (with-cell-stop ((mem0-start "api_key" "k-test" "base_url" "https://mem0.test" "user_id" "u-7"))
      (let ((answer (cell-entry "nodecode-mem0" "mem0" "")))
        (is (search "Mem0 memory: platform at https://mem0.test, user \"u-7\"" answer))
        (is (null (mem0-asked)) "a status read asks the backend nothing"))
      (mem0-answer "{\"results\":[{\"id\":\"m1\",\"memory\":\"lives in Oslo\"}]}")
      (is (search "[m1] lives in Oslo" (cell-entry "nodecode-mem0" "mem0" "where do I live")))
      (is (equal "where do I live"
                 (nlk:json-value (mem0-request-body (first (mem0-asked))) :string "query"))))))

;;; --- the turn capture ----------------------------------------------------------

(deftest mem0-cell-a-finished-turn-becomes-one-extraction-request ()
  (with-mem0-wire
    (with-cell-stop ((mem0-start "api_key" "k-test" "base_url" "https://mem0.test"
                                  "max_chars" 50 "agent_id" "laptop"))
      (mem0-answer "{\"event_id\":\"evt-9\"}")
      ;; the turn's two texts arrive as their durable facts, as the gateway publishes them
      (cell-publish "nodecode-mem0"
                    (cell-frame-op "turn.input_committed" :turn "t9"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "My name is Ada Lovelace. I live in Oslo and drink green tea.")))
      (cell-publish "nodecode-mem0"
                    (cell-frame-op "turn.assistant_message_completed" :turn "t9"
                                   :payload (nlk:json-object "message" "Noted, Oslo and tea.")))
      (cell-publish "nodecode-mem0" (cell-frame-op "turn.completed" :turn "t9"))
      (nodecode-mem0::capture-step)
      (await (:timeout 5) (= 1 (length (mem0-asked))))
      (let* ((request (first (mem0-asked)))
             (body (mem0-request-body request)))
        (is (equal "https://mem0.test/v3/memories/add/" (second request)) "the extraction goes to add")
        (is (eq t (nlk:json-value body :boolean "infer")) "extraction, not a verbatim write")
        (is (equal "laptop" (nlk:json-value body :string "agent_id")))
        (is (equal "My name is Ada Lovelace."
                   (nlk:json-value (mem0-message body 0) :string "content")) "cut at a sentence end")
        (is (equal "Noted, Oslo and tea."
                   (nlk:json-value (mem0-message body 1) :string "content")) "the answer, whole")))))
