;;;; cell-test.lisp --- lifecycle, the four tools, /supermemory, and the turn capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong choice and an integer under its floor refused, a start that
;;;; could reach no service refused), the four tools through the seam the model
;;;; calls them by -- their URL, their body, their headers and their answer --
;;;; /supermemory with and without a query, and a finished turn going from the
;;;; :frame point to one conversation document. Every request is a stubbed
;;;; NLK:HTTP, so nothing reaches Supermemory or the network.

(in-package #:nodecode.test)

;;; --- fixtures -------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-supermemory-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered by
*ANSWER-FN*, so no test dials Supermemory."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected supermemory request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun sm-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun sm-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun sm-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun sm-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun sm-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ------------------------------------------------------------------

(define-test-slice "supermemory" "SUPERMEMORY-CELL-" :start nodecode-supermemory:start-cell)

(define-cell-lifecycle-tests "supermemory"
  (:fixture with-supermemory-wire)
  (:config "api_key" "k-test" "base_url" "https://sm.test")
  (:hooks :frame)
  (:help :supermemory)
  (:command "supermemory")
  (:running
   (is (nle::find-tool "supermemory_store") "supermemory_store is registered")
   (is (nle::find-tool "supermemory_search"))
   (is (nle::find-tool "supermemory_forget"))
   (is (nle::find-tool "supermemory_profile"))
   (is (equal "https://sm.test" (nodecode-supermemory::root)))
   (is (equal "nodecode" (nodecode-supermemory::container-tag)) "the default container")
   (is (equal "hybrid" (nodecode-supermemory::setting :search-mode)))
   (is (= 10 (nodecode-supermemory::setting :search-limit)))
   (is nodecode-supermemory::*worker* "the capture worker runs"))
  (:stopped
   (is (null (nle::find-tool "supermemory_store")) "stop unregisters the tools")
   (is (null nodecode-supermemory::*worker*) "and the worker is gone"))
  (:refused ("search_limit" 0) ("timeout_seconds" 0) ("search_mode" "everything"))
  (:idle nodecode-supermemory:supermemory-error
         (nodecode-supermemory::run-command "" nil)))

(deftest supermemory-cell-a-missing-key-is-refused ()
  (with-supermemory-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (text (refusal-text nlk:config-refusal
                                       (supermemory-start "base_url" "https://sm.test")))
                   ("needs an API key" "the refusal names the key") ("SUPERMEMORY_API_KEY"))
      (is (null (nle::find-tool "supermemory_search")) "a refused start installed nothing"))))

;;; --- the tools -------------------------------------------------------------------

(deftest supermemory-cell-search-answers-the-nearest-memories ()
  (with-supermemory-wire
    (with-cell-stop ((supermemory-start "api_key" "k-test" "base_url" "https://sm.test"))
      (sm-answer "{\"results\":[{\"id\":\"m1\",\"memory\":\"drinks tea, never coffee\",\"similarity\":0.912},{\"id\":\"m2\",\"memory\":\"lives in Oslo\"}]}")
      (let ((answer (sm-tool "supermemory_search"
                              (nlk:json-object "query" "what do I drink" "limit" 3))))
        (is (search "2 memories in container \"nodecode\" for \"what do I drink\"" answer))
        (is (search "[m1] drinks tea, never coffee (similarity 0.91)" answer))
        (is (search "[m2] lives in Oslo" answer)))
      (let ((request (first (sm-asked))))
        (is (= 1 (length (sm-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://sm.test/v3/search" (second request)))
        (is (equal "Bearer k-test" (sm-request-header request "authorization")))
        (is (equal "nodecode" (sm-request-header request "x-sm-source")))
        (let ((body (sm-request-body request)))
          (is (equal "what do I drink" (nlk:json-value body :string "q")))
          (is (equal "nodecode" (nlk:json-value body :string "containerTag")))
          (is (= 3 (nlk:json-value body :integer "limit")) "the call's limit")
          (is (equal "hybrid" (nlk:json-value body :string "searchMode"))))))))

(deftest supermemory-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-supermemory-wire
    (with-cell-stop ((supermemory-start "api_key" "k-test" "base_url" "https://sm.test"))
      (sm-answer "{\"error\":\"Invalid API key\"}" 401)
      (is-carrying (text (sm-tool "supermemory_search" (nlk:json-object "query" "x")))
                   ("ERROR: supermemory:" "the refusal is the model's repair")
                   ("HTTP 401") ("Invalid API key"))
      (sm-answer "{\"results\":[]}")
      (is (equal "No memory matched \"x\" in container \"nodecode\"."
                 (sm-tool "supermemory_search" (nlk:json-object "query" "x")))))))

(deftest supermemory-cell-store-writes-one-document ()
  (with-supermemory-wire
    (with-cell-stop ((supermemory-start "api_key" "k-test" "base_url" "https://sm.test"
                                         "container_tag" "team work"))
      (sm-answer "{\"id\":\"doc-7\",\"status\":\"queued\"}")
      (is (search "doc-7" (sm-tool "supermemory_store"
                                     (nlk:json-object "content" "prefers window seats"
                                                      "metadata" (nlk:json-object "topic" "travel")))))
      (let* ((request (first (sm-asked)))
             (body (sm-request-body request))
             (tags (nlk:json-value body :array "containerTags"))
             (metadata (nlk:json-value body :object "metadata")))
        (is (eq :post (first request)))
        (is (equal "https://sm.test/v3/documents" (second request)))
        (is (equal "prefers window seats" (nlk:json-value body :string "content")))
        (is (= 1 (length tags)) "one container tag")
        (is (equal "team_work" (aref tags 0)) "the tag is sanitized as the original does")
        (is (equal "nodecode" (nlk:json-value metadata :string "sm_source")) "the source stamp")
        (is (equal "travel" (nlk:json-value metadata :string "topic")) "and the caller's own metadata")
        (is (null (nlk:json-value body :string "entityContext")) "no entityContext on a store")))))

(deftest supermemory-cell-forget-by-id-and-by-match ()
  (with-supermemory-wire
    (with-cell-stop ((supermemory-start "api_key" "k-test" "base_url" "https://sm.test"))
      (sm-answer "{}")
      (is (search "m1" (sm-tool "supermemory_forget" (nlk:json-object "id" "m1"))))
      (let ((request (first (sm-asked))))
        (is (eq :delete (first request)))
        (is (equal "https://sm.test/v4/memories" (second request)))
        (is (equal "m1" (nlk:json-value (sm-request-body request) :string "id")))
        (is (equal "nodecode" (nlk:json-value (sm-request-body request) :string "containerTag"))))
      (setf *requests* '())
      (sm-answer "{\"results\":[{\"id\":\"m2\",\"memory\":\"lives in Bergen\",\"similarity\":0.8}]}")
      (is (search "Bergen" (sm-tool "supermemory_forget"
                                      (nlk:json-object "query" "where do I live"))))
      (is (= 2 (length (sm-asked))) "a search, then the forget")
      (is (eq :post (first (first (sm-asked)))) "the search came first")
      (is (eq :delete (first (second (sm-asked)))))
      (is (equal "m2" (nlk:json-value (sm-request-body (second (sm-asked))) :string "id")))
      (is-carrying (text (sm-tool "supermemory_forget" (nlk:json-object)))
                   ("either id or query is required")))))

(deftest supermemory-cell-profile-answers-facts-and-context ()
  (with-supermemory-wire
    (with-cell-stop ((supermemory-start "api_key" "k-test" "base_url" "https://sm.test"))
      (sm-answer "{\"profile\":{\"static\":[\"Lives in Oslo\"],\"dynamic\":[\"Porting a Lisp cell\"]},\"searchResults\":{\"results\":[{\"memory\":\"Drinks green tea\"}]}}")
      (let ((answer (sm-tool "supermemory_profile" (nlk:json-object "query" "what do I drink"))))
        (is (search "Lasting facts" answer))
        (is (search "Lives in Oslo" answer))
        (is (search "Recent context" answer))
        (is (search "Porting a Lisp cell" answer))
        (is (search "Drinks green tea" answer)))
      (let* ((request (first (sm-asked)))
             (body (sm-request-body request)))
        (is (equal "https://sm.test/v4/profile" (second request)))
        (is (equal "what do I drink" (nlk:json-value body :string "q")))
        (is (equal "nodecode" (nlk:json-value body :string "containerTag")))))))

;;; --- /supermemory -----------------------------------------------------------------

(deftest supermemory-cell-the-command-shows-the-container-or-searches-it ()
  (with-supermemory-wire
    (with-cell-stop ((supermemory-start "api_key" "k-test" "base_url" "https://sm.test"
                                         "container_tag" "team"))
      (let ((answer (cell-entry "nodecode-supermemory" "supermemory" "")))
        (is (search "container \"team\", hybrid search" answer) "the status names the backend")
        (is (null (sm-asked)) "a status read asks the service nothing"))
      (sm-answer "{\"results\":[{\"id\":\"m1\",\"memory\":\"lives in Oslo\"}]}")
      (is (search "[m1] lives in Oslo"
                  (cell-entry "nodecode-supermemory" "supermemory" "where do I live")))
      (is (equal "where do I live"
                 (nlk:json-value (sm-request-body (first (sm-asked))) :string "q"))))))

;;; --- the turn capture ---------------------------------------------------------------

(deftest supermemory-cell-a-finished-turn-becomes-one-conversation-document ()
  (with-supermemory-wire
    (with-cell-stop ((supermemory-start "api_key" "k-test" "base_url" "https://sm.test"))
      (sm-answer "{\"id\":\"doc-9\"}")
      ;; the turn's two texts arrive as their durable facts, as the gateway publishes them
      (cell-publish "nodecode-supermemory"
                    (cell-frame-op "turn.input_committed" :session "s-9" :turn "t1"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "My name is Ada. I live in Oslo.")))
      (cell-publish "nodecode-supermemory"
                    (cell-frame-op "turn.assistant_message_completed" :session "s-9" :turn "t1"
                                   :payload (nlk:json-object "message" "Noted: Oslo.")))
      (cell-publish "nodecode-supermemory" (cell-frame-op "turn.completed" :session "s-9" :turn "t1"))
      (nodecode-supermemory::capture-step)
      (await (:timeout 5) (= 1 (length (sm-asked))))
      (let* ((request (first (sm-asked)))
             (body (sm-request-body request))
             (content (nlk:json-value body :string "content"))
             (metadata (nlk:json-value body :object "metadata"))
             (custom (nlk:json-value body :string "customId")))
        (is (equal "https://sm.test/v3/documents" (second request)) "the capture is a document")
        (is (search "[role: user]" content) "the turn layout the entity context describes")
        (is (search "My name is Ada. I live in Oslo." content))
        (is (search "[user:end]" content))
        (is (search "[role: assistant]" content))
        (is (search "Noted: Oslo." content))
        (is (equal "conversation" (nlk:json-value metadata :string "type")))
        (is (equal "s-9" (nlk:json-value metadata :string "session_id")))
        (is (equal "nodecode" (nlk:json-value metadata :string "sm_source")))
        (is (cl-ppcre:scan "^s_9_[0-9]{4}-[0-9]{2}-[0-9]{2}_b[0-5]$" custom) "the 4-hour bucket id")
        (let ((guidance (nlk:json-value body :string "entityContext")))
          (is (search "User-assistant conversation" guidance) "the default extraction guidance rides the write"))))))
