;;;; cell-test.lisp --- lifecycle, the four tools, /hyatlas, recall and capture.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong layer and an integer under its floor refused, a start with
;;;; no server address refused), the four tools through the seam the model
;;;; calls them by -- their URL, their body and their answer -- /hyatlas with
;;;; and without an argument, the block a turn is given at its first round,
;;;; and a finished turn going from the :frame point to one add. Every request
;;;; is a stubbed NLK:HTTP, so nothing reaches a server or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-hyatlas-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials a server."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected hyatlas request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun hyatlas-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun hyatlas-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun hyatlas-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun hyatlas-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defparameter +search-answer+
  "{\"order\":[\"now\",\"pages\",\"profile\",\"normal\"],\"now\":{\"text\":\"Working on the cells hub.\",\"updated\":\"2026-11-08T09:00:00Z\"},\"memories\":{\"pages\":[{\"memory_id\":\"p1\",\"content\":\"HyAtlas is a self-hosted memory server.\",\"layer\":\"l8_entity\",\"score\":0.71,\"gmt_created\":1794096000}],\"profile\":[{\"memory_id\":\"m1\",\"content\":\"prefers concise replies\",\"layer\":\"l1_profile\",\"score\":0.93,\"gmt_created\":1794096000,\"event_date\":\"2026-10-05\",\"history\":[{\"content\":\"prefers long replies\",\"date\":\"2026-03-02\"}]}],\"normal\":[{\"memory_id\":\"m2\",\"content\":\"ships on Fridays\",\"layer\":\"l3_fact\",\"score\":0.42,\"gmt_created\":1794096000,\"series_count\":3,\"series_dates\":[\"2026-03-02\",\"2026-05-14\",\"2026-10-13\",\"not-a-date\"]}]}}"
  "One /api/v1/search answer, shaped as the server's own README documents it.")

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "hyatlas" "HYATLAS-CELL-" :start nodecode-hyatlas:start-cell)

(define-cell-lifecycle-tests "hyatlas"
  (:fixture with-hyatlas-wire)
  (:config "base_url" "http://hyatlas.test" "user_id" "u-1")
  (:help :hyatlas)
  (:command "hyatlas")
  (:hooks :round :frame)
  (:running (is (nle::find-tool "hyatlas_search") "hyatlas_search is registered")
            (is (nle::find-tool "hyatlas_status"))
            (is (nle::find-tool "hyatlas_recent"))
            (is (nle::find-tool "hyatlas_add"))
            (is (equal "http://hyatlas.test" (nodecode-hyatlas::root)))
            (is (equal "u-1" (nodecode-hyatlas::setting :user-id)))
            (is (= 10 (nodecode-hyatlas::setting :search-limit)))
            (is nodecode-hyatlas::*worker* "the capture worker runs"))
  (:stopped (is (null (nle::find-tool "hyatlas_search")) "stop unregisters the tools")
            (is (null nodecode-hyatlas::*worker*) "and the worker is gone"))
  (:refused ("user_id" "u-1") ("search_limit" 0) ("recall_limit" 0))
  (:idle nodecode-hyatlas:hyatlas-error (nodecode-hyatlas::root)))

(deftest hyatlas-cell-an-unknown-layer-is-refused-before-the-wire ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test"))
      (is-carrying (answer (hyatlas-tool "hyatlas_search"
                                          (nlk:json-object "query" "x" "layer" "l9_cookie")))
                   ("ERROR: hyatlas" "the refusal names the cell") ("l1_profile" "and the layers it accepts") ("l9_cookie" "and the layer it was given"))
      (is (null (hyatlas-asked)) "no request was made"))))

(deftest hyatlas-cell-no-server-address-refuses ()
  (with-hyatlas-wire
    (is-carrying (answer (refusal-text nlk:config-refusal (hyatlas-start "user_id" "u-1")))
                 ("base_url" "required"))
    (is (null (nle::find-tool "hyatlas_search")) "a refused start installed nothing")))

;;; --- the tools ------------------------------------------------------------------

(deftest hyatlas-cell-search-answers-the-hits-it-was-given ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test" "user_id" "u-1"
                                    "agent_id" "laptop"))
      (hyatlas-answer +search-answer+)
      (let ((answer (hyatlas-tool "hyatlas_search" (nlk:json-object "query" "memory" "limit" 4))))
        (is (search "[NOW] (updated 2026-11-08)" answer) "the Now page leads")
        (is (search "Working on the cells hub." answer))
        (is (search "[PAGES]" answer) "the server's order comes first")
        (is (search "[PROFILE]" answer))
        (is (search "prefers concise replies" answer))
        (is (search "happened 2026-10-05, saved 2026-11-08, score 0.93" answer) "the dates a hit carries")
        (is (search "was: prefers long replies (until 2026-03-02)" answer) "the version it replaced")
        (is (search "3 times (2026-03-02, 2026-05-14, 2026-10-13)" answer)
            "a recurring event says how often, its newest dates, and drops a bad one")
        (is (search "ships on Fridays" answer)))
      (let* ((request (first (hyatlas-asked)))
             (body (hyatlas-request-body request)))
        (is (eq :post (first request)))
        (is (equal "http://hyatlas.test/api/v1/search" (second request)))
        (is (equal "memory" (nlk:json-value body :string "query")))
        (is (equal "u-1" (nlk:json-value body :string "user_id")))
        (is (equal "laptop" (nlk:json-value body :string "agent_id")))
        (is (= 4 (nlk:json-value body :integer "limit")) "the call's limit, not the section's")))))

(deftest hyatlas-cell-recent-asks-the-mode-for-its-raw-rows ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test" "user_id" "u-1"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method args))
              (if (search "/api/v1/status" url)
                  (values "{\"mode\":\"lite\"}" 200)
                  (values "{\"total\":1,\"memories\":[{\"content\":\"just said this\",\"layer\":\"l2_raw\",\"gmt_created\":1794096000}]}" 200))))
      (let ((answer (hyatlas-tool "hyatlas_recent" (nlk:json-object "limit" 5))))
        (is (search "1 of 1 memories" answer))
        (is (search "just said this" answer)))
      (is (= 2 (length (hyatlas-asked))) "the mode read, then the list")
      (let ((body (hyatlas-request-body (second (hyatlas-asked)))))
        (is (= 5 (nlk:json-value body :integer "limit")))
        (is (eq t (nlk:json-value body :boolean "include_raw")) "lite stores only raw rows, so they are listed")))))

(deftest hyatlas-cell-add-and-status-answer-what-the-server-said ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test" "user_id" "u-1" "agent_id" "laptop"))
      (hyatlas-answer "{\"success\":true,\"memory_id\":\"mem-7\",\"extraction_status\":\"pending\"}")
      (is (equal "Stored as mem-7 (extraction pending)."
                 (hyatlas-tool "hyatlas_add" (nlk:json-object "text" "the user keeps bees"))))
      (let ((body (hyatlas-request-body (first (hyatlas-asked)))))
        (is (equal "http://hyatlas.test/api/v1/add" (second (first (hyatlas-asked)))))
        (is (equal "the user keeps bees" (nlk:json-value body :string "text")))
        (is (equal "u-1" (nlk:json-value body :string "user_id")))
        (is (equal "laptop" (nlk:json-value body :string "agent_id"))))
      (setf *requests* '())
      (hyatlas-answer "{\"version\":\"5.0.0\",\"mode\":\"ultra\",\"vdb\":\"ok\",\"embed\":\"ok\",\"llm\":\"ok\",\"llm_model\":\"a-model\",\"write_pipeline\":\"ok\",\"writes\":12,\"searches\":30,\"graph_nodes\":4,\"graph_edges\":9,\"unextracted\":0,\"layers\":{\"l1_profile\":3,\"l3_fact\":7}}")
      (let ((answer (hyatlas-tool "hyatlas_status" (nlk:json-object))))
        (is (search "HyAtlas 5.0.0: mode ultra" answer))
        (is (search "graph 4 nodes, 9 edges" answer))
        (is (search "layers: l1_profile 3, l2_raw 0, l3_fact 7" answer) "a layer the server did not send reads 0")
        (is (eq :get (first (first (hyatlas-asked)))) "status is a read")))))

(deftest hyatlas-cell-a-refused-answer-is-a-repair-not-a-crash ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test"))
      (hyatlas-answer "{\"error\":\"unauthorized\"}" 401)
      (is-carrying (answer (hyatlas-tool "hyatlas_search" (nlk:json-object "query" "x")))
                   ("ERROR: hyatlas" "the refusal is the model's repair") ("HTTP 401" "with the status") ("unauthorized" "and what the server said")))))

;;; --- /hyatlas -------------------------------------------------------------------

(deftest hyatlas-cell-the-command-answers-every-subcommand ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test" "user_id" "u-1"))
      (hyatlas-answer "{\"version\":\"5.0.0\",\"mode\":\"lite\",\"vdb\":\"ok\",\"embed\":\"ok\",\"llm\":\"unused\",\"write_pipeline\":\"ok\"}")
      (let ((answer (cell-entry "nodecode-hyatlas" "hyatlas" "")))
        (is (search "HyAtlas at http://hyatlas.test, user \"u-1\", agent \"default\"." answer))
        (is (search "mode lite" answer)))
      (setf *requests* '())
      (hyatlas-answer +search-answer+)
      (is (search "prefers concise replies" (cell-entry "nodecode-hyatlas" "hyatlas" "search memory")))
      (is (equal "memory"
                 (nlk:json-value (hyatlas-request-body (first (hyatlas-asked))) :string "query")))
      (setf *requests* '())
      (hyatlas-answer "{\"memory_id\":\"mem-9\"}")
      (is (search "Stored as mem-9"
                  (cell-entry "nodecode-hyatlas" "hyatlas" "add keeps bees")))
      (is (search "Unknown subcommand" (cell-entry "nodecode-hyatlas" "hyatlas" "dance"))))))

;;; --- recall before a turn, capture after it ---------------------------------------

(deftest hyatlas-cell-a-turn-is-given-the-memories-nearest-its-message ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test" "user_id" "u-1"
                                    "recall_limit" 3))
      (hyatlas-answer +search-answer+)
      (let ((turn :a-turn) (told nil))
        (with-stubbed-fdefinition (nodecode-hyatlas::turn-prompt (session turn)
                                   (declare (ignore session turn))
                                   (values "what do I prefer?" "turn-r1"))
          (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &key text)
                                     (declare (ignore turn)) (setf told text))
            (flet ((recall-round (number)
                     (funcall (cell-hook "nodecode-hyatlas" :round)
                              (list :session-id "s1" :turn turn :round number)
                              (lambda (op) (declare (ignore op)) t))))
              (recall-round 0)
              (is (equal "what do I prefer?"
                         (nlk:json-value (hyatlas-request-body (first (hyatlas-asked))) :string "query")))
              (is (= 3 (nlk:json-value (hyatlas-request-body (first (hyatlas-asked))) :integer "limit"))
                  "the section's recall_limit, not the search limit")
              (is (search "prefers concise replies" told) "the reminder carries the hits")
              (is (search "<relevant-memories query=\"what do I prefer?\">" told)
                  "under the turn's own question")
              (setf *requests* '())
              (recall-round 1)
              (is (null (hyatlas-asked)) "one search per turn, not per round"))))))))

(deftest hyatlas-cell-a-finished-turn-becomes-one-add ()
  (with-hyatlas-wire
    (with-cell-stop ((hyatlas-start "base_url" "http://hyatlas.test" "user_id" "u-1" "agent_id" "laptop"))
      (hyatlas-answer "{\"memory_id\":\"mem-1\"}")
      (cell-publish "nodecode-hyatlas"
                    (cell-frame-op "turn.input_committed" :turn "t9"
                                   :payload (nlk:json-object "role" "user" "message" "I keep bees.")))
      (cell-publish "nodecode-hyatlas"
                    (cell-frame-op "turn.assistant_message_completed" :turn "t9"
                                   :payload (nlk:json-object "message" "Noted: bees.")))
      (cell-publish "nodecode-hyatlas" (cell-frame-op "turn.completed" :turn "t9"))
      (nodecode-hyatlas::capture-step)
      (await (:timeout 5) (= 1 (length (hyatlas-asked))))
      (let* ((request (first (hyatlas-asked)))
             (body (hyatlas-request-body request)))
        (is (equal "http://hyatlas.test/api/v1/add" (second request)))
        (is (equal (format nil "USER: I keep bees.~%~%ASSISTANT: Noted: bees.")
                   (nlk:json-value body :string "text")) "the turn's two messages, as the original sends them")
        (is (equal "s1" (nlk:json-value body :string "session_id")))
        (is (equal "laptop" (nlk:json-value body :string "agent_id")))))))
