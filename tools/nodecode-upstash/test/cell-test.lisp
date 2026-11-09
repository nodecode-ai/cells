;;;; cell-test.lisp --- lifecycle, the four tools, /upstash, recall and the transcript.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor refused, a start that could reach no
;;;; database refused), the four tools through the seam the model calls them by
;;;; -- their commands, their bodies and their answers -- /upstash with and
;;;; without a query, a turn's facts becoming a harness block and a saved
;;;; transcript. Every request is a stubbed NLK:HTTP, so nothing reaches a
;;;; real database or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-upstash-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Upstash."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected upstash request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun upstash-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun upstash-command (request)
  "REQUEST's Redis command as a list: its JSON body's elements."
  (coerce (nlk:decode-json (getf (cddr request) :content)) 'list))

(defun upstash-answer (text &optional (status 200))
  "Answer every request with TEXT and STATUS."
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun upstash-answering (function)
  "Answer each request through FUNCTION of the Redis command as a list."
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url))
                      (funcall function
                               (coerce (nlk:decode-json (getf args :content)) 'list)))))

(defun upstash-ok (&rest arguments)
  "The REST answer a command that only has to succeed gets."
  (declare (ignore arguments))
  (values "{\"result\":\"OK\"}" 200))

(defun upstash-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun upstash-write (requests)
  "REQUESTS' last JSON.SET, the one document write."
  (or (find "JSON.SET" requests :key (lambda (request) (first (upstash-command request)))
                                :test #'equal)
      (error "no JSON.SET among ~d requests" (length requests))))

(defun upstash-filter-of (request)
  "REQUEST's search filter, decoded: SEARCH.QUERY's third element."
  (nlk:decode-json (third (upstash-command request))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "upstash" "UPSTASH-CELL-" :start nodecode-upstash:start-cell)

(define-cell-lifecycle-tests "upstash"
  (:fixture with-upstash-wire)
  (:config "url" "https://db.test" "token" "t-test")
  (:hooks :frame)
  (:help :upstash)
  (:command "upstash")
  (:running (is (nle::find-tool "upstash_memory_save") "upstash_memory_save is registered")
            (is (nle::find-tool "upstash_memory_search"))
            (is (nle::find-tool "upstash_memory_forget"))
            (is (nle::find-tool "upstash_chat_search"))
            (is (equal "https://db.test" (nodecode-upstash::endpoint)))
            (is (equal "default" (nodecode-upstash::user-id)) "the default user")
            (is (equal "agentkit" (nodecode-upstash::namespace)) "the shared namespace")
            (is (equal "agentkit" (nodecode-upstash::index-name)))
            (is (= 5 (nodecode-upstash::setting :recall-limit))))
  (:stopped (is (null (nle::find-tool "upstash_memory_search")) "stop unregisters the tools"))
  (:refused ("recall_limit" 0) ("timeout_seconds" 0))
  (:idle nodecode-upstash:upstash-error (nodecode-upstash::run-command "" nil)))

(deftest upstash-cell-a-database-is-required ()
  (with-upstash-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (upstash-start)))
                   ("needs \"url\"" "UPSTASH_REDIS_REST_URL"))
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (upstash-start "url" "https://db.test")))
                   ("needs \"token\"" "UPSTASH_REDIS_REST_TOKEN"))
      (is (null (nle::find-tool "upstash_memory_save")) "a refused start installed nothing"))))

(deftest upstash-cell-a-user-id-may-not-hold-the-key-separator ()
  (with-upstash-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                       (upstash-start "url" "https://db.test" "token" "t-test"
                                                      "user_id" "a:b")))
                 ("may not contain"))))

;;; --- the tools ------------------------------------------------------------------

(deftest upstash-cell-save-writes-one-document-and-waits-for-the-index ()
  (with-upstash-wire
    (with-cell-stop ((upstash-start "url" "https://db.test" "token" "t-test" "user_id" "u-7"))
      (upstash-answer "{\"result\":\"OK\"}")
      (let ((answer (nlk:decode-json
                     (upstash-tool "upstash_memory_save"
                                    (nlk:json-object "text" "prefers window seats")))))
        (is (eq t (nlk:json-value answer :boolean "saved")))
        (is (stringp (nlk:json-value answer :string "id")) "an id comes back"))
      (let* ((requests (upstash-asked))
             (create (first requests))
             (write (upstash-write requests)) 
             (command (upstash-command write))
             (document (nlk:decode-json (fourth command))))
        (is (equal "SEARCH.CREATE" (first (upstash-command create))) "the index is made first")
        (is (equal '("SEARCH.CREATE" "agentkit" "EXISTSOK" "ON" "JSON" "PREFIX" "2"
                     "agentkit:memory:" "agentkit:chat:" "SCHEMA"
                     "kind" "TEXT" "NOTOKENIZE" "userId" "TEXT" "NOTOKENIZE"
                     "text" "TEXT" "sessionId" "TEXT" "NOTOKENIZE"
                     "userMessages" "TEXT" "modelMessages" "TEXT")
                   (upstash-command create)) "one index over both prefixes")
        (is (equal "JSON.SET" (first command)))
        (is (equal "agentkit:memory:u-7:" (subseq (second command) 0 20)) "the key layout")
        (is (equal "$" (third command)))
        (is (equal "prefers window seats" (nlk:json-value document :string "text")))
        (is (equal "u-7" (nlk:json-value document :string "userId")))
        (is (integerp (nlk:json-value document :integer "createdAt")))
        (is (equal "Bearer t-test"
                   (cdr (assoc "authorization" (getf (cddr write) :headers) :test #'string-equal))))
        (is (equal "SEARCH.WAITINDEXING" (first (upstash-command (car (last requests))))) "the index is caught up")))))

(deftest upstash-cell-search-answers-with-ids-and-text ()
  (with-upstash-wire
    (with-cell-stop ((upstash-start "url" "https://db.test" "token" "t-test" "user_id" "u-7"))
      (upstash-answer
       (format nil "{\"result\":[{\"key\":\"agentkit:memory:u-7:m1\",\"score\":0.91,\"data\":{\"text\":\"drinks tea, never coffee\",\"userId\":\"u-7\"}},{\"key\":\"agentkit:memory:someone-else:m9\",\"score\":0.5,\"data\":{\"text\":\"not this store's\"}}]}"))
      (let* ((answer (nlk:decode-json
                      (upstash-tool "upstash_memory_search"
                                     (nlk:json-object "query" "what do I drink" "limit" 3))))
             (results (nlk:json-value answer :array "results")))
        (is (= 1 (length results)) "only this user's rows")
        (is (equal "m1" (nlk:json-value (aref results 0) :string "id")))
        (is (equal "drinks tea, never coffee" (nlk:json-value (aref results 0) :string "text"))))
      (let* ((query (car (last (upstash-asked))))
             (command (upstash-command query))
             (filter (upstash-filter-of query))
             (must-not (nlk:json-value filter :array "$mustNot")))
        (is (equal "SEARCH.QUERY" (first command)))
        (is (equal "agentkit" (second command)) "the namespace's index")
        (is (equal "what do I drink" (nlk:json-value filter :string "text" "$smart")))
        (is (equal "u-7" (nlk:json-value filter :string "userId" "$eq")))
        (is (equal "chat" (nlk:json-value (aref must-not 0) :string "kind" "$eq")) "chats are excluded")
        (is (equal (list "LIMIT" "3") (subseq command 3)))))))

(deftest upstash-cell-forget-deletes-the-key-and-a-failure-is-a-repair ()
  (with-upstash-wire
    (with-cell-stop ((upstash-start "url" "https://db.test" "token" "t-test" "user_id" "u-7"))
      (upstash-answering (lambda (command)
                           (if (equal "DEL" (first command))
                               (values "{\"result\":1}" 200)
                               (upstash-ok))))
      (let ((answer (nlk:decode-json
                     (upstash-tool "upstash_memory_forget" (nlk:json-object "id" "m1")))))
        (is (eq t (nlk:json-value answer :boolean "forgotten")))
        (is (equal (list "DEL" "agentkit:memory:u-7:m1")
                   (upstash-command (car (last (upstash-asked)))))))
      (upstash-answer "{\"error\":\"WRONGTYPE Operation against a key\"}" 400)
      (is-carrying (answer (upstash-tool "upstash_memory_forget" (nlk:json-object "id" "m1")))
                   ("ERROR: upstash:") ("HTTP 400") ("WRONGTYPE")))))

(deftest upstash-cell-chat-search-finds-past-sessions ()
  (with-upstash-wire
    (with-cell-stop ((upstash-start "url" "https://db.test" "token" "t-test" "user_id" "u-7"))
      (upstash-answer
       (format nil "{\"result\":[{\"key\":\"agentkit:chat:u-7:s3\",\"score\":0.8,\"data\":{\"kind\":\"chat\",\"sessionId\":\"s3\",\"userMessages\":\"where do I live\",\"modelMessages\":\"in Oslo\",\"updatedAt\":1700000000000}}]}"))
      (let* ((answer (nlk:decode-json
                      (upstash-tool "upstash_chat_search" (nlk:json-object "query" "Oslo"))))
             (sessions (nlk:json-value answer :array "sessions")))
        (is (= 1 (length sessions)))
        (is (equal "s3" (nlk:json-value (aref sessions 0) :string "session_id")))
        (is (equal "where do I live" (nlk:json-value (aref sessions 0) :string "user_excerpt")))
        (is (equal "in Oslo" (nlk:json-value (aref sessions 0) :string "assistant_excerpt"))))
      (let* ((filter (upstash-filter-of (car (last (upstash-asked)))))
             (and (nlk:json-value filter :array "$and")))
        (is (equal "chat" (nlk:json-value (aref and 0) :string "kind" "$eq")) "chats only")
        (is (equal "u-7" (nlk:json-value (aref and 1) :string "userId" "$eq")))))))

;;; --- /upstash ---------------------------------------------------------------------

(deftest upstash-cell-the-command-shows-the-settings-or-searches ()
  (with-upstash-wire
    (with-cell-stop ((upstash-start "url" "https://db.test" "token" "t-test" "user_id" "u-7"))
      (let ((answer (cell-entry "nodecode-upstash" "upstash" "")))
        (is (search "Upstash memory: https://db.test, user \"u-7\", namespace \"agentkit\""
                    answer))
        (is (null (upstash-asked)) "a status read asks the database nothing"))
      (upstash-answer
       (format nil "{\"result\":[{\"key\":\"agentkit:memory:u-7:m1\",\"score\":0.9,\"data\":{\"text\":\"lives in Oslo\"}}]}"))
      (is (search "[m1] lives in Oslo" (cell-entry "nodecode-upstash" "upstash" "where do I live")))
      (is (equal "where do I live"
                 (nlk:json-value (upstash-filter-of (car (last (upstash-asked))))
                                 :string "text" "$smart"))))))

;;; --- recall and the transcript -------------------------------------------------

(deftest upstash-cell-a-user-message-recalls-into-the-harness-block ()
  (with-upstash-wire
    (with-cell-stop ((upstash-start "url" "https://db.test" "token" "t-test" "user_id" "u-7"))
      (upstash-answer
       (format nil "{\"result\":[{\"key\":\"agentkit:memory:u-7:m1\",\"score\":0.9,\"data\":{\"text\":\"drinks green tea\"}}]}"))
      (let ((store (nlk:open-store
                    :path (merge-pathnames "upstash-store/store.db" (uiop:temporary-directory)))))
        (unwind-protect
             (progn
               (cell-publish "nodecode-upstash"
                             (cell-frame-op "turn.input_committed" :turn "t1"
                                            :payload (nlk:json-object "role" "user"
                                                                       "message" "what should I order")))
               (multiple-value-bind (text present) (nlk:get-harness-section "s1" "upstash-memory")
                 (is present "the block is installed")
                 (is (search "## Upstash Memory" text))
                 (is (search "- drinks green tea" text))))
          (nlk:close-store store)))
      (is (equal "what should I order"
                 (nlk:json-value (upstash-filter-of (car (last (upstash-asked))))
                                 :string "text" "$smart")) "the message is the query"))))

(deftest upstash-cell-a-finished-turn-is-appended-to-the-transcript ()
  (with-upstash-wire
    (with-cell-stop ((upstash-start "url" "https://db.test" "token" "t-test"
                                     "user_id" "u-7" "recall" nil))
      (upstash-answering (lambda (command)
                           (if (equal "JSON.GET" (first command))
                               (values "{\"result\":null}" 200)
                               (upstash-ok))))
      (cell-publish "nodecode-upstash"
                    (cell-frame-op "turn.input_committed" :turn "t1"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "I live in Oslo")))
      (cell-publish "nodecode-upstash"
                    (cell-frame-op "turn.assistant_message_completed" :turn "t1"
                                   :payload (nlk:json-object "message" "Noted: Oslo.")))
      (cell-publish "nodecode-upstash" (cell-frame-op "turn.completed" :turn "t1"))
      (let* ((command (upstash-command (upstash-write (upstash-asked))))
             (document (nlk:decode-json (fourth command)))
             (messages (nlk:json-value document :array "messages")))
        (is (equal "JSON.SET" (first command)))
        (is (equal "agentkit:chat:u-7:s1" (second command)))
        (is (equal "chat" (nlk:json-value document :string "kind")))
        (is (equal "s1" (nlk:json-value document :string "sessionId")))
        (is (equal "I live in Oslo" (nlk:json-value document :string "userMessages")))
        (is (equal "Noted: Oslo." (nlk:json-value document :string "modelMessages")))
        (is (= 2 (length messages)))
        (is (equal "user" (nlk:json-value (aref messages 0) :string "role")))
        (is (equal "assistant" (nlk:json-value (aref messages 1) :string "role")))))))

