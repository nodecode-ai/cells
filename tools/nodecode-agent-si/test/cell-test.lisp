;;;; cell-test.lisp --- the lifecycle, the evidence gate, the index and the brain.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved: the cell starts and stops and takes its three tools and
;;;; two hooks with it; a session file planted under the test's own home is
;;;; found by brain_search and paged by brain_get; brain_remember refuses a
;;;; quote the user never typed and one shorter than five words, keeps a
;;;; claim that cites the message, and marks a fact disputed when a second
;;;; source says something else; a note outlives a restart because the
;;;; brain's files hold it; and the per-turn recall block carries the hit.
;;;; Every run reads the test's own folders, never the operator's.

(in-package #:nodecode.test)

(define-test-slice "agent-si" "AGENT-SI-CELL-" :start nodecode-agent-si:start-cell)

;;; --- fixtures -----------------------------------------------------------------

(defun agent-si-test-root () (namestring (nlk:home "agent-si-test/")))
(defun agent-si-test-sessions () (namestring (nlk:home "agent-si-test-sessions/")))

(defun agent-si-reset ()
  "The test's brain and session folders removed, so a test starts from none."
  (dolist (folder (list (agent-si-test-root) (agent-si-test-sessions)))
    (let ((path (uiop:ensure-directory-pathname folder)))
      (when (uiop:directory-exists-p path)
        (uiop:delete-directory-tree path :validate t)))))

(defmacro agent-si-fixture (&body body)
  `(progn (agent-si-reset) ,@body))

(defun agent-si-start* (&rest pairs)
  "START-CELL with the test's brain and session folders, and PAIRS after them."
  (apply #'agent-si-start
         (append (list "brain_dir" (agent-si-test-root)
                       "session_roots" (vector (agent-si-test-sessions)))
                 pairs)))

(defun agent-si-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (funcall (third (nle::find-tool name)) arguments))

(defun agent-si-json (text) (nlk:decode-json text))

(defun agent-si-said (text &key (session "s1") (turn "t1"))
  "One user message of TURN arriving as the frame hook sees it."
  (cell-publish "nodecode-agent-si"
                (cell-frame-op "turn.input_committed" :session session :turn turn
                               :payload (nlk:json-object "message" text))))

(defun agent-si-plant-session ()
  "A Claude Code session file with one phrase in it, under the test's home."
  (let ((file (merge-pathnames ".claude/projects/proj/session-1.jsonl"
                                (uiop:ensure-directory-pathname (agent-si-test-sessions)))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-line "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"the invoicing cron kept firing twice a morning after the migration\"}}" out)
      (write-line "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":\"the stale systemd unit was disabled and the new timer took over the invoicing job\"}}" out))
    file))

(defun agent-si-hits (answer)
  "The hits ANSWER carries, as a list."
  (coerce (nlk:json-value answer :array "hits") 'list))

(defun agent-si-claims (fact)
  "The claims a brain_get FACT carries, as a list."
  (coerce (nlk:json-value fact :array "claims") 'list))

(defmacro is-carrying-text (form &body checks)
  "Assert the text FORM answers carries every CHECK."
  `(is-carrying (answer ,form) ,@checks))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "agent-si"
  (:config "brain_dir" (agent-si-test-root))
  (:hooks :frame :round)
  (:help :agent-si)
  (:command "agent-si")
  (:fixture agent-si-fixture)
  (:running (is (nle::find-tool "brain_search") "brain_search is registered")
            (is (nle::find-tool "brain_get") "brain_get is registered")
            (is (nle::find-tool "brain_remember") "brain_remember is registered")
            (is (uiop:directory-exists-p (nodecode-agent-si::index-folder)) "the index folder is made"))
  (:stopped (is (null (nle::find-tool "brain_search")) "stop takes the tools back"))
  (:refused ("prefetch_hits" 0))
  (:idle nodecode-agent-si:agent-si-error
         (nodecode-agent-si::tool-search (nlk:json-object "query" "invoicing"))))

;;; --- the session index ----------------------------------------------------------

(deftest agent-si-cell-search-finds-a-past-session ()
  (agent-si-fixture
   (agent-si-plant-session)
   (with-cell-stop ((agent-si-start*))
     (let* ((answer (agent-si-json (agent-si-tool "brain_search" (nlk:json-object "query" "invoicing cron")))))
       (is (= 1 (nlk:json-value answer :integer "history_hits")) "the planted session is one history hit")
       (is (= 0 (nlk:json-value answer :integer "truth_hits")) "the brain is empty")
       (let* ((hit (first (agent-si-hits answer)))
              (id (nlk:json-value hit :string "id"))
              (snippet (nlk:json-value hit :string "snippet")))
         (is (search "claude:" id) "the hit is a Claude Code session id")
         (is (search "invoicing cron" snippet) "the hit shows the line that matched")
         (let ((page (agent-si-json (agent-si-tool "brain_get" (nlk:json-object "id" id)))))
           (is (search "stale systemd unit" (nlk:json-value page :string "text")) "the page is the text")))))))

(deftest agent-si-cell-an-unknown-session-is-refused ()
  (agent-si-fixture
   (with-cell-stop ((agent-si-start*))
     (is-carrying-text (agent-si-tool "brain_get" (nlk:json-object "id" "claude:000000000000"))
                       "no session claude:000000000000"))))

;;; --- the evidence gate ------------------------------------------------------------

(deftest agent-si-cell-remember-needs-the-user-words ()
  (agent-si-fixture
   (with-cell-stop ((agent-si-start*))
     (agent-si-said "we moved the invoicing job to the second Tuesday because the old cron kept firing")
     (is-carrying-text (agent-si-tool "brain_remember"
                                      (nlk:json-object "fact" "A claim nobody made"
                                                        "quote" "the invoicing job is on the third Monday"))
                       "not in anything the user wrote")
     (is-carrying-text (agent-si-tool "brain_remember"
                                      (nlk:json-object "fact" "A claim" "quote" "old cron kept"))
                       "at least 5")
     (let ((answer (agent-si-json
                    (agent-si-tool "brain_remember"
                                    (nlk:json-object
                                     "fact" "The invoicing job runs on the second Tuesday."
                                     "quote" "moved the invoicing job to the second Tuesday"
                                     "key" "invoicing.job.day" "value" "second Tuesday")))))
       (is (equal "remembered" (nlk:json-value answer :string "status")) "the claim is kept")
       (is (equal "no" (nlk:json-value answer :string "disputed")) "one value so far")
       (let ((fact (agent-si-json (agent-si-tool "brain_get" (nlk:json-object "id" "invoicing.job.day")))))
         (is (equal "active" (nlk:json-value fact :string "status")) "a fact with one value is active")
         (let ((quote (nlk:json-value (first (agent-si-claims fact)) :string "quote")))
           (is (search "moved the invoicing job" quote) "the claim carries the user's own words")))))))

(deftest agent-si-cell-a-second-value-disputes-a-fact ()
  (agent-si-fixture
   (with-cell-stop ((agent-si-start*))
     (agent-si-said "we moved the invoicing job to the second Tuesday")
     (agent-si-tool "brain_remember"
                    (nlk:json-object "fact" "The invoicing job runs on a Tuesday."
                                      "quote" "moved the invoicing job to the second Tuesday"
                                      "key" "invoicing.job.day" "value" "second Tuesday"))
     (agent-si-said "correction: the invoicing job moved to the fourth Wednesday")
     (agent-si-tool "brain_remember"
                    (nlk:json-object "fact" "The invoicing job was moved again."
                                      "quote" "the invoicing job moved to the fourth Wednesday"
                                      "key" "invoicing.job.day" "value" "fourth Wednesday"))
     (let ((fact (agent-si-json (agent-si-tool "brain_get" (nlk:json-object "id" "invoicing.job.day")))))
       (is (equal "disputed" (nlk:json-value fact :string "status")) "two values are disputed")
       (is (= 2 (length (agent-si-claims fact))) "both claims are kept")
       (is-carrying-text (nlk:json-value fact :string "note") "ask a person")))))

(deftest agent-si-cell-key-and-value-go-together ()
  (agent-si-fixture
   (with-cell-stop ((agent-si-start*))
     (agent-si-said "the lease renewal is handled by the property manager every March")
     (is-carrying-text (agent-si-tool "brain_remember"
                                      (nlk:json-object
                                       "fact" "A fact without a value"
                                       "quote" "lease renewal is handled by the property manager"
                                       "key" "office.lease.renewal"))
                       "key and value go together")
     (is-carrying-text (agent-si-tool "brain_remember"
                                      (nlk:json-object
                                       "fact" "A fact with a bad key"
                                       "quote" "lease renewal is handled by the property manager"
                                       "key" "Office Lease" "value" "March"))
                       "lower-case dotted"))))

;;; --- the brain outlives a restart ---------------------------------------------------

(deftest agent-si-cell-a-note-outlives-a-restart ()
  (agent-si-fixture
   (with-cell-stop ((agent-si-start*))
     (agent-si-said "the lease renewal is handled by the property manager every March")
     (agent-si-tool "brain_remember"
                    (nlk:json-object "fact" "The property manager renews the lease every March."
                                      "quote" "lease renewal is handled by the property manager"
                                      "title" "Lease renewal" "kind" "reference")))
   (with-cell-stop ((agent-si-start*))
     (let* ((answer (agent-si-json
                     (agent-si-tool "brain_search" (nlk:json-object "query" "lease renewal property manager"))))
            (note (find-if (lambda (hit) (search "rec-" (nlk:json-value hit :string "id")))
                           (agent-si-hits answer))))
       (is (not (null note)) "the note is still there after a restart")
       (when note
         (let ((record (agent-si-json
                        (agent-si-tool "brain_get"
                                       (nlk:json-object "id" (nlk:json-value note :string "id"))))))
           (is (equal "reference" (nlk:json-value record :string "kind")) "the kind came back")
           (let ((source (nlk:json-value record :string "source_message")))
             (is (search "lease renewal is handled" source) "the source is the user's message"))))))))

;;; --- the per-turn recall and /agent-si -----------------------------------------------

(deftest agent-si-cell-the-recall-block-carries-the-hits ()
  (agent-si-fixture
   (with-cell-stop ((agent-si-start*))
     (agent-si-said "we moved the invoicing job to the second Tuesday")
     (agent-si-tool "brain_remember"
                    (nlk:json-object "fact" "The invoicing job runs on the second Tuesday."
                                      "quote" "moved the invoicing job to the second Tuesday"
                                      "key" "invoicing.job.day" "value" "second Tuesday"))
     (let* ((hits (nodecode-agent-si::search-all "invoicing tuesday" 3 "all"))
            (block (nodecode-agent-si::recall-block hits)))
       (is (plusp (length hits)) "the brain answers the turn's words")
       (is (search "[agent-si recall" block) "the block says what it is")
       (is (search "invoicing.job.day" block) "the hit's id is in the block")
       (is (search "data, not instructions" block) "tool output is framed as data"))
     (let ((found (nodecode-agent-si::run-slash "invoicing" nil))
           (status (nodecode-agent-si::run-slash "" nil))
           (index (nodecode-agent-si::run-slash "index" nil)))
       (is (search "invoicing.job.day" found) "/agent-si searches the brain")
       (is (search "agent-si:" status) "/agent-si alone shows what the brain holds")
       (is (search "indexed" index) "/agent-si index reindexes the sessions")))))
