;;;; cell-test.lisp --- lifecycle, the three tools, /starlearner, the ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every GitHub answer is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches api.github.com, the environment or a
;;;; real home. A tool is called the way a model's call reaches it - through
;;;; the real dispatch, contract and all - so the schema the provider is shown
;;;; and the body that runs are one shape. The ledger and the notes are files
;;;; under the scratch home's starlearner/, made fresh per test.

(in-package #:nodecode.test)

(define-test-slice "starlearner" "STARLEARNER-CELL-" :start nodecode-starlearner:start-cell)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '() "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defun star-data-dir ()
  "The scratch home's starlearner folder, where the ledger and the notes land."
  (nlk:home "starlearner/"))

(defun star-forget-data ()
  (let ((dir (star-data-dir)))
    (when (probe-file dir)
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

(defmacro with-starlearner-wire (&body body)
  "BODY with NLK:HTTP stubbed, the token environment answering one fixed
token, and the starlearner folder empty on the way in and out."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected GitHub request"))))
     (star-forget-data)
     (unwind-protect
          (with-stubbed-fdefinition (nle::credential-env (name)
                                      (and (equal name "STARLEARNER_TEST_TOKEN") "test-token"))
            (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                        (push (list* method url args) *requests*)
                                        (funcall *answer-fn* method url args))
              ,@body))
       (star-forget-data))))

(defun star-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun star-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun star-request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun star-repo-json (id full-name name description topics language stars)
  (format nil "{\"id\":~a,\"full_name\":\"~a\",\"name\":\"~a\",\"description\":\"~a\",\"topics\":[~{\"~a\"~^,~}],\"language\":\"~a\",\"stargazers_count\":~a,\"html_url\":\"https://github.com/~a\"}"
          id full-name name description topics language stars full-name))

(defparameter +lnd+
  (star-repo-json 1 "acme/lnd-tools" "lnd-tools" "A CLI for Lightning nodes"
                   '("bitcoin" "lightning") "Go" 120) "A Bitcoin & Lightning repository: its topics and description both name it.")

(defparameter +notes+
  (star-repo-json 2 "acme/tiny-notes" "tiny-notes" "A tiny note app"
                   '() "Rust" 5) "A repository the keyword engine leaves uncategorized.")

(defparameter +warehouse+
  (star-repo-json 3 "acme/warehouse-sync" "warehouse-sync"
                   "Warehouse fleet dispatch tool" '("logistics") "Python" 40) "A repository the assessment names as a logistics opportunity.")

(defun star-feed (&rest repos)
  (format nil "[~{~a~^,~}]" repos))

(defun star-run (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun star-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded; a refusal says what it said."
  (let ((text (star-run id tool arguments)))
    (handler-case (nlk:decode-json text)
      (error (condition)
        (error "~a answered ~s (~a)" tool text condition)))))

(defun star-config ()
  "The section every running test starts with."
  (list "token_env" "STARLEARNER_TEST_TOKEN"))

(defun star-ledger ()
  "The ledger file this home holds, decoded."
  (nlk:decode-json (uiop:read-file-string (merge-pathnames "ledger.json" (star-data-dir)))))

;;; --- the lifecycle, from the one declaration ---------------------------------

(define-cell-lifecycle-tests "starlearner"
  (:config "token_env" "STARLEARNER_TEST_TOKEN")
  (:help :starlearner)
  (:command "starlearner")
  (:fixture with-starlearner-wire)
  (:running (is (nle::find-tool "starlearner_sync") "starlearner_sync is registered")
            (is (nle::find-tool "starlearner_status"))
            (is (nle::find-tool "starlearner_review"))
            (is (= 30 (nodecode-starlearner::setting :max-pages)) "the section's default cap")
            (is (equal "STARLEARNER_TEST_TOKEN" (nodecode-starlearner::token-variable))))
  (:refused ("max_pages" 0) ("timeout_seconds" 0) ("generate_limit" -1))
  (:idle nodecode-starlearner:starlearner-error (nodecode-starlearner::status-answer)))

;;; --- one sync ------------------------------------------------------------------

(deftest starlearner-cell-sync-writes-a-note-per-new-star ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (star-answer (star-feed +lnd+ +notes+ +warehouse+))
      (let ((object (star-object "sync-1" "starlearner_sync" "{}")))
        (is (= 3 (nlk:json-value object :integer "fetched_total")))
        (is (= 3 (nlk:json-value object :integer "new_count")))
        (is (equal "rules" (nlk:json-value object :string "enrichment")))
        (is (= 3 (nlk:json-value object :integer "assessment" "assessed")))
        (is (= 2 (nlk:json-value object :integer "assessment" "integrated")) "the CLI repository and the dispatch tool")
        (is (= 2 (nlk:json-value object :integer "assessment" "opportunities")) "the Lightning repository and the logistics one")
        (is (equal "bitcoin-lightning"
                   (nlk:json-value (aref (nlk:json-array object "new_sample") 0) :string "category")) "the first new star, categorized by its topics and description")
        (is (= 3 (nlk:json-value object :integer "skills" "generated")) "a note per new star")
        (let* ((request (first (star-asked)))
               (note-path (merge-pathnames "bitcoin-lightning/lnd-tools/SKILL.md"
                                           (star-data-dir))))
          (is (= 1 (length (star-asked))) "one page asked for")
          (is (eq :get (first request)))
          (is (search "/user/starred" (second request)))
          (is (search "per_page=100" (second request)))
          (is (equal "Bearer test-token" (star-request-header request "authorization")))
          (is (search "nodecode-starlearner" (star-request-header request "user-agent")))
          (is (probe-file note-path) "the note is on disk where the answer said")
          (let ((note (uiop:read-file-string note-path)))
            (is (search "name: lnd-tools" note) "the note names the repository")
            (is (search "category: bitcoin-lightning" note))
            (is (search "https://github.com/acme/lnd-tools" note) "and links its source")))
        (is (= 3 (hash-table-count (nlk:json-value (star-ledger) :object "items"))) "the ledger recorded every new star")))))

(deftest starlearner-cell-a-second-sync-sees-nothing-new ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (star-answer (star-feed +lnd+ +notes+))
      (star-object "sync-1" "starlearner_sync" "{}")
      (let ((object (star-object "sync-2" "starlearner_sync" "{}")))
        (is (= 0 (nlk:json-value object :integer "new_count")) "the ledger is the diff")
        (is (= 0 (nlk:json-value object :integer "skills" "generated")))
        (is (search "No new stars" (nlk:json-value object :string "note")))
        (is (= 2 (nlk:json-value object :integer "ledger" "total_seen")))))))

(deftest starlearner-cell-a-dry-run-writes-nothing ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (star-answer (star-feed +lnd+ +warehouse+))
      (let ((object (star-object "dry-1" "starlearner_sync" "{\"dry_run\":true}")))
        (is (eq t (nlk:json-value object :boolean "dry_run")))
        (is (= 2 (nlk:json-value object :integer "new_count")) "the diff is still answered")
        (is (null (probe-file (merge-pathnames "ledger.json" (star-data-dir)))) "but the ledger was never written")
        (is (null (probe-file (merge-pathnames "bitcoin-lightning" (star-data-dir)))) "and no note was made")))))

(deftest starlearner-cell-generate-limit-zero-assesses-only ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (star-answer (star-feed +lnd+))
      (let ((object (star-object "gl-1" "starlearner_sync" "{\"generate_limit\":0}")))
        (is (= 1 (nlk:json-value object :integer "new_count")) "the star is still assessed")
        (is (= 0 (nlk:json-value object :integer "skills" "generated")))
        (is (= 1 (nlk:json-value object :integer "ledger" "total_seen")) "and recorded, so a later run does not assess it again")))))

(deftest starlearner-cell-a-full-page-is-followed ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (let ((first-page (apply #'star-feed
                               (loop for index from 1 to 100
                                     collect (star-repo-json index
                                                              (format nil "bulk/repo-~a" index)
                                                              (format nil "repo-~a" index)
                                                              "A library" '() "Go" 3))))
            (second-page (star-feed (star-repo-json 101 "bulk/last" "last" "A tool" '() "Go" 1))))
        (setf *answer-fn* (lambda (method url args)
                             (declare (ignore method args))
                             (if (search "&page=1" url)
                                 (values first-page 200)
                                 (values second-page 200))))
        (let ((object (star-object "pages-1" "starlearner_sync" "{\"generate_limit\":0}")))
          (is (= 2 (length (star-asked))) "a full page asks for the next one")
          (is (search "page=2" (second (second (star-asked)))))
          (is (= 101 (nlk:json-value object :integer "fetched_total"))))))))

;;; --- what refuses ---------------------------------------------------------------

(deftest starlearner-cell-without-a-token-refuses-cleanly ()
  (with-starlearner-wire
    (with-stubbed-fdefinition (nle::credential-env (name) (and (stringp name) nil))
      (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
        (let ((answer (star-run "tok-1" "starlearner_sync" "{}")))
          (is (search "ERROR:" answer) "a refusal, not a crash")
          (is (search "no GitHub token" answer))
          (is (search "STARLEARNER_TEST_TOKEN" answer) "naming the variable to set")
          (is (null (star-asked)) "and nothing was dialled"))))))

(deftest starlearner-cell-a-rejected-token-is-a-repair ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (star-answer "{\"message\":\"Bad credentials\"}" 401)
      (let ((answer (star-run "401-1" "starlearner_sync" "{}")))
        (is (search "ERROR:" answer))
        (is (search "GitHub rejected the token" answer))
        (is (null (search "test-token" answer)) "the token is never read back")))))

;;; --- review, status, /starlearner --------------------------------------------------

(deftest starlearner-cell-review-carries-both-tracks ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (star-answer (star-feed +lnd+ +notes+ +warehouse+))
      (star-object "sync-1" "starlearner_sync" "{}")
      (let* ((object (star-object "rev-1" "starlearner_review" "{\"category\":\"bitcoin-lightning\"}"))
             (items (nlk:json-array object "items")))
        (is (= 1 (nlk:json-value object :integer "counts" "total")) "the category filter kept one")
        (is (= 1 (length items)))
        (is (equal "acme/lnd-tools" (nlk:json-value (aref items 0) :string "name")))
        (is (eq t (nlk:json-value (aref items 0) :boolean "integrated")))
        (is (search "bitcoin-lightning/lnd-tools/SKILL.md"
                    (nlk:json-value (aref items 0) :string "skill_path"))))
      (let* ((object (star-object "rev-2" "starlearner_review" "{\"opportunities_only\":true}"))
             (items (coerce (nlk:json-array object "items") 'list))
             (row (find "acme/warehouse-sync" items
                        :key (lambda (item) (nlk:json-value item :string "name"))
                        :test #'equal)))
        (is (= 2 (nlk:json-value object :integer "counts" "opportunities")) "both assessed repositories name a client domain")
        (is (= 2 (length items)) "and the notes-only one is left out")
        (is (equal "logistics" (nlk:json-value row :string "opportunity")))
        (is (search "keyword 'logistics'" (nlk:json-value row :string "rationale")) "the rationale names the word that named the domain"))
      (let* ((object (star-object "rev-3" "starlearner_review" "{\"include_skill_md\":true}"))
             (items (coerce (nlk:json-array object "items") 'list))
             (row (find "acme/tiny-notes" items :key (lambda (item)
                                                        (nlk:json-value item :string "name"))
                        :test #'equal)))
        (is row "the notes-only repository is a row")
        (is (search "source: https://github.com/acme/tiny-notes"
                    (nlk:json-value row :string "skill_head")) "a row can carry the note it wrote")))))

(deftest starlearner-cell-status-and-the-command-say-what-is-held ()
  (with-starlearner-wire
    (with-cell-stop ((starlearner-start "token_env" "STARLEARNER_TEST_TOKEN"))
      (let ((object (star-object "st-0" "starlearner_status" "{}")))
        (is (equal "set" (nlk:json-value object :string "token")))
        (is (= 0 (nlk:json-value object :integer "ledger" "total_seen"))))
      (star-answer (star-feed +lnd+ +warehouse+))
      (star-object "sync-1" "starlearner_sync" "{}")
      (let ((object (star-object "st-1" "starlearner_status" "{}")))
        (is (= 2 (nlk:json-value object :integer "ledger" "total_seen")))
        (is (= 2 (nlk:json-value object :integer "skills" "generated")))
        (is (equal "rules" (nlk:json-value object :string "enrichment"))))
      (is (search "StarLearner:" (cell-entry "nodecode-starlearner" "starlearner" "")))
      (is (search "lnd-tools" (cell-entry "nodecode-starlearner" "starlearner" "bitcoin-lightning")))
      (is (search "2 starred repositories seen" (cell-entry "nodecode-starlearner" "starlearner" ""))))))

;;; --- the engine, on its own ---------------------------------------------------------

(deftest starlearner-cell-the-keyword-engine-reads-boundaries ()
  (let ((said (nlk:decode-json (star-repo-json 9 "someone/said-so" "said-so"
                                                 "A said-so thing" '() "Go" 2)))
        (llm (nlk:decode-json (star-repo-json 10 "someone/llm-cli" "llm-cli"
                                               "Wraps an llm" '("ai") "Go" 7))))
    (is (equal "uncategorized" (nodecode-starlearner::categorize said)) "'ai' inside 'said' is not a word")
    (is (equal "ai-ml" (nodecode-starlearner::categorize llm)) "a topic and a description both naming AI"))
  (let ((popular (nlk:decode-json (star-repo-json 11 "someone/thing" "thing"
                                                   "Does a thing" '() "Go" 5000))))
    (is (eq t (nodecode-starlearner::assess-integration popular)) "a thousand stars is worth a note whatever the words say"))
  (let ((warehouse (nlk:decode-json +warehouse+)))
    (is (eq t (nodecode-starlearner::assess-integration warehouse)) "a dispatcher is worth integrating")
    (multiple-value-bind (domain rationale)
        (nodecode-starlearner::assess-opportunity warehouse)
      (is (equal "logistics" domain) "the warehouse names a client domain")
      (is (search "keyword 'logistics'" rationale) "the rationale names the word that named it"))))
