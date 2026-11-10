;;;; cell-test.lisp --- an entry, its refusals, the topic file, the resident index.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: the cell starts and stops cleanly, its tool and its
;;;; command answer, an entry is validated and appended, a malformed one is
;;;; refused in the original's words, a paragraph is routed to a topic file,
;;;; a whole-file rewrite needs the section's approval, an unset index_file
;;;; refuses, and the index reaches a turn's first request. The index and its
;;;; topics are files in the scratch home, so no network is touched at all.

(in-package #:nodecode.test)

(define-test-slice "memory-shaper" "MEMORY-SHAPER-CELL-" :start nodecode-memory-shaper:start-cell)

;;; --- helpers -------------------------------------------------------------------

(defvar *memory-shaper-calls* 0
  "How many tool calls this test file made, for call ids.")

(defun memory-shaper-index-file ()
  "The index file this test's scratch home holds."
  (namestring (nlk:home "memory/index.md")))

(defun memory-shaper-arrow ()
  "The one arrow an entry carries."
  (string (code-char 8594)))

(defun memory-shaper-entry (keywords target)
  "An index entry over KEYWORDS, pointing at TARGET."
  (format nil "~{~a~^,~} ~a ~a" keywords (memory-shaper-arrow) target))

(defmacro with-memory-shaper ((&rest pairs) &body body)
  "BODY with a session store open and the cell started on its defaults, PAIRS
overriding the section."
  `(with-session-store ("memory-shaper-test")
     (with-cell-stop ((memory-shaper-start "index_file" (memory-shaper-index-file) ,@pairs))
       ,@body)))

(defun memory-shaper-json (&rest pairs)
  "PAIRS as the JSON body a tool call carries."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun tool-text (name &rest arguments)
  "The text the tool called NAME answers, called as the provider's call is."
  (princ-to-string
   (nle::message-content
    (execute-wire-call (format nil "memory-shaper-call-~d" (incf *memory-shaper-calls*)) name
                        (if arguments (apply #'memory-shaper-json arguments) "{}")))))

(defun memory-shaper-blocks (context)
  "The index blocks one compiled request carries."
  (loop for message in (coerce (nle::compiled-turn-context-messages context) 'list)
        for text = (nle::content-text (gethash "content" message))
        when (and (stringp text) (search "[Keyword index" text))
          collect text))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "memory-shaper"
  (:config "index_file" (memory-shaper-index-file))
  (:hooks :round)
  (:help :memory-shaper)
  (:command "memory-shaper")
  (:running (is (string= (memory-shaper-index-file) (namestring (nodecode-memory-shaper::index-pathname))) "the section's file is where the index goes")
            (is (= 140 (nodecode-memory-shaper::setting :verbose-chars)))
            (is (= 800 (nodecode-memory-shaper::setting :index-chars))))
  (:refused ("min_keywords" 0) ("max_keywords" 1))
  (:idle nodecode-memory-shaper::memory-shaper-error (nodecode-memory-shaper::status-text)))

;;; --- the index -----------------------------------------------------------------

(deftest memory-shaper-cell-an-entry-is-added-and-lints-clean ()
  (with-memory-shaper ()
    (let ((entry (memory-shaper-entry '("python" "packaging" "poetry" "ruff" "pytest" "typing") "python.md")))
      (is-carrying (added (tool-text "memory_index" "line" entry))
                   ("Added (6 keywords)") ("python.md"))
      (is (equal (list entry) (nodecode-memory-shaper::index-lines)) "the file holds exactly the one line")
      (is-carrying (report (tool-text "memory_index" "action" "lint"))
                   ("1 entry in") ("every one well formed"))
      (is-carrying (again (tool-text "memory_index" "line" entry)) ("Already in the index"))
      (is-carrying (shown (nodecode-memory-shaper::run-slash "show" nil)) ("python.md"))
      (is-carrying (status (nodecode-memory-shaper::run-slash "" nil))
                   ("1 entry in") ("every one well formed")))))

(deftest memory-shaper-cell-a-malformed-entry-is-refused ()
  (with-memory-shaper ()
    (let ((too-few (memory-shaper-entry '("java" "jvm" "heap") "java.md"))
          (ascii (format nil "java,jvm,heap,profiling,jmx ~a java.md" "->"))
          (prose "This is just regular English text with no commas at all")
          (absolute (memory-shaper-entry '("a" "b" "c" "d" "e") "/notes.md")))
      (is-carrying (answer (tool-text "memory_index" "line" too-few))
                   ("ERROR") ("too few") ("5 to 20"))
      (is-carrying (answer (tool-text "memory_index" "line" ascii))
                   ("ERROR") ("wrong arrow") ("Unicode arrow"))
      (is-carrying (answer (tool-text "memory_index" "line" prose))
                   ("ERROR") ("sentence"))
      (is-carrying (answer (tool-text "memory_index" "line" absolute))
                   ("ERROR") ("absolute path"))
      (is (null (nodecode-memory-shaper::index-lines)) "nothing was written"))))

(deftest memory-shaper-cell-a-long-line-is-sent-to-its-topic-file ()
  (with-memory-shaper ()
    (let* ((long (format nil "~{~a~^,~} ~a long-topic.md"
                          (loop for n from 1 to 12 collect (format nil "keyword-number-~d" n))
                          (memory-shaper-arrow)))
           (answer (tool-text "memory_index" "line" long)))
      (is-carrying answer ("ERROR") ("too long for the index") ("long-topic.md"))
      (is (null (nodecode-memory-shaper::index-lines)) "the index still holds nothing"))))

(deftest memory-shaper-cell-a-paragraph-is-routed-to-a-topic-file ()
  (with-memory-shaper ()
    (let ((body "Deploys go out from the release branch only, canary first for ten minutes, then the fleet."))
      (is-carrying (routed (tool-text "memory_index" "action" "route"
                                       "topic" "deploy.md" "text" body))
                   ("Wrote") ("deploy.md") ("memory_index action=add"))
      (let ((path (merge-pathnames "deploy.md" (nodecode-memory-shaper::topics-dir))))
        (is (search "canary first" (nlk:read-text path)) "the paragraph is on disk"))
      (is-carrying (added (tool-text "memory_index"
                                      "line" (memory-shaper-entry
                                               '("canary" "deploy" "pipeline" "release" "rollback" "staging")
                                               "deploy.md")))
                   ("Added (6 keywords)"))
      (is-carrying (report (tool-text "memory_index" "action" "lint")) ("1 entry in")))))

(deftest memory-shaper-cell-a-rewrite-needs-the-sections-approval ()
  (let ((whole (format nil "~a~%~a"
                        (memory-shaper-entry '("alpha" "beta" "gamma" "delta" "epsilon") "one.md")
                        (memory-shaper-entry '("zeta" "eta" "theta" "iota" "kappa") "two.md"))))
    (with-memory-shaper ()
      (is-carrying (answer (tool-text "memory_index" "action" "rewrite" "text" whole))
                   ("ERROR") ("allow_rewrite")))
    (with-memory-shaper ("allow_rewrite" t)
      (is-carrying (answer (tool-text "memory_index" "action" "rewrite" "text" whole))
                   ("Rewrote") ("2 lines"))
      (is (= 2 (length (nodecode-memory-shaper::index-lines))) "both entries landed"))))

(deftest memory-shaper-cell-a-malformed-rewrite-writes-nothing ()
  (with-memory-shaper ("allow_rewrite" t)
    (let ((whole (memory-shaper-entry '("one" "two" "three") "three.md")))
      (is-carrying (answer (tool-text "memory_index" "action" "rewrite" "text" whole))
                   ("ERROR") ("nothing written") ("too few"))
      (is (null (nodecode-memory-shaper::index-lines)) "the file is untouched"))))

(deftest memory-shaper-cell-without-an-index-file-the-tool-names-it ()
  (with-session-store ("memory-shaper-nofile")
    (with-cell-stop ((memory-shaper-start))
      (is (nle::find-tool "memory_index") "the tool is there")
      (is-carrying (answer (tool-text "memory_index"
                                          "line" (memory-shaper-entry
                                                   '("alpha" "beta" "gamma" "delta" "epsilon")
                                                   "x.md")))
                   ("ERROR") ("memory-shaper.index_file is not set"))
      (is-carrying (listed (handler-case (nodecode-memory-shaper::run-slash "" nil)
                             (nodecode-memory-shaper::memory-shaper-error (condition)
                               (princ-to-string condition))))
                   ("memory-shaper.index_file is not set")))))

;;; --- every turn -------------------------------------------------------------------

(deftest memory-shaper-cell-the-index-opens-the-next-turn ()
  (with-memory-shaper ()
    (ensure-durable-session "memory-shaper-a")
    (let ((entry (memory-shaper-entry '("router" "wifi" "dns" "subnet" "firewall") "network.md")))
      (tool-text "memory_index" "line" entry)
      (let ((asked '()))
        (with-provider-stub (context
                             (push (memory-shaper-blocks context) asked)
                             (wire-message "assistant" :content "ok"))
          (nle:run "go" :session (nle:intern-session "memory-shaper-a") :command-id "memory-shaper-a-1"))
        (is (= 1 (length asked)) "one request")
        (is-carrying (block (first (first asked)))
                     ("[Keyword index") ("network.md"))))))
