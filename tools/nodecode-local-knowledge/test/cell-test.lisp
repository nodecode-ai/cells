;;;; cell-test.lisp --- the local-knowledge cell against a tree it writes itself.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs the network: the index reads files this file makes under
;;;; the scratch home, and the record it keeps is a file there too. What is
;;;; under test is the part the cell owns -- which files become which
;;;; artifacts, what a question routes to first, and what an answer says.

(in-package #:nodecode.test)

(define-test-slice "local-knowledge" "LOCAL-KNOWLEDGE-CELL-"
  :start nodecode-local-knowledge:start-cell)

;;; --- the tree --------------------------------------------------------------

(defun fixture-write (relative text)
  "TEXT at RELATIVE under the scratch home's fixture tree, once. => the path."
  (let ((path (nlk:home (format nil "fixture/~a" relative))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (format out "~?" text nil))
    path))

(defun fixture-root ()
  "One skill with a support document, a runbook, a script, a config and a note.
Written on every call, so a scratch home left by an older run cannot lie."
  (fixture-write "ops/deploy/SKILL.md"
                  "---~%name: Deploy a release~%---~%~%# Deploy~%~%Run the deploy script, then check the rollback path before you leave.~%")
  (fixture-write "ops/deploy/references/checklist.md"
                  "# Preflight checklist~%~%What to check before a deploy.~%")
  (fixture-write "runbooks/backup-restore.md"
                  "# Backup and restore~%~%A nightly backup, then a restore drill.~%")
  (fixture-write "scripts/backup.sh" "#!/bin/sh~%# nightly backup~%")
  (fixture-write "services/api.json" "{\"backup_window\": \"02:00\"}~%")
  (fixture-write "notes/random.md" "# Notes~%~%nothing operational here~%")
  (namestring (uiop:ensure-directory-pathname (nlk:home "fixture/"))))

(defmacro with-knowledge ((&rest pairs) &body body)
  "BODY with the cell started on the fixture tree, then stopped."
  `(with-cell-stop ((local-knowledge-start "roots" (vector (fixture-root)) ,@pairs))
     ,@body))

(defun knowledge-call (name arguments)
  "NAME called the way the model calls it, as the text it reads back."
  (nle::message-content (execute-wire-call "knowledge-call" name arguments)))

;;; --- the lifecycle ------------------------------------------------------------

(define-cell-lifecycle-tests "local-knowledge"
  (:config "roots" (vector (fixture-root)))
  (:help :local-knowledge)
  (:command "knowledge")
  (:running
   (is (nle::find-tool "knowledge_search") "the search tool is registered")
   (is (nle::find-tool "knowledge_usage_report") "and the report's")
   (is (= 5 (count-if (lambda (entry) (eql 0 (search "knowledge_" (first entry)))) nle::*tools*)) "five of them, as the original had"))
  (:stopped
   (is (null (nle::find-tool "knowledge_search")) "and stop takes every one back"))
  (:refused

   ("max_depth" 0)
   ("max_files" 0)))

(deftest local-knowledge-cell-a-missing-root-setting-refuses ()
  (with-cell-stop ((local-knowledge-start))
    (is (signals-error nodecode-local-knowledge:local-knowledge-error
                        (nodecode-local-knowledge::search-text "backup")) "a question with no roots configured refuses, naming the setting")))

;;; --- what a question routes to -------------------------------------------------

(deftest local-knowledge-cell-a-question-routes-to-the-whole-artifact ()
  (with-knowledge ()
    (let ((answer (knowledge-call "knowledge_search" "{\"query\": \"backup restore\"}")))
      (is (probe-file (merge-pathnames "runbooks/backup-restore.md" (fixture-root))) "the fixture runbook is on disk where the roots name it")
      (is (search "runbook:runbooks/backup-restore" answer) answer)
      (is (search "script:scripts/backup" answer) "and the script that does it")
      (is (search "routing hint" answer) "the answer says a hit is a hint"))))

(deftest local-knowledge-cell-the-type-filter-keeps-one-kind ()
  (with-knowledge ()
    (let ((answer (knowledge-call "knowledge_search"
                                   "{\"query\": \"backup\", \"artifact_type\": \"script\"}")))
      (is (search "script:scripts/backup" answer) "the script is there")
      (is (null (search "runbook:" answer)) "and no runbook is"))))

(deftest local-knowledge-cell-a-word-nothing-carries-finds-nothing ()
  (with-knowledge ()
    (let ((answer (knowledge-call "knowledge_search" "{\"query\": \"kubernetes ingress\"}")))
      (is (search "no artifact matches" answer) "a question nothing carries says so"))))

;;; --- one artifact, and the edges beside it ---------------------------------------

(deftest local-knowledge-cell-get-answers-the-file-and-its-neighbours ()
  (with-knowledge ()
    (let ((answer (knowledge-call "knowledge_get"
                                   "{\"artifact_id\": \"skill:ops/deploy\"}")))
      (is (search "Deploy a release" answer) "the title the frontmatter declares")
      (is (search "ops/deploy/SKILL.md" answer) "and the file it is")
      (is (search "rollback" answer) "with the start of its text")
      (is (search "doc:ops/deploy/references/checklist" answer) "and the support document under it"))))

(deftest local-knowledge-cell-neighbours-name-the-relation ()
  (with-knowledge ()
    (let ((answer (knowledge-call "knowledge_neighbors"
                                   "{\"artifact_id\": \"skill:ops/deploy\"}")))
      (is (search "supports" answer) "a skill's own directory is a support edge")
      (is (search "checklist" answer) "and names the document"))))

(deftest local-knowledge-cell-an-unknown-id-refuses-by-name ()
  (with-knowledge ()
    (is (signals-error nodecode-local-knowledge:local-knowledge-error
                        (nodecode-local-knowledge::fetch-text "skill:nope")) "an id the index does not hold refuses")))

;;; --- the local record ------------------------------------------------------------

(deftest local-knowledge-cell-feedback-is-kept-and-reported ()
  (with-knowledge ()
    (let ((answer (knowledge-call "knowledge_feedback"
                                   "{\"rating\": \"missing\", \"query\": \"kubernetes ingress\"}")))
      (is (search "recorded missing" answer) "a rating is recorded")
      (is (search "kept on this machine" answer) "and says it stays here"))
    (let ((report (knowledge-call "knowledge_usage_report" "{}")))
      (is (search "found nothing" report) "the report counts what found nothing")
      (is (search "kubernetes ingress" report) "and names the question"))))

(deftest local-knowledge-cell-a-rating-outside-the-set-refuses ()
  (with-knowledge ()
    (is (signals-error nodecode-local-knowledge:local-knowledge-error
                        (nodecode-local-knowledge::feedback-text "great")) "a rating the original never had refuses")
    (is (nodecode-local-knowledge::feedback-text "useful") "and one it had is taken")))

;;; --- what the operator types -------------------------------------------------------

(deftest local-knowledge-cell-the-command-searches-and-rebuilds ()
  (with-knowledge ()
    (let ((text (cell-entry "nodecode-local-knowledge" "knowledge" "backup")))
      (is (search "runbook:runbooks/backup-restore" text) "/knowledge WORDS searches"))
    (multiple-value-bind (text dialog)
        (cell-entry "nodecode-local-knowledge" "knowledge" "")
      (is (search "artifact" text) "bare /knowledge says what is indexed")
      (is dialog "and opens a panel"))
    (let ((text (cell-entry "nodecode-local-knowledge" "knowledge" "rebuild")))
      (is (search "rebuilt" text) "/knowledge rebuild walks the roots again"))))
