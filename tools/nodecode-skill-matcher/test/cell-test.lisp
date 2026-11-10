;;;; cell-test.lisp --- lifecycle; scanning; scoring; the gate; the log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test reads a fixture inventory written under the run's scratch
;;;; home: no skill of the machine, no network, nothing of the operator's.
;;;; The scoring tests call ROUTE on the fixture entries directly, so they
;;;; assert the numbers the rewrite promises rather than whatever this image
;;;; happens to have installed.

(in-package #:nodecode.test)

(define-test-slice "skill-matcher" "SKILL-MATCHER-" :start nodecode-skill-matcher:start-cell)

(define-cell-lifecycle-tests "skill-matcher"
  (:config)
  (:help :skill-matcher)
  (:command "skill-matcher")
  (:refused ("max_matches" 0)))

;;; --- a fixture inventory ----------------------------------------------------

(defun router-write (relative text)
  "Write TEXT at RELATIVE under this run's cells folder: the catalog a cell
starts against, written by the test and nothing else."
  (let ((path (nlk:home (format nil "cells/~a" relative))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (write-string text out))
    path))

(defun router-fill-fixture ()
  "Five entries: two that answer a pdf task, one unrelated, one that is only a
stem overlap, and four that all answer one word."
  (router-write "ocr-kit/ocr-kit.asd"
                "(defsystem \"ocr-kit\" :description \"Extract text from scanned pdf documents with ocr\" :depends-on (\"nodecode\"))")
  (router-write "ocr-kit/skills/pdf-extraction/SKILL.md"
                (format nil "---~%name: pdf-extraction~%description: Pull tables out of a pdf~%tags: [pdf, tables]~%---~%~%# PDF extraction~%"))
  (router-write "calendar-sync/calendar-sync.asd"
                "(defsystem \"calendar-sync\" :description \"Sync appointments with a calendar service\")")
  (router-write "stems-only/stems-only.asd"
                "(defsystem \"stems-only\" :description \"excerpts\")")
  (loop for name in '("router-one" "router-two" "router-three" "router-four")
        do (router-write (format nil "~a/~a.asd" name name)
                         (format nil "(defsystem \"~a\" :description \"a router of things\")" name))))

(defmacro with-started-skill-matcher (&body body)
  "Run BODY with the cell started against the fixture inventory, stopped on
unwind: the tool, the command and the log as the gateway would install them."
  `(progn
     (router-fill-fixture)
     (with-cell-stop ((skill-matcher-start))
       ,@body)))

(defun router-entries (&rest names)
  "The fixture entries named NAMES, in that order."
  (let ((catalog (nodecode-skill-matcher:scan-catalog :dirs (list (nlk:home "cells/")))))
    (loop for name in names
          collect (or (find name catalog :test #'string= :key #'nodecode-skill-matcher:catalog-entry-name)
                      (error "fixture entry ~a did not scan" name)))))

(defun router-task-request (task &optional max)
  (nodecode-skill-matcher::decode-route-request
   (nlk:json-object "task" task :opt "max_matches" max)))

;;; --- what it reads, and how it scores ---------------------------------------

(deftest skill-matcher-scans-cells-and-skills-into-one-catalog ()
  (router-fill-fixture)
  (let ((catalog (nodecode-skill-matcher:scan-catalog :dirs (list (nlk:home "cells/")))))
    (is (= 8 (length catalog)) "seven cell folders and the one SKILL.md under them: eight entries")
    (let ((skill (find "pdf-extraction" catalog :test #'string= :key #'nodecode-skill-matcher:catalog-entry-name)))
      (is-present skill "the SKILL.md is in the catalog")
      (is (equal '("pdf" "tables") (nodecode-skill-matcher:catalog-entry-tags skill)) "its frontmatter tags")
      (is (equal "ocr-kit" (nodecode-skill-matcher:catalog-entry-category skill)) "filed under the cell that holds it")
      (is (search "Pull tables" (nodecode-skill-matcher:catalog-entry-description skill))))
    (is (= 1 (count "ocr-kit" catalog :test #'string= :key #'nodecode-skill-matcher:catalog-entry-name)) "one entry per name, never the folder and its asd twice")))

(deftest skill-matcher-ranks-a-name-hit-over-a-description-hit ()
  (router-fill-fixture)
  (let* ((decision (nodecode-skill-matcher:route "extract text from a scanned pdf with ocr"
                                                :entries (router-entries "ocr-kit" "calendar-sync" "pdf-extraction")))
         (matches (nodecode-skill-matcher:decision-matches decision))
         (first-match (first matches)))
    (is (= 2 (length matches)) "two entries answer a pdf task")
    (is (equal "ocr-kit" (nodecode-skill-matcher:catalog-entry-name
                          (nodecode-skill-matcher:match-entry first-match))) "the entry whose name carries the task's word outranks a skill that only describes it")
    (is (eq :required (nodecode-skill-matcher:match-decision first-match)) "a name hit is required")
    (is (member :name (mapcar #'first (nodecode-skill-matcher:match-evidence first-match))) "the evidence says which kind of hit it was")
    (is (< 0.9 (nodecode-skill-matcher:match-confidence first-match)))
    (is (search "## Skill routing - 2 matches" (nodecode-skill-matcher:render-routing decision)))
    (is (search "(help 'name)" (nodecode-skill-matcher:render-routing decision)) "the block says how to read a match")
    (is (eq :reject (nodecode-skill-matcher:match-decision
                     (first (nodecode-skill-matcher:decision-rejected decision)))) "the calendar is not offered")))

(deftest skill-matcher-needs-exact-evidence-and-says-so-when-it-has-none ()
  (router-fill-fixture)
  (let* ((entries (router-entries "stems-only" "calendar-sync"))
         (decision (nodecode-skill-matcher:route "excerpt" :entries entries)))
    (is (null (nodecode-skill-matcher:decision-matches decision)) "a stem overlap in a description alone is supporting evidence, not a match")
    (is (equal "no_high_confidence_match" (nodecode-skill-matcher:decision-fallback decision)))
    (is (equal "No high-confidence match. Continue with the normal index."
               (nodecode-skill-matcher:render-routing decision)) "the fallback is one line, the original's wording")
    (is (= 2 (length (nodecode-skill-matcher:decision-rejected decision))) "both are reported, neither is named")))

(deftest skill-matcher-caps-the-matches-and-reports-the-rest ()
  (router-fill-fixture)
  (let ((decision (nodecode-skill-matcher:route "router"
                                                :entries (router-entries
                                                          "router-one" "router-two" "router-three" "router-four")
                                                :max-matches 3)))
    (is (= 3 (length (nodecode-skill-matcher:decision-matches decision))) "three named, at most")
    (is (= 1 (length (nodecode-skill-matcher:decision-rejected decision))) "the fourth is a rejection, not silence")
    (is (eq :reject (nodecode-skill-matcher:match-decision
                     (first (nodecode-skill-matcher:decision-rejected decision)))))))

;;; --- the tool, the command, the log ---------------------------------------

(deftest skill-matcher-tool-answers-through-its-contract (with-started-skill-matcher)
  (is (assoc "match_skills" nle:*tools* :test #'equal) "the tool is registered")
  (let ((text (nodecode-skill-matcher:route-skills (router-task-request "extract pdf text" 2))))
    (is (search "ocr-kit" text) "the tool answers the routing block")
    (is (search "confidence" text)))
  (let ((text (nodecode-skill-matcher:route-skills (router-task-request "calendar appointments"))))
    (is (search "calendar-sync" text) "and follows the task it is given")
    (is (not (search "ocr-kit" text))))
  (let ((twice (nodecode-skill-matcher:route-skills (router-task-request "calendar appointments"))))
    (is (search "unchanged since your last routing" twice) "the same answer a second time says so, as the original's topic check did")))

(deftest skill-matcher-command-routes-words-and-remembers-the-last-one (with-started-skill-matcher)
  (multiple-value-bind (text dialog)
      (nodecode-skill-matcher:run-command "calendar appointments" "s-router")
    (is (search "calendar-sync" text) "the words are routed")
    (is-present dialog "and the dialog asks for the next task"))
  (multiple-value-bind (text)
      (nodecode-skill-matcher:run-command "" "s-router")
    (is (search "task: calendar appointments" text) "bare, it routes the session's last task again"))
  (multiple-value-bind (text)
      (nodecode-skill-matcher:run-command "" "a-session-that-routed-nothing")
    (is (search "No task routed in this session yet" text) "with nothing to re-route it asks for words")))

(deftest skill-matcher-log-keeps-the-answer-and-not-the-ask (with-started-skill-matcher)
  (let* ((path (nlk:home "skill-matcher/decisions.jsonl"))
         (lines (progn (nodecode-skill-matcher:route-skills
                        (router-task-request "extract text from a scanned pdf with ocr"))
                       (nlk:read-json-lines path)))
         (last-line (car (last lines))))
    (is-present last-line "a routing writes one line")
    (is (equal '("ocr-kit" "pdf-extraction") (coerce (gethash "matches" last-line) 'list)) "the line names what was routed")
    (is (not (search "scanned" (uiop:read-file-string path))) "and holds no word of the operator's task")))
