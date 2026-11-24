;;;; cell-test.lisp --- lifecycle, the selector's decisions, the tool, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its own arithmetic: the
;;;; section (a floor outside 0..1, a ceiling under 1, refuses the start),
;;;; mcp_select through the real preflight and the real body, the three
;;;; decisions the original can take -- select, abstain, one fallback hop --
;;;; the rejections it names, the section's ceiling answering a call that
;;;; names none, and /mcp-router. Nothing here dials: the selector has no wire.

(in-package #:nodecode.test)

(define-test-slice "mcp-router" "MCP-ROUTER-CELL-" :start nodecode-mcp-router:start-cell)

(defparameter +tools-json+
  "[{\"id\":\"filesystem_read\",\"name\":\"Filesystem Read\",\"description\":\"Read local files from the workspace.\",\"server\":\"filesystem\",\"tags\":[\"files\",\"read\",\"local\"],\"tokens\":80,\"permissions\":[\"read_files\"],\"fallback_id\":\"docs_search\",\"max_calls\":20},
    {\"id\":\"github_search\",\"name\":\"GitHub Search\",\"description\":\"Search GitHub repositories and issues.\",\"server\":\"github\",\"tags\":[\"github\",\"search\",\"issues\"],\"tokens\":120,\"permissions\":[\"read_github\"],\"fallback_id\":\"docs_search\",\"max_calls\":8},
    {\"id\":\"docs_search\",\"name\":\"Docs Search\",\"description\":\"Search project documentation and the API reference.\",\"server\":\"docs\",\"tags\":[\"docs\",\"documentation\",\"api\"],\"tokens\":60,\"permissions\":[\"read_docs\"],\"max_calls\":12},
    {\"id\":\"shell\",\"name\":\"Shell\",\"description\":\"Run shell commands.\",\"server\":\"shell\",\"tags\":[\"shell\",\"exec\"],\"tokens\":200,\"permissions\":[\"read_files\",\"write_files\",\"exec_shell\"],\"max_calls\":4}]"
  "The catalog a test selects from: four tools, one of them too big for a small budget.")

(defun router-call (query &rest extras)
  "A mcp_select call as JSON: QUERY, the shared catalog, then EXTRAS as raw members."
  (format nil "{\"query\":~s,\"tools\":~a~{~a~}}" query +tools-json+ extras))

(defun router-answer (json-text)
  "JSON-TEXT through the real preflight and the real body. => the answer decoded,
or the text a refusal answered with."
  (let ((entry (nle::find-tool "mcp_select")))
    (multiple-value-bind (arguments error)
        (nle::tool-call-preflight "mcp_select" json-text entry)
      (if error
          error
          (handler-case
              (nlk:decode-json
               (funcall (third entry) (nle::tool-body-arguments entry arguments)))
            (condition (condition) (format nil "ERROR: ~a" condition)))))))

(defun selected-ids (answer)
  (loop for item across (nlk:json-array answer "selected")
        collect (nlk:json-value item :string "id")))

(defun rejection-of (answer id)
  "Why ANSWER rejected ID, or NIL."
  (loop for item across (nlk:json-array answer "rejected")
        when (equal id (nlk:json-value item :string "id"))
          return (nlk:json-value item :string "reason")))

(defparameter +several+ "read local files search github issues docs shell"
  "A query that names a word of every tool, so the ceiling and the budget are what
decide, not the floor.")

;;; --- the cell, installed and taken back -------------------------------------------

(define-cell-lifecycle-tests "mcp-router"
  (:help :mcp-router)
  (:command "mcp-router")
  (:running
   (is (nle::find-tool "mcp_select") "mcp_select is registered")
   (is (plusp (length nodecode-mcp-router::+demo-tools+)) "the demo catalog /mcp-router selects from is there"))
  (:stopped
   (is (null (nle::find-tool "mcp_select")) "stop unregisters mcp_select"))
  (:refused
   ("max_tools" 0)
   ("budget_tokens" 0)
   ("min_relevance" "2")
   ("min_relevance" "warm")))

;;; --- the selector's decisions -----------------------------------------------------

(deftest mcp-router-cell-keeps-the-tool-that-shares-the-query-s-words (with-cell-stop ((mcp-router-start)))
  (let ((answer (router-answer (router-call "read a local file from the docs" ",\"max_tools\":1"))))
    (is (equal "select" (nlk:json-value answer :string "decision")))
    (is (equal '("filesystem_read") (selected-ids answer)) "the tool whose words the query shares is the one kept")
    (is (= 80 (nlk:json-value answer :integer "total_tokens")) "and the budget counts its cost")
    (is (search "relevance=" (aref (nlk:json-array
                                     (aref (nlk:json-array answer "selected") 0)
                                     "reasons")
                                    0)) "the choice carries its reasons")
    (is (equal "docs_search" (nlk:json-value (nlk:json-value answer :object "fallback") :string "id")) "the declared fallback behind it is previewed")
    (is (equal "low_relevance" (rejection-of answer "shell")) "a tool the query does not name is rejected, and says why")))

(deftest mcp-router-cell-the-count-and-the-budget-both-cut (with-cell-stop ((mcp-router-start)))
  (let ((answer (router-answer (router-call +several+ ",\"min_relevance\":0.1,\"max_tools\":1"))))
    (is (equal '("filesystem_read") (selected-ids answer)) "max_tools is a ceiling")
    (is (equal "max_tools" (rejection-of answer "github_search")))
    (is (equal "max_tools" (rejection-of answer "docs_search"))))
  (let ((answer (router-answer (router-call +several+ ",\"min_relevance\":0.1,\"budget_tokens\":80"))))
    (is (equal '("filesystem_read") (selected-ids answer)) "what fits the budget is what is kept")
    (is (equal "budget" (rejection-of answer "github_search")))
    (is (equal "budget" (rejection-of answer "shell"))))
  (let ((answer (router-answer (router-call +several+ ",\"min_relevance\":0.1,\"budget_tokens\":50"))))
    (is (equal "abstain" (nlk:json-value answer :string "decision")) "nothing fits, nothing is kept")
    (is (equal "no_candidate" (nlk:json-value answer :string "abstain_reason")))))

(deftest mcp-router-cell-a-permission-the-catalog-lacks-is-a-hard-rejection (with-cell-stop ((mcp-router-start)))
  (let ((answer (router-answer (router-call "read local files and run shell commands"
                                             ",\"required_permissions\":[\"exec_shell\"]"))))
    (is (equal '("shell") (selected-ids answer)) "only the tool declaring the permission is kept")
    (is (equal "missing_permissions" (rejection-of answer "filesystem_read")))
    (is (equal "missing_permissions" (rejection-of answer "github_search"))))
  (let ((answer (router-answer (router-call "run shell commands"
                                             ",\"forbidden_permissions\":[\"exec_shell\"]"))))
    (is (equal "forbidden_permissions" (rejection-of answer "shell")) "a forbidden permission rejects it outright")
    (is (not (member "shell" (selected-ids answer) :test #'equal)))))

(deftest mcp-router-cell-nothing-relevant-abstains-instead-of-choosing (with-cell-stop ((mcp-router-start)))
  (let ((answer (router-answer (router-call "kubernetes frobnicator"))))
    (is (equal "abstain" (nlk:json-value answer :string "decision")))
    (is (equal "no_candidate" (nlk:json-value answer :string "abstain_reason")))
    (is (zerop (length (nlk:json-array answer "selected"))) "no tool is offered")
    (is (equal "low_relevance" (rejection-of answer "filesystem_read")) "and every candidate says what kept it out")))

(deftest mcp-router-cell-a-failed-tool-takes-one-hop-to-its-own-fallback (with-cell-stop ((mcp-router-start)))
  (let ((answer (router-answer (router-call "read a local file from the docs"
                                             ",\"failed_tool_id\":\"filesystem_read\""))))
    (is (equal "fallback" (nlk:json-value answer :string "decision")))
    (is (equal '("docs_search") (selected-ids answer)) "the declared fallback is tried first")
    (is (equal "already_failed" (rejection-of answer "filesystem_read")) "the tool that failed is not offered again"))
  (let ((answer (router-answer (router-call "read a local file from the docs"
                                             ",\"failed_tool_id\":\"filesystem_read\""
                                             ",\"no_fallback\":true"))))
    (is (equal "abstain" (nlk:json-value answer :string "decision")))
    (is (equal "fallback_disabled" (nlk:json-value answer :string "abstain_reason")))))

(deftest mcp-router-cell-the-section-answers-a-call-that-names-no-ceiling (with-cell-stop ((mcp-router-start "max_tools" 1 "min_relevance" "0.1")))
  (let ((answer (router-answer (router-call +several+))))
    (is (= 1 (length (selected-ids answer))) "the section's max_tools is the ceiling")
    (is (equal "max_tools" (rejection-of answer "github_search"))))
  (let ((answer (router-answer (router-call +several+ ",\"max_tools\":2"))))
    (is (member "github_search" (selected-ids answer) :test #'equal) "a call's own max_tools wins over the section's")))

(deftest mcp-router-cell-a-section-can-leave-the-rejections-out (with-cell-stop ((mcp-router-start "omit_rejected" t)))
  (let ((answer (router-answer (router-call "read a local file from the docs" ",\"max_tools\":1"))))
    (is (equal "select" (nlk:json-value answer :string "decision")) "the selection is unaffected")
    (is (null (gethash "rejected" answer)) "and the rejected list is left out")))

(deftest mcp-router-cell-a-call-that-breaks-the-contract-is-refused-before-the-body (with-cell-stop ((mcp-router-start)))
  (let ((refusal (router-answer "{\"tools\":[]}")))
    (is (stringp refusal) "a call with no query never reaches the body")
    (is (search "contract_refused" refusal) "and the refusal names the contract"))
  (let ((refusal (router-answer "{\"query\":\"read\",\"tools\":[{\"name\":\"nameless\"}]}")))
    (is (search "ERROR:" refusal) "a candidate with no id refuses in the body")
    (is (search "no id" refusal) "and says what it wanted")))

;;; --- the command ------------------------------------------------------------------

(deftest mcp-router-cell-the-command-answers-and-demonstrates (with-cell-stop ((mcp-router-start)))
  (let ((text (cell-entry "nodecode-mcp-router" "mcp-router" "")))
    (is (search "mcp_select" text) "typed alone, it names the tool"))
  (let ((text (cell-entry "nodecode-mcp-router" "mcp-router" "read a local file")))
    (is (search "filesystem_read" text) "typed with a query, it selects over the demo catalog")
    (is (search "tokens" text) "and says what each kept tool costs"))
  (let ((text (cell-entry "nodecode-mcp-router" "mcp-router" "kubernetes frobnicator")))
    (is (search "nothing was kept" text) "and says so when nothing clears the floor")))
