;;;; cell-test.lisp --- the provenance gate, with no network and no operator.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every check is a pure function of a text and what the user typed, so the
;;;; tools are called the way a model's call reaches them -- through the real
;;;; dispatch and contract -- and the hook is funcalled by hand, on this
;;;; thread. The evidence log lands in the test home's own folder.

(in-package #:nodecode.test)

(define-test-slice "provenance" "PROVENANCE-CELL-"
  :start nodecode-provenance:start-cell)

(define-cell-lifecycle-tests "provenance"
  (:hooks 'nle::snapshot-context-request :tool)
  (:help :provenance)
  (:command "provenance")
  (:running
   (is (nle::find-tool "provenance_check") "provenance_check is registered")
   (is (nle::find-tool "provenance_report") "provenance_report is registered")
   (is (equal "observe" (nodecode-provenance::mode-setting)) "observe is the default mode"))
  (:stopped
   (is (zerop (hash-table-count nodecode-provenance::*said*)) "stop forgets the captured texts"))
  (:refused ("retention_days" 0)))

;;; --- fixtures ---------------------------------------------------------------------

(defun provenance-tool (name arguments)
  "NAME's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call "call-provenance-1" name arguments)))

(defun provenance-op (text &key (call-id "c1"))
  "The :TOOL op of an eval call that keeps TEXT as a memory."
  (list :name "eval" :call-id call-id
        :arguments (nlk:make-json-object
                    "form" (format nil "(define-memory ship-friday ~s)" text))))

(defun provenance-run (op)
  "Run OP through the gate => (VALUES RAN RESULT)."
  (let ((ran nil))
    (values (let ((result (nodecode-provenance::gate-call
                           op (lambda (op) (declare (ignore op)) (setf ran t) :ok))))
              result)
            ran)))

;;; --- the detector ------------------------------------------------------------------

(deftest provenance-cell-detects-a-decision-with-the-user-as-subject ()
  (let ((claims (nodecode-provenance:claims-in "User decided to ship on Friday.")))
    (is (= 1 (length claims)) "one claim")
    (is (equal "decided" (nodecode-provenance::claim-pattern (first claims))) "by the verb that matched"))
  (is (= 1 (length (nodecode-provenance:claims-in "The owner already approved the migration."))) "an adverb between the subject and the verb does not hide it")
  (is (= 1 (length (nodecode-provenance:claims-in "用户决定周五发布。"))) "the Chinese form is caught too"))

(deftest provenance-cell-leaves-open-and-generic-forms-alone ()
  (is (null (nodecode-provenance:claims-in "The user has not decided yet.")) "a negated form")
  (is (null (nodecode-provenance:claims-in "If the user approved, proceed.")) "a conditional")
  (is (null (nodecode-provenance:claims-in "Has the user decided?")) "a question")
  (is (null (nodecode-provenance:claims-in "Step 3: the user confirmed the order.")) "a spec line")
  (is (null (nodecode-provenance:claims-in "In the usability study, the user chose the cheaper plan.")) "a generic actor in a study")
  (is (null (nodecode-provenance:claims-in "发布日期待用户确认。")) "待 makes it open"))

(deftest provenance-cell-a-preference-counts-only-when-the-scope-says-so ()
  (is (null (nodecode-provenance:claims-in "The user prefers dark mode.")) "decisions are the default scope")
  (is (= 1 (length (nodecode-provenance:claims-in "The user prefers dark mode."
                                                  :scope "decisions+preferences"))) "and decisions+preferences asks for it"))

;;; --- the tools ---------------------------------------------------------------------

(deftest provenance-cell-check-passes-a-quote-the-user-typed ()
  (with-cell-stop ((provenance-start))
    (let ((answer (provenance-tool
                   "provenance_check"
                   "{\"text\": \"User decided to ship on Friday (\\\"I decided we ship on Friday\\\").\", \"user_text\": \"I decided we ship on Friday\"}")))
      (is (search "pass" answer) "the claim passes")
      (is (search "I decided we ship on Friday" answer) "and the answer names the quote"))))

(deftest provenance-cell-check-records-a-claim-with-no-quote ()
  (with-cell-stop ((provenance-start))
    (let ((answer (provenance-tool "provenance_check"
                                    "{\"text\": \"User decided to ship on Friday.\", \"user_text\": \"I decided we ship on Friday\"}")))
      (is (search "would_block" answer) "observe mode never refuses, it records")
      (is (search "no_quote" answer) "with the reason"))
    (is (plusp (length (nodecode-provenance::evidence-rows))) "and the evidence log has the row")))

(deftest provenance-cell-a-quote-in-a-question-does-not-back-a-decision ()
  (with-cell-stop ((provenance-start))
    (let ((answer (provenance-tool
                   "provenance_check"
                   "{\"text\": \"The user approved deleting the prod database (\\\"Should we delete the prod database?\\\").\", \"user_text\": \"Should we delete the prod database?\"}")))
      (is (search "quote_from_question" answer) "a question is not a decision"))))

(deftest provenance-cell-a-dropped-negation-does-not-back-an-approval ()
  (with-cell-stop ((provenance-start))
    (let ((answer (provenance-tool
                   "provenance_check"
                   "{\"text\": \"The user approved deleting the prod database (\\\"delete the prod database\\\").\", \"user_text\": \"Never delete the prod database without asking me\"}")))
      (is (search "quote_negated" answer) "the quote follows a negation"))))

(deftest provenance-cell-an-unrelated-quote-does-not-back-a-decision ()
  (with-cell-stop ((provenance-start))
    (let ((answer (provenance-tool
                   "provenance_check"
                   "{\"text\": \"The user approved deleting the prod database (\\\"can you look at the logs\\\").\", \"user_text\": \"can you look at the logs for me\"}")))
      (is (search "quote_unrelated" answer) "a quote of something else shares no key words"))))

(deftest provenance-cell-the-contract-refuses-a-text-that-is-missing ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-provenance::check-ask)
                              (nlk:make-json-object "user_text" "hi"))) "text is required at the boundary"))

(deftest provenance-cell-the-report-names-what-it-saw ()
  (with-cell-stop ((provenance-start))
    (provenance-tool "provenance_check"
                      "{\"text\": \"User decided to ship on Friday.\", \"user_text\": \"a question about lunch\"}")
    (let ((report (provenance-tool "provenance_report" "{\"limit\": 3}")))
      (is (search "would_block" report) "the report names the flagged verdict")
      (is (search "User decided" report) "and the report quotes the claim"))))

;;; --- capture and the hook ------------------------------------------------------------

(deftest provenance-cell-capture-keeps-what-the-user-typed ()
  (nodecode-provenance::capture-around
   (lambda (&rest arguments) (declare (ignore arguments)) :context)
   "s-capture" :prior-context nil
   :messages (list (nlk:json-object "role" "user" "content" "I decided we ship on Friday.")))
  (is (equal '("I decided we ship on Friday.") (nodecode-provenance::session-said "s-capture")) "the user's own words are kept for the gate this turn runs"))

(deftest provenance-cell-observe-mode-lets-the-write-through ()
  (with-cell-stop ((provenance-start))
    (multiple-value-bind (result ran) (provenance-run (provenance-op "User decided to ship on Friday."))
      (declare (ignore result))
      (is (eq t ran) "the call ran")
      (is (search "would_block" (nodecode-provenance::report-text)) "and the flag was recorded"))))

(deftest provenance-cell-block-mode-refuses-a-write-the-words-do-not-back ()
  (with-cell-stop ((provenance-start "mode" "block"))
    (multiple-value-bind (result ran) (provenance-run (provenance-op "User decided to ship on Friday."))
      (is (null ran) "the call did not run")
      (is (search "Nothing was written" result) "and the model is told how to fix it"))))

(deftest provenance-cell-a-call-that-keeps-nothing-is-left-alone ()
  (with-cell-stop ((provenance-start "mode" "block"))
    (let ((op (list :name "eval" :call-id "c2"
                    :arguments (nlk:make-json-object
                                "form" "(+ 1 2)"))))
      (multiple-value-bind (result ran) (provenance-run op)
        (declare (ignore result))
        (is (eq t ran) "an eval that keeps no memory runs")
        (is (null (nodecode-provenance::memory-write-text
                   (list :name "shell" :arguments (nlk:make-json-object "command" "ls")))) "and so does every other tool")))))
