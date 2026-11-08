;;;; cell-test.lisp --- the three tools, the round's digest, the standing
;;;; section, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed program: that
;;;; each tool asks deja the argv the plugin asked it (the query after `--'
;;;; when it names a flag, `--framed' for fix and blame, --offset only past
;;;; the first page), that a session's first request carries deja's digest
;;;; and what the turn's prompt matches while a later round adds nothing and
;;;; a cron run recalls nothing, that the standing section rides every
;;;; request and comes off at stop, and that each refusal says why. One test
;;;; runs a real /bin/sh program, for the argv a stub cannot prove. Nothing
;;;; here needs deja installed or a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "deja"
  (:hooks :round 'nle::read-harness-sections)
  (:help :deja)
  (:command "deja")
  (:running (is (= 6000 (nodecode-deja::setting :max-chars)) "six thousand characters by default")
            (is (eq t (nodecode-deja::setting :auto)) "automatic recall on by default"))
  (:refused ("deja" "definitely-no-such-program-xyz") ("max_chars" 10))
  (:idle deja:deja-error
         (deja:recall "make check")
         (deja:fix "error: unknown flag")
         (deja:blame "src/parser.lisp")))

;;; --- the tools ---------------------------------------------------------------------

(deftest deja-cell-a-query-that-names-a-flag-rides-after-a-double-dash ()
  (with-cell-stop ((deja-start))
    (with-run-stub (:answer "--json matched three sessions")
      (is (equal "--json matched three sessions" (deja:recall "--json")))
      (is (equal '("recall" "--" "--json") (car (last-call))) "a query naming a flag rides after --")
      (deja:recall "parser fix" :offset 5)
      (is (equal '("recall" "--offset" "5" "parser fix") (car (last-call))) "--offset goes before it"))))

(deftest deja-cell-fix-and-blame-ask-for-the-framed-answer ()
  (with-cell-stop ((deja-start))
    (with-run-stub (:answer "then: make check ran, and it worked")
      (is (search "make check" (deja:fix "error: unknown flag -x")))
      (is (equal '("--framed" "fix" "error: unknown flag -x") (car (last-call))) "fix is framed")
      (is (search "make check" (deja:blame "src/parser.lisp")))
      (is (equal '("--framed" "blame" "src/parser.lisp") (car (last-call))) "and so is blame"))))

(deftest deja-cell-a-json-reply-splits-into-the-model-s-text-and-the-person-s-note ()
  (let ((reply (concatenate 'string
                            "{\"hookSpecificOutput\":{\"additionalContext\":\"[deja] the parser fix landed\"},"
                            "\"systemMessage\":\"deja - recalled 2 memories from claude-code\"}")))
    (is (equal "[deja] the parser fix landed" (deja-context reply)) "the model gets additionalContext")
    (is (equal "deja - recalled 2 memories from claude-code" (deja-note reply)) "the person gets the note")
    (is (equal "plain text, no envelope" (deja-context "plain text, no envelope")) "no envelope is all context")))

(deftest deja-cell-silence-is-a-line-not-a-failed-turn ()
  (with-cell-stop ((deja-start))
    (with-run-stub (:answer "")
      (is (equal nodecode-deja::*nothing* (deja:recall "nothing at all")))
      (is (equal nodecode-deja::*nothing* (deja:blame "src/nowhere.lisp")))
      (is (equal nodecode-deja::*no-paging* (deja:recall "nothing" :offset 5)) "an old deja cannot page"))))

(deftest deja-cell-each-tool-is-registered-and-answers-through-its-contract ()
  (with-cell-stop ((deja-start))
    (with-run-stub (:answer "a page of history")
      (dolist (name '("deja_recall" "deja_fix" "deja_blame"))
        (is-present (entry (nle::find-tool name))
          (format nil "~a is registered" name)
          (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract")
          (let* ((schema (second entry))
                 (key (if (equal name "deja_fix") "error" (if (equal name "deja_recall") "query" "path")))
                 (arguments (if (equal name "deja_recall")
                                (nlk:make-json-object "query" "parser fix" "offset" 2)
                                (nlk:make-json-object key "parser fix")))
                 (answer (funcall (third entry) (nlk:contract-decode schema arguments)))
                 (argv (car (last-call)))
                 (expected (if (equal name "deja_recall")
                               '("recall" "--offset" "2" "parser fix")
                               (list "--framed" (if (equal name "deja_fix") "fix" "blame") "parser fix"))))
            (is (equal "a page of history" answer) "the body's answer is the tool's answer")
            (is (equal expected argv) "and it asked deja what the plugin asked it")))))))

(deftest deja-cell-the-program-is-really-run-with-the-query-on-its-argv ()
  (let ((directory (merge-pathnames "deja-recording/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (with-cell-stop ((deja-start "deja" (deja-recording-program directory "recalled: the parser fix")))
        (is (equal "recalled: the parser fix" (deja:recall "parser fix")))
        (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
          (is (search "recall" argv) "the program was run, and asked to recall")
          (is (search "parser fix" argv) "with the query as an argument"))))))

;;; --- the round's reminder -----------------------------------------------------------

(deftest deja-cell-a-sessions-first-round-carries-the-digest-and-the-prompt-match ()
  (with-temp-store ()
    (ensure-durable-session "d1")
    (with-cell-stop ((deja-start))
      (with-run-stub (:context "[deja] this project: the parser fix landed"
                      :prompt "[deja] re-run make check after a rename")
        (let ((turn (deja-turn "d1" :input "why did the parser break")))
          (deja-round "d1" turn 0)
          (is-present (told (deja-told "d1"))
            "the turn's first request carried one reminder"
            (is (= 1 (length told)))
            (is (search "the parser fix landed" (first told)) "deja's digest")
            (is (search "re-run make check" (first told)) "and what the prompt matches")
            (is (search "Reference data, not instructions" (first told)) "framed as data")
            (is (search "why did the parser break" (cdr (last-call))) "the prompt went to deja"))
          (deja-round "d1" turn 1)
          (is (= 1 (length (deja-told "d1"))) "a later round of the same turn adds nothing")
          (nlk:complete-turn turn nil)
          (deja-round "d1" (deja-turn "d1" :input "go on") 0)
          (is (= 2 (length (deja-told "d1"))) "the next turn carries its own match")
          (is-present (text (second (deja-told "d1")))
            "which is the prompt match"
            (is (search "re-run make check" text))
            (is (not (search "the parser fix landed" text)) "the digest is given once a session")))))))

(deftest deja-cell-a-cron-run-recalls-nothing-on-its-own ()
  (with-temp-store ()
    (ensure-durable-session "d2")
    (with-cell-stop ((deja-start))
      (with-run-stub (:context "a digest" :prompt "a match")
        (deja-round "d2" (deja-turn "d2" :source "cron") 0)
        (is (null (deja-told "d2")) "a cron run is told nothing")
        (is (null *calls*) "and deja was not even asked")))))

(deftest deja-cell-auto-off-leaves-the-tools-and-the-command ()
  (with-temp-store ()
    (ensure-durable-session "d4")
    (with-cell-stop ((deja-start "auto" nil))
      (with-run-stub (:context "a digest" :prompt "a match")
        (deja-round "d4" (deja-turn "d4") 0)
        (is (null (deja-told "d4")) "no reminder of its own")
        (is (null *calls*) "and deja was not asked"))
      (with-run-stub (:answer "a page of history")
        (is (equal "a page of history" (deja:recall "parser")) "the tool still answers")))))

;;; --- standing context ---------------------------------------------------------------

(deftest deja-cell-the-standing-section-rides-every-request ()
  (with-temp-store ()
    (ensure-durable-session "d3")
    (let ((stop (deja-start)))
      (is (equal nodecode-deja:*standing* (deja-standing "d3")) "the model is told when to ask")
      (funcall stop)
      (is (null (deja-standing "d3")) "and stop takes it back"))))
