;;;; cell-test.lisp --- the lifecycle, the tools, the round, the command,
;;;; and the store across a restart.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the cell starts and stops cleanly and refuses
;;;; a bad setting; that a call without its query is refused before any body;
;;;; that a turn's ask and answer are kept verbatim and answered back through
;;;; zeromem_recall with their provenance, never the calling session's own;
;;;; that forgetting deletes and is not undone by the next read; that a
;;;; turn's first round carries past sessions' words once and no more; that
;;;; /zeromem shows the counters, previews an import, writes it, and forgets;
;;;; and that the ledger is read back by the next start.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "zeromem"
  (:hooks :round)
  (:help :zeromem)
  (:command "zeromem")
  (:running (is (nle::find-tool "zeromem_recall") "the recall tool is registered")
            (is (nle::find-tool "zeromem_stats") "and the counters")
            (is (nle::find-tool "zeromem_forget_session") "and the forget")
            (is (= 5 (nodecode-zeromem::setting :top-k)) "top_k defaults to five")
            (is (eql t (nodecode-zeromem::setting :recall)) "a turn is told by default")
            (is (search "Recalled from past sessions"
                        (nodecode-zeromem::recall-block
                         (list (list :session-id "s-1" :session-turn 2 :speaker "user"
                                     :text "the words" :at "2026-11-08T10:00:00.000Z"))
                         700))
                "the block names itself and the turn"))
  (:refused ("top_k" 0) ("chars" 10) ("store" 5))
  (:idle nodecode-zeromem:zeromem-error (zeromem:recall "anything") (zeromem:stats)
         (zeromem:import-turns)))

;;; --- a call's own shape --------------------------------------------------------------

(deftest zeromem-cell-a-call-without-its-query-is-refused ()
  (is (signals-error nlk:contract-refusal (nodecode-zeromem:decode-zeromem-recall (nlk:json-object)))
      "an empty call has no query and is refused")
  (is (signals-error nlk:contract-refusal
        (nodecode-zeromem:decode-zeromem-forget-call (nlk:json-object "session_id" "")))
      "an empty session id is no session id")
  (is (= 3 (nodecode-zeromem:zeromem-recall-top-k (nodecode-zeromem:decode-zeromem-recall
                                  (nlk:json-object "query" "x" "top_k" 3))))
      "a call may ask for its own count"))

;;; --- keeping and answering ------------------------------------------------------------

(deftest zeromem-cell-keeps-a-turn-and-answers-it-with-provenance ()
  (with-zeromem ()
    (ensure-durable-session "z-past")
    (zeromem-say (zeromem-admit "z-past" "the deploy script lives in ops/deploy.sh")
                 "it runs just release, with no arguments")
    (is (= 2 (length (zeromem-ingest "z-past"))) "a turn's ask and its answer, both kept")
    (is (equal '("the deploy script lives in ops/deploy.sh"
                 "it runs just release, with no arguments")
               (mapcar (lambda (turn) (getf turn :text)) (zeromem-kept)))
        "kept verbatim, whole, and in the order they were said")
    (let* ((answer (nodecode-zeromem::recall-tool
                    (nodecode-zeromem:decode-zeromem-recall
                     (nlk:json-object "query" "deploy release"))))
           (decoded (nlk:decode-json answer))
           (evidence (nlk:json-array decoded "evidence")))
      (is (= 2 (length evidence)) "both turns of the past session answer")
      (is (search "ops/deploy.sh" answer) "the ask's words, verbatim")
      (is (search "just release" answer) "and the answer's")
      (is (equal "z-past" (nlk:json-value (aref evidence 0) :string "session_id"))
          "each with its session")
      (is (member (nlk:json-value (aref evidence 0) :string "speaker") '("user" "assistant")
                  :test #'equal)
          "and its speaker")
      (is (search "2026" (nlk:json-value (aref evidence 0) :string "at"))
          "and the day it was said"))
    (is (null (nodecode-zeromem::recall-turns (zeromem-store) "deploy" :exclude "z-past"))
        "the calling session's own turns are never among them")
    (is (null (nodecode-zeromem::recall-turns (zeromem-store) "unrelated zebra"))
        "a query nothing answers answers nothing")
    (is (search "no past turn answers that" (zeromem:recall "unrelated zebra"))
        "and the verb says so")))

(deftest zeromem-cell-one-turn-is-kept-once ()
  (with-zeromem ()
    (ensure-durable-session "z-once")
    (zeromem-say (zeromem-admit "z-once" "say it once") "said")
    (zeromem-ingest "z-once")
    (zeromem-ingest "z-once")
    (is (= 2 (length (zeromem-kept))) "a second read keeps nothing new")
    (is (= 2 (length (zeromem-kept))) "and the position holds")))

;;; --- forgetting -----------------------------------------------------------------------

(deftest zeromem-cell-forgetting-deletes-and-stays-deleted ()
  (with-zeromem ()
    (ensure-durable-session "z-gone")
    (zeromem-say (zeromem-admit "z-gone" "remember the code 4711") "noted, 4711")
    (zeromem-ingest "z-gone")
    (is (= 2 (length (zeromem-kept))))
    (let ((answer (nlk:decode-json
                   (nodecode-zeromem::forget-tool
                    (nodecode-zeromem:decode-zeromem-forget-call
                     (nlk:json-object "session_id" "z-gone"))))))
      (is (= 2 (nlk:json-value answer :integer "deleted_turns")) "both turns went")
      (is (zerop (length (zeromem-kept))) "and the store is empty"))
    (is (null (zeromem-ingest "z-gone"))
        "the log still holds them, but they are not read in again")
    (is (zerop (length (zeromem-kept))) "the store stays empty")
    (is (search "no turn of z-gone" (nodecode-zeromem::run-slash "forget z-gone" "z-x"))
        "the command says nothing is stored")
    (let ((nlk:*scribe-session-id* "z-live"))
      (is (search "refusing to delete the active session"
                  (nodecode-zeromem::run-slash "forget z-live" "z-live"))
          "the running session is never forgotten"))))

;;; --- the round -----------------------------------------------------------------------

(deftest zeromem-cell-a-turn-is-told-what-past-sessions-answered ()
  (with-zeromem ()
    (ensure-durable-session "z-old")
    (zeromem-say (zeromem-admit "z-old" "the weekly invoice cron is called biller")
                 "it runs on Mondays")
    (zeromem-ingest "z-old")
    (ensure-durable-session "z-new")
    (let ((turn (zeromem-admit "z-new" "when does the invoice cron run?")))
      (zeromem-round "z-new" turn 0)
      (let ((told (zeromem-told "z-new")))
        (is (= 1 (length told)) "one reminder, once")
        (is (search "Recalled from past sessions" (first told)) "named for what it is")
        (is (search "the weekly invoice cron is called biller" (first told))
            "the past session's words, verbatim")
        (is (search "z-old #" (first told)) "with their provenance")
        (is (not (search "when does the invoice cron run" (first told)))
            "and never the turn's own ask"))
      (zeromem-round "z-new" turn 1)
      (is (= 1 (length (zeromem-told "z-new"))) "a later round of the same turn adds none")
      (is (= 3 (length (zeromem-kept))) "the round kept the new turn's ask and the old two"))))

(deftest zeromem-cell-recall-false-tells-a-turn-nothing ()
  (with-zeromem ("recall" nil)
    (ensure-durable-session "z-old")
    (zeromem-say (zeromem-admit "z-old" "the invoice cron is called biller") "Mondays")
    (zeromem-ingest "z-old")
    (ensure-durable-session "z-new")
    (let ((turn (zeromem-admit "z-new" "when does the invoice cron run?")))
      (zeromem-round "z-new" turn 0)
      (is (null (zeromem-told "z-new")) "recall false puts nothing into a turn")
      (is (= 3 (length (zeromem-kept))) "but the turns are still kept"))))

;;; --- the command ----------------------------------------------------------------------

(deftest zeromem-cell-the-command-counts-imports-and-forgets ()
  (with-zeromem ()
    (ensure-durable-session "z-cmd")
    (zeromem-say (zeromem-admit "z-cmd" "ship it") "shipped")
    (is (search "0 turns" (nodecode-zeromem::run-slash "" "z-cmd")) "an empty store says so")
    (is (search "would import 2 turns" (nodecode-zeromem::run-slash "import" "z-cmd"))
        "a preview")
    (is (zerop (length (zeromem-kept))) "and a preview writes nothing")
    (is (search "imported 2 turns" (nodecode-zeromem::run-slash "import yes" "z-cmd"))
        "the import writes")
    (is (search "2 turns from 1 session" (nodecode-zeromem::run-slash "stats" "z-cmd"))
        "and the counters know it")
    (is (search "nothing new to import" (nodecode-zeromem::run-slash "import" "z-cmd"))
        "a second import has nothing left")
    (is (search "unknown subcommand" (nodecode-zeromem::run-slash "wat" "z-cmd"))
        "a word it does not know says so")))

;;; --- across a restart -----------------------------------------------------------------

(deftest zeromem-cell-the-ledger-outlives-a-restart ()
  (with-temp-store ()
    (ensure-durable-session "z-keep")
    (zeromem-say (zeromem-admit "z-keep" "the pass phrase is violet") "kept")
    (with-cell-stop ((zeromem-start))
      (zeromem-ingest "z-keep")
      (is (= 2 (length (zeromem-kept))) "kept once"))
    (with-cell-stop ((zeromem-start))
      (is (= 2 (length (zeromem-kept))) "the next start reads the ledger back")
      (is (search "violet" (zeromem:recall "pass phrase")) "and answers from it")
      (is (null (zeromem-ingest "z-keep")) "with no turn kept twice"))))
