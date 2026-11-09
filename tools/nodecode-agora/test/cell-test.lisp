;;;; cell-test.lisp --- lifecycle, the nine tools, /agora, and every refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not a model's judgement:
;;;; that a motion is argued one round at a time and a closed one takes no
;;;; more speech; that a vote replaces its role's earlier vote and the tally
;;;; decides; that only an adopted motion may be cited by a task; that each
;;;; tool answers through its declared contract, which refuses a call that
;;;; does not satisfy it; that a section with no roles refuses and names the
;;;; setting; that the ledger is a file under the home that survives a
;;;; restart; and that /agora reads and raises from the command line.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "agora"
  (:config "roles" #("architect" "developer" "reviewer"))
  (:help :agora)
  (:command "agora")
  (:running
   (dolist (name '("agora_raise_motion" "agora_speak" "agora_vote" "agora_close_motion"
                   "agora_get_motion" "agora_list_motions" "agora_create_task"
                   "agora_list_tasks" "agora_close_task"))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name)))
   (is (= 40 (nodecode-agora::setting :max-speeches)) "a transcript holds forty speeches")
   (is (equal "agora/ledger.sexp" (nodecode-agora::setting :ledger)) "the ledger's default path")
   (is (equal '("architect" "developer" "reviewer") (nodecode-agora::setting :roles))
       "the roles the section names"))
  (:stopped
   (dolist (name '("agora_raise_motion" "agora_speak" "agora_vote" "agora_close_motion"
                   "agora_get_motion" "agora_list_motions" "agora_create_task"
                   "agora_list_tasks" "agora_close_task"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("max_speeches" 0)
   ("roles" "not a list"))
  (:idle agora:agora-error (agora:raise-motion "is anyone home?")))

;;; --- the rounds ----------------------------------------------------------------

(deftest agora-cell-a-motion-is-argued-one-round-at-a-time ()
  (with-agora ()
    (is (search "m-1 raised: SQLite or PostgreSQL?"
                (agora:raise-motion "SQLite or PostgreSQL?"
                                     :description "two options, one year" :rounds 2)))
    (is (search "judged by 3 roles (architect, developer, reviewer) over 3 rounds"
                (agora:raise-motion "A second question")))
    (is (search "m-1 round 1" (agora:speak "m-1" "architect" "SQLite: one file, no server."
                                            :stance "propose")))
    (is (search "round 1" (agora:speak "m-1" "developer" "Agreed for the first year."
                                        :stance "support")))
    (is (search "round 1" (agora:speak "m-1" "reviewer" "Only if backups exist.")))
    (is (search "round 2" (agora:speak "m-1" "architect" "Then a migration path."))
        "a new round starts once every role has spoken")
    (is (search "5 words in all" (agora:speak "m-1" "developer" "It does.")))
    (let ((transcript (agora:get-motion "m-1")))
      (is (search "round 2  architect (propose): Then a migration path." transcript))
      (is (search "no vote cast yet" transcript))
      (is (search "open: agora_vote, then agora_close_motion" transcript)))))

;;; --- the votes -----------------------------------------------------------------

(deftest agora-cell-the-tally-decides-and-a-role-may-change-its-mind ()
  (with-agora ()
    (agora:raise-motion "Move the build to Linux")
    (is (search "1 for, 0 against" (agora:vote "m-1" "architect" "for" :reason "cheaper")))
    (agora:vote "m-1" "developer" "against")
    (is (search "2 for, 1 against" (agora:vote "m-1" "reviewer" "for")))
    ;; the same role votes again: the earlier vote is replaced, not added
    (is (search "1 for, 1 against, 1 abstain" (agora:vote "m-1" "reviewer" "abstain")))
    (let ((decision (agora:close-motion "m-1" :rationale "one plan, one build farm")))
      (is (search "m-1 rejected: 1 for, 1 against, 1 abstain" decision) "a tie is not adopted")
      (is (search "rationale: one plan, one build farm" decision)))
    (agora:raise-motion "Move the build to Linux, again" :rounds 1)
    (agora:vote "m-2" "architect" "for")
    (agora:vote "m-2" "developer" "for")
    (is (search "m-2 adopted: 2 for, 0 against" (agora:close-motion "m-2")))
    (is (search "adopted, closed" (agora:get-motion "m-2")))
    (is (search "architect: for" (agora:get-motion "m-2")))))

;;; --- the tasks -----------------------------------------------------------------

(deftest agora-cell-only-an-adopted-motion-becomes-a-task ()
  (with-agora ()
    (agora:raise-motion "Adopt a deadline for reviews")
    (is (search "still open" (refusal (lambda () (agora:create-task "Write it up" :motion "m-1")))))
    (agora:vote "m-1" "architect" "against")
    (agora:vote "m-1" "developer" "against")
    (agora:close-motion "m-1")
    (is (search "was rejected" (refusal (lambda () (agora:create-task "Write it up" :motion "m-1")))))
    (agora:raise-motion "Adopt a deadline, revised")
    (agora:vote "m-2" "architect" "for")
    (agora:close-motion "m-2")
    (is (search "t-1 added: Publish the deadline  (for m-2)"
                (agora:create-task "Publish the deadline" :assignee "writer" :motion "m-2")))
    (is (search "assignee: writer" (agora:create-task "Tell the team" :assignee "writer")))
    (is (search "2 tasks" (agora:list-tasks :status "all")))
    (is (search "t-1 done -- shipped in the release" (agora:close-task "t-1" :outcome "shipped in the release")))
    (is (search "1 task" (agora:list-tasks :status "open")) "the done one is not open")
    (is (search "t-1" (agora:list-tasks :status "done")) "the done one is listed as done")))

;;; --- the refusals --------------------------------------------------------------

(deftest agora-cell-refusals-say-why ()
  (with-agora ()
    (is (search "a motion needs a title" (refusal (lambda () (agora:raise-motion "  ")))))
    (is (search "no motion m-9" (refusal (lambda () (agora:get-motion "m-9")))))
    (agora:raise-motion "A question")
    (is (search "a speech needs words" (refusal (lambda () (agora:speak "m-1" "architect" "")))))
    (is (search "a stance is propose, support, oppose, question"
                (refusal (lambda () (agora:speak "m-1" "architect" "hi" :stance "shrug")))))
    (is (search "a vote is for, against, abstain" (refusal (lambda () (agora:vote "m-1" "architect" "maybe")))))
    (is (search "has no votes yet" (refusal (lambda () (agora:close-motion "m-1")))))
    (agora:vote "m-1" "architect" "for")
    (agora:close-motion "m-1")
    (is (search "m-1 was closed already" (refusal (lambda () (agora:close-motion "m-1")))))
    (is (search "is closed (adopted)" (refusal (lambda () (agora:speak "m-1" "reviewer" "later")))))
    (is (search "is closed (adopted)" (refusal (lambda () (agora:vote "m-1" "reviewer" "for")))))))

;;; --- the setting, the contract and the file ------------------------------------

(deftest agora-cell-a-motion-with-no-roles-names-the-setting ()
  (with-temp-store ()
    (with-cell-stop ((agora-start))
      (is (search "set agora.roles" (refusal (lambda () (agora:raise-motion "Who decides?")))))
      (is (search "m-1 raised" (agora:raise-motion "Who decides?" :participants '("tester" "writer"))))
      (is (search "tester, writer over 3 rounds" (agora:get-motion "m-1"))))))

(deftest agora-cell-a-tool-answers-through-its-contract ()
  (with-agora ()
    (is (search "m-1 raised: Where does the ledger live?"
                (agora-tool "agora_raise_motion" "{\"title\":\"Where does the ledger live?\"}")))
    (is (signals-error nlk:contract-refusal
          (agora-tool "agora_raise_motion" "{\"title\":42}"))
        "a title that is not text is refused before the body")
    (is (signals-error nlk:contract-refusal
          (agora-tool "agora_speak" "{\"motion_id\":\"m-1\",\"role\":\"architect\",\"text\":\"hi\",\"stance\":\"shrug\"}"))
        "a stance outside its options is refused")
    (is (search "m-1 round 1: architect (oppose) spoke"
                (agora-tool "agora_speak" "{\"motion_id\":\"m-1\",\"role\":\"architect\",\"text\":\"against it\",\"stance\":\"oppose\"}")))
    (is (search "1 for, 0 against" (agora-tool "agora_vote" "{\"motion_id\":\"m-1\",\"role\":\"architect\",\"choice\":\"for\"}")))
    (is (search "m-1 adopted: 1 for" (agora-tool "agora_close_motion" "{\"motion_id\":\"m-1\"}")))
    (is (search "t-1 added" (agora-tool "agora_create_task" "{\"title\":\"Move it\",\"motion_id\":\"m-1\"}")))
    (is (search "1 task" (agora-tool "agora_list_tasks" "{}")))
    (is (search "t-1 done" (agora-tool "agora_close_task" "{\"task_id\":\"t-1\",\"outcome\":\"moved\"}")))))

(deftest agora-cell-the-ledger-is-a-file-that-survives-a-restart ()
  (with-temp-store ()
    (with-cell-stop ((agora-start "roles" +roles+))
      (agora:raise-motion "A decision worth keeping")
      (agora:speak "m-1" "architect" "yes, keep it")
      (agora:vote "m-1" "architect" "for")
      (agora:close-motion "m-1" :rationale "the file is the log"))
    (with-cell-stop ((agora-start "roles" +roles+))
      (let* ((path (nodecode-agora::ledger-path))
             (transcript (agora:get-motion "m-1")))
        (is (probe-file path) "the ledger is on disk after the restart")
        (is (search "the file is the log" (nlk:read-text path)) "and it is readable text")
        (is (search "yes, keep it" transcript) "the speech outlived the restart")
        (is (search "adopted, closed" transcript) "and so did the decision")
        (is (search "architect: for" transcript))))))

;;; --- /agora --------------------------------------------------------------------

(deftest agora-cell-the-command-reads-and-raises ()
  (with-agora ()
    (let ((text (nodecode-agora::run-slash "" "s-1")))
      (is (search "agora: 0 open motion" text))
      (let ((dialog (nle::dialog-json (nth-value 1 (nodecode-agora::run-slash "" "s-1")))))
        (is (equal "Agora" (nle::json-value dialog :text "title")) "typed alone, its panel")
        (is (equal '("/agora motion" "/agora task")
                   (remove nil (map 'list (lambda (row) (nle::json-value row :text "ask" "line"))
                                    (nle::json-array dialog "rows"))))
            "two rows raise, two run")))
    (is (search "m-1 raised" (nodecode-agora::run-slash "motion Shall the board be the ledger?" "s-1")))
    (is (search "Shall the board be the ledger?" (nodecode-agora::run-slash "show m-1" "s-1"))
        "show reads it back")
    (is (search "1 open motion" (nodecode-agora::run-slash "" "s-1")) "the overview counts what is open")
    (is (search "m-1" (nodecode-agora::run-slash "list" "s-1")))
    (is (search "m-1 has no votes yet" (refusal (lambda () (nodecode-agora::run-slash "close m-1" "s-1")))))))
