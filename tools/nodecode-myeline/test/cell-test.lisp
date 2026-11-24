;;;; cell-test.lisp --- the lifecycle, the four tools, /myeline, and the receipts file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, a remembered fact is staged
;;;; and not answered until the person approves it, an approved fact answers
;;;; with its source, a fact that looks like a credential is refused, a fact
;;;; offered twice is one fact, a retraction takes a reason that stays in the
;;;; audit log, the day's card stages one fact per line, and the receipts
;;;; outlive a restart because the file holds them. Every call runs against a
;;;; receipts file in the test's own home, and nothing here dials anything.

(in-package #:nodecode.test)

;;; --- fixtures -------------------------------------------------------------------

(defun myeline-test-folder ()
  "The folder one test's receipts are kept in, inside the test's own home."
  (namestring (nlk:home "myeline-test/")))

(defun myeline-receipts-file ()
  (merge-pathnames "receipts.json" (nlk:home "myeline-test/")))

(defun myeline-empty-receipts ()
  (when (probe-file (myeline-receipts-file)) (delete-file (myeline-receipts-file))))

(defun myeline-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun myeline-answer (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it; a refusal is its own text."
  (if (and (stringp text) (plusp (length text)) (char= (char text 0) #\{))
      (nlk:decode-json text)
      text))

(defun myeline-remember (fact source)
  (myeline-answer (myeline-tool "cortex_remember"
                                 (nlk:json-object "fact" fact "source" source))))

(defun myeline-query (&optional (query ""))
  (myeline-answer (myeline-tool "cortex_query" (nlk:json-object "query" query))))

(defun myeline-ids (answer)
  (map 'list (lambda (fact) (nlk:json-value fact :string "id"))
       (nlk:json-value answer :array "facts")))

;;; --- lifecycle ------------------------------------------------------------------

(define-test-slice "myeline" "MYELINE-CELL-" :start nodecode-myeline:start-cell)

(define-cell-lifecycle-tests "myeline"
  (:config "data_dir" (myeline-test-folder))
  (:help :myeline)
  (:command "myeline")
  (:running (is (nle::find-tool "cortex_query") "cortex_query is registered")
            (is (nle::find-tool "cortex_remember"))
            (is (nle::find-tool "cortex_daily_card"))
            (is (nle::find-tool "cortex_retract"))
            (is (pathnamep (nodecode-myeline::receipts-file)) "the receipts file is resolved")
            (is (eq t (nodecode-myeline::setting :model-may-retract)) "the model may retract unless the section says otherwise"))
  (:stopped (is (null (nle::find-tool "cortex_query")) "stop unregisters the tools"))
  (:refused ("max_live" 0))
  (:idle nodecode-myeline:myeline-error (nodecode-myeline::run-command "" nil)))

(deftest myeline-cell-a-missing-data-dir-refuses-by-name ()
  (is-carrying (refusal (refusal-text nlk:config-refusal (myeline-start "data_dir" "")))
               "data_dir"
               "the folder the receipts are kept in")
  (is (null (nle::find-tool "cortex_query")) "a refused start installed nothing"))

;;; --- staging, approval, and the receipt -------------------------------------------

(deftest myeline-cell-a-fact-waits-for-the-person-and-answers-with-its-source ()
  (myeline-empty-receipts)
  (with-cell-stop ((myeline-start "data_dir" (myeline-test-folder)))
    (let ((staged (myeline-remember "the user prefers short answers" "chat of 8 November")))
      (is (equal "pending" (nlk:json-value staged :string "state")) "a save is staged, never live")
      (is (equal "f-1" (nlk:json-value staged :string "id")))
      (is (eq nil (nlk:json-value staged :boolean "duplicate"))))
    (let ((before (myeline-query "short")))
      (is (= 0 (nlk:json-value before :integer "total")) "nothing approved answers yet")
      (is (= 1 (nlk:json-value before :integer "pending")) "and the queue knows it waits"))
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "review"))
                 "waiting for you"
                 "the user prefers short answers"
                 "chat of 8 November")
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "approve f-1"))
                 "f-1 is live")
    (let* ((after (myeline-query "short"))
           (fact (aref (nlk:json-value after :array "facts") 0)))
      (is (= 1 (nlk:json-value after :integer "total")) "the approved fact answers")
      (is (equal "the user prefers short answers" (nlk:json-value fact :string "fact")))
      (is (equal "chat of 8 November" (nlk:json-value fact :string "source")) "the source rides with the fact, as the receipt"))
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" ""))
                 "1 live fact"
                 "0 waiting for you")
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "live"))
                 "the user prefers short answers")))

(deftest myeline-cell-a-secret-is-refused-and-a-fact-is-offered-once ()
  (myeline-empty-receipts)
  (with-cell-stop ((myeline-start "data_dir" (myeline-test-folder)))
    (is-carrying (text (myeline-tool "cortex_remember"
                                     (nlk:json-object "fact" "the panel password is hunter2"
                                                       "source" "chat")))
                 "ERROR: myeline:"
                 "looks like a password")
    (is-carrying (text (myeline-tool "cortex_remember"
                                     (nlk:json-object "fact" "the card is 4111 1111 1111 1111"
                                                       "source" "chat")))
                 "card number")
    (let* ((first-offer (myeline-remember "the release is on Friday" "chat"))
           (second-offer (myeline-remember "The release  is on Friday" "chat")))
      (is (equal (nlk:json-value first-offer :string "id") (nlk:json-value second-offer :string "id")) "the same fact offered twice is the same fact")
      (is (eq t (nlk:json-value second-offer :boolean "duplicate"))))
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "review"))
                 "1 fact waiting")
    ;; A fact with no source has no receipt, and is refused.
    (is-carrying (text (myeline-tool "cortex_remember"
                                     (nlk:json-object "fact" "no receipt" "source" "")))
                 "source is required")))

;;; --- retraction and the audit log ----------------------------------------------------

(deftest myeline-cell-a-retraction-takes-a-reason-and-keeps-it ()
  (myeline-empty-receipts)
  (with-cell-stop ((myeline-start "data_dir" (myeline-test-folder)))
    (myeline-remember "the user lives in Ljubljana" "chat")
    (cell-entry "nodecode-myeline" "myeline" "approve f-1")
    (is-carrying (text (myeline-tool "cortex_retract"
                                     (nlk:json-object "id" "f-1" "reason" "")))
                 "reason is required")
    (let ((retired (myeline-answer
                    (myeline-tool "cortex_retract"
                                   (nlk:json-object "id" "f-1" "reason" "moved to Maribor")))))
      (is (equal "retired" (nlk:json-value retired :string "state")))
      (is (equal "moved to Maribor" (nlk:json-value retired :string "reason"))))
    (is (= 0 (nlk:json-value (myeline-query "Ljubljana") :integer "total")) "a retired fact is not answered")
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "audit"))
                 "retracted"
                 "moved to Maribor"
                 "the user lives in Ljubljana")
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "approve f-1"))
                 "f-1 is retired, not pending")
    (is-carrying (text (myeline-tool "cortex_retract"
                                     (nlk:json-object "id" "f-1" "reason" "again")))
                 "not live")))

;;; --- the daily card and the person's reject -----------------------------------------

(deftest myeline-cell-the-daily-card-stages-one-fact-per-line ()
  (myeline-empty-receipts)
  (with-cell-stop ((myeline-start "data_dir" (myeline-test-folder)))
    (let ((card (myeline-answer
                 (myeline-tool "cortex_daily_card"
                                (nlk:json-object
                                 "facts" (vector "the user shipped the ledger")
                                 "decisions" (vector "the release waits for review")
                                 "open_loops" (vector "the invoice is unpaid")
                                 "source" "sign-off of 8 November")))))
      (is (= 3 (nlk:json-value card :integer "staged")) "one fact per line")
      (is (equal '("f-1" "f-2" "f-3") (map 'list #'identity (nlk:json-value card :array "ids"))))
      (is (equal "pending" (nlk:json-value card :string "state"))))
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "reject f-2"))
                 "f-2 is dismissed")
    (is-carrying (text (cell-entry "nodecode-myeline" "myeline" "review"))
                 "the user shipped the ledger"
                 (:absent "the release waits for review" "a dismissed line is out of the queue"))))

;;; --- the receipts outlive the restart ------------------------------------------------

(deftest myeline-cell-the-receipts-outlive-a-restart ()
  (myeline-empty-receipts)
  (with-cell-stop ((myeline-start "data_dir" (myeline-test-folder)))
    (myeline-remember "the team brain is invite-only" "documentation")
    (cell-entry "nodecode-myeline" "myeline" "approve f-1"))
  (with-cell-stop ((myeline-start "data_dir" (myeline-test-folder)))
    (let ((answer (myeline-query "invite-only")))
      (is (= 1 (nlk:json-value answer :integer "total")) "the fact is still there")
      (is (equal "documentation"
                 (nlk:json-value (aref (nlk:json-value answer :array "facts") 0) :string "source"))))))

