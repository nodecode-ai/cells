;;;; cell-test.lisp --- the lifecycle, the ledger, the answer, the round, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring and its arithmetic: that a start
;;;; installs the tool, /dq, the manual topic and the round hook and a stop
;;;; takes every one back; that a question written down comes back out of the
;;;; file with its words, its options and its expiry; that the same
;;;; idempotency key is one question and not two; that an answer by number is
;;;; stored as the option's own words, that the first answer stands and the
;;;; second refuses, that a cancelled or expired question takes none; that what
;;;; arrived is handed to the session that asked at the next request, once;
;;;; that the tool and the command answer through the same verbs; and that each
;;;; refusal says why.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "deferred-questions"
  (:hooks :round)
  (:help :deferred-questions)
  (:command "dq")
  (:running (is (equal nodecode-deferred-questions::+primer+ (nle:help :deferred-questions)))
            (is (= 24 (nodecode-deferred-questions::setting :max-options)) "max_options defaults to 24")
            (is (= 72 (nodecode-deferred-questions::setting :expires-hours)) "the expiry defaults to seventy-two hours"))
  (:refused ("max_options" 0) ("expires_hours" 0) ("retention_days" -1))
  (:idle dq:deferred-questions-error
         (dq:request :question "which date works?")
         (dq:board)))

;;; --- the ledger -------------------------------------------------------------------

(deftest deferred-questions-cell-a-question-is-asked-and-left ()
  (with-questions ()
    (let ((text (dq:request :question "Which date works?"
                             :options '("Friday" "Monday"))))
      (is-carrying text ("asked #1: Which date works?" "the answer names the question")
                        ("1. Friday" "with its options in order")
                        ("/dq 1 ANSWER" "and the way to answer it")))
    (is-carrying (text (dq:board)) ("#1 [pending] Which date works?" "the board shows it"))
    (is (= 1 (question-count)) "one question in the ledger, in the file")
    (is (equal "Which date works?" (getf (question-by-id 1) :question)) "the words round-trip")))

(deftest deferred-questions-cell-the-same-key-is-one-question ()
  (with-questions ()
    (dq:request :question "Which date works?" :key "date")
    (let ((text (dq:request :question "Which date works?" :key "date")))
      (is-carrying text ("asked #1 again (same key \"date\")" "the second write names the first")))
    (is (= 1 (question-count)) "and adds nothing")
    (is-carrying (text (dq:request :question "Which branch?" :key "branch"))
                  ("asked #2: Which branch?" "a different key is a second question"))))

;;; --- the answer --------------------------------------------------------------------

(deftest deferred-questions-cell-an-answer-by-number-is-the-option ()
  (with-questions ()
    (dq:request :question "Which date works?" :options '("Friday" "Monday"))
    (is-carrying (text (dq:answer 1 "2")) ("answered #1: Which date works? -> \"Monday\"" "the option's own words are stored")
                                           ("option 2 of 2" "and the number it came from"))
    (is (equal "Monday" (getf (question-by-id 1) :answer)) "the ledger holds the words, not the number")
    (is (equal "answered" (getf (question-by-id 1) :state)) "and the question is answered")))

(deftest deferred-questions-cell-one-answer-stands ()
  (with-questions ()
    (dq:request :question "Which date works?")
    (dq:answer 1 "Friday")
    (is (search "one answer stands"
                (refusal-text dq:deferred-questions-error (dq:answer 1 "Monday"))) "a second answer is refused, and says the first stands")
    (is (equal "Friday" (getf (question-by-id 1) :answer)) "and does not overwrite it")))

(deftest deferred-questions-cell-a-cancelled-or-expired-question-takes-none ()
  (with-questions ()
    (dq:request :question "Which date works?")
    (dq:request :question "Which branch?" :expires "+2h")
    (is-carrying (text (dq:cancel 1)) ("cancelled #1" "cancelling names the question"))
    (is (search "nothing to cancel"
                (refusal-text dq:deferred-questions-error (dq:cancel 1))) "a cancelled question cannot be cancelled twice")
    (is (search "is cancelled, not waiting"
                (refusal-text dq:deferred-questions-error (dq:answer 1 "Friday"))) "and takes no answer")
    (let* ((ledger (question-ledger))
           (item (nodecode-deferred-questions::item-by-id ledger 2)))
      (setf (getf item :expires-at) (- (nlk:unix-now) 60))
      (nodecode-deferred-questions::write-ledger ledger))
    (is (search "is expired, not waiting"
                (refusal-text dq:deferred-questions-error (dq:answer 2 "main"))) "an expired question takes no answer either")
    (is-carrying (text (dq:show 2)) ("#2 [expired]" "and the ledger says so"))))

;;; --- what the next request carries ----------------------------------------------------

(deftest deferred-questions-cell-an-answer-reaches-the-next-request-once ()
  (with-questions ()
    (dq:request :question "Which date works?" :session "s-1")
    (dq:answer 1 "Friday")
    (let ((text (nodecode-deferred-questions:continuation "s-1")))
      (is-present text
        (is-carrying text ("[deferred answers: 1" "the framing line counts it")
                          ("#1 Q: Which date works?" "the question is quoted")
                          ("A: Friday" "with the answer")))
      (is (null (nodecode-deferred-questions:continuation "s-1")) "handed over once, not every round")
      (is (null (nodecode-deferred-questions:continuation "another-session")) "and only to the session that asked"))))

(deftest deferred-questions-cell-two-answers-ride-one-request ()
  (with-questions ()
    (dq:request :question "Which date works?" :session "s-1")
    (dq:request :question "Which branch?" :session "s-1")
    (dq:answer 1 "Friday")
    (dq:answer 2 "main")
    (let ((text (nodecode-deferred-questions:continuation "s-1")))
      (is-present text
        (is-carrying text ("[deferred answers: 2" "both are counted in the framing line")
                          ("#1 Q: Which date works?" "the first question is quoted")
                          ("#2 Q: Which branch?" "and so is the second")
                          ("A: main" "with its answer"))))))

;;; --- the tool and the command ---------------------------------------------------------

(deftest deferred-questions-cell-the-tool-asks-through-the-same-verbs ()
  (with-questions ()
    (let ((call (nodecode-deferred-questions::decode-deferred-call
                 (nlk:make-json-object
                  "action" "request" "question" "Which date works?"
                  "options" (vector "Friday" "Monday")
                  "idempotency_key" "date"))))
      (is-carrying (text (nodecode-deferred-questions::tool-call call))
                    ("asked #1: Which date works?" "the tool asks")))
    (let ((list-call (nodecode-deferred-questions::decode-deferred-call
                      (nlk:make-json-object "action" "list"))))
      (is-carrying (text (nodecode-deferred-questions::tool-call list-call))
                    ("1 question (1 waiting)" "and the tool reads the ledger")))))

(deftest deferred-questions-cell-the-command-answers-text-and-a-panel ()
  (with-questions ()
    (dq:request :question "Which date works?" :options '("Friday" "Monday"))
    (multiple-value-bind (text dialog) (nodecode-deferred-questions::run-slash "" nil)
      (is-carrying text ("deferred questions: 1 waiting" "the command summarises")
                        ("#1 [pending] Which date works?" "with the question"))
      (is dialog "and answers a panel"))
    (is (search "answered #1" (nodecode-deferred-questions::run-slash "1 Friday" nil)) "/dq ID ANSWER answers one")))

(deftest deferred-questions-cell-a-bad-write-says-why ()
  (with-questions ()
    (is (search "needs words"
                (refusal-text dq:deferred-questions-error (dq:request :question "  "))) "a question needs words")
    (is (search "is not a delay"
                (refusal-text dq:deferred-questions-error
                              (dq:request :question "x" :expires "+soon"))) "an expiry must be a delay or a time")
    (is (search "must allow text"
                (refusal-text dq:deferred-questions-error
                              (dq:request :question "x" :options '() :allow-text nil))) "a question with no options needs free text")
    (is (= 0 (question-count)) "and nothing was written")
    (with-questions ("max_options" 2)
      (is (search "at most 2 options"
                  (refusal-text dq:deferred-questions-error
                                (dq:request :question "x" :options '("a" "b" "c")))) "the section's cap refuses a wider question"))))
