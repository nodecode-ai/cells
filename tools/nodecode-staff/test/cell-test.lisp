;;;; cell-test.lisp --- the staff cell against a stubbed model and a stubbed
;;;; webhook, and against a real folder for the inbox half.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one model call a brief makes is a stubbed NLE:COMPLETE and the one
;;;; network call a handoff makes is a stubbed NLK:HTTP, so nothing reaches a
;;;; provider or a peer; each call is kept, so a test reads the system prompt,
;;;; the user message and the body that were actually sent. The tools are
;;;; called the way a model's call reaches them -- through the real dispatch,
;;;; contract and all -- and /staff the way a shell runs it.

(in-package #:nodecode.test)

(define-test-slice "staff" "STAFF-CELL-"
  :start nodecode-staff:start-cell)

(defvar *asked* '()
  "(SYSTEM USER) pairs the stubbed side call was asked with, newest first.")

(defvar *sent* '()
  "(URL BODY) pairs the stubbed POST was given, newest first.")

(defmacro with-model ((answer) &body body)
  "BODY with the one side call answered by ANSWER and every (SYSTEM USER) pair
it was asked with collected in *ASKED*."
  `(let ((*asked* '()))
     (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (declare (ignore session-id max-tokens fallback-p))
                                (push (list system user) *asked*)
                                ,answer)
       ,@body)))

(defmacro with-webhook ((status) &body body)
  "BODY with the one network call answered by STATUS and every (URL BODY) pair
it was asked with collected in *SENT*."
  `(let ((*sent* '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method))
                                (push (list url (getf options :content)) *sent*)
                                (values "{}" ,status))
       ,@body)))

(defvar *scratch* 0 "How many scratch folders this run has made.")

(defun scratch-dir (name)
  "A fresh, empty folder under this run's Nodecode home."
  (let ((dir (merge-pathnames (format nil "staff-test/~a-~d/" name (incf *scratch*))
                               (nlk:home))))
    (when (probe-file dir)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))
    (ensure-directories-exist dir)
    (uiop:native-namestring dir)))

(defun call-tool (id name &rest pairs)
  "NAME's answer to PAIRS, as the model reads it: through the real dispatch."
  (nle::message-content
   (execute-wire-call id name (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))))

;;; --- the lifecycle, from the one declaration ------------------------------------

(define-cell-lifecycle-tests "staff"
  (:help :staff)
  (:command "staff")
  (:refused ("webhook_timeout_seconds" 0) ("max_output_tokens" 5))
  (:idle nodecode-staff:staff-error (nodecode-staff::answer-board (nodecode-staff::decode 'nodecode-staff::board-request))))

;;; --- a brief to a specialist ------------------------------------------------------

(deftest staff-cell-asks-one-specialist-and-keeps-the-job (with-cell-stop ((staff-start "state_dir" (scratch-dir "assign")))
  (with-model ("Goal: cheaper onboarding. 1. Price the three plans.")
    (let ((answer (call-tool "s1" "staff_assign"
                              "specialist" "saas"
                              "task" "what should onboarding cost?"
                              "context" "we sell three plans")))
      (is (search "Goal: cheaper onboarding." answer) "the specialist's answer is the tool's answer")
      (is (search "staff-" answer) "and the job id it was filed under")
      (is (= 1 (length *asked*)) "one side call, no session and no tools")
      (let ((system (first (first *asked*)))
            (user (second (first *asked*))))
        (is (search "SaaS strategy analyst" system) "the specialist's own instructions are the system prompt")
        (is (search "Team roster:" system) "with the team it belongs to named after it")
        (is (search "what should onboarding cost?" user))
        (is (search "Context:" user) "the context rides under the heading the original gives it")))
    (let ((board (call-tool "s2" "staff_status")))
      (is (search "saas" board) "the board lists the job under the specialist")
      (is (search "done" board))
      (is (search "Chief of staff" board) "and the whole roster")))))

(deftest staff-cell-refuses-an-unknown-specialist (with-cell-stop ((staff-start "state_dir" (scratch-dir "unknown")))
  (with-model ("never asked")
    (let ((answer (call-tool "u1" "staff_assign" "specialist" "nobody" "task" "do it")))
      (is (search "no specialist called" answer))
      (is (search "chief" answer) "the roster is named so the caller can pick again"))
    (is (null *asked*) "an unknown id spends no model turn"))))

(deftest staff-cell-reads-a-specialist-it-was-given (with-cell-stop ((staff-start "state_dir" (scratch-dir "roster")
                                                                     "specialists" (vector
                                                                                    (nlk:json-object "id" "ops"
                                                                                                     "title" "Ops lead"
                                                                                                     "focus" "runs the week"
                                                                                                     "prompt" "You run operations.")
                                                                                    (nlk:json-object "id" "cfo"
                                                                                                     "title" "Numbers")))))
  (with-model ("Done.")
    (let ((answer (call-tool "r1" "staff_assign" "specialist" "ops" "task" "plan the week")))
      (is (search "Done." answer) "a configured specialist answers"))
    (is (search "You run operations." (first (first *asked*))) "with the prompt the section wrote")
    (is (search "cfo" (first (first *asked*))) "and the team roster is the configured one, not the built-in team")))

(deftest staff-cell-reports-a-failed-call (with-cell-stop ((staff-start "state_dir" (scratch-dir "failed")))
  (with-model ((error "the lane is down"))
    (let ((answer (call-tool "f1" "staff_assign" "specialist" "design" "task" "look at the flow")))
      (is (search "could not answer" answer) "a refused provider is reported, not thrown at the model")
      (is (search "failed" answer))))
  (is (search "failed" (call-tool "f2" "staff_status")) "and the board keeps it, failed")))

;;; --- the handoff -----------------------------------------------------------------

(deftest staff-cell-delegate-needs-a-path (with-cell-stop ((staff-start "state_dir" (scratch-dir "nopath")))
  (with-webhook (200)
    (let ((answer (call-tool "d1" "staff_delegate" "task" "research the market")))
      (is (search "No handoff path is configured" answer))
      (is (search "webhook_url" answer) "and says what to set"))
    (is (null *sent*) "nothing is sent when no path is configured"))))

(deftest staff-cell-delegates-over-the-webhook (with-cell-stop ((staff-start "state_dir" (scratch-dir "hook")
                                                                 "webhook_url" "https://example.test/hook/secret-token")))
  (with-webhook (202)
    (let ((answer (call-tool "w1" "staff_delegate" "task" "write the pricing page" "specialist" "saas")))
      (is (search "handed off" answer))
      (is (search "HTTP 202" answer) "the webhook's own status is reported")
      (is (= 1 (length *sent*)) "one POST")
      (let ((url (first (first *sent*)))
            (body (second (first *sent*))))
        (is (equal "https://example.test/hook/secret-token" url))
        (is (search "write the pricing page" body) "the task is the body's own")
        (is (search "\"saas\"" body) "framed as the specialist's work")
        (is (search "reply_hint" body)))
      (let ((board (call-tool "w2" "staff_status")))
        (is (null (search "secret-token" board)) "the host is shown, never the path and its token")))))

(deftest staff-cell-refuses-a-webhook-that-is-not-https (with-cell-stop ((staff-start "state_dir" (scratch-dir "plain")
                                                                         "webhook_url" "http://example.test/hook")))
  (with-webhook (200)
    (let ((answer (call-tool "p1" "staff_delegate" "task" "do it")))
      (is (search "nothing was delivered" answer))
      (is (search "must be https" answer) "a plain-http webhook that is not loopback is refused before the call"))
    (is (null *sent*) "and nothing left the process")))

(deftest staff-cell-delegates-through-a-folder (with-cell-stop ((staff-start "state_dir" (scratch-dir "inbox-state")
                                                                  "inbox_dir" (scratch-dir "inbox"))))
  (let* ((dir (getf (nodecode-staff::running-settings) :inbox-dir))
         (answer (call-tool "i1" "staff_delegate" "task" "audit the schema"))
         (id (let ((start (search "job " answer))) (subseq answer (+ start 4) (+ start 4 20))))
         (handoff (merge-pathnames (format nil "handoffs/~a.json" (string-trim " " id))
                                    (uiop:ensure-directory-pathname dir))))
    (is (search "handed off" answer))
    (is (probe-file handoff) "the handoff is written where the coworker reads it")
    (is (search "audit the schema" (uiop:read-file-string handoff)))
    (let ((late (call-tool "i2" "staff_collect" "job_id" (string-trim " " id))))
      (is (search "nothing in results" late) "an unanswered handoff says so"))
    (let ((result-file (merge-pathnames (format nil "results/~a.md" (string-trim " " id))
                                        (uiop:ensure-directory-pathname dir))))
      (ensure-directories-exist result-file)
      (with-open-file (out result-file :direction :output :if-exists :supersede)
        (write-string "The schema is fine." out))
      (let ((collected (call-tool "i3" "staff_collect" "job_id" (string-trim " " id))))
        (is (search "The schema is fine." collected) "the answer is read out of the shared folder")
        (is (search "done" collected))))))

(deftest staff-cell-collects-a-pasted-answer (with-cell-stop ((staff-start "state_dir" (scratch-dir "paste")
                                                               "inbox_dir" (scratch-dir "paste-inbox"))))
  (let* ((answer (call-tool "c1" "staff_delegate" "task" "check the numbers"))
         (id (subseq answer (+ 4 (search "job " answer)) (+ 4 (search "job " answer) 20)))
         (id (string-trim " " (if (position #\Newline id) (subseq id 0 (position #\Newline id)) id))))
    (let ((collected (call-tool "c2" "staff_collect" "job_id" id "result" "The numbers add up.")))
      (is (search "The numbers add up." collected) "a pasted answer is an answer")
      (is (search "pasted" collected))
      (let ((finished (call-tool "c3" "staff_handoffs" "status" "done")))
        (is (search "done" finished) "and the handoff is then on the done list")))))

;;; --- the URL rules, on their own ----------------------------------------------------

(deftest staff-cell-holds-the-webhook-url-to-its-rules ()
  (is (null (nodecode-staff:validate-webhook-url "https://example.test/hook")))
  (let ((loopback "http://127.0.0.1:8080/hook"))
    (is (null (nodecode-staff:validate-webhook-url loopback)) "plain http is allowed for loopback"))
  (is (search "must be https" (nodecode-staff:validate-webhook-url "http://example.test/hook")))
  (is (search "user info" (nodecode-staff:validate-webhook-url "https://key:secret@example.test/hook")))
  (is (search "does not parse" (nodecode-staff:validate-webhook-url "example.test/hook")))
  (let ((shown (nodecode-staff:redact-url "https://example.test/hook?token=abc")))
    (is (equal "https://example.test/..." shown) "the path and the query are never shown")))

;;; --- the command --------------------------------------------------------------------

(deftest staff-cell-command-lists-the-team (with-cell-stop ((staff-start "state_dir" (scratch-dir "slash")))
  (let ((answer (nodecode-staff::run-command "" "s-slash")))
    (is (search "Staff (6)" answer) "the built-in team is listed")
    (is (search "vision-agents" answer)))))

(deftest staff-cell-command-assigns-one-brief (with-cell-stop ((staff-start "state_dir" (scratch-dir "slash-two")))
  (with-model ("Two options: narrow it, or price it per seat.")
    (let ((answer (nodecode-staff::run-command "saas price the new tier" "s-slash-2")))
      (is (search "Two options" answer) "the specialist's answer is what the shell shows")
      (is (search "price the new tier" (second (first *asked*))) "the argument's rest is the task")))
  (let ((answer (nodecode-staff::run-command "nobody do it" "s-slash-3")))
    (is (search "No specialist called" answer) "a word that is not a specialist is not a brief"))))

(deftest staff-cell-keeps-the-board-in-one-file (with-cell-stop ((staff-start "state_dir" (scratch-dir "store")))
  (let ((job (nodecode-staff::new-job :kind "local" :specialist "saas" :title "t" :task "t")))
    (nodecode-staff::add-job job)
    (let ((written (uiop:read-file-string (nodecode-staff:jobs-file))))
      (is (search "saas" written) (format nil "the board's file holds the job: ~a" written))
      (is (= 1 (length (nodecode-staff:read-jobs)))
          (format nil "and reads back as one job; the file is ~a" written))
      (is (equal "saas" (getf (first (nodecode-staff:read-jobs)) :specialist))
          (format nil "with its fields whole; read back ~s from ~a"
                  (nodecode-staff:read-jobs) written))))))
