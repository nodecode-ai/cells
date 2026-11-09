;;;; cell-test.lisp --- the router's policy, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a network, a model or a store: the backend is the
;;;; cell's own lexical scorer, so "a named topic routes" and "a contaminated
;;;; session is never reused" are facts a test reads rather than a provider's
;;;; mood.

(in-package #:nodecode.test)

(define-test-slice "ordaprompt" "ORDAPROMPT-CELL-" :start nodecode-ordaprompt:start-cell)

(define-cell-lifecycle-tests "ordaprompt"
  (:config "context_cost_norm" 8000)
  (:help :ordaprompt)
  (:command "ordaprompt")
  (:refused ("context_cost_norm" 5)))

(defun ordaprompt-call (prompt topics &optional sessions)
  "One tool call through the contract the tool advertises, with PROMPT, TOPICS
and SESSIONS as a model sends them. A session is (ID SUMMARY TOKENS FLAGS)."
  (let ((object (nlk:make-json-object "prompt" prompt "topics" (coerce topics 'vector))))
    (when sessions
      (setf (gethash "sessions" object)
            (coerce (loop for (id summary tokens flags) in sessions
                          collect (nlk:make-json-object "session_id" id
                                                        "summary" summary
                                                        "tokens" tokens
                                                        "flags" (coerce flags 'vector)))
                    'vector)))
    (nodecode-ordaprompt::route-tool
     (nodecode-ordaprompt::decode-route-arguments object))))

;;; --- the topic side ------------------------------------------------------------

(deftest ordaprompt-cell-a-named-topic-routes ()
  (with-cell-stop ((ordaprompt-start))
    (let ((answer (ordaprompt-call "the billing export is empty again" '("billing" "deploy"))))
      (is (search "decision topic_assign" answer) "the label the ask names wins")
      (is (search "band automatic" answer) "and a well-margined match routes on its own")
      (is (search "billing 1.00" answer) "the ranking shows the score that decided it"))))

(deftest ordaprompt-cell-an-unmatched-ask-proposes-a-novel-topic ()
  (with-cell-stop ((ordaprompt-start))
    (let ((answer (ordaprompt-call "the photosynthesis rate in algae" '("billing" "deploy"))))
      (is (search "decision novel_propose" answer) "no kept topic fits, so a new one is proposed")
      (is (search "novel 1.00" answer) "the residue is the whole score"))))

(deftest ordaprompt-cell-a-locked-band-never-routes ()
  (with-cell-stop ((ordaprompt-start "calibration_active" nil))
    (let ((answer (ordaprompt-call "the billing export is empty again" '("billing"))))
      (is (search "decision escalate" answer) "with no calibration nothing is automatic")
      (is (search "calibration_missing" answer) "and the code says why"))))

(deftest ordaprompt-cell-an-empty-call-refuses ()
  (with-cell-stop ((ordaprompt-start))
    (is (search "nothing to route" (ordaprompt-call "the billing export is empty" '())) "a call with no candidate names what it needs")))

;;; --- the session side ------------------------------------------------------------

(deftest ordaprompt-cell-a-session-that-fits-is-reused ()
  (with-cell-stop ((ordaprompt-start))
    (let ((answer (ordaprompt-call "the billing export is empty again" '("billing")
                                    '(("s-1" "billing export" 1000 ())))))
      (is (search "decision session_reuse" answer) "a cheap, clean, similar session is reused")
      (is (search "new_session 0.35" answer) "against the fixed price of starting fresh"))))

(deftest ordaprompt-cell-a-contaminated-session-is-not-reused ()
  (with-cell-stop ((ordaprompt-start))
    (let ((answer (ordaprompt-call "the billing export is empty again" '("billing")
                                    '(("s-1" "billing export" 1000 ("project_mismatch"))))))
      (is (search "decision new_session" answer) "a contaminated session never wins, however similar")
      (is (search "contamination_high" answer) "and the code names the reason"))))

(deftest ordaprompt-cell-a-receipt-holds-hashes-not-words ()
  (with-temp-file (path :type "jsonl")
    (with-cell-stop ((ordaprompt-start "receipts_file" path))
      (let ((answer (ordaprompt-call "the zanzibar export is broken" '("billing" "deploy"))))
        (is (search "receipt: " answer) "the answer says where the receipt went")
        (let ((lines (nlk:read-json-lines path)))
          (is (= 1 (length lines)) (format nil "one line for the one decision: ~a" answer))
          (is (search "sha256:" (gethash "prompt_hash" (first lines))) "the prompt is kept as its hash")
          (is (null (gethash "prompt" (first lines))) "and never written whole")
          (is (not (search "zanzibar" (nlk:read-text path))) "no word of the prompt is legible in it"))))))

;;; --- the command ------------------------------------------------------------------

(deftest ordaprompt-cell-the-command-shows-its-settings ()
  (with-cell-stop ((ordaprompt-start))
    (let ((status (cell-entry "nodecode-ordaprompt" "ordaprompt" "" "ordaprompt-cell-status")))
      (is (search "tau_topic" status) "it names the threshold that decides")
      (is (search "calibration on" status) "and whether the automatic band is open"))))

(deftest ordaprompt-cell-the-command-routes-what-it-is-typed ()
  (with-cell-stop ((ordaprompt-start))
    (let ((answer (cell-entry "nodecode-ordaprompt" "ordaprompt"
                              "billing,deploy :: the billing export is empty" "ordaprompt-cell-status")))
      (is (search "decision topic_assign" answer) "two labels and an ask are one decision")
      (is (search "deploy 0.00" answer) "and the label the ask does not name is ranked under it"))))
