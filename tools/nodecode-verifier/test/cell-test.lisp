;;;; cell-test.lisp --- the verifier cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a model: the reviewer and the phase gate are bound
;;;; to scripted answers, so "the finding was delivered", "two of three
;;;; agreed" and "a blocked call never ran" are facts a test reads rather
;;;; than a provider's mood.

(in-package #:nodecode.test)

(define-test-slice "verifier" "VERIFIER-CELL-" :start nodecode-verifier:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "verifier"
  (:config "clip" 500)
  (:hooks :frame :tool)
  (:help :verifier)
  (:command "verify")
  (:refused ("max_finding_chars" 5)))

;;; --- helpers ----------------------------------------------------------------

(defmacro with-scripted-reviewer ((&rest answers) &body body)
  "Run BODY with the cell's reviewer answering ANSWERS in turn."
  `(let* ((answers (list ,@answers))
          (nodecode-verifier::*reviewer*
            (lambda (system user)
              (declare (ignore system user))
              (pop answers))))
     ,@body))

(defun verifier-gate (op)
  "Run OP through the cell's :TOOL hook with a NEXT that records the call.
=> (values REFUSAL RAN)."
  (let ((ran nil))
    (values (funcall (cell-hook "nodecode-verifier" :tool)
                      op
                      (lambda (op) (declare (ignore op)) (setf ran t) "ran"))
            ran)))

(defun risky-op ()
  (list :name "eval"
        :arguments (nlk:json-object "form" "(sh \"rm -rf build\")")))

(defun safe-op ()
  (list :name "eval"
        :arguments (nlk:json-object "form" "(+ 1 2)")))

;;; --- what a review does with its answer -------------------------------------

(deftest verifier-cell-a-finding-stands-until-silence-clears-it ()
  (with-cell-stop ((verifier-start "enabled" t))
    (with-scripted-reviewer ("caution: the migration drops the index")
      (multiple-value-bind (text finding count)
          (nodecode-verifier::review "the agent wrote a migration" :session "verifier-cell-a")
        (is (string= "caution" (getf finding :severity)) "the finding keeps its severity")
        (is (= 1 count) "one review raised it")
        (is (search "drops the index" text) "and it is what the review answers")))
    (let ((notice (second (cell-notice "nodecode-verifier"))))
      (is (search "[CAUTION] the migration drops the index" notice)
          "the finding stands where the agent reads it"))
    (with-scripted-reviewer ("nothing to flag")
      (multiple-value-bind (text finding count)
          (nodecode-verifier::review "the agent fixed it" :session "verifier-cell-a")
        (is (null finding) "silence raises nothing")
        (is (= 0 count) "and counts nothing")
        (is (string= "nothing to flag" text) "and says so")))
    (is (null (cell-notice "nodecode-verifier")) "a resolved finding stops standing")))

(deftest verifier-cell-a-blocker-outranks-a-note ()
  (with-cell-stop ((verifier-start "enabled" t))
    (with-scripted-reviewer ("note: rename the helper")
      (nodecode-verifier::review "the agent added a helper" :session "verifier-cell-b"))
    (is (search "[NOTE]" (second (cell-notice "nodecode-verifier"))) "a note is delivered")
    (with-scripted-reviewer ("blocker: the cache is never invalidated")
      (nodecode-verifier::review "the agent added a cache" :session "verifier-cell-b"))
    (is (search "[BLOCKER]" (second (cell-notice "nodecode-verifier")))
        "a later blocker replaces it")))

(deftest verifier-cell-a-lone-voice-is-not-enough ()
  (with-cell-stop ((verifier-start "enabled" t "agreement" 3 "agreement_min" 2))
    (with-scripted-reviewer ("note: same point" "note: same point" "caution: other thing")
      (multiple-value-bind (text finding count)
          (nodecode-verifier::review "work" :session "verifier-cell-c")
        (is (string= "note" (getf finding :severity)) "the point two reviews raised wins")
        (is (= 2 count) "and its count is what they agreed on")
        (is (search "same point" text) "it is delivered")))
    (with-scripted-reviewer ("note: one" "note: two" "note: three")
      (multiple-value-bind (text finding count)
          (nodecode-verifier::review "work" :session "verifier-cell-c")
        (is (null finding) "three different findings agree on nothing")
        (is (string= "nothing to flag" text) "so the review says nothing")))))

;;; --- what the verifier is allowed to see ------------------------------------

(deftest verifier-cell-redaction-cuts-the-credentials ()
  (let ((text (nodecode-verifier::redact
               "the key is sk-abcdefghijklmnop and password: hunter2 and the words stay")))
    (is (null (search "sk-abc" text)) "a token prefix is cut")
    (is (null (search "hunter2" text)) "a value after a key-ish label is cut")
    (is (search "the words stay" text) "prose survives"))
  (let ((text (nodecode-verifier::redact "-----BEGIN PRIVATE KEY----- MIIabc -----END PRIVATE KEY----- tail")))
    (is (null (search "MIIabc" text)) "a private key block is cut whole")
    (is (search "tail" text) "what follows it is kept")))

(deftest verifier-cell-priorities-ride-the-reviewers-prompt ()
  (with-cell-stop ((verifier-start))
    (let ((path (merge-pathnames (format nil "verifier-cell-priorities-~a.md" (get-universal-time))
                                  (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
               (write-string "watch the retry loops" out))
             (let ((nodecode-verifier::*verifier*
                     (list :priorities (namestring path))))
               (is (search "watch the retry loops" (nodecode-verifier::review-system-prompt))
                   "the priorities file reaches the reviewer's instruction")))
        (ignore-errors (delete-file path))))))

;;; --- the phase gate ----------------------------------------------------------

(deftest verifier-cell-the-gate-holds-a-risky-call ()
  (with-cell-stop ((verifier-start "enabled" t "phase_gate" t))
    (let ((nodecode-verifier::*judge*
            (lambda (system user)
              (declare (ignore system user))
              "block: it deletes the only copy of the build")))
      (multiple-value-bind (refusal ran) (verifier-gate (risky-op))
        (is (search "held by nodecode-verifier" refusal) "a block answers a refusal")
        (is (search "deletes the only copy" refusal) "naming what would be lost")
        (is (null ran) "and the call never runs")))
    (let ((nodecode-verifier::*judge* (lambda (system user) (declare (ignore system user)) "allow")))
      (multiple-value-bind (refusal ran) (verifier-gate (risky-op))
        (is (string= "ran" refusal) "an allow runs the call")
        (is (eq t ran) "and it was the call")))
    (let ((nodecode-verifier::*judge* (lambda (system user) (declare (ignore system user))
                                        (error "the gate is down"))))
      (multiple-value-bind (refusal ran) (verifier-gate (risky-op))
        (is (string= "ran" refusal) "a gate that raises allows the call")
        (is (eq t ran) "which then runs")))
    (let ((asked nil))
      (let ((nodecode-verifier::*judge* (lambda (system user) (declare (ignore system user))
                                          (setf asked t) "block: no")))
        (multiple-value-bind (refusal ran) (verifier-gate (safe-op))
          (is (eq t ran) "a call that reads safe runs")
          (is (null asked) "without even asking the gate"))))))

(deftest verifier-cell-the-gate-is-off-until-it-is-turned-on ()
  (with-cell-stop ((verifier-start "enabled" t))
    (let ((nodecode-verifier::*judge* (lambda (system user) (declare (ignore system user)) "block: no")))
      (multiple-value-bind (refusal ran) (verifier-gate (risky-op))
        (is (eq t ran) "with phase_gate off the call runs")
        (is (string= "ran" refusal) "and nothing is refused")))
    (let ((nodecode-verifier::*agreement* nil)
          (nodecode-verifier::*reviewer* (lambda (system user)
                                            (declare (ignore system user))
                                            "note: whatever")))
      (declare (ignore nodecode-verifier::*agreement*))
      (cell-entry "nodecode-verifier" "verify" "gate on" "verifier-cell-gate")
      (let ((nodecode-verifier::*judge* (lambda (system user) (declare (ignore system user)) "block: no")))
        (multiple-value-bind (refusal ran) (verifier-gate (risky-op))
          (is (null ran) "/verify gate on turns it on")
          (is (search "held by" refusal) "and the call is held"))))))

;;; --- the tool and the command -------------------------------------------------

(deftest verifier-cell-the-tool-reviews-what-it-is-handed ()
  (with-cell-stop ((verifier-start))
    (with-scripted-reviewer ("blocker: that rewrite loses the cache")
      (let ((args (nodecode-verifier::decode-verifier-review-arguments
                   (nlk:json-object "transcript" "the agent rewrote the cache"
                                    "focus" "correctness"))))
        (let ((answer (nodecode-verifier::review-tool args)))
          (is (search "[BLOCKER]" answer) "the tool answers the finding")
          (is (string= "the agent rewrote the cache"
                       (nodecode-verifier::verifier-review-arguments-transcript args))
              "the contract carries the transcript"))))))

(deftest verifier-cell-the-command-answers-each-verb ()
  (with-cell-stop ((verifier-start))
    (let ((status (cell-entry "nodecode-verifier" "verify" "status" "verifier-cell-status")))
      (is (search "verifier off" status) "it starts off unless the section says enabled")
      (is (search "phase gate off" status) "the gate is off too")
      (is (search "/verify" status) "and the verbs are listed"))
    (is (search "verifier on"
                (cell-entry "nodecode-verifier" "verify" "on" "verifier-cell-status"))
        "/verify on turns it on")
    (is (search "phase gate on"
                (cell-entry "nodecode-verifier" "verify" "gate on" "verifier-cell-status"))
        "/verify gate on turns the gate on")
    (let ((agreed (cell-entry "nodecode-verifier" "verify" "agree 3 2" "verifier-cell-status")))
      (is (search "agreement 2 of 3" agreed) "/verify agree sets the rounds and the bar"))
    (is (search "delivered a test blocker"
                (cell-entry "nodecode-verifier" "verify" "test blocker the cache is stale"
                            "verifier-cell-status"))
        "/verify test delivers a finding")
    (is (search "[BLOCKER] the cache is stale" (second (cell-notice "nodecode-verifier")))
        "which stands where the agent reads it")
    (is (search "the cache is stale"
                (cell-entry "nodecode-verifier" "verify" "dump" "verifier-cell-status"))
        "/verify dump answers the last finding")
    (is (search "verifier off"
                (progn (cell-entry "nodecode-verifier" "verify" "off" "verifier-cell-status")
                       (cell-entry "nodecode-verifier" "verify" "status" "verifier-cell-status")))
        "/verify off turns it off")
    (is (search "not one of its choices"
                (cell-entry "nodecode-verifier" "verify" "bogus" "verifier-cell-status"))
        "an unknown verb names itself")))

;;; --- a finished turn is queued and reviewed -----------------------------------

(deftest verifier-cell-a-finished-turn-is-queued ()
  (setf nodecode-verifier::*workers* nil)
  (unwind-protect
       (with-cell-stop ((verifier-start "enabled" t))
         (let ((nodecode-verifier::*pending* '()))
           (with-stubbed-fdefinition (nodecode-verifier::transcript-from-log
                                      (session &key limit)
                                      "#### user\n\nadd a migration")
             (with-scripted-reviewer ("caution: the migration drops the index")
               (is (null (cell-notice "nodecode-verifier")) "nothing stands yet")
               (cell-publish "nodecode-verifier"
                             (cell-frame-op "turn.completed" :session "verifier-cell-turn"))
               (is (= 1 (length nodecode-verifier::*pending*)) "the turn is queued")
               (nodecode-verifier::tick)
               (is (null nodecode-verifier::*pending*) "the queue is drained")
               (is (search "[CAUTION]" (second (cell-notice "nodecode-verifier")))
                   "and the review stands")))))
    (setf nodecode-verifier::*workers* t)))
