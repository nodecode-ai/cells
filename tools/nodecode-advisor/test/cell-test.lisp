;;;; cell-test.lisp --- the advisor cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a model or a store: the reviewer is bound to a
;;;; scripted answer and the turn transcript to a literal, so "the advice
;;;; was held" and "the same point raised twice was delivered" are facts a
;;;; test reads rather than a provider's mood.

(in-package #:nodecode.test)

(define-test-slice "advisor" "ADVISOR-CELL-" :start nodecode-advisor:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "advisor"
  (:config "clip" 500)
  (:hooks :frame)
  (:help :advisor)
  (:command "advisor")
  (:refused ("clip" 5)))

;;; --- what a review does with its answer -------------------------------------

(defmacro with-scripted-reviewer ((&rest answers) &body body)
  "Run BODY with the cell's reviewer answering ANSWERS in turn."
  `(let* ((answers (list ,@answers))
          (nodecode-advisor::*reviewer*
            (lambda (system user)
              (declare (ignore system user))
              (pop answers))))
     ,@body))

(deftest advisor-cell-a-concern-is-held-until-reconfirmed ()
  (with-cell-stop ((advisor-start "clip" 500))
    (let ((session "advisor-cell-held"))
      (nodecode-advisor::save-held session '())
      (with-scripted-reviewer ("[CONCERN] the retry loop can spin forever"
                               "[CONCERN] the retry loop can spin forever"
                               "nothing to flag")
        (multiple-value-bind (text advice)
            (nodecode-advisor::review "the agent wrote a retry loop" :session session)
          (is (equal '() advice) "a first concern is held, not delivered")
          (is (search "nothing to flag" text) "and nothing is said")
          (is (= 1 (length (nodecode-advisor::load-held session))) "one note held"))
        (multiple-value-bind (text advice)
            (nodecode-advisor::review "the agent wrote a retry loop" :session session)
          (is (= 1 (length advice)) "the second raising is delivered")
          (is (search "[CONCERN] the retry loop" text) "as the note it was"))
        (multiple-value-bind (text advice)
            (nodecode-advisor::review "the agent fixed it" :session session)
          (is (equal '() advice) "silence after the fix delivers nothing")
          (is (search "nothing to flag" text) "and says so")
          (is (equal '() (nodecode-advisor::load-held session)) "the held note is dropped")))
      (nodecode-advisor::save-held session '()))))

(deftest advisor-cell-a-nit-is-delivered-at-once ()
  (with-cell-stop ((advisor-start))
    (let ((session "advisor-cell-nit"))
      (nodecode-advisor::save-held session '())
      (with-scripted-reviewer ("[NIT] name the constant")
        (multiple-value-bind (text advice)
            (nodecode-advisor::review "the agent inlined a literal" :session session)
          (is (= 1 (length advice)) "a nit needs no reconfirmation")
          (is (search "[NIT] name the constant" text) "and is delivered as written")
          (is (equal '() (nodecode-advisor::load-held session)) "holding nothing"))
        (with-scripted-reviewer ("<tool_call>{\"name\": \"read\"}")
          (multiple-value-bind (text advice)
              (nodecode-advisor::review "leaked machine output" :session session)
            (is (equal '() advice) "leaked tool output is dropped, not delivered")
            (is (search "nothing to flag" text) "a bad review costs silence")))))
    ))

;;; --- the tool and the command -------------------------------------------------

(deftest advisor-cell-the-tool-reviews-what-it-is-handed ()
  (with-cell-stop ((advisor-start))
    (with-scripted-reviewer ("[NIT] check the empty case")
      (let ((args (nodecode-advisor::decode-advisor-review-arguments
                   (nlk:json-object "transcript" "the agent added a parser"
                                    "focus" "edge cases"))))
        (let ((answer (nodecode-advisor::review-tool args)))
          (is (search "[NIT] check the empty case" answer) "the tool answers the advice")
          (let ((transcript (nodecode-advisor::advisor-review-arguments-transcript args)))
            (is (string= "the agent added a parser" transcript) "the contract carries the transcript")))))))

(deftest advisor-cell-the-command-answers-each-verb ()
  (with-cell-stop ((advisor-start))
    (let ((status (cell-entry "nodecode-advisor" "advisor" "status" "advisor-cell-status")))
      (is (search "advisor off" status) "it starts off unless the section says auto")
      (is (search "/advisor" status) "and shows the verbs"))
    (let ((turned (cell-entry "nodecode-advisor" "advisor" "on" "advisor-cell-status")))
      (is (search "advisor on" turned) "/advisor on turns it on"))
    (with-scripted-reviewer ("[BLOCKER] that rewrite loses the cache")
      (let ((answer (cell-entry "nodecode-advisor" "advisor"
                                "test blocker that rewrite loses the cache" "advisor-cell-status")))
        (is (search "delivered a test blocker" answer) "/advisor test answers what it did"))
      (let ((notice (second (cell-notice "nodecode-advisor"))))
        (is (search "[BLOCKER] that rewrite loses the cache" notice) "/advisor test stands the note where the agent reads it")))
    (let ((bogus (cell-entry "nodecode-advisor" "advisor" "bogus" "advisor-cell-status")))
      (is (search "not one of its choices" bogus) "an unknown verb names itself"))))

;;; --- a finished turn is queued, reviewed, and posted -------------------------

(deftest advisor-cell-a-finished-turn-is-reviewed-and-posted ()
  (setf nodecode-advisor::*workers* nil)
  (unwind-protect
       (with-cell-stop ((advisor-start "auto" t))
         (let ((nodecode-advisor::*reviewer*
                 (lambda (system user)
                   (declare (ignore system user))
                   "[CONCERN] the migration drops the index"))
               (nodecode-advisor::*pending* '()))
           (with-stubbed-fdefinition (nodecode-advisor::transcript-from-log
                                      (session &key limit)
                                      "#### User\n\nadd a migration")
             (is (null (cell-notice "nodecode-advisor")) "nothing stands yet")
             (cell-publish "nodecode-advisor"
                           (cell-frame-op "turn.completed" :session "advisor-cell-turn"))
             (is (= 1 (length nodecode-advisor::*pending*)) "the turn is queued")
             (nodecode-advisor::tick)
             (is (null nodecode-advisor::*pending*) "and the queue is drained")
             (is (null (cell-notice "nodecode-advisor")) "a held concern is not posted until it is raised again"))))
    (setf nodecode-advisor::*workers* t)))

(deftest advisor-cell-the-transcript-clips-each-tool-result ()
  (with-cell-stop ((advisor-start "clip" 100))
    (let ((rows (list (list :seq 1 :role "user" :text "fix the parser")
                      (list :seq 2 :tool "eval" :text (make-string 400 :initial-element #\x)))))
      (with-stubbed-fdefinition (nlk:recall-log (&key &allow-other-keys)
                                  rows)
        (let ((transcript (nodecode-advisor::transcript-from-log "advisor-cell-clip")))
          (is (search "#### user" transcript) "the ask is in the transcript")
          (is (search "#### eval" transcript) "so is the tool call")
          (is (< (length transcript) 300) "and a long result is clipped"))))))
