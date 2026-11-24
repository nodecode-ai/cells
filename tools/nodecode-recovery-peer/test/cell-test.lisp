;;;; cell-test.lisp --- the recovery peer cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a model: the peer is bound to scripted answers, so
;;;; "the third identical failure is the one that asks", "one stall asks
;;;; once" and "a peer that raises says so instead of failing the call" are
;;;; facts a test reads rather than a provider's mood.

(in-package #:nodecode.test)

(define-test-slice "recovery-peer" "RECOVERY-PEER-CELL-" :start nodecode-recovery-peer:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "recovery-peer"
  (:config "peer" "some-lane/some-model")
  (:hooks :tool)
  (:help :recovery-peer)
  (:command "recovery-peer")
  (:refused ("peer" "some-lane") ("peer" "") ("repeat_threshold" 1)))

;;; --- helpers ----------------------------------------------------------------

(defmacro with-scripted-peer ((&rest answers) &body body)
  "Run BODY with the cell's peer answering ANSWERS in turn: ASKS counts how
often it was asked, SEEN holds what each ask showed it."
  `(let* ((answers (list ,@answers))
          (asks 0)
          (seen '())
          (nodecode-recovery-peer::*peer-call*
            (lambda (system user)
              (declare (ignore system))
              (incf asks)
              (push user seen)
              (pop answers))))
     (declare (ignorable asks seen))
     ,@body))

(defun peer-through (result &optional (name "eval"))
  "Run one call through the cell's :TOOL hook, a NEXT answering RESULT."
  (funcall (cell-hook "nodecode-recovery-peer" :tool)
           (list :name name :arguments (nlk:json-object "form" "(read \"src/cell.lisp\")"))
           (lambda (op) (declare (ignore op)) result)))

(defun peer-ask-tool (&rest pairs)
  "The recovery_peer tool as the model calls it, over PAIRS."
  (nodecode-recovery-peer:ask-tool
   (nodecode-recovery-peer::decode-recovery-peer-ask (apply #'nlk:make-json-object pairs))))

;;; --- a stall, and what it costs -------------------------------------------------

(deftest recovery-peer-cell-the-third-identical-failure-asks-once ()
  (with-cell-stop ((recovery-peer-start "peer" "some-lane/some-model"))
    (nodecode-recovery-peer::reset-state)
    (with-scripted-peer ("read the whole path, then retry once")
      (is (null (search "[Recovery peer suggestion" (peer-through "ERROR: no such file"))) "the first failure is handed back untouched")
      (is (null (search "[Recovery peer suggestion" (peer-through "ERROR: no such file"))) "so is the second")
      (let ((third (peer-through "ERROR: no such file")))
        (is (search "[Recovery peer suggestion (some-lane/some-model" third) "the third carries the peer's advice")
        (is (search "read the whole path" third) "which is what the peer said")
        (is (search "Verify it before claiming success" third) "and it claims nothing is verified"))
      (is (null (search "[Recovery peer suggestion" (peer-through "ERROR: no such file"))) "a fourth asks nobody")
      (is (= 1 asks) "one stall, one call"))))

(deftest recovery-peer-cell-a-different-result-resets-the-count ()
  (with-cell-stop ((recovery-peer-start "peer" "some-lane/some-model"))
    (nodecode-recovery-peer::reset-state)
    (with-scripted-peer ("unreachable")
      (peer-through "ERROR: boom")
      (peer-through "ERROR: boom")
      (is (null (search "[Recovery peer suggestion" (peer-through "wrote 12 bytes"))) "a different result is progress, not a stall")
      (is (= 0 asks) "so nobody was asked"))))

(deftest recovery-peer-cell-a-quiet-repeat-is-a-stall-too ()
  (with-cell-stop ((recovery-peer-start "peer" "some-lane/some-model"))
    (nodecode-recovery-peer::reset-state)
    (with-scripted-peer ("search with a shorter pattern")
      (peer-through "3 matches")
      (peer-through "3 matches")
      (let ((third (peer-through "3 matches")))
        (is (search "search with a shorter pattern" third) "an unchanged successful read is reviewed as well")
        (is (search "quiet" (first seen)) "the peer hears it as no progress"))
      (is (= 1 asks) "once"))))

(deftest recovery-peer-cell-a-call-that-does-not-count-never-asks ()
  (with-cell-stop ((recovery-peer-start "peer" "some-lane/some-model"))
    (nodecode-recovery-peer::reset-state)
    (with-scripted-peer ("whatever")
      (peer-through "3 matches" "step")
      (peer-through "3 matches" "step")
      (peer-through "3 matches" "step")
      (is (= 0 asks) "a tool outside no_progress_tools that succeeded is progress every time"))))

;;; --- what a peer is allowed to see --------------------------------------------

(deftest recovery-peer-cell-redaction-cuts-the-credentials ()
  (let ((text (nodecode-recovery-peer:redact "the key is sk-abcdefghijklmnop and password hunter2 and the words stay")))
    (is (null (search "sk-abc" text)) "a token prefix is cut")
    (is (null (search "hunter2" text)) "the word after a credential label is cut")
    (is (search "the words stay" text) "prose survives"))
  (let ((text (nodecode-recovery-peer:redact "-----BEGIN PRIVATE KEY----- MIIabc -----END PRIVATE KEY----- tail")))
    (is (null (search "MIIabc" text)) "a PEM block is cut whole")
    (is (search "tail" text) "what follows it is kept")))

;;; --- the tool and the command ---------------------------------------------------

(deftest recovery-peer-cell-the-tool-answers-with-the-advice ()
  (with-cell-stop ((recovery-peer-start "peer" "some-lane/some-model"))
    (with-scripted-peer ("check the path, then retry once")
      (let ((answer (peer-ask-tool "task" "add a cell" "failure" "eval answered ERROR")))
        (is (search "check the path" answer) "the tool answers the peer's advice")
        (is (search "some-lane/some-model" answer) "and names the peer that gave it")))
    (let ((nodecode-recovery-peer::*peer-call*
            (lambda (system user) (declare (ignore system user)) (error "down"))))
      (let ((answer (peer-ask-tool "task" "t" "failure" "f")))
        (is (search "no second opinion" answer) "a peer that fails answers a refusal")
        (is (search "peer call failed" answer) "naming what happened")))))

(deftest recovery-peer-cell-the-command-answers-each-verb ()
  (with-cell-stop ((recovery-peer-start "peer" "some-lane/some-model"))
    (let ((status (cell-entry "nodecode-recovery-peer" "recovery-peer" "status" "recovery-peer-cell-status")))
      (is (search "peer some-lane" status) "status names the peer")
      (is (search "some-model" status) "and its model")
      (is (search "recovery peer on" status) "and it starts watching"))
    (is (search "recovery peer off" (cell-entry "nodecode-recovery-peer" "recovery-peer" "off" "recovery-peer-cell-status")) "/recovery-peer off stops the watching")
    (is (search "recovery peer on" (cell-entry "nodecode-recovery-peer" "recovery-peer" "on" "recovery-peer-cell-status")) "/recovery-peer on starts it again")
    (is (search "forgotten" (cell-entry "nodecode-recovery-peer" "recovery-peer" "reset" "recovery-peer-cell-status")) "/recovery-peer reset forgets the streaks")
    (is (search "not one of its words" (cell-entry "nodecode-recovery-peer" "recovery-peer" "bogus" "recovery-peer-cell-status")) "an unknown word names itself")))

(deftest recovery-peer-cell-an-unset-peer-refuses ()
  (is (signals-error nlk:config-refusal (recovery-peer-start)) "a section with no peer refuses")
  (is (signals-error nlk:config-refusal (recovery-peer-start "peer" "some-lane/")) "and so does a peer with no model")
  (is (null (cell-hook "nodecode-recovery-peer" :tool)) "and nothing was installed"))
