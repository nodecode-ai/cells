;;;; cell-test.lisp --- the lifecycle every cell owes, and what a turn does with MSP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts `muse' except the one test that asks what the cell
;;;; says when the executable is missing: the frames come from a script
;;;; (support.lisp), so "the prompt reached turn/start", "a denial answered
;;;; the approval" and "a stalled child is a repair the model can read" are
;;;; facts, not a provider's mood.

(in-package #:nodecode.test)

(define-test-slice "muse-harness" "MUSE-HARNESS-CELL-" :start nodecode-muse-harness:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------------

(define-cell-lifecycle-tests "muse-harness"
  (:help :muse)
  (:command "muse")
  (:running
   (is (equal "denyUnmatched" (nodecode-muse-harness::setting :approval-mode)) "Muse's own tools ask by default")
   (is (equal "muse" (nodecode-muse-harness::setting :binary)) "the executable is the one on PATH until the section says otherwise")
   (is (= 600 (nodecode-muse-harness::setting :timeout-seconds)) "one turn waits ten minutes"))
  (:refused ("approval_mode" "sometimes") ("timeout_seconds" 0)))

;;; --- one turn --------------------------------------------------------------------

(deftest muse-harness-cell-a-turn-answers-what-muse-said ()
  (let ((script (make-muse-script :texts '("the build " "is green"))))
    (with-cell-stop ((muse-harness-start))
      (with-muse-script (script)
        (let ((answer (muse-tool "prompt" "is the build green?")))
          (is (equal "the build is green" answer) "the agent message's deltas, joined, and nothing else")
          (is-present (turn (muse-sent script "turn/start")) "the turn was started over MSP"
            (is (search "is the build green?"
                        (nlk:encode-json-object turn)) "carrying the prompt"))
          (is (equal "c-deny" (muse-decision script)) "denyUnmatched answers Muse's approval with a denial, so no unsupervised tool runs")
          (is (equal "muse-session-1"
                     (nlk:json-value (muse-sent script "turn/start") :string "params" "sessionId")) "the session session/start answered is the one the turn runs on"))))))

(deftest muse-harness-cell-allowall-approves-muses-own-tools ()
  (let ((script (make-muse-script)))
    (with-cell-stop ((muse-harness-start "approval_mode" "allowAll"))
      (with-muse-script (script)
        (muse-tool "prompt" "tidy the folder")
        (is (equal "c-allow" (muse-decision script)) "allowAll answers the approval Muse offered")))))

(deftest muse-harness-cell-a-call-sets-its-own-model ()
  (let ((script (make-muse-script)))
    (with-cell-stop ((muse-harness-start))
      (with-muse-script (script)
        (muse-tool "prompt" "ping" "model" "muse-wonder-full-2.0")
        (is (search "muse-wonder-full-2.0"
                    (nlk:encode-json-object (muse-sent script "session/start"))) "the model the call named is the one session/start was given")
        (is (equal '("muse" "serve" "--provider" "meta" "--contributor-mode" "off"
                     "--no-session-log" "--model" "muse-wonder-full-2.0")
                   (nodecode-muse-harness::serve-argv "muse" "muse-wonder-full-2.0")) "and muse serve is started for it, contributor mode off")
        (is (search "one token"
                    (handler-case (progn (nodecode-muse-harness::serve-argv "muse" "two words")
                                         "")
                      (error (condition) (princ-to-string condition)))) "a model id that is not one token is refused before anything starts")))))

;;; --- what a call may not say ------------------------------------------------------

(deftest muse-harness-cell-a-workspace-or-server-outside-is-refused ()
  (with-cell-stop ((muse-harness-start))
    (let ((outside (uiop:native-namestring (uiop:temporary-directory))))
      (is (search "workspace must stay inside the session directory"
                  (muse-tool "prompt" "x" "workspace" outside)) "a workspace outside the session directory is refused"))
    (is (search "must be streamableHttp on loopback"
                (muse-tool "prompt" "x" "mcp_servers"
                           "{\"remote\":{\"transport\":\"stdio\",\"command\":\"rm\"}}")) "a model-supplied stdio command is refused before it can run")
    (is (search "on 127.0.0.1, localhost or ::1"
                (muse-tool "prompt" "x" "mcp_servers"
                           "{\"web\":{\"transport\":\"streamableHttp\",\"url\":\"http://example.com:80/mcp\"}}")) "and a remote host is refused too")))

(deftest muse-harness-cell-an-empty-prompt-is-refused ()
  (with-cell-stop ((muse-harness-start))
    (is (search "needs a prompt" (muse-tool "prompt" "   ")) "a turn with no words says so")))

;;; --- what it says when the executable is not there ----------------------------------

(deftest muse-harness-cell-without-the-muse-executable-it-says-so ()
  (with-cell-stop ((muse-harness-start "binary" "no-such-muse-executable-xyz"))
    (let ((answer (muse-tool "prompt" "ping")))
      (is (search "ERROR:" answer) "the call is a failure the model can repair, not a thrown condition")
      (is (search "no-such-muse-executable-xyz" answer) "and names what it could not start"))))

(deftest muse-harness-cell-a-stalled-child-fails-with-what-it-said ()
  (let ((script (make-muse-script :stall t)))
    (with-cell-stop ((muse-harness-start))
      (with-muse-script (script)
        (let ((answer (muse-tool "prompt" "ping" "timeout_seconds" 1)))
          (is (search "did not answer" answer) "a child that never answers is a failure, not a hang"))))))

;;; --- the command -------------------------------------------------------------------

(deftest muse-harness-cell-the-command-runs-one-turn ()
  (let ((script (make-muse-script :texts '("done"))))
    (with-cell-stop ((muse-harness-start))
      (with-muse-script (script)
        (is (equal "done" (nodecode-muse-harness::run-muse "tidy up" nil)) "/muse PROMPT runs the same turn")
        (is (search "approval: denyUnmatched"
                    (nodecode-muse-harness::run-muse "" nil)) "typed alone, it says the mode it would run under")))))
