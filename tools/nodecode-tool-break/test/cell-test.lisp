;;;; cell-test.lisp --- the break hook, the three verbs, and one session's scope.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: that a call in flight is
;;;; listed while it runs and gone once it returns; that /break, /break MESSAGE
;;;; and /again lay a mark a running call answers with a note instead of its
;;;; own text; that the note carries the operator's words and the tail of what
;;;; the call returned; that /again's budget runs out; that a call belongs to
;;;; the session waiting on it; and that each verb on an idle turn says so.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "tool-break"
  (:hooks :tool)
  (:help :break)
  (:command "break")
  (:running (is (plusp (nodecode-tool-break::setting :salvage-chars)) "the note keeps a tail of what the call returned")
            (is (= 2 (nodecode-tool-break::setting :again-limit)) "again defaults to two uses"))
  (:refused ("again_limit" 0) ("salvage_chars" -1) ("again_limit" "two"))
  (:idle nodecode-tool-break::tool-break-error
         (nodecode-tool-break::run-break "" "s1")
         (nodecode-tool-break::run-again "" "s1")
         (nodecode-tool-break::run-break-status "" "s1")))

;;; --- the hook -----------------------------------------------------------------

(deftest tool-break-cell-a-call-in-flight-is-listed-and-then-gone ()
  (with-tool-break ()
    (let ((inside nil))
      (is (equal "the call's own answer"
                 (tool-break-call "eval"
                                   (lambda ()
                                     (setf inside (tool-break-command "break-status" "" "s1"))
                                     "the call's own answer")
                                   :arguments (tool-break-slow-arguments)
                                   :session "s1")) "an unmarked call answers its own text")
      (is (search "\"inflight\":true" inside) "the call is in flight while it runs")
      (is (search "\"count\":1" inside) "one call")
      (is (search "\"name\":\"eval\"" inside) "named as the model called it")
      (is (search "make test" inside) "and labelled with what it runs"))
    (is (search "\"inflight\":false" (tool-break-command "break-status" "" "s1")) "once it returns nothing is in flight")))

(deftest tool-break-cell-break-rewrites-the-result-of-a-running-call ()
  (with-tool-break ()
    (let ((answer nil))
      (let ((text (tool-break-call "eval"
                                    (lambda ()
                                      (setf answer (tool-break-command "break" "try page 2" "s1"))
                                      "the stale answer")
                                    :arguments (tool-break-slow-arguments)
                                    :session "s1")))
        (is (search "Broke eval" answer) "the operator is told which call was broken")
        (is (search "Broken by /break" text) "the model reads a break note")
        (is (search "Do not retry the same call" text) "and is told not to retry it")
        (is (search "Steer (before any other queued steer):" text) "the operator's words ride it")
        (is (search "try page 2" text))
        (is (search "the stale answer" text) "with the tail of what the call did return")))))

(deftest tool-break-cell-again-says-once-and-runs-out ()
  (with-tool-break ()
    (flet ((again ()
             (tool-break-call "eval"
                               (lambda ()
                                 (tool-break-command "again" "be patient" "s1")
                                 "the call's own text")
                               :arguments (tool-break-slow-arguments)
                               :session "s1")))
      (let ((first (again)) (second (again)) (third (again)))
        (is (search "Broken by /again" first) "the first reissues")
        (is (search "Reissue this exact call once" first))
        (is (search "Tweak:" first) "with the operator's tweak")
        (is (search "be patient" first))
        (is (search "the call's own text" first) "with the tail of what it returned")
        (is (search "make test" first) "and the arguments, so the model need not rebuild them")
        (is (search "Broken by /again" second) "the second too")
        (is (equal "the call's own text" third) "the third is over the budget")))))

(deftest tool-break-cell-a-message-with-nothing-in-flight-is-said-back ()
  (with-tool-break ()
    (is (search "Nothing in flight" (tool-break-command "break" "" "s1")))
    (is (search "Nothing in flight to reissue" (tool-break-command "again" "" "s1")))
    (let ((answer (tool-break-command "break" "use the cache" "s1")))
      (is (search "use the cache" answer) "a steer with nothing to break is handed back")
      (is (search "send this as a message" answer)))))

(deftest tool-break-cell-a-call-belongs-to-the-session-waiting-on-it ()
  (with-tool-break ()
    (let ((other nil) (named nil))
      (tool-break-call "eval"
                        (lambda () (setf other (tool-break-command "break" "" "s2")))
                        :arguments (tool-break-slow-arguments) :session "s1" :call-id "c9")
      (is (equal "Nothing in flight" (string-trim " " other)) "another session's /break leaves it alone")
      (let ((text (tool-break-call "eval"
                                    (lambda ()
                                      (setf named (tool-break-command "break" "--id cx" "s1"))
                                      "own text")
                                    :arguments (tool-break-slow-arguments)
                                    :session "s1" :call-id "c1")))
        (is (search "No tool call cx is in flight" named) "--id names a call")
        (is (equal "own text" text) "and a call it does not name is untouched")))))
