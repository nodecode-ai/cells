;;;; cell-test.lisp --- lifecycle, the store, the four tools, /lcm.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over the harness seams it
;;;; reads: that a start installs the four tools, /lcm, the manual and the
;;;; frame hook, and a stop takes every one of them back; that an unnamed
;;;; store refuses; that a turn's three facts land in the store verbatim and
;;;; a restart numbers on where the file left off; that a search, an expand,
;;;; a recent list and the command answer out of that store through the
;;;; declared contracts; and that a tool result is marked failed and cut to
;;;; the section's ceiling.

(in-package #:nodecode.test)

;;; --- the slice, and the cell ------------------------------------------------------

(define-test-slice "lcm-x" "LCM-X-CELL-" :start nodecode-lcm-x:start-cell)

(define-cell-lifecycle-tests "lcm-x"
  (:config "dir" "lcm-x-lifecycle")
  (:hooks :frame)
  (:help :lcm-x)
  (:command "lcm")
  (:running (is (nle::find-tool "lcm_search") "the search tool is registered")
            (is (nle::find-tool "lcm_expand") "the expand tool is registered")
            (is (nle::find-tool "lcm_recent") "the recent tool is registered")
            (is (nle::find-tool "lcm_status") "the status tool is registered"))
  (:stopped (is (null (nle::find-tool "lcm_search")) "stop takes the tools back")
            (is (null (nle::find-tool "lcm_status"))))
  (:refused () ("dir" 5) ("max_chars" 10) ("capture" "maybe")))

;;; --- the seams a test drives ---------------------------------------------------------

(defun lcm-x-call (name &rest pairs)
  "Call the registered tool NAME through its declared contract with PAIRS, as
a tool call does."
  (let* ((entry (nle::find-tool name))
         (schema (second entry)))
    (funcall (third entry) (nlk:contract-decode schema (apply #'nlk:make-json-object pairs)))))

(defun lcm-x-publish (type session turn &rest pairs)
  "Publish one durable fact of TYPE from SESSION's TURN through the frame hook,
with PAYLOAD built from PAIRS."
  (cell-publish "nodecode-lcm-x"
                (cell-frame-op type :session session :turn turn
                                    :payload (apply #'nlk:make-json-object pairs))))

(defun lcm-x-turn (session turn &key (ask "the parser fix landed") (answer "it did: examples pass"))
  "One whole turn of SESSION: the ask, the answer and one tool result."
  (lcm-x-publish "turn.input_committed" session turn "message" ask)
  (lcm-x-publish "turn.assistant_message_completed" session turn "message" answer)
  (lcm-x-publish "turn.tool_result" session turn
                 "tool-name" "shell" "result" "make check: ok"))

;;; --- what it keeps -------------------------------------------------------------------

(deftest lcm-x-cell-every-turn-lands-in-the-store-verbatim ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-turns"))
    (is (equal "lcm-x-turns" (nodecode-lcm-x::setting :dir)) "the section's folder is the one that runs")
    (lcm-x-turn "s1" "t1")
    (let ((blocks (nodecode-lcm-x:read-blocks "lcm-x-turns" "s1")))
      (is (= 3 (length blocks)) "the ask, the answer and the tool result are kept")
      (is (equal '("user" "assistant" "tool") (mapcar (lambda (block) (getf block :role)) blocks)) "in the order they landed")
      (is (equal "the parser fix landed" (getf (first blocks) :text)) "verbatim, not summarised")
      (is (equal '("" "" "shell") (mapcar (lambda (block) (getf block :name)) blocks)) "the result names its tool")
      (is (equal '("t1" "t1" "t1") (mapcar (lambda (block) (getf block :turn)) blocks)) "and each knows its turn")
      (is (equal '(1 2 3) (mapcar (lambda (block) (getf block :seq)) blocks)) "numbered from one"))))

(deftest lcm-x-cell-a-restart-numbers-on-where-the-file-left-off ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-restart"))
    (lcm-x-turn "s1" "t1")
    (setf stop (funcall stop))
    (setf stop (lcm-x-start "dir" "lcm-x-restart"))
    (lcm-x-publish "turn.input_committed" "s1" "t2" "message" "and what did we decide")
    (let ((blocks (nodecode-lcm-x:read-blocks "lcm-x-restart" "s1")))
      (is (= 4 (length blocks)) "the store is a file, so a restart keeps it")
      (is (= 4 (getf (car (last blocks)) :seq)) "and the next block takes the next number"))))

(deftest lcm-x-cell-capture-off-keeps-nothing ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-quiet" "capture" nil))
    (lcm-x-turn "s1" "t1")
    (is (null (nodecode-lcm-x:read-blocks "lcm-x-quiet" "s1")) "nothing is written with capture off")))

(deftest lcm-x-cell-a-failed-tool-result-is-marked-and-a-long-one-cut ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-clip" "max_chars" 200))
    (lcm-x-publish "turn.tool_result" "s1" "t1"
                   "tool-name" "shell" "failed" t
                   "result" (make-string 500 :initial-element #\x))
    (let ((text (getf (first (nodecode-lcm-x:read-blocks "lcm-x-clip" "s1")) :text)))
      (is (eql 0 (search "[failed]" text)) "a failed result says so")
      (is (< (length text) 400) "a long one is cut to the section's ceiling")
      (is (search "truncated" text) "and says how much it cut"))))

;;; --- what it answers -------------------------------------------------------------------

(deftest lcm-x-cell-a-search-answers-the-blocks-its-words-are-in ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-search"))
    (lcm-x-turn "s1" "t1")
    (lcm-x-publish "turn.input_committed" "s2" "t9" "message" "another session's parser note")
    (let ((nlk:*scribe-session-id* "s1"))
      (let ((hits (lcm-x-call "lcm_search" "query" "parser fix")))
        (is (search "lcm:s1:1" hits) "the ask is a hit, with the reference that reads it back")
        (is (search "the parser fix landed" hits) "and a preview of it")
        (is (null (search "make check" hits)) "a block without the words is not"))
      (is (search "nothing in the store" (lcm-x-call "lcm_search" "query" "zebra")) "a word nothing carries is empty, not a failure")
      (is (search "lcm:s2:1" (lcm-x-call "lcm_search" "query" "parser" "scope" "all")) "scope all reads every session")
      (is (null (search "lcm:s2:1" (lcm-x-call "lcm_search" "query" "parser"))) "the default scope is this one")
      (is (signals-error nlk:contract-refusal (lcm-x-call "lcm_search" "scope" "all")) "a search with no query refuses"))))

(deftest lcm-x-cell-expand-reads-a-hit-back-with-its-neighbours ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-expand"))
    (lcm-x-turn "s1" "t1")
    (let ((nlk:*scribe-session-id* "s1"))
      (let ((answer (lcm-x-call "lcm_expand" "refs" "2" "around" 1)))
        (is (search "it did: examples pass" answer) "the block itself, verbatim")
        (is (search "the parser fix landed" answer) "the block before it")
        (is (search "make check: ok" answer) "and the block after")
        (is (search "--- lcm:s1:2 assistant" answer) "under a heading naming the role"))
      (is (search "no such block" (lcm-x-call "lcm_expand" "refs" "99")) "a reference to nothing says so")
      (is (signals-error nlk:contract-refusal (lcm-x-call "lcm_expand")) "an expand with no reference refuses"))))

(deftest lcm-x-cell-recent-lists-the-newest-and-filters-by-role ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-recent"))
    (lcm-x-turn "s1" "t1")
    (lcm-x-turn "s1" "t2" :ask "and the second ask" :answer "the second answer")
    (let ((nlk:*scribe-session-id* "s1"))
      (let ((answer (lcm-x-call "lcm_recent" "count" 2)))
        (is (search "the second answer" answer) "the newest block is there")
        (is (null (search "the parser fix landed" answer)) "and the oldest is not"))
      (let ((answer (lcm-x-call "lcm_recent" "count" 10 "role" "tool")))
        (is (search "make check: ok" answer) "the tool blocks are there")
        (is (null (search "the second ask" answer)) "and the asks are not")))))

(deftest lcm-x-cell-the-status-tool-says-what-the-store-holds ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-status"))
    (lcm-x-turn "s1" "t1")
    (let ((answer (funcall (third (nle::find-tool "lcm_status")) nil)))
      (is (search "3 blocks" answer) "how many blocks")
      (is (search "1 session" answer) "in how many sessions")
      (is (search "lcm-x-status" answer) "and the folder that keeps them"))))

(deftest lcm-x-cell-the-command-answers-the-store-and-a-search ()
  (with-cell-stop ((lcm-x-start "dir" "lcm-x-cmd"))
    (lcm-x-turn "s1" "t1")
    (let ((alone (cell-entry "nodecode-lcm-x" "lcm" "" "s1")))
      (is (search "lcm-x: 3 blocks" alone) "typed alone it says what the store holds")
      (is (search "lcm:s1:3 tool" alone) "and the newest blocks"))
    (let ((queried (cell-entry "nodecode-lcm-x" "lcm" "parser" "s1")))
      (is (search "lcm:s1:1" queried) "a query searches the store")
      (is (null (search "lcm-x: 3 blocks" queried)) "and answers the hits alone"))))
