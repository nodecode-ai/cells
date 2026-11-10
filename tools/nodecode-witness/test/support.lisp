;;;; support.lisp --- witness test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Witness tests register into the SAME nodecode.test registry under a
;;;; WITNESS-CELL- name prefix; RUN-WITNESS-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed and nothing leaves the machine: the
;;;; grades and the ledger are files under a scratch folder, the trail is
;;;; facts in a scratch store, and the skills graded are definitions kept in
;;;; a fixture knowledge cell (WITH-KNOWLEDGE-FIXTURE, KEEP-DEFINITION-TEXT).

(in-package #:nodecode.test)

(define-test-slice "witness" "WITNESS-CELL-" :start nodecode-witness:start-cell)

(defmacro with-witness ((&rest pairs) &body body)
  "BODY over a fixture knowledge cell and a scratch store, the cell started
on PAIRS (its section) plus a scratch dir, and stopped on unwind."
  `(with-knowledge-fixture ()
     (with-temp-store ()
       (with-cell-stop ((witness-start "dir" (namestring (temp-path "witness")) ,@pairs))
         ,@body))))

(defun witness-ledger-path ()
  (merge-pathnames "ledger.jsonl" (witness:witness-directory)))

(defun witness-seed-turn (session-id tools)
  "One completed turn of SESSION-ID journaling TOOLS, each
(CALL-ID TOOL ARGUMENTS RESULT FAILED)."
  (ensure-durable-session session-id)
  (let ((turn (nlk:admit-turn session-id (nlk::make-durable-id "command") "go")))
    (dolist (spec tools turn)
      (destructuring-bind (call-id tool arguments result failed) spec
        (nlk:record-tool-call turn call-id tool arguments)
        (nlk:record-tool-result turn call-id tool result :failed failed)))
    (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "done"))
    (nlk:complete-turn turn nil)))

(defun witness-keep (session-id text)
  "Keep TEXT, one define-skill form, as SESSION-ID's evaluation does -- and
through NLK:RECORD-DEFINITIONS, so the cell's advice grades it on the way."
  (let ((nlk:*scribe-session-id* session-id))
    (keep-definition-text text :session session-id)))

(defun witness-call (action &optional skill text)
  "The `witness' tool's answer to one call, as the boundary would build it."
  (witness:witness-verb
   (witness::decode-witness-call
    (nlk:make-json-object "action" action "skill" (or skill "") "text" (or text "")))))
