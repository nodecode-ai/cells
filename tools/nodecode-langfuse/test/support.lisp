;;;; support.lisp --- the langfuse slice, and the one seam that leaves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell's tests register into the SHARED nodecode.test registry under a
;;;; LANGFUSE-CELL- name prefix and RUN-LANGFUSE-TESTS runs exactly that
;;;; slice. The seam that reaches outside is NODECODE-LANGFUSE::*POST*, the
;;;; function a batch goes through: WITH-STUBBED-INGESTION captures every
;;;; batch and answers what the test names, so no test ever dials Langfuse.
;;;; The frame hook is driven by hand through the helpers this tree already
;;;; owns (CELL-PUBLISH, CELL-FRAME-OP), the tool hook through the chain
;;;; itself.

(in-package #:nodecode.test)

(define-test-slice "langfuse" "LANGFUSE-CELL-"
  :start nodecode-langfuse:start-cell
  :defaults (("public_key" "pk-lf-test")
             ("secret_key" "sk-lf-test")
             ("base_url" "https://langfuse.test")))

(defvar *posts* '()
  "Each batch the stubbed endpoint received, newest first: a plist of :URL,
:HEADERS and :BODY.")

(defvar *answer* t
  "What the stubbed endpoint answers: T, or the reason it refuses with.")

(defun stub-ingestion (url headers body)
  (push (list :url url :headers headers :body body) *posts*)
  (if (eq *answer* t) t (values nil *answer*)))

(defmacro with-stubbed-ingestion (&body body)
  "Run BODY with the endpoint stubbed: batches land on *POSTS*, answered as
*ANSWER* says; nothing dials."
  `(with-saved-globals (nodecode-langfuse::*post* nodecode-langfuse::*last-error*)
     (setf *posts* '()
           *answer* t
           nodecode-langfuse::*last-error* nil
           nodecode-langfuse::*post* #'stub-ingestion)
     ,@body))

(defmacro with-langfuse ((&rest pairs) &body body)
  "Run BODY with the cell started against a test endpoint: PAIRS are section
members beyond the slice's defaults; stopped on unwind."
  `(with-stubbed-ingestion
     (with-cell-stop ((langfuse-start ,@pairs))
       ,@body)))

(defun posted-batch (&optional (index 0))
  "The INDEXth newest batch the endpoint received, decoded."
  (nlk:decode-json (getf (nth index *posts*) :body)))

(defun posted-events (&optional (index 0))
  "That batch's events, oldest first, as a list."
  (coerce (gethash "batch" (posted-batch index)) 'list))

(defun event-of-type (type &optional (index 0))
  "The one event in the INDEXth newest batch whose type is TYPE."
  (find type (posted-events index) :test #'equal :key (lambda (event) (gethash "type" event))))

(defun event-body (event) (gethash "body" event))

(defun usage-payload ()
  "One provider round's usage, as the turn.usage fact carries it."
  (nlk:json-object "provider" "anthropic" "model" "claude-langfuse-1"
                   "input-tokens" 1200 "output-tokens" 300
                   "cached-input-tokens" 4000 "reasoning-tokens" 120
                   "cost-known" t "cost-usd" 0.0123 "finish-reason" "stop"))

(defun end-the-turn (&optional (turn "t1") (type "turn.completed"))
  "Publish the fact that ends TURN through the cell's frame hook."
  (cell-publish "nodecode-langfuse"
                (cell-frame-op type :turn turn
                               :payload (nlk:json-object "status" "completed"))))

(defun run-tool-call (name arguments result &key (call-id "c1"))
  "Run NAME's call through the cell's :TOOL hook chain, with NEXT answering
RESULT, and answer what the chain answered."
  (funcall (cell-hook "nodecode-langfuse" :tool)
           (list :name name :arguments arguments :call-id call-id)
           (lambda (op) (declare (ignore op)) result)))
