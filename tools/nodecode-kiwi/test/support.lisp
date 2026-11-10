;;;; support.lisp --- the test slice: a stubbed buyer runtime.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; KIWI-CELL- name prefix; RUN-KIWI-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-KIWI::EXCHANGE, the cell's
;;;; only way to the child process. WITH-RUNTIME stubs that function with a
;;;; canned reply and records every frame, so no test starts Node.js and no
;;;; test needs a network.

(in-package #:nodecode.test)

(define-test-slice "kiwi" "KIWI-CELL-" :start nodecode-kiwi:start-cell)

;;; --- the canned runtime ----------------------------------------------------------

(defvar *calls* '()
  "Every frame the stub took, newest first: (:method :params).")

(defun frame-result (result)
  "The reply frame a runtime writes for RESULT."
  (nlk:json-object "jsonrpc" "2.0" "id" 2 "result" result))

(defun frame-error (message &key (code -32603))
  "The reply frame a runtime writes when it refuses."
  (nlk:json-object "jsonrpc" "2.0" "id" 2
                    "error" (nlk:json-object "code" code "message" message)))

(defun content (text &key error-p)
  "An MCP tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                    "isError" (and error-p t)))

(defmacro with-runtime ((&body answer) &body body)
  "Run BODY with the child-process seam replaced by a function of (METHOD
PARAMS) that answers what ANSWER says, every frame recorded in *CALLS*."
  `(let ((*calls* '()))
     (with-stubbed-fdefinition (nodecode-kiwi::exchange (method params)
                                (push (list :method method :params params) *calls*)
                                ,@answer)
       ,@body)))

(defun answered (method result)
  "Have the stub answer METHOD with RESULT, whatever was asked."
  (declare (ignore method))
  (frame-result result))

(defmacro with-kiwi ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS and stopped on unwind."
  `(with-cell-stop ((kiwi-start ,@pairs))
     ,@body))

;;; --- calling a tool, reading the frame --------------------------------------------

(defun kiwi-call-tool (name arguments)
  "NAME called through the real dispatch seam, as a provider sends it; => the
result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun last-frame ()
  "The newest frame the stub took."
  (first *calls*))

(defun asked (frame &rest path)
  "FRAME's params, walked down PATH; => text, or NIL."
  (let ((value (getf frame :params)))
    (dolist (step path)
      (setf value (if (stringp step) (nlk:json-value value :any step) value)))
    (typecase value
      (null nil)
      ((eql :null) nil)
      (string value)
      (t (princ-to-string value)))))
