;;;; support.lisp --- the vaultknox test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; VaultKnox tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a VAULTKNOX-CELL- name prefix,
;;;; so RUN-VAULTKNOX-TESTS runs exactly that slice. Nothing here reaches
;;;; outside the image: the detectors read a string, the advice runs against a
;;;; `next' the test writes, and the tool is called the way the engine's body
;;;; calls it.

(in-package #:nodecode.test)

(define-test-slice "vaultknox" "VAULTKNOX-CELL-" :start nodecode-vaultknox:start-cell)

(defparameter +openai-key+ "sk-abcdefghijklmnopqrstuvwx"
  "A string the OpenAI detector reads as a key: twenty characters after `sk-'.")

(defparameter +github-token+ "ghp_abcdefghijklmnopqrstuvwxyz0123456789"
  "A string the classic GitHub detector reads as a token: thirty-six after the prefix.")

(defun vaultknox-wire-hook ()
  "The cell's advice on the fold, or NIL while it is not started."
  (cell-hook "nodecode-vaultknox" 'nodecode.evolved::wire-messages-from-events))

(defun vaultknox-message (role content)
  "A folded provider message in the shape the fold answers with."
  (nlk:json-object "role" role "content" content))

(defun vaultknox-tool-call (text &optional detectors)
  "scan_secrets through the registered tool, the way the engine's body calls it."
  (let ((entry (nle::find-tool "scan_secrets")))
    (funcall (third entry)
             (nodecode-vaultknox::decode-scan-secrets-request
              (if detectors
                  (nlk:json-object "text" text "detectors" detectors)
                  (nlk:json-object "text" text))))))

(defun vaultknox-slash (args)
  "/vaultknox ARGS in a session, answering its text."
  (nodecode-vaultknox::run-slash args "s1"))

(defun vaultknox-wire-answer (messages)
  "The cell's advice run over MESSAGES as the fold would answer them:
(values SCRUBBED CLEAN-P)."
  (funcall (vaultknox-wire-hook)
           (lambda (&rest args) (declare (ignore args)) (values messages :clean))))
