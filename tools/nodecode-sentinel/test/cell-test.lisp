;;;; cell-test.lisp --- the trap, the redaction, the bound and the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: nothing here reaches the network, and every tool body the hook
;;;; wraps is a lambda in this file, so the cell is proved against its own
;;;; seam and no model, shell or store.

(in-package #:nodecode.test)

(defparameter +fake-key+ (concatenate 'string "sk-" (make-string 32 :initial-element #\0))
  "An all-zero key: it exercises the recognizer without shipping a live one.")

(defun sentinel-op (form &optional (call-id "call-1"))
  "A tool call as the :tool point hands it to the hook."
  (list :name "eval" :arguments (nlk:json-object "form" form) :call-id call-id))

(defun sentinel-answer (op &optional (answer :allowed))
  "What the installed :tool hook answers for OP: :ALLOWED, or the text it
gives in place of the body's — a refusal, a redacted result, a bounded one."
  (funcall (cell-hook "nodecode-sentinel" :tool) op
           (lambda (ignored) (declare (ignore ignored)) answer)))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "sentinel"
  (:hooks :tool)
  (:help :sentinel)
  (:command "sentinel")
  (:running (is (equal nodecode-sentinel::*help* (nle:help :sentinel)) "the topic answers the manual text")
            (is (= 4000 (nodecode-sentinel::setting :max-chars)) "the defaults are a section-less start's"))
  (:refused ("loop_threshold" 1)))

;;; --- the loop trap ---------------------------------------------------------

(deftest sentinel-cell-stops-a-repeated-call (with-cell-stop ((sentinel-start))
  (let ((op (sentinel-op "(* 6 7)")))
    (is (eq :allowed (sentinel-answer op)) "the first call runs")
    (is (eq :allowed (sentinel-answer op)) "the second identical call runs")
    (is (search "refused by nodecode-sentinel" (sentinel-answer op)) "the third identical call is refused")
    (is (eq :allowed (sentinel-answer (sentinel-op "(* 7 7)"))) "a changed call runs")
    (is (search "epoch" (nodecode-sentinel::run-slash "reset" "s")) "reset answers")
    (is (eq :allowed (sentinel-answer op)) "after a reset the same call runs again"))))

;;; --- credential redaction ---------------------------------------------------

(deftest sentinel-cell-redacts-a-result (with-cell-stop ((sentinel-start))
  (let ((answer (sentinel-answer (sentinel-op "t") (format nil "key: ~a" +fake-key+))))
    (is (search "[REDACTED_OPENAI_KEY]" answer) "the key is replaced by its marker")
    (is (null (search "sk-000" answer)) "no part of the key survives")
    (is (search "[REDACTED_SECRET]" (sentinel-answer (sentinel-op "t") "password = hunter2hunter2hunter2")) "a named secret is recognized too"))))

(deftest sentinel-cell-refuses-a-credentialed-message (with-cell-stop ((sentinel-start))
  (let ((advice (cell-hook "nodecode-sentinel" 'nle:submit))
        (next (lambda (&rest arguments) (declare (ignore arguments)) :sent)))
    (is (functionp advice) "the message guard is installed on the ingress")
    (is (eq :sent (funcall advice next "s" "which file holds the config")) "an ordinary message starts")
    (let ((refused (handler-case (progn (funcall advice next "s" (format nil "key ~a" +fake-key+)) :sent)
                      (nodecode-sentinel::sentinel-error () :refused))))
      (is (eq :refused refused) "a message carrying a key is refused before its turn starts")))))

;;; --- a bounded result -------------------------------------------------------

(deftest sentinel-cell-bounds-a-long-result (with-cell-stop ((sentinel-start))
  (let ((answer (sentinel-answer (sentinel-op "long") (make-string 9000 :initial-element #\x))))
    (is (<= (length answer) 4000) "the result is cut to the character bound")
    (is (search "sentinel: truncated" answer) "the cut says how much went"))))

(deftest sentinel-cell-bounds-lines-too (with-cell-stop ((sentinel-start))
  (let ((answer (sentinel-answer (sentinel-op "lines")
                                  (format nil "~{~a~%~}" (loop repeat 300 collect "one line")))))
    (is (<= (length (nodecode-sentinel::split-lines answer)) 100) "the result is cut to the line bound")
    (is (search "truncated" answer) "the cut is named in the text"))))

;;; --- the command -----------------------------------------------------------

(deftest sentinel-cell-command-answers (with-cell-stop ((sentinel-start))
  (is (search "sentinel is on" (nodecode-sentinel::run-slash "" "s")) "status answers")
  (is (search "ledger" (nodecode-sentinel::run-slash "audit" "s")) "audit answers the ledger")
  (is (search "cleared" (nodecode-sentinel::run-slash "clear" "s")) "clear answers")))

(deftest sentinel-cell-refuses-while-stopped ()
  (let ((answer (handler-case (progn (nodecode-sentinel::run-slash "status" "s") :answered)
                  (nodecode-sentinel::sentinel-error () :refused))))
    (is (eq :refused answer) "the command refuses while the cell is not running")))
