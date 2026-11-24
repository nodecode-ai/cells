;;;; support.lisp --- the telemetry slice, and its one scratch file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell's tests register into the SHARED nodecode.test registry under a
;;;; TELEMETRY-CELL- name prefix, and RUN-TELEMETRY-TESTS runs exactly that
;;;; slice. Nothing here dials: the ledger is a file under the run's scratch
;;;; home, emptied before each test, and the hook points are driven by hand
;;;; through the helpers the tree already owns (CELL-PUBLISH, CELL-FRAME-OP)
;;;; and through the cell's own :TOOL chain.

(in-package #:nodecode.test)

(define-test-slice "telemetry" "TELEMETRY-CELL-"
  :start nodecode-telemetry:start-cell
  :defaults (("mode" "observe")
             ("daily_usd" "5")
             ("monthly_usd" "100")
             ("session_usd" "0")))

(defmacro with-telemetry ((&rest pairs) &body body)
  "Run BODY with the cell started on a fresh ledger: PAIRS are section
members beyond the slice's defaults, written as KEY VALUE. The file and the
cell are gone after."
  `(progn
     (when (fboundp 'nodecode-telemetry::rebuild)
       (nodecode-telemetry:reset-ledger))
     (with-cell-stop ((telemetry-start ,@pairs))
       ,@body)))

(defmacro with-empty-ledger (&body body)
  "Run BODY with the ledger file gone, and gone again after."
  `(unwind-protect (progn (nodecode-telemetry:reset-ledger) ,@body)
     (nodecode-telemetry:reset-ledger)))

(defun usage-payload (&key (provider "anthropic") (model "claude-telemetry-1")
                        (in 1200) (out 300) (cache 4000) (reasoning 120)
                        (cost 0.25) (known t) &allow-other-keys)
  "One provider round's usage, as the turn.usage fact carries it."
  (nlk:json-object "provider" provider "model" model
                    "input-tokens" in "output-tokens" out
                    "cached-input-tokens" cache "cache-write-tokens" 0
                    "reasoning-tokens" reasoning "total-tokens" (+ in out)
                    "cost-known" known "cost-usd" cost "finish-reason" "stop"))

(defun request-payload (&key (ttft 700) (duration 3200) &allow-other-keys)
  "One round's timings, as the turn.provider_request fact carries them."
  (nlk:json-object "provider" "anthropic" "model" "claude-telemetry-1"
                    "provider-round" 0 "ttft-ms" ttft "stream-duration-ms" duration))

(defun publish-fact (type payload &key (session "s1") (turn "t1"))
  "Publish one durable fact through the cell's :frame hook."
  (cell-publish "nodecode-telemetry"
                (cell-frame-op type :session session :turn turn :payload payload)))

(defun publish-round (&rest keys &key (session "s1") (turn "t1") &allow-other-keys)
  "Publish one round's request and usage for (SESSION, TURN)."
  (publish-fact "turn.provider_request" (apply #'request-payload
                                               (loop for (key value) on keys by #'cddr
                                                     unless (member key '(:session :turn))
                                                       append (list key value)))
                :session session :turn turn)
  (publish-fact "turn.usage" (apply #'usage-payload
                                    (loop for (key value) on keys by #'cddr
                                          unless (member key '(:session :turn))
                                            append (list key value)))
                :session session :turn turn))

(defvar *ran* nil
  "Set by NEXT in a tool call the hook let through.")

(defun run-tool-call (name &key (answer "done") (arguments nil))
  "Run NAME's call through the cell's :TOOL hook chain, NEXT answering
ANSWER (or signalling), and answer what the chain answered."
  (setf *ran* nil)
  (funcall (cell-hook "nodecode-telemetry" :tool)
           (list :name name :arguments (or arguments (nlk:json-object)) :call-id "c1")
           (lambda (op)
             (declare (ignore op))
             (setf *ran* t)
             (if (eq answer :signal) (error "the tool itself failed") answer))))
