;;;; tools.lisp --- the seven tools the original package gives the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original names them search, inspect, run, runs_get, runs_list,
;;;; runs_stop and balance (README.md's table, and the skill that ships
;;;; beside it). Here they carry the cell's prefix and the same arguments,
;;;; each one an MCP tools/call on the server the original points at.
;;;;
;;;; A call's refusal the server reports is the answer text, prefixed ERROR:,
;;;; which is the shape the model repairs from: insufficient_balance,
;;;; rate_limited and policy refusals come back saying so.

(in-package #:nodecode-glasser)

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model reads."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: glasser: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun optional-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or NIL."
  (let ((value (gethash name args)))
    (and (stringp value)
         (plusp (length (string-trim '(#\Space) value)))
         (string-trim '(#\Space) value))))

(defun optional-integer (args name)
  "ARGS' NAME as an integer, or NIL when it is absent; a non-integer refuses."
  (let ((value (gethash name args)))
    (cond ((null value) nil)
          ((integerp value) value)
          (t (fail "~a must be an integer" (substitute #\_ #\- name))))))

(defun new-idempotency-key ()
  "A version-4 UUID: the key the original's skill says to make for every run,
so a retry with the same key returns the first run instead of paying twice."
  (format nil "~8,'0x-~4,'0x-4~3,'0x-~4,'0x-~12,'0x"
          (random (expt 2 32))
          (random (expt 2 16))
          (random (expt 2 12))
          (+ #x8000 (random #x4000))
          (random (expt 2 48))))

(defun idempotency-key (args)
  "ARGS' idempotency_key, else a fresh one."
  (or (optional-text args "idempotency_key") (new-idempotency-key)))

(defun arguments-object (&rest pairs)
  "PAIRS, key/value, as the JSON object a tools/call carries, skipping a NIL value."
  (let ((object (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr
          when value do (setf (gethash key object) value))
    object))

;;; --- search -------------------------------------------------------------------

(defun tool-search (args)
  (answering
    (call-tool "search"
                (arguments-object "query" (argument-text args "query")
                                  "limit" (optional-integer args "limit")))))

;;; --- inspect ------------------------------------------------------------------

(defun tool-inspect (args)
  (answering
    (call-tool "inspect" (arguments-object "endpoint" (argument-text args "endpoint")))))

;;; --- run ----------------------------------------------------------------------

(defun tool-run (args)
  (answering
    (let ((input (gethash "input" args)))
      (call-tool "run" (arguments-object "endpoint" (argument-text args "endpoint")
                                          "input" (and (hash-table-p input) input)
                                          "idempotency_key" (idempotency-key args))))))

;;; --- runs_get, runs_list, runs_stop -------------------------------------------

(defun tool-runs-get (args)
  (answering
    (call-tool "runs_get" (arguments-object "run_id" (argument-text args "run_id")))))

(defun tool-runs-list (args)
  (answering
    (call-tool "runs_list" (arguments-object "limit" (optional-integer args "limit")))))

(defun tool-runs-stop (args)
  (answering
    (call-tool "runs_stop" (arguments-object "run_id" (argument-text args "run_id")))))

;;; --- balance ------------------------------------------------------------------

(defun tool-balance (args)
  (declare (ignore args))
  (answering (call-tool "balance" (nlk:json-object))))

;;; --- the schemas the model reads ----------------------------------------------

(defun string-property (description)
  (nlk:json-object "type" "string" "description" description))

(defparameter *search-schema*
  (list :description (concatenate 'string
                                  "Search Glasser's data sources: the runnable paid endpoints this key may run, "
                                  "across people, company, SEO, web, social, news, property and market data. "
                                  "Describe the data you need in plain words. Costs nothing.")
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "query" (string-property
                                            "What data you need, e.g. 'work emails and titles for the engineering leads at a company'.")
                                   "limit" (nlk:json-object "type" "integer"
                                                             "description" "How many endpoints to list at most."))
                     "required" #("query")))
  "The glasser_search tool.")

(defparameter *inspect-schema*
  (list :description (concatenate 'string
                                  "One endpoint's input schema, its exact price with every charge clause, its "
                                  "version and run mode. Inspect before the first run. Costs nothing.")
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "endpoint" (string-property "The endpoint id glasser_search answered."))
                     "required" #("endpoint")))
  "The glasser_inspect tool.")

(defparameter *run-schema*
  (list :description (concatenate 'string
                                  "Run one endpoint and charge the account its published price per call. Every run "
                                  "carries an idempotency key: a retry with the same key returns the first run "
                                  "instead of paying twice, and a new key is a new charge. Say the price first.")
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "endpoint" (string-property "The endpoint id to run, as glasser_inspect showed it.")
                                   "input" (nlk:json-object
                                            "type" "object"
                                            "description" "The endpoint's input, as its schema asks for it.")
                                   "idempotency_key" (string-property
                                                       "A UUID for this run; omit it and the cell makes one, which a retry of yours would not reuse."))
                     "required" #("endpoint")))
  "The glasser_run tool.")

(defparameter *runs-get-schema*
  (list :description "One run by id: its status, its output, the provider's own response and the charge in USD."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "run_id" (string-property "The run id glasser_run answered."))
                     "required" #("run_id")))
  "The glasser_runs_get tool.")

(defparameter *runs-list-schema*
  (list :description "This workspace's runs, newest first, with their status and charge."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "limit" (nlk:json-object "type" "integer"
                                                             "description" "How many runs to list."))
                     "required" #()))
  "The glasser_runs_list tool.")

(defparameter *runs-stop-schema*
  (list :description (concatenate 'string
                                  "Stop an in-flight run. Before dispatch it becomes STOPPED and is free; "
                                  "after dispatch the charge stands.")
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "run_id" (string-property "The run id to stop."))
                     "required" #("run_id")))
  "The glasser_runs_stop tool.")

(defparameter *balance-schema*
  (list :description (concatenate 'string
                                  "The workspace's balance, held and available, in USD. Costs nothing, and is "
                                  "the probe that says whether the key works.")
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object) "required" #()))
  "The glasser_balance tool.")
