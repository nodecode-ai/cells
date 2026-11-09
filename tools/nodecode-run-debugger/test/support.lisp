;;;; support.lisp --- the slice, and a trace file a test can point the cell at.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; run-debugger tests register into the shared nodecode.test registry under
;;;; a RUN-DEBUGGER-CELL- name prefix; RUN-RUN-DEBUGGER-TESTS runs exactly
;;;; that slice.
;;;;
;;;; Nothing here needs a network: the cell reads one file from the scratch
;;;; home and answers from it. The files a test writes are its own, under
;;;; (HOME "run-debugger-test/"), and it removes them on the way out.

(in-package #:nodecode.test)

(define-test-slice "run-debugger" "RUN-DEBUGGER-CELL-" :start nodecode-run-debugger:start-cell)

;;; --- one trace, as the format writes it -----------------------------------------

(defun envelope (run-id &optional (completeness "complete"))
  "The first record of a trace: the envelope."
  (nlk:json-object
   "kind" "trace" "schema" "hermes-run-debugger/trace" "version" 1
   "trace_id" (format nil "~a-trace" run-id)
   "run_id" run-id
   "source" (nlk:json-object "kind" "file" "ref" "sha256:0f1e2d3c4b5a69788796a5b4c3d2e1f0")
   "redaction" (nlk:json-object "policy" "redacted-summary")
   "completeness" (nlk:json-object "status" completeness)))

(defun attempt-record (run-id id outcome)
  "One attempt record, with its reported outcome."
  (nlk:json-object "kind" "attempt" "attempt_id" id "run_id" run-id
                    "outcome" outcome "sequence" 1 "model" "a-model"))

(defun event-record (run-id id kind status sequence &key name error parents attempt tokens)
  "One event record: what the source reports, and nothing it does not."
  (nlk:json-object
   "kind" "event" "event_id" id "run_id" run-id
   "attempt_id" (or attempt "a1")
   "event_kind" kind
   "reported_status" status
   "semantic_provenance" "reported"
   "source_sequence" sequence
   "wall_time" (format nil "2026-01-0~dT10:00:0~dZ" (1+ (mod sequence 9)) (mod sequence 10))
   "name" name
   "reported_error" error
   "causal_parents" (coerce (or parents '()) 'vector)
   "usage" (if tokens
              (nlk:json-object "input_tokens" tokens "output_tokens" 12)
              (nlk:json-object))))

(defparameter +scenario+
  (lambda (run-id)
    "A failed run: an unrecovered tool failure, one provider error, and a
success whose own text says nothing was verified."
    (list (envelope run-id "complete")
          (attempt-record run-id "a1" "failed")
          (event-record run-id "e1" "tool_call" "failed" 1
                         :name "read_file" :error "permission denied: /srv/data" :tokens 400)
          (event-record run-id "e2" "provider_error" "failed" 2
                         :error "HTTP 429 too many requests" :parents '("e1"))
          (event-record run-id "e3" "model_call" "ok" 3
                         :name "a-model" :error "0 tests ran" :parents '("e2"))))
  "The trace every reading test imports, built for one run id.")

(defun trace-path (name)
  "A trace file of this run's own, under the scratch home."
  (merge-pathnames (format nil "~a.jsonl" name)
                   (nlk:home "run-debugger-test/")))

(defun write-trace (name records)
  "Write RECORDS as the JSONL trace NAME; => its pathname."
  (let ((path (trace-path name)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically
     path (format nil "~{~a~%~}" (mapcar #'nlk:encode-json-object records)))
    path))

(defun scenario-trace (name &optional (run-id name))
  "The scenario trace as a file called NAME, for the run RUN-ID."
  (write-trace name (funcall +scenario+ run-id)))

(defun scratch-file (name text)
  "NAME, as given, under the scratch home, holding TEXT."
  (let ((path (merge-pathnames name (nlk:home "run-debugger-test/"))))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path text)
    path))

;;; --- asking the cell what a model would -----------------------------------------

(defun tool-text (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content
                    (execute-wire-call "call-1" name
                                       (if (stringp arguments)
                                           arguments
                                           (nlk:encode-json-object arguments))))))

(defun import-text (path &key allow-partial)
  "rundebug_import called with one file."
  (tool-text "rundebug_import"
             (nlk:json-object "files" (vector (princ-to-string path))
                               "allow_partial" allow-partial)))

(defun run-of (text)
  "The run id an import answer names, so a test need not know it."
  (let* ((at (cl:search "run " text))
         (end (and at (position #\: text :start at))))
    (and at end (subseq text (+ at 4) end))))

;;; --- the fixtures ----------------------------------------------------------------

(defmacro with-rundebug ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as the `run-debugger' section,
stopped on unwind, and the scratch trace folder removed afterwards."
  `(with-cell-stop ((run-debugger-start ,@pairs))
     (nlk:with-cleanup ((ignore-errors
                          (uiop:delete-directory-tree (nlk:home "run-debugger-test/")
                                                       :validate t
                                                       :if-does-not-exist :ignore)))
       ,@body)))
