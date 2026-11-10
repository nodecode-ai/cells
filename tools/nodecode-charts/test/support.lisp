;;;; support.lisp --- the charts test runner and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a CHARTS name prefix;
;;;; RUN-CHARTS-TESTS runs exactly that slice. Nothing here dials anything:
;;;; the cell reads no network, and a chart is drawn into the run's own
;;;; scratch home, which its section's default directory puts under
;;;; NODECODE_HOME.

(in-package #:nodecode.test)

(define-test-slice "charts" "CHARTS" :start nodecode-charts:start-cell)

(defun charts-call (id tool arguments)
  "TOOL called through the wire a provider's call comes through, with
ARGUMENTS as a JSON string; => the answer's text."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun charts-answer (id arguments &optional (tool "chart_render"))
  "TOOL's answer as decoded JSON."
  (nlk:decode-json (charts-call id tool arguments)))

(defun charts-json (table)
  "TABLE as the JSON string a provider would send."
  (nlk:encode-json-object table))

(defun charts-member (object key)
  (and (hash-table-p object) (gethash key object)))

(defun charts-png-p (path)
  "T when PATH is a PNG: its own leading bytes, not its name."
  (and (stringp path) (probe-file path)
       (with-open-file (in path :element-type '(unsigned-byte 8))
         (let ((head (make-array 8 :element-type '(unsigned-byte 8))))
           (and (= 8 (read-sequence head in))
                (equalp head #(137 80 78 71 13 10 26 10)))))))
