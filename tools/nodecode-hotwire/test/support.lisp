;;;; support.lisp --- the hotwire test runner and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a HOTWIRE name prefix;
;;;; RUN-HOTWIRE-TESTS runs exactly that slice. Nothing here dials anything:
;;;; the simulator reads no file, no environment and no network, and the one
;;;; seam a test reaches for is the cell's own tools, called through the wire
;;;; the way a provider's call calls them.

(in-package #:nodecode.test)

(define-test-slice "hotwire" "HOTWIRE" :start nodecode-hotwire:start-cell)

(defun hotwire-json-string (text)
  "TEXT as one JSON string, quotes and newlines escaped."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (#\Return nil)
               (t (write-char character out))))
    (write-char #\" out)))

(defun hotwire-arguments (&rest pairs)
  "One tool call's JSON from PAIRS, a key and a string value each."
  (format nil "{~{~a~^, ~}}"
          (loop for (key value) on pairs by #'cddr
                collect (format nil "~a: ~a" (hotwire-json-string key)
                                (hotwire-json-string value)))))

(defun hotwire-tool (id tool &rest pairs)
  "TOOL called as a provider's call calls it, with PAIRS: the answer's text."
  (nle::message-content (execute-wire-call id tool (apply #'hotwire-arguments pairs))))

(defun hotwire-arguments-raw (&rest pairs)
  "One tool call's JSON from PAIRS, each value already written as JSON."
  (format nil "{~{~a~^, ~}}"
          (loop for (key value) on pairs by #'cddr
                collect (format nil "~a: ~a" (hotwire-json-string key) value))))

(defun hotwire-tool-raw (id tool &rest pairs)
  "TOOL called with PAIRS whose values are JSON as written: the answer's text."
  (nle::message-content (execute-wire-call id tool (apply #'hotwire-arguments-raw pairs))))

(defun hotwire-command (args)
  "/hotwire typed with ARGS: the answer's text."
  (nodecode-hotwire:run-slash args nil))

(defparameter +divider+
  "*@ title Ten-to-five volt divider
*@ in VIN
*@ out out
*@ spec v(out) in 4.9..5.1 V
VIN in 0 DC 10 AC 1
R1 in out 10k
R2 out 0 10k
.end")

(defparameter +common-emitter+
  "*@ title Common-emitter stage
*@ in VIN
*@ out c
*@ supply VCC
*@ spec region(Q1) == active
*@ spec v(c) in 5..8 V
VIN in 0 DC 0 AC 1
VCC vcc 0 DC 9
R1 vcc b 470k
R2 b 0 100k
RC vcc c 1k
Q1 c b 0 2N3904
.end")

(defparameter +floating+
  "*@ title A node no resistor reaches
VIN in 0 DC 1
R1 in 0 10k
C1 in out 1u
C2 out 0 1u
.end"
  "A node behind two capacitors only: no DC path, which the rule check must catch.")

(defparameter +no-output+
  "VIN in 0 DC 1
R1 in 0 1k
.end"
  "A circuit with no '*@ out' annotation, which an AC sweep cannot do without.")

(defparameter +detuned-lowpass+
  "*@ title RC low-pass that needs a standard value
*@ in VIN
*@ out out
*@ spec fc in 1900..2100 Hz
VIN in 0 DC 0 AC 1
R1 in out 4.5k
C1 out 0 15n
.end"
  "Its corner sits above the spec: the resistor is not the value the filter needs.")

(defparameter +twelve-parts+
  "*@ title Twelve resistors
*@ in VIN
*@ out a
VIN in 0 DC 1
R1 in a 1k
R2 a 0 1k
R3 a 0 1k
R4 a 0 1k
R5 a 0 1k
R6 a 0 1k
R7 a 0 1k
R8 a 0 1k
R9 a 0 1k
R10 a 0 1k
R11 a 0 1k
.end"
  "More parts than a lowered element bound allows.")
