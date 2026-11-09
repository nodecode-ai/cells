;;;; support.lisp --- freight test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a FREIGHT-CELL-
;;;; name prefix; RUN-FREIGHT-TESTS runs exactly that slice. Every test gets a
;;;; scratch home, so the case library lands nowhere near the operator's.
;;;;
;;;; The section's data_dir is defaulted to a folder under that scratch home,
;;;; so a test only names it when it is testing what a missing one refuses.

(in-package #:nodecode.test)

(defun freight-test-library ()
  "The data_dir a test's section carries: under the test's scratch home."
  "freight-test")

(define-test-slice "freight" "FREIGHT-CELL-" :start nodecode-freight:start-cell
  :defaults (("data_dir" (freight-test-library))))

(defmacro with-freight ((&rest config) &body body)
  "BODY with the cell started on its defaults, or on the section pairs CONFIG
gives, and stopped on unwind."
  `(with-cell-stop ((freight-start ,@config))
     ,@body))

(defun freight-fixture (name text)
  "Write TEXT as the fixture file NAME under the scratch home, and answer its
path, so an import reads a real file without the network or the operator's
disk."
  (let ((path (nlk:home (format nil "freight-fixtures/~a" name))))
    (nlk:write-file-atomically path text :mode #o600 :directory-mode #o700)
    (namestring path)))

(defparameter +trips+
  "trip_id,carrier_id,route_id,vehicle_class,execution_status,departed_at
T1,C1,R1,V1,EXECUTED,2026-11-05T08:00:00+00:00
"
  "Two executed trips in the November case's period: T1 billed, T2 not.")

(defparameter +trips-two+
  "trip_id,carrier_id,route_id,vehicle_class,execution_status,departed_at
T1,C1,R1,V1,EXECUTED,2026-11-05T08:00:00+00:00
T2,C1,R1,V1,EXECUTED,2026-11-06T08:00:00+00:00
"
  "T2 is executed and inside the period, and no bill line names it.")

(defparameter +base-rates+
  "rule_id,version,carrier_id,route_id,vehicle_class,charge_code,currency,amount_basis,effective_from,effective_to,formula,rate
R1,1,C1,R1,V1,BASE,CNY,,2026-11-01T00:00:00+00:00,2026-12-01T00:00:00+00:00,PER_TRIP,1000.00
"
  "One confirmed per-trip rule at 1000.00 CNY.")

(defparameter +base-bill+
  "line_id,carrier_id,trip_ref,charge_code,currency,amount
L1,C1,T1,BASE,CNY,1100.00
"
  "One bill line: the flat rate plus 100.00.")

(defun freight-open (&optional (name "november"))
  "Open the November case every test works on."
  (freight:open-case name :carrier "C1"
                      :period-from "2026-11-01T00:00:00+00:00"
                      :period-to "2026-12-01T00:00:00+00:00"))
