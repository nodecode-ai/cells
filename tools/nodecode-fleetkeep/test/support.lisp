;;;; support.lisp --- the fleetkeep test runner, and the one scratch folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a FLEETKEEP name
;;;; prefix; RUN-FLEETKEEP-TESTS runs exactly that slice. Nothing here dials
;;;; anything: the cell reads no network and no key, so the only fixture is a
;;;; scratch folder under this run's own home for the two files a review
;;;; writes.

(in-package #:nodecode.test)

(define-test-slice "fleetkeep" "FLEETKEEP" :start nodecode-fleetkeep:start-cell)

(defun fleetkeep-dir ()
  "A scratch folder under this run's home: where a review writes its files."
  (ensure-directories-exist (nlk:home "fleetkeep/")))

(defun fleetkeep-out ()
  "That folder as the string a section's output_dir takes."
  (namestring (fleetkeep-dir)))

(defun fleetkeep-file (name text)
  "NAME written with TEXT under the scratch folder: => its namestring."
  (let ((path (merge-pathnames name (fleetkeep-dir))))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))
    (namestring path)))

(defun fleetkeep-read (name)
  "NAME's text under the scratch folder, or NIL when nothing was written."
  (let ((path (merge-pathnames name (fleetkeep-dir))))
    (and (probe-file path) (uiop:read-file-string path))))

(defun fleetkeep-issues (result)
  "RESULT's issues, in file order."
  (getf result :issues))

(defun fleetkeep-kinds (result &optional column)
  "The kinds of RESULT's issues, in file order; only COLUMN's when given."
  (loop for issue in (fleetkeep-issues result)
        when (or (null column) (string= column (getf issue :column)))
          collect (getf issue :kind)))

(defun fleetkeep-rows-of (csv result)
  "RESULT's import-ready CSV, read back as rows."
  (declare (ignore result))
  (nodecode-fleetkeep:parse-csv csv))

(defun fleetkeep-json-string (text)
  "TEXT as a JSON string literal: a quote and a backslash escaped, a newline
as \\n, so a CSV rides one wire member whole."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (t (write-char character out))))
    (write-char #\" out)))

(defparameter +messy-csv+
  (format nil "registration,label,insurance due,last service date,mileage,interval,notes~%~
AB12 CDE,\"Van, big\",03/04/2026,2026-01-15,12000,12,fleet A~%~
AB12 CDE,Same again,2026-05-01,15/01/2026,-500,0,~%~
,No reg,2026-06-01,2026-02-01,abc,6,keep out of the import~%~
=HYPERLINK(\"x\"),Formula,2026-07-01,2026-03-01,1000,12,~%~
EF34 GHI,Good,2026-08-01,2026-04-01,\"12,500\",6,~%")
  "The spreadsheet the tests review: a quoted comma, an ambiguous date, a
duplicate registration, no registration, a negative mileage, a zero
interval, a formula cell and a column the six do not take.")

(defparameter +clean-csv+
  (format nil "registration,label,insurance_due,last_service_date,last_service_mileage,service_interval_months~%~
AB12 CDE,Van,2026-04-03,2026-01-15,12000,12~%")
  "A spreadsheet the import takes as it stands.")
