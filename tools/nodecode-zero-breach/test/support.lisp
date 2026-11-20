;;;; support.lisp --- the stubbed engine, a report the test writes, a scratch
;;;; report folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST) under a ZERO-BREACH-CELL- name prefix; RUN-ZERO-BREACH-TESTS
;;;; runs that slice. What is stubbed is the PROCESS, never the cell: the
;;;; stubbed runner answers the argv the cell builds and writes the file the
;;;; cell asked for, so the argv, the report reading, the gap/hit split and
;;;; the words are all the cell's own code.

(in-package #:nodecode.test)

(define-test-slice "zero-breach" "ZERO-BREACH-CELL-" :start nodecode-zero-breach:start-cell
  :defaults (("scope" "quick")))

(defvar *zb-calls* nil
  "Every argv the stubbed engine was handed, newest first.")

(defvar *zb-status* "NO_KNOWN_INDICATORS"
  "The verdict the stubbed engine writes.")

(defvar *zb-hits* nil
  "The hit rows the stubbed engine writes: (INDICATOR FAMILY TIER LOCATION) each.")

(defvar *zb-gaps* nil
  "The gap sentences the stubbed engine writes.")

(defvar *zb-describe* nil
  "What the stubbed engine answers to --describe, as JSON text.")

(defun zb-scratch-directory ()
  "A scratch directory no other test shares."
  (merge-pathnames (format nil "zb-cell-~a/" (random most-positive-fixnum))
                   (uiop:temporary-directory)))

(defmacro with-zero-breach ((&rest pairs) &body body)
  "BODY with the cell started on the `zero-breach' section: PAIRS, plus a
scratch report folder and the stubbed engine when they name none."
  (let ((directory (gensym "DIRECTORY")))
    `(let ((,directory (zb-scratch-directory))
           (*zb-calls* nil)
           (*zb-status* "NO_KNOWN_INDICATORS")
           (*zb-hits* nil)
           (*zb-gaps* nil)
           (*zb-describe* nil))
       (unwind-protect
            (with-cell-stop ((zero-breach-start
                              ,@(unless (loop for (key) on pairs by #'cddr
                                              thereis (and (stringp key) (string= key "report_directory")))
                                  (list "report_directory" `(namestring ,directory)))
                              ,@pairs))
              (setf nodecode-zero-breach::*run* #'zb-fake-run)
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,directory :validate t))))))

(defun zb-json-escape (text)
  "TEXT as a JSON string body."
  (with-output-to-string (s)
    (loop for ch across (or text "")
          do (case ch
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (#\Newline (write-string "\\n" s))
               (t (write-char ch s))))))

(defun zb-report-text (&key status hits gaps)
  "The report document the stubbed engine writes, as JSON text."
  (format nil "{\"schema_version\":\"1.6\",\"tool\":{\"name\":\"zero-breach-toolkit\",\"version\":\"3.0.2\"},\"platform\":{\"system\":\"Linux\",\"machine\":\"x86_64\"},\"scope\":{\"scan_type\":\"quick\"},\"ioc_set\":{\"count\":3122,\"age_days\":18,\"stale\":false,\"freshness_unknown\":false,\"families\":{\"Candiru\":125,\"Paragon Graphite\":60}},\"modules\":{\"browser\":{\"name\":\"browser\",\"status\":\"CLEAN\",\"checked\":3338,\"skipped\":0,\"duration_s\":14.2,\"hits\":[~{~a~^,~}],\"checks\":[{\"name\":\"chrome:profiles\",\"ran\":true}]},\"hash\":{\"name\":\"hash\",\"status\":\"UNKNOWN\",\"checked\":0,\"skipped\":0,\"duration_s\":0.0,\"hits\":[],\"checks\":[{\"name\":\"file-fingerprints\",\"ran\":false,\"reason\":\"skipped by --quick\"}]}},\"worth_a_look\":[],\"doors\":[],\"verdict\":{\"status\":\"~a\",\"hits_total\":~d,\"findings_total\":~d,\"references_total\":0,\"withdrawn_total\":0,\"gaps\":[~{~a~^,~}]},\"exit_code\":~d}"
          (loop for (indicator family tier location) in hits
                collect (format nil "{\"indicator\":\"~a\",\"family\":\"~a\",\"tier\":\"~a\",\"confidence\":\"primary\",\"location\":\"~a\",\"detail\":\"\"}"
                                (zb-json-escape indicator) (zb-json-escape family)
                                (zb-json-escape tier) (zb-json-escape location)))
          status (length hits) (length hits)
          (loop for gap in gaps collect (format nil "\"~a\"" (zb-json-escape gap)))
          (cond ((plusp (length hits)) 2) ((plusp (length gaps)) 3) (t 0))))

(defun zb-summary-text (&key status hits gaps)
  "The sharing summary the stubbed engine writes, as JSON text."
  (format nil "{\"schema_version\":\"1.6\",\"report_type\":\"sharing-summary\",\"tool\":{\"name\":\"zero-breach-toolkit\",\"version\":\"3.0.2\"},\"verdict\":{\"status\":\"~a\",\"hits_total\":~d,\"findings_total\":~d,\"references_total\":0,\"withdrawn_total\":0},\"exit_code\":~d,\"scope\":{\"scan_type\":\"quick\"},\"coverage\":{\"checked_count\":75,\"gap_count\":~d},\"must_relay\":[{\"id\":\"not-a-clean-bill\",\"text\":\"No known indicators were found in the places that can be checked. This is not a clean bill of health.\"},{\"id\":\"gaps\",\"text\":\"Some checks could not run (listed under not_checked). Nothing was cleared in those places.\"}],\"next_steps\":[{\"id\":\"close-gaps\",\"text\":\"Some checks could not run. Where the report explains how, close the gap and run again.\"}],\"worth_a_look_count\":0,\"doors_open_count\":0}"
          status (length hits) (length hits)
          (cond ((plusp (length hits)) 2) ((plusp (length gaps)) 3) (t 0))
          (length gaps)))

(defun zb-argument (argv flag)
  "The word after FLAG in ARGV, or NIL."
  (let ((tail (member flag argv :test #'string=)))
    (second tail)))

(defun zb-fake-run (argv)
  "The stubbed engine: record ARGV, write the file the cell asked for, answer its exit code.
The --describe run answers *ZB-DESCRIBE* instead of writing a report."
  (push argv *zb-calls*)
  (cond
    ((member "--describe" argv :test #'string=)
     (values (or *zb-describe* (zb-describe-text)) "" 0))
    (t
     (let ((full (zb-argument argv "--json"))
           (summary (zb-argument argv "--share-json")))
       (when full
         (with-open-file (out full :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
           (write-string (zb-report-text :status *zb-status* :hits *zb-hits*
                                          :gaps *zb-gaps*)
                         out)))
       (when summary
         (with-open-file (out summary :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
           (write-string (zb-summary-text :status *zb-status* :hits *zb-hits*
                                           :gaps *zb-gaps*)
                         out)))
       (values "" "" (cond ((plusp (length *zb-hits*)) 2)
                           ((plusp (length *zb-gaps*)) 3)
                           (t 0)))))))

(defun zb-describe-text ()
  "A --describe document with two checks, for the describe tests."
  "{\"schema_version\":\"1.6\",\"tool\":{\"name\":\"zero-breach-toolkit\",\"version\":\"3.0.2\",\"summary\":\"Local, read-only check for traces of known commercial spyware.\"},\"exit_codes\":{\"0\":\"every applicable check ran\",\"2\":\"indicator matches were found\",\"3\":\"checks were incomplete\"},\"modules\":{\"browser\":{\"platforms\":[\"all\"],\"reads\":\"browser profiles, cookies, caches and stored data\",\"blind_spots\":[\"a profile this account cannot read\"]},\"phones-backup\":{\"platforms\":[\"all\"],\"reads\":\"an iPhone backup or an Android collection\",\"blind_spots\":[]}},\"instructions_for_assistants\":[\"Relay must_relay verbatim.\",\"Never call a gap clean.\"]}")
