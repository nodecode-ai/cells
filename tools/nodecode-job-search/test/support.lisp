;;;; support.lisp --- Job Search test runner, stubs and sample answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a JOB-SEARCH-CELL-
;;;; name prefix; RUN-JOB-SEARCH-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the agent-data program, so that
;;;; is what the tests stub, two ways: a special variable they bind to a Lisp
;;;; function (WITH-RUN-STUB: fast, and it records every argv), and a real
;;;; /bin/sh script (AGENT-DATA-RECORDING-PROGRAM), for the test that proves
;;;; the program is really run and its exit code really read. No test needs
;;;; agent-data installed and none touches the network.

(in-package #:nodecode.test)

(define-test-slice "job-search" "JOB-SEARCH-CELL-" :start nodecode-job-search:start-cell
  :defaults (("command" (agent-data-stub-program))))

(defvar *calls* '()
  "The ARGV of every run this test stubbed, newest first.")

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun executable-script (path &rest lines)
  "Write LINES to PATH, one per line, make it executable, and answer its
native namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line (remove nil lines)) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path))
                     :output nil :error-output nil)
  (uiop:native-namestring path))

(defun agent-data-stub-program ()
  "A program the cell accepts as agent-data: it answers one word, so a start
finds something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (executable-script (merge-pathnames "agent-data-stub/agent-data"
                                                (uiop:temporary-directory))
                               "cat >/dev/null"
                               "printf '%s' 'stub'"))))

(defun agent-data-recording-program (directory &key (output "{}") (status 0) (error ""))
  "An agent-data that records what it was run with -- DIRECTORY/argv holds
one argument per line -- prints OUTPUT, writes ERROR to stderr and exits
STATUS."
  (executable-script
   (merge-pathnames "agent-data" directory)
   (format nil "printf '%s\\n' \"$@\" > ~s" (namestring (merge-pathnames "argv" directory)))
   (format nil "printf '%s' ~s" output)
   (unless (zerop (length error))
     (format nil "printf '%s' ~s >&2" error))
   (format nil "exit ~d" status)))

(defmacro with-run-stub ((&key (search "{}") (posting "{}") (whoami "{}") (code 0)) &body body)
  "BODY with agent-data stubbed by route: SEARCH answers a search, POSTING a
posting read, WHOAMI anything else, all at exit CODE. Every run pushes its
ARGV on *CALLS*."
  `(let ((*calls* '())
         (nodecode-job-search::*run*
           (lambda (argv)
             (push argv *calls*)
             (cond ((member "--keywords" argv :test #'equal) (values ,search ,code))
                   ((member "--posting_id" argv :test #'equal) (values ,posting ,code))
                   (t (values ,whoami ,code))))))
     ,@body))

(defun last-call ()
  "The ARGV of the newest stubbed run."
  (first *calls*))

(defun refusal (thunk)
  "What THUNK refuses with, as text, or its own answer when it did not."
  (handler-case (funcall thunk)
    (nodecode-job-search:job-search-error (condition) (princ-to-string condition))))

;;; --- the answers agent-data really gives --------------------------------------

(defparameter +search-json+
  (concatenate 'string
               "{\"data\":{\"status\":\"completed\","
               "\"query\":{\"keywords\":\"senior designer\",\"location\":\"United States\",\"source\":\"ashby\"},"
               "\"results\":["
               "{\"id\":\"jp_22d0d871db24\",\"title\":\"Senior Product Designer\","
               "\"company_name\":\"Tidewater Health\",\"location_display\":\"Remote (US)\","
               "\"salary_display\":\"$180K - $210K\",\"source\":\"ashby\","
               "\"source_url\":\"https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24\","
               "\"source_id\":\"ashby-0001\",\"posted_at\":null,"
               "\"published_at\":\"2026-08-06T00:45:57\",\"employment_type\":\"FullTime\","
               "\"missing_fields\":[]},"
               "{\"id\":\"jp_0099aa11bb22\",\"title\":\"Staff Designer\","
               "\"company_name\":\"Northwind\",\"location_display\":\"San Francisco\","
               "\"salary_display\":null,\"source\":\"ashby\","
               "\"source_url\":\"https://jobs.ashbyhq.com/northwind/jp_0099aa11bb22\","
               "\"posted_at\":\"2026-08-01T00:00:00+00:00\",\"published_at\":null,"
               "\"employment_type\":\"FullTime\",\"missing_fields\":[]}],"
               "\"pagination\":{\"has_more\":true,\"next_cursor\":\"cur_2\"}},"
               "\"meta\":{\"request_id\":\"req_test\"}}")
  "A search answer: two rows, one with no salary, and a next page.")

(defparameter +empty-search-json+
  "{\"data\":{\"query\":{\"source\":\"linkedin\"},\"results\":[]},\"meta\":{\"request_id\":\"req_none\"}}"
  "A search that found nothing: an empty page, not an error.")

(defparameter +posting-json+
  (concatenate 'string
               "{\"data\":{\"id\":\"jp_22d0d871db24\",\"title\":\"Senior Product Designer\","
               "\"company_name\":\"Tidewater Health\",\"location_display\":\"Remote (US)\","
               "\"posted_at\":\"2026-08-06T00:45:57\",\"employment_type\":null,"
               "\"source\":\"ashby\","
               "\"source_url\":\"https://jobs.ashbyhq.com/tidewater/jp_22d0d871db24\","
               "\"salary_display\":\"$180K - $210K\","
               "\"missing_fields\":[\"employment_type\",\"workplace_type\"],"
               "\"description_markdown\":\"Owns a care-navigation area end to end. Remote within the US.\"},"
               "\"meta\":{\"mode\":\"live_detail\",\"request_id\":\"req_detail\"}}")
  "A posting read: a description, and two fields the page did not yield.")

(defparameter +whoami-key+
  "{\"api_key_set\":true,\"listing\":\"f9a6ec16\",\"version\":\"1.0\"}"
  "agent-data whoami with a key saved.")

(defparameter +whoami-nokey+
  "{\"api_key_set\":false,\"version\":\"1.0\"}"
  "agent-data whoami with no key saved.")

(defparameter +refused-json+
  "{\"error\":{\"code\":\"insufficient_credits\",\"message\":\"The free tier is spent\",\"source\":\"service\",\"retryable\":false},\"meta\":{}}"
  "A metered call the service refused for want of credits.")
