;;;; cell-test.lisp --- the cell against a stubbed wevtutil.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs Windows, a log or a real wevtutil: every read goes
;;;; through NODECODE-WINDOWS-EVENTLOG::*RUN*, a stub that records the command
;;;; line it was handed and answers the XML the test chose. What is under test
;;;; is what this cell owns: the XPath it builds from a declared call, the
;;;; window it walks, the classification and decoding of a record, and the
;;;; JSON it answers. The tool is called the way a model's call reaches it,
;;;; through the real dispatch and its contract, so the schema the provider is
;;;; shown and the body that runs are one shape.

(in-package #:nodecode.test)

;;; --- the stub ------------------------------------------------------------------

(defvar *calls* nil
  "The runs the stub was handed, newest first, each (ARGV TIMEOUT).")

(defvar *log-xml* ""
  "What the stub answers on stdout: the rendered records, one after another.")

(defvar *log-status* 0
  "The exit status the stub answers.")

(defvar *log-errors* ""
  "What the stub writes on stderr.")

(defmacro with-log-fixture (&body body)
  "BODY with the cell started and every read answered by the stub."
  `(let ((*calls* nil)
         (*log-xml* "")
         (*log-status* 0)
         (*log-errors* "")
         (nodecode-windows-eventlog::*run*
           (lambda (argv &key timeout)
             (push (list argv timeout) *calls*)
             (values *log-xml* *log-errors* *log-status*)))
         (nodecode-windows-eventlog::*windows* t))
     (with-cell-stop ((windows-eventlog-start))
       ,@body)))

(defun evt-argv (&optional (newest 0))
  "The command line of the NEWEST'th read, 0 the last one."
  (first (nth newest *calls*)))

(defun evt-answer (tool &rest pairs)
  "TOOL's answer to PAIRS, as the model reads it."
  (nle::message-content
   (execute-wire-call "windows-eventlog-1" tool
                      (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))))

(defun evt-object (tool &rest pairs)
  (nlk:decode-json (apply #'evt-answer tool pairs)))

(defun evt-events (object)
  "The records OBJECT carries, as a list."
  (coerce (nlk:json-array object "events") 'list))

(defun evt-field (event name)
  (nlk:json-value event :string name))

;;; --- one rendered record, as Windows renders it --------------------------------

(defparameter +crash-data+
  '(("Application Name" . "hermes.exe")
    ("Application Version" . "0.22.2")
    ("Faulting Module Name" . "kernelbase.dll")
    ("Exception Code" . "0xc0000005")
    ("Fault Offset" . "0x1234"))
  "The names an Application Error record carries, as one build spells them.")

(defun event-xml (event-id &key (provider "Application Error") (level 2)
                              (minutes-ago 10) data (channel "Application")
                              (parameters nil))
  "One Event as wevtutil renders it, stamped MINUTES-AGO before now."
  (let* ((when (- (nodecode-windows-eventlog::now-seconds) (* 60 minutes-ago)))
         (stamp (nodecode-windows-eventlog::format-time when 0))
         (fields (with-output-to-string (out)
                   (loop for (name . value) in data
                         do (format out "<Data Name='~a'>~a</Data>" name value))
                   (loop for value in parameters
                         do (format out "<Data>~a</Data>" value)))))
    (format nil "<Event xmlns='http://schemas.microsoft.com/win/2004/08/events/event'><System><Provider Name='~a'/><EventID>~d</EventID><Level>~d</Level><TimeCreated SystemTime='~a'/><Execution ProcessID='4242' ThreadID='1'/><Channel>~a</Channel></System><EventData>~a</EventData></Event>"
            provider event-id level stamp channel fields)))

(defun evt-log (&rest records)
  "The stub's stdout: RECORDS, a newline between them, as wevtutil writes them."
  (format nil "~{~a~%~}" records))

;;; --- the slice, and the lifecycle every cell answers the same way ----------

(define-test-slice "windows-eventlog" "WINDOWS-EVENTLOG-CELL-"
  :start nodecode-windows-eventlog:start-cell)

(define-cell-lifecycle-tests "windows-eventlog"
  (:fixture with-log-fixture)
  (:help :windows-eventlog)
  (:command "windows-eventlog")
  (:running (is (nle::find-tool "win_crash_events") "the crash tool is registered")
            (is (nle::find-tool "win_shutdown_events") "and the shutdown one")
            (is (nle::find-tool "win_eventlog_query") "and the general query")
            (is (equal '("wevtutil") (nodecode-windows-eventlog::setting :wevtutil)) "the wevtutil Windows ships on PATH is the default")
            (is (= 5000 (nodecode-windows-eventlog::setting :scan-cap)) "and the scan cap the original used"))
  (:stopped (is (null (nle::find-tool "win_crash_events")) "stop unregisters every tool")
            (is (null (nle::find-tool "win_eventlog_query"))))
  (:refused ("wevtutil" (vector)) ("scan_cap" 0) ("timeout_seconds" 0)))

(deftest windows-eventlog-cell-a-section-without-a-program-refuses ()
  (with-log-fixture
    (is (search "wevtutil"
                 (refusal-text nlk:config-refusal
                               (windows-eventlog-start "wevtutil" (vector)))) "the refusal names the setting that has to be filled")
    (is (null (nle::find-tool "win_crash_events")) "a refused start installed nothing")))

;;; --- the crash tool ------------------------------------------------------------

(deftest windows-eventlog-cell-crash-records-and-groups ()
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 1000 :data +crash-data+ :minutes-ago 5)
               (event-xml 1000 :data +crash-data+ :minutes-ago 30)
               (event-xml 1001 :provider "Windows Error Reporting" :level 4
                          :minutes-ago 60
                          :data '(("P1" . "ego.exe") ("EventName" . "APPCRASH")))))
    (let* ((object (evt-object "win_crash_events" "hours" 24))
           (events (evt-events object)))
      (is (eq t (nlk:json-value object :boolean "ok")) (format nil "the answer is ok: ~a" (or (evt-field object "error") "")))
      (is (= 3 (nlk:json-value object :integer "matched")) "three crash records")
      (is (search "EventID=1000 or EventID=1001" (fourth (evt-argv))) "the XPath carries the crash ids")
      (is (equal "Application" (third (evt-argv))) "and the Application log")
      (is (equal "application_error" (evt-field (first events) "kind")) "classified by id and provider")
      (is (equal "hermes.exe" (evt-field (first events) "app")) "the crashing image is named")
      (is (equal "kernelbase.dll" (evt-field (first events) "module")) "and the faulting module")
      (is (equal "0xC0000005 (ACCESS_VIOLATION)" (evt-field (first events) "exception")) "and the exception code decoded to its debugger name")
      (is (equal "wer" (evt-field (third events) "kind")) "the WER record keeps its own kind")
      (let ((groups (coerce (nlk:json-array object "groups") 'list)))
        (is (= 2 (length groups)) "the repeats collapse into one group")
        (is (= 2 (nlk:json-value (first groups) :integer "count")) "two of the same signature")
        (is (equal "hermes.exe" (evt-field (first groups) "app")))
        (is (search "0xC0000005" (evt-field (first groups) "signature")) "and the signature is the exception it repeats")))))

(deftest windows-eventlog-cell-crash-window-and-process ()
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 1000 :data +crash-data+ :minutes-ago 10)
               (event-xml 1000 :data +crash-data+ :minutes-ago (* 60 48))
               (event-xml 1000 :data +crash-data+ :minutes-ago 20)))
    (let ((object (evt-object "win_crash_events" "hours" 24)))
      (is (= 1 (nlk:json-value object :integer "matched")) "a record past the window start is not in the window")
      (is (= 2 (nlk:json-value object :integer "scanned")) "and the walk stopped where the window ended")))
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 1000 :data +crash-data+)
               (event-xml 1000 :data (cons '("Application Name" . "codex.exe")
                                           (remove "Application Name" +crash-data+
                                                   :key #'car :test #'equal)))))
    (let* ((object (evt-object "win_crash_events" "process" "CODEX"))
           (events (evt-events object)))
      (is (= 1 (nlk:json-value object :integer "matched")) "the process filter is case-blind")
      (is (equal "codex.exe" (evt-field (first events) "app"))))))

(deftest windows-eventlog-cell-crash-kinds-and-dotnet-text ()
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 1000 :data +crash-data+)
               (event-xml 1002 :provider "Application Hang" :data +crash-data+)))
    (let* ((object (evt-object "win_crash_events" "kinds" (vector "application_hang")))
           (events (evt-events object)))
      (is (= 1 (nlk:json-value object :integer "matched")) "the kind filter keeps one")
      (is (equal "application_hang" (evt-field (first events) "kind")))))
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 1026 :provider ".NET Runtime"
                          :data '(("Application" . "hermes.exe")
                                  ("Exception Info" . "System.InvalidOperationException: boom")))))
    (let* ((object (evt-object "win_crash_events"))
           (events (evt-events object)))
      (is (equal "dotnet" (evt-field (first events) "kind")) "a .NET record is its own kind")
      (is (search "InvalidOperationException" (evt-field (first events) "exception_text")) "and keeps the exception text")))
  (with-log-fixture
    ;; The ids 1000 and 1001 are reused by application-specific providers, so
    ;; a record outside the crash family is dropped rather than reported.
    (setf *log-xml*
           (evt-log (event-xml 1000 :provider "MyApp Telemetry" :level 4
                          :data '(("P1" . "started")))))
    (let ((object (evt-object "win_crash_events")))
      (is (zerop (nlk:json-value object :integer "matched")) "an unrelated 1000 is not a crash")
      (is (find-if (lambda (note) (search "no crash records" note))
                    (coerce (nlk:json-array object "notes") 'list)) "and the empty answer says why it can be empty"))))

;;; --- the shutdown tool ----------------------------------------------------------

(deftest windows-eventlog-cell-shutdown-decodes-bugchecks ()
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 41 :provider "Microsoft-Windows-Kernel-Power" :level 1
                          :minutes-ago 5 :channel "System"
                          :data '(("BugcheckCode" . "159") ("PowerButtonTimestamp" . "0")))
               (event-xml 6008 :provider "EventLog" :minutes-ago 6 :channel "System"
                          :data '(("param1" . "8:00:00") ("param2" . "2026-10-20")))
               (event-xml 1001 :provider "Microsoft-Windows-WER-SystemErrorReporting"
                          :minutes-ago 7 :channel "System"
                          :data '(("BugcheckCode" . "0x0000009f")))
               ;; a 1001 from a provider that is not a bugcheck report
               (event-xml 1001 :provider "User32" :level 4 :minutes-ago 8 :channel "System")
               (event-xml 1074 :provider "User32" :level 4 :minutes-ago 3 :channel "System"
                          :data '(("param1" . "C:\\Windows\\system32\\svchost.exe")
                                  ("param2" . "0x80010000")
                                  ("param5" . "Windows Update")))))
    (let* ((object (evt-object "win_shutdown_events" "hours" 24))
           (events (evt-events object)))
      (is (equal "System" (third (evt-argv))) "the System log is read")
      (is (= 4 (nlk:json-value object :integer "matched")) "four machine-level records: the unrelated 1001 is dropped")
      (is (equal "initiated_shutdown" (evt-field (first events) "kind")) "the newest record first: the operator restart")
      (is (search "svchost.exe" (evt-field (first events) "process")) "it names the process that asked for the restart")
      (is (equal "Windows Update" (evt-field (first events) "comment")))
      (is (equal "unexpected_restart" (evt-field (second events) "kind")))
      (is (equal "DRIVER_POWER_STATE_FAILURE" (evt-field (second events) "bugcheck")) "the decimal bugcheck code is decoded")
      (is (equal "unexpected_shutdown" (evt-field (third events) "kind")))
      (is (equal "bugcheck" (evt-field (fourth events) "kind")) "WER 1001 is a bugcheck")
      (is (equal "DRIVER_POWER_STATE_FAILURE" (evt-field (fourth events) "bugcheck")) "and its hex code decodes to the same name"))))

;;; --- the general query -----------------------------------------------------------

(deftest windows-eventlog-cell-query-filters-and-details ()
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 7031 :provider "Service Control Manager" :minutes-ago 1
                          :channel "System" :data '(("param1" . "HermesGateway")))
               (event-xml 7036 :provider "Service Control Manager" :level 4
                          :minutes-ago 2 :channel "System"
                          :data '(("param1" . "Spooler")))))
    (let* ((object (evt-object "win_eventlog_query" "channel" "System"
                               "event_ids" (vector 7031) "include_details" t))
           (events (evt-events object))
           (event (first events)))
      (is (= 1 (nlk:json-value object :integer "matched")) "one record matches the id")
      (is (= 7031 (nlk:json-value event :integer "event_id")))
      (is (equal "System" (nlk:json-value object :string "channel")))
      (is (search "EventID=7031" (fourth (evt-argv))) "the XPath carries the id")
      (is (equal "HermesGateway"
                 (nlk:json-value (nlk:json-value event :object "data") :string "param1")) "include_details carries the provider's own fields")))
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 7031 :provider "Service Control Manager" :minutes-ago 1
                          :channel "System" :data '(("param1" . "HermesGateway")))))
    (is (= 1 (nlk:json-value (evt-object "win_eventlog_query" "contains" "hermesgateway")
                              :integer "matched")) "the substring filter is case-blind")
    (is (zerop (nlk:json-value (evt-object "win_eventlog_query" "contains" "nothing exists")
                                :integer "matched")) "and a substring nobody carries matches nothing")))

(deftest windows-eventlog-cell-query-refuses-a-name-that-breaks-the-xpath ()
  (with-log-fixture
    (let ((object (evt-object "win_eventlog_query" "channel" "Application']")))
      (is (eq nil (nlk:json-value object :boolean "ok")) "the answer is not ok")
      (is (search "plain log name" (evt-field object "error")) "and says what a channel is")
      (is (null *calls*) "nothing was run"))
    (let ((object (evt-object "win_eventlog_query" "provider" "a'b")))
      (is (eq nil (nlk:json-value object :boolean "ok")) "a quote in a provider name is refused")
      (is (null *calls*) "and nothing was run"))))

(deftest windows-eventlog-cell-off-windows-and-a-failing-wevtutil ()
  (with-log-fixture
    (let ((nodecode-windows-eventlog::*windows* nil))
      (let ((object (evt-object "win_crash_events")))
        (is (eq nil (nlk:json-value object :boolean "ok")) "off Windows the answer is not ok")
        (is (search "only on Windows" (evt-field object "error")) "and says why")
        (is (null *calls*) "nothing was run"))))
  (with-log-fixture
    (setf *log-status* 5
          *log-errors* "The request is not supported.")
    (let ((object (evt-object "win_eventlog_query" "channel" "Security")))
      (is (eq nil (nlk:json-value object :boolean "ok")) "a channel that needs elevation is an answer, not an exception")
      (is (search "exited 5" (evt-field object "error")) "the exit code is kept")
      (is (search "not supported" (evt-field object "error")) "and wevtutil's own words"))))

;;; --- /windows-eventlog, and the reader under it ------------------------------------

(deftest windows-eventlog-cell-command-reads-one-channel ()
  (with-log-fixture
    (setf *log-xml*
           (evt-log (event-xml 7031 :provider "Service Control Manager" :minutes-ago 1
                          :channel "System" :data '(("param1" . "HermesGateway")))))
    (let ((text (nodecode-windows-eventlog::run-command "System" "windows-eventlog-1")))
      (is (search "windows-eventlog -- System" text) "the channel is named")
      (is (search "7031" text) "the record's id is on its line")
      (is (search "HermesGateway" text) "and what it says")
      (is (equal "System" (third (evt-argv))) "the channel asked for is the channel read"))
    (is (search "no channel name"
                (nodecode-windows-eventlog::run-command "Application']" "windows-eventlog-1")) "a name that would break the query is refused in words")
    (let ((nodecode-windows-eventlog::*windows* nil))
      (is (search "only on Windows"
                  (nodecode-windows-eventlog::run-command "" "windows-eventlog-1")) "and off Windows the command says so"))))

(deftest windows-eventlog-cell-reads-a-record-with-entities-and-positional-data ()
  (let* ((text (concatenate 'string
                             "<Event><System><Provider Name='H&amp;S'/>"
                             "<EventID>1000</EventID><Level>2</Level>"
                             "<TimeCreated SystemTime='2026-10-20T03:04:05.1234567Z'/>"
                             "<Channel>Application</Channel></System>"
                             "<EventData><Data>alpha</Data><Data>beta &lt;gamma&gt;</Data>"
                             "<Data Name='Named'>value</Data></EventData></Event>"))
         (roots (nodecode-windows-eventlog:parse-xml-elements text "Event"))
         (record (nodecode-windows-eventlog:event-record (first roots))))
    (is (= 1 (length roots)) "one document is one record")
    (is (equal "H&S" (nodecode-windows-eventlog:record-provider record)) "an entity in an attribute is resolved")
    (is (= 1000 (nodecode-windows-eventlog:record-event-id record)))
    (is (equal "error" (nodecode-windows-eventlog:record-level record)))
    (is (equal "2026-10-20T03:04:05.123456Z"
               (nodecode-windows-eventlog:record-iso-time record)) "the seven-digit fraction is trimmed to microseconds")
    (is (equal "beta <gamma>"
               (nodecode-windows-eventlog::field record "#1")) "a positional value keeps its index")
    (is (equal "value" (nodecode-windows-eventlog::field record "named")) "a named value is found case-blind")))
