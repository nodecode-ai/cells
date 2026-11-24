;;;; cell-test.lisp --- the section, the two tools, the command, and the blob.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every child process is stubbed: RUN-POWERSHELL answers a canned listing
;;;; or event list, so nothing here needs Windows, PowerShell or the network.
;;;; What is proved is the cell's own wiring -- the section and its refusal,
;;;; the tools the model calls and the text they answer, the filters, the
;;;; failure-action blob decoded out of the numbers a registry hands over --
;;;; and /windows-services saying what is failing on this machine.

(in-package #:nodecode.test)

(define-test-slice "windows-services" "WINDOWS-SERVICES-CELL-"
  :start nodecode-windows-services:start-cell)

;;; --- the canned service manager -------------------------------------------

(defvar *answer* nil
  "What the stubbed PowerShell answers: a function of SCRIPT and ENVIRONMENT
returning the text the child would have written.")

(defun dword (n)
  "N as the four little-endian bytes a registry value carries."
  (list (logand n #xff) (logand (ash n -8) #xff) (logand (ash n -16) #xff)
        (logand (ash n -24) #xff)))

(defun actions-bytes (&key (reset 0) (count 0) (actions '()))
  "A FailureActions blob -- reset period, two string offsets, the count, the
offset of the array, then (type, delay-ms) pairs -- as ConvertTo-Json writes
one: an array of numbers."
  (append (dword reset) (dword 0) (dword 0) (dword count)
          (dword (if actions 20 0))
          (loop for (type . delay) in actions
                append (append (dword type) (dword delay)))))

(defun service-json (&key name display (state "Stopped") (start "Auto") delayed
                         (pid 0) (exit-code 0) (account "LocalSystem") binary
                         (description "") depends failure-actions)
  "One service as the listing script writes it."
  (format nil (concatenate 'string
                           "{\"name\": ~s, \"display\": ~s, \"state\": ~s, \"start\": ~s, "
                           "\"delayed\": ~a, \"pid\": ~d, \"exit_code\": ~d, \"account\": ~s, "
                           "\"binary\": ~s, \"description\": ~s, \"depends_on\": [~{~s~^, ~}], "
                           "\"failure_actions\": [~{~d~^, ~}]}")
          name display state start (if delayed "true" "false") pid exit-code
          account (or binary "") description depends failure-actions))

(defparameter +listing+
  (format nil "[~{~a~^,~}]"
          (list (service-json :name "wuauserv" :display "Windows Update" :state "Running"
                              :start "Auto" :delayed t :pid 1234 :account "LocalSystem"
                              :binary "C:\\Windows\\system32\\svchost.exe -k netsvcs"
                              :description "Enables the detection of updates"
                              :depends '("rpcss"))
                (service-json :name "FooSvc" :display "Foo Helper" :start "Auto"
                              :exit-code 1053 :account "LocalService"
                              :binary "\"C:\\Program Files\\Foo\\foo.exe\" --serve"
                              :depends '("wuauserv")
                              :failure-actions (actions-bytes
                                                 :reset 86400 :count 2
                                                 :actions '((1 . 60000) (1 . 120000))))
                (service-json :name "BarWatch" :display "Bar Watch" :start "Manual"
                              :exit-code 1077 :binary "C:\\Nowhere\\bar.exe")))
  "Three services: one running and delayed, one that timed out and carries a
failure policy, one that has simply never started.")

(defparameter +events+
  "[{\"time\": \"2026-02-03T04:05:06.0000000Z\", \"id\": 7031, \"what\": \"The Foo Helper service terminated unexpectedly.\"}]"
  "The Service Control Manager's record for the failed service.")

(defun answer-from-listing (script environment)
  (declare (ignore environment))
  (if (search "Get-WinEvent" script) +events+ +listing+))

(defmacro with-service-manager (&body body)
  "BODY with the PowerShell seam stubbed: no child process runs, and *ANSWER*
says what one would have written."
  `(let ((*answer* (function answer-from-listing)))
     (with-stubbed-fdefinition (nodecode-windows-services::run-powershell (script &key environment)
                                (funcall *answer* script environment))
       ,@body)))

(defun windows-services-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "windows-services-call" name arguments)))

(defmacro with-running-services (&body body)
  "BODY with the cell started on a named PowerShell, so no PATH is probed."
  `(with-service-manager
     (with-cell-stop ((windows-services-start "powershell" "powershell.exe"))
       ,@body)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "windows-services"
  (:fixture with-service-manager)
  (:config "powershell" "powershell.exe")
  (:help :windows-services)
  (:command "windows-services")
  (:running
   (is (nle::find-tool "win_service_table") "the listing tool is registered")
   (is (nle::find-tool "win_service_detail") "and the detail tool")
   (is (= 90 (nodecode-windows-services::setting :timeout-seconds)) "the query ceiling defaults to 90 s")
   (is (= 100 (nodecode-windows-services::setting :max-rows)) "a listing lays out 100 rows by default"))
  (:stopped
   (is (null (nle::find-tool "win_service_table")) "and stop takes both tools back"))
  (:refused ("timeout_seconds" 0)
            ("max_rows" 0))
  (:idle nodecode-windows-services:windows-services-error
         (nodecode-windows-services:table)
         (nodecode-windows-services:detail "wuauserv")))

(deftest windows-services-cell-a-machine-with-no-powershell-refuses (with-service-manager)
  (with-stubbed-fdefinition (nodecode-windows-services::find-powershell (&optional configured)
                              (declare (ignore configured))
                              nil)
    (let ((text (refusal-text nlk:config-refusal (windows-services-start))))
      (is-carrying text ("no PowerShell found" "it names what is missing")
                     ("powershell" "and the setting that fixes it"))
      (is (null (nle::find-tool "win_service_table")) "nothing was installed"))))

;;; --- the listing -----------------------------------------------------------------

(deftest windows-services-cell-the-table-lists-state-mode-and-edges ()
  (with-running-services
    (let ((text (windows-services-call "win_service_table" "{}")))
      (is-carrying text
                   ("3 of 3 matching service(s) listed, 3 registered, 1 running" "the counts come first")
                   ("auto, delayed" "the delayed flag is folded into the start mode")
                   ("waits for rpcss" "a dependency edge")
                   ("waited on by FooSvc" "and the reverse one")
                   ("exit 1053 (0x0000041D) - it did not answer the start request in time"
                    "the failure is in words")
                   ("C:\\Windows\\system32\\svchost.exe -k netsvcs" "the binary is shown"))
      (is (not (search "exit 1077" text)) "1077 is an untouched service, not a failure"))))

(deftest windows-services-cell-the-filters-narrow-the-listing ()
  (with-running-services
    (let ((stopped (windows-services-call "win_service_table"
                                          "{\"state\": \"stopped\", \"nonzero_exit\": true}")))
      (is-carrying stopped ("FooSvc" "the one that timed out")
                            ("1 of 1 matching" "and only it"))
      (is (not (search "BarWatch" stopped)) "1077 keeps the never-started one out"))
    (let ((one (windows-services-call "win_service_table" "{\"limit\": 1, \"query\": \"s\"}")))
      (is-carrying one ("stopped at limit=1" "a short page says so")))
    (let ((none (windows-services-call "win_service_table" "{\"query\": \"nothing-here\"}")))
      (is-carrying none ("nothing matched" "an empty answer explains itself")))))

;;; --- one service in full ----------------------------------------------------------

(deftest windows-services-cell-the-detail-decodes-the-failure-policy ()
  (with-running-services
    (let ((text (windows-services-call "win_service_detail" "{\"name\": \"FooSvc\"}")))
      (is-carrying text
                   ("FooSvc - Foo Helper" "the service, by its display name")
                   ("state: stopped   start: auto" "its state and start mode")
                   ("restart the service after 60 s" "the first failure action, decoded")
                   ("restart the service after 120 s" "and the second")
                   ("the count resets after 24 h" "with when Windows forgets the count")
                   ("the registered binary is not on disk any more" "the missing binary")
                   ("7031 ended unexpectedly" "and the SCM's own event, in words")
                   ("The Foo Helper service terminated unexpectedly." "with its message"))
      (is (search "C:\\Program Files\\Foo\\foo.exe" text) "the quoted path is read out of the command line"))))

(deftest windows-services-cell-a-service-with-no-policy-says-so ()
  (with-running-services
    (let ((text (windows-services-call "win_service_detail" "{\"name\": \"BarWatch\"}")))
      (is-carrying text
                   ("no failure action is configured" "nothing would restart it")
                   ("exit code: 1077 (0x00000435) - it has not tried to start since the last boot"
                    "and its code is explained")))))

(deftest windows-services-cell-an-unknown-name-answers-with-the-nearest ()
  (with-running-services
    (let ((text (windows-services-call "win_service_detail" "{\"name\": \"Foo\"}")))
      (is-carrying text ("no service named Foo is registered" "it says what is missing")
                        ("closest: FooSvc" "and offers the nearest name")))))

;;; --- the command ------------------------------------------------------------------

(deftest windows-services-cell-the-command-says-what-is-failing ()
  (with-running-services
    (let ((summary (nodecode-windows-services::run-command "" nil)))
      (is-carrying summary
                   ("3 services registered, 1 running, 1 in a failed state" "the counts")
                   ("FooSvc" "the failing service")
                   ("exit 1053: it did not answer the start request in time" "and why")))
    (let ((one (nodecode-windows-services::run-command "BarWatch" nil)))
      (is-carrying one ("BarWatch - Bar Watch" "a name answers that service in full")))))

;;; --- the blob, on its own -----------------------------------------------------

(deftest windows-services-cell-the-blob-decodes-without-windows ()
  (let* ((bytes (coerce (actions-bytes :reset 3600 :count 2
                                       :actions '((1 . 60000) (2 . 0)))
                         'vector))
         (policy (nodecode-windows-services::decode-failure-actions bytes)))
    (is (= 3600 (getf policy :reset-seconds)) "the reset period comes first")
    (is (= 2 (getf policy :count)) "then the count")
    (is (equal '("restart the service" "restart the computer")
               (mapcar (function car) (getf policy :actions))) "each action in words")
    (is (= 60 (cdr (first (getf policy :actions)))) "and its delay in seconds")
    (let ((said (nodecode-windows-services::policy-text policy)))
      (is (search "the count resets after 1 h" said) "the sentence says when the count resets")))
  (is (null (nodecode-windows-services::decode-failure-actions #())) "a blob too short to be one decodes to nothing"))
