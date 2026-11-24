;;;; cell.lisp --- the octoprint section, three printer tools, /octoprint, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's surfaces were three tools, a /print-job-watch
;;;; command, a pre_tool_call approval hook, a skill and a cron job. This cell
;;;; keeps the tools, the command and the guidance, folds the approval into the
;;;; tool call itself (a preview, then the same call with confirm), and
;;;; replaces the cron job with a worker that reports events as a standing
;;;; notice while the cell runs.

(in-package #:nodecode-octoprint)

(nlk:define-peripheral octoprint :not-running t)

(defparameter +json-timeout+ 10)
(defparameter +snapshot-timeout+ 15)
(defparameter +json-max-bytes+ 1000000)
(defparameter +still-keep+ 20)
(defparameter +control-reads+ 10
  "How many times a confirmed move re-reads the job before it stops waiting.")
(defparameter +control-pause+ 0.5
  "Seconds between those reads.")
(defparameter +failure-repeat-seconds+ 86400
  "A printer that cannot be reached is reported once a day after the first time.")

(defparameter +primer+
  "octoprint reads one OctoPrint printer through its REST API, on the
settings' octoprint.url and octoprint.api_key. Nothing moves the printer
except a confirmed call.

  octoprint_status [include_snapshot]
      job state, connection state, the filename, completion, the file
      position, OctoPrint's printTimeLeft estimate (which may be 0), and the
      hotend (tool0) and bed temperatures. A disconnected printer is named
      disconnected, not paused; Error and Offline after error are errors even
      when /api/printer answers 409. include_snapshot saves one webcam still
      inside the cell's own folder.
  octoprint_control action [confirm]
      action is pause, resume or cancel. Without confirm it answers what
      would be sent and the state now, and sends nothing; with confirm it
      reads the state, sends the move, and reads it again, so moved is a
      fact and not a promise. Pause is sent only from Printing or Printing
      from SD; resume only from Paused; a job in Pausing is left alone.
      Cancel ends the print and cannot be resumed. Any other action, G-code
      or temperature is refused.
  octoprint_watch
      compares this check with the stored one and reports
      printer_disconnected, error, complete, paused (only after a print was
      running), cancelled, and stalled (no file position change for
      octoprint.stall_minutes while printing). A printer that cannot be
      reached is reported once per cause, then once a day, and once when it
      answers again.
  /octoprint status|watch|snapshot
      the same readings for the operator; it never moves the printer.

With octoprint.watch_minutes above zero a background check runs on that
period and raises the same events as a standing notice. (help :octoprint)
answers this while the cell runs."
  "What (help :octoprint) answers while the cell runs.")

;;; --- one OctoPrint origin ---------------------------------------------------------

(define-condition printer-failure (error)
  ((code :initarg :code :reader failure-code)
   (text :initarg :text :reader failure-text)
   (next-step :initarg :next-step :initform "" :reader failure-next-step))
  (:report (lambda (condition stream)
             (format stream "~a: ~a" (failure-code condition) (failure-text condition)))))

(defun printer-fail (code text &optional (next-step ""))
  (error 'printer-failure :code code :text text :next-step next-step))

(defun failure-answer (failure)
  "FAILURE as the text a tool answers with: what failed, and what to do."
  (format nil "ERROR ~a: ~a~@[ Next: ~a~]"
          (failure-code failure) (failure-text failure)
          (let ((step (failure-next-step failure)))
            (and (plusp (length step)) step))))

(defun origin-base ()
  "octoprint.url as scheme://host[:port]; signals for anything but one origin."
  (let ((raw (nlk:trimmed (or (setting :url) ""))))
    (when (zerop (length raw))
      (printer-fail "missing_url" "octoprint.url is not set."
                    "Point it at one OctoPrint origin, such as http://printer.local:5000."))
    (when (find-if (lambda (character)
                     (member character '(#\Space #\Tab #\Newline #\Return #\\)))
                   raw)
      (printer-fail "bad_url" "octoprint.url holds a space, a tab or a backslash."
                    "Use scheme://host:port with no path, query, user or password."))
    (let* ((uri (quri:uri raw))
           (scheme (string-downcase (or (quri:uri-scheme uri) "")))
           (host (or (quri:uri-host uri) ""))
           (port (quri:uri-port uri))
           (path (quri:uri-path uri))
           (default (if (string= scheme "https") 443 80)))
      (unless (member scheme '("http" "https") :test #'string=)
        (printer-fail "bad_url" "octoprint.url must start with http:// or https://."
                      "Example: http://printer.local:5000"))
      (when (zerop (length host))
        (printer-fail "bad_url" "octoprint.url has no host."
                      "Example: http://printer.local:5000"))
      (when (quri:uri-userinfo uri)
        (printer-fail "bad_url" "octoprint.url carries a user or password."
                      "Put the key in octoprint.api_key; it is sent only as X-Api-Key."))
      (when (or (and path (plusp (length path)) (not (string= path "/")))
                (quri:uri-query uri) (quri:uri-fragment uri))
        (printer-fail "bad_url" "octoprint.url has a path, query or fragment."
                      "Use only scheme://host:port; the cell adds OctoPrint's paths itself."))
      (format nil "~a://~a~@[:~d~]" scheme (string-downcase host)
              (and port (/= port default) port)))))

(defun api-headers ()
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (printer-fail "no_key" "octoprint.api_key is empty, so nothing was requested."
                    "Set the API key. With access control off OctoPrint treats a missing key as an admin, so this cell refuses to call it."))
    (list (cons "Accept" "application/json") (cons "X-Api-Key" key))))

(defun same-host-p (url base)
  "Whether URL is on BASE's host, whatever the port or scheme: where a still may come from."
  (handler-case
      (let ((one (quri:uri url)) (two (quri:uri base)))
        (and (member (string-downcase (or (quri:uri-scheme one) "")) '("http" "https") :test #'string=)
             (equal (string-downcase (or (quri:uri-host one) ""))
                    (string-downcase (or (quri:uri-host two) "")))))
    (error () nil)))

(defun same-origin-p (url base)
  "Whether URL is under BASE's scheme, host and port: where the key may go."
  (handler-case
      (let* ((one (quri:uri url)) (two (quri:uri base)))
        (and (equal (quri:uri-scheme one) (quri:uri-scheme two))
             (equal (string-downcase (or (quri:uri-host one) ""))
                    (string-downcase (or (quri:uri-host two) "")))
             (eql (quri:uri-port one) (quri:uri-port two))))
    (error () nil)))

(defun api-request (method path &key body (timeout +json-timeout+) (max-bytes +json-max-bytes+))
  "One OctoPrint exchange, bounded; => (values BODY STATUS HEADERS TRUNCATED)."
  (let* ((base (origin-base))
         (url (concatenate 'string base path)))
    (handler-case
        (nlk:http method url
                  :headers (append (api-headers)
                                   (and body (list (cons "Content-Type" "application/json"))))
                  :content body
                  :timeout timeout :max-bytes max-bytes)
      (printer-failure (condition) (error condition))
      (error (condition)
        (printer-fail "network"
                      (format nil "Could not reach OctoPrint (~a)."
                              (nlk:one-line (princ-to-string condition) :cap 200))
                      "Check that octoprint.url is reachable from this machine and that OctoPrint runs.")))))

(defun http-error-text (parsed)
  (let ((text (jstr (jget parsed "error"))))
    (and text (plusp (length text)) (subseq text 0 (min (length text) 300)))))

(defun api-json (method path &key body (timeout +json-timeout+) tolerate)
  "OctoPrint's decoded JSON; signals PRINTER-FAILURE for a status not in TOLERATE."
  (multiple-value-bind (raw status headers truncated)
      (api-request method path :body body :timeout timeout)
    (declare (ignore headers))
    (when truncated
      (printer-fail "too_large"
                    (format nil "OctoPrint's answer was over ~d bytes and was discarded." +json-max-bytes+)
                    "Nothing was kept."))
    (let ((parsed (handler-case (nlk:decode-json (or raw "")) (error () nil))))
      (cond
        ((member status tolerate) parsed)
        ((member status '(401 403))
         (printer-fail "unauthorized"
                       (format nil "OctoPrint refused the API key (HTTP ~d)." status)
                       "Check octoprint.api_key, a user key rather than the global one from the settings page."))
        ((not (hash-table-p parsed))
         (printer-fail "bad_body"
                       (format nil "OctoPrint answered HTTP ~d with a body that is not a JSON object." status)
                       "Check that octoprint.url points at OctoPrint, not another web server."))
        ((or (< status 200) (>= status 300))
         (printer-fail "octoprint"
                       (format nil "OctoPrint answered HTTP ~d~@[ ~a~]." status (http-error-text parsed))
                       "A failed request does not move the printer."))
        (t parsed)))))

;;; --- the printer's state -----------------------------------------------------------

(defun temperatures-of (raw)
  "RAW (OctoPrint's temperature member) as tool0 and bed, actual and target."
  (when (hash-table-p raw)
    (let ((picked (make-hash-table :test #'equal)))
      (dolist (name '("tool0" "bed"))
        (let ((item (jget raw name)))
          (when (hash-table-p item)
            (setf (gethash name picked)
                  (json-of "actual" (or (jnum (jget item "actual")) :absent)
                           "target" (or (jnum (jget item "target")) :absent))))))
      (when (plusp (hash-table-count picked)) picked))))

(defun heater-line (name temperatures)
  (let ((item (and temperatures (gethash name temperatures))))
    (if (null item)
        "unknown"
        (format nil "~@[~,1F~]/~@[~,1F~] C"
                (jnum (gethash "actual" item)) (jnum (gethash "target" item))))))

(defun temperatures-text (temperatures)
  (format nil "Hotend ~a, bed ~a."
          (heater-line "tool0" temperatures) (heater-line "bed" temperatures)))

(defun read-sample ()
  "One reading of the printer, as the plist the watch compares; signals PRINTER-FAILURE."
  (let* ((connection (api-json :get "/api/connection"))
         (job (api-json :get "/api/job"))
         (printer (api-json :get "/api/printer" :tolerate '(409)))
         (current (jget connection "current"))
         (progress (jget job "progress"))
         (info (jget job "job")))
    (unless (hash-table-p current)
      (printer-fail "bad_body" "OctoPrint's connection JSON had no current state." "Nothing was saved."))
    (unless (and (hash-table-p job) (stringp (jget job "state"))
                 (hash-table-p progress) (hash-table-p info))
      (printer-fail "bad_body" "OctoPrint's job JSON had no state, progress and job." "Nothing was saved."))
    (let* ((job-state (or (jstr (jget job "state")) ""))
           (connection-state (or (jstr (jget current "state")) ""))
           (sample (list :job-state job-state
                         :connection-state connection-state
                         :filename (jstr (jget (jget info "file") "name"))
                         :completion (jnum (jget progress "completion"))
                         :filepos (jnum (jget progress "filepos"))
                         :print-time-left (jnum (jget progress "printTimeLeft"))
                         :error-text (or (jstr (jget job "error")) "")
                         :temperatures (temperatures-of (jget printer "temperature")))))
      (setf (getf sample :kind) (kind-of job-state connection-state))
      sample)))

(defun status-message (sample)
  (let ((kind (getf sample :kind)))
    (cond
      ((string= kind "disconnected")
       (format nil "The printer is disconnected (connection ~a, job ~a). This is not a pause. Temperatures were not read."
               (getf sample :connection-state) (getf sample :job-state)))
      ((string= kind "error")
       (format nil "The printer is in error (~a: ~a). This is not a disconnect and not a pause."
               (getf sample :job-state)
               (let ((detail (getf sample :error-text)))
                 (if (plusp (length detail)) detail "no error text"))))
      (t
       (format nil "Job state ~a, connection ~a, completion ~@[~,1F%~], printTimeLeft ~a seconds. printTimeLeft is OctoPrint's estimate, including 0. A state string is not proof the toolhead moved."
               (getf sample :job-state) (getf sample :connection-state)
               (getf sample :completion)
               (let ((left (getf sample :print-time-left)))
                 (if left (round left) "unknown")))))))

;;; --- one webcam still, inside the cell's own folder --------------------------------

(defun still-directory ()
  (uiop:ensure-directory-pathname (merge-pathnames "octoprint/stills/" (nlk:home))))

(defun still-name (extension)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "still-~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ.~a"
            year month day hour minute second extension)))

(defun image-kind (octets)
  (cond ((and (>= (length octets) 3)
              (= (aref octets 0) #xff) (= (aref octets 1) #xd8) (= (aref octets 2) #xff))
         (values "jpg" "image/jpeg"))
        ((and (>= (length octets) 8) (equalp (subseq octets 0 8) #(137 80 78 71 13 10 26 10)))
         (values "png" "image/png"))
        (t nil)))

(defun prune-stills (directory)
  (let ((files (sort (remove-if-not (lambda (path)
                                      (member (string-downcase (or (pathname-type path) ""))
                                              '("jpg" "png") :test #'string=))
                                    (uiop:directory-files directory))
                     #'string< :key #'namestring)))
    (loop for old in (butlast files (min +still-keep+ (length files)))
          do (ignore-errors (delete-file old)))))

(defun save-still ()
  "One webcam still into the cell's folder; => a JSON object saying what happened."
  (let ((raw-url (nlk:trimmed (or (setting :snapshot-url) ""))))
    (if (zerop (length raw-url))
        (json-of "ok" nil "message" "octoprint.snapshot_url is not set, so no still was downloaded.")
        (handler-case
            (let* ((base (origin-base))
                   (url (if (char= (char raw-url 0) #\/)
                            (concatenate 'string base raw-url)
                            raw-url)))
              (unless (same-host-p url base)
                (printer-fail "snapshot_host" "The snapshot URL is not on the printer's host, so it was not downloaded."
                              "Set octoprint.snapshot_url to an http(s) URL on that host. Another port is allowed."))
              (multiple-value-bind (raw status headers truncated)
                  (nlk:http :get url
                            :headers (if (same-origin-p url base)
                                         (api-headers)
                                         '(("Accept" . "*/*")))
                            :timeout +snapshot-timeout+
                            :max-bytes (setting :snapshot-max-bytes))
                (declare (ignore headers))
                (cond
                  ((/= status 200)
                   (json-of "ok" nil "message" (format nil "The snapshot request returned HTTP ~d, so nothing was saved." status)))
                  (truncated
                   (json-of "ok" nil
                            "message" (format nil "The still is over the ~d byte cap and was discarded."
                                               (setting :snapshot-max-bytes))))
                  (t
                   (multiple-value-bind (extension mime) (image-kind raw)
                     (if (null extension)
                         (json-of "ok" nil "message" "The snapshot was not a JPEG or PNG, so it was not saved.")
                         (let* ((directory (still-directory))
                                (path (merge-pathnames (still-name extension) directory)))
                           (ensure-directories-exist path)
                           (with-open-file (out path :direction :output :if-exists :supersede
                                                      :element-type '(unsigned-byte 8))
                             (write-sequence raw out))
                           (prune-stills directory)
                           (json-of "ok" t "path" (namestring path) "bytes" (length raw)
                                    "mime" mime "message" "Saved the webcam still."))))))))
          (printer-failure (failure)
            (json-of "ok" nil "message" (failure-text failure)))))))

;;; --- the watch: state between checks -----------------------------------------------

(defun watch-state-path ()
  (merge-pathnames "octoprint/watch_state.json" (nlk:home)))

(defun load-watch-state ()
  "=> (values STATE PROBLEM); a file this cell did not write is left as it is."
  (let ((path (watch-state-path)))
    (if (null (probe-file path))
        (values nil nil)
        (handler-case
            (let* ((raw (uiop:read-file-string path))
                   (object (and (plusp (length raw)) (nlk:decode-json raw))))
              (if (and (hash-table-p object) (eql (jget object "version") +state-version+))
                  (values (state-from-json object) nil)
                  (values nil "The watch state file is not one this cell wrote; it was left as it is.")))
          (error () (values nil "The watch state file is unreadable; it was left as it is."))))))

(defun save-watch-state (state)
  (let ((path (watch-state-path)))
    (handler-case
        (progn
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
            (write-string (nlk:encode-json-object (state-to-json state)) out))
          t)
      (error () nil))))

(defun failure-report (previous code now)
  "Whether this failure is news, and the state to keep. The first of its kind,
a new one, or one a day later is news; a repeat inside that is not."
  (let* ((known (and previous (getf previous :failure)))
         (same (and known (string= (car known) code)))
         (fresh (or (null known) (not same) (>= (- now (or (cdr known) 0)) +failure-repeat-seconds+)))
         (state (copy-list (or previous '()))))
    (setf (getf state :failure) (cons code (if fresh now (or (cdr known) now))))
    (values fresh state)))

(defun events-text (events)
  (format nil "~{~a~^ ~}" (mapcar (lambda (one) (getf one :text)) events)))

(defun watch-once (&key (advance t))
  (running-settings)
  "One watch check; => a JSON object. The printer's failure is an answer here."
  (multiple-value-bind (previous problem) (load-watch-state)
    (if problem
        (json-of "ok" nil "moved" nil "notify" t "events" #() "state_saved" nil
                 "message" (format nil "~a Nothing was compared or stored." problem))
        (handler-case
            (let* ((sample (read-sample))
                   (now (get-universal-time)))
              (multiple-value-bind (state events note)
                  (compare previous sample now (setting :stall-minutes))
                (let* ((recovered (and (getf previous :failure) t))
                       (saved (when advance (save-watch-state state)))
                       (message (cond ((and events recovered)
                                       (format nil "The printer can be checked again. ~a" (events-text events)))
                                      (events (events-text events))
                                      (recovered "The printer can be checked again. Nothing new to report.")
                                      (t note))))
                  (json-of "ok" t "moved" nil
                           "notify" (and (or (plusp (length events)) recovered) t)
                           "note" note
                           "recovered" (and recovered t)
                           "events" (coerce (mapcar (lambda (one)
                                                       (json-of "kind" (getf one :kind)
                                                                "text" (getf one :text)))
                                                     events)
                                             'vector)
                           "job_state" (or (getf sample :job-state) :absent)
                           "kind" (or (getf sample :kind) :absent)
                           "completion" (or (getf sample :completion) :absent)
                           "message" message
                           "state_saved" (and saved t)))))
          (printer-failure (failure)
            (multiple-value-bind (notify state)
                (failure-report previous (failure-code failure) (get-universal-time))
              (let ((saved (when advance (save-watch-state state))))
                (json-of "ok" nil "moved" nil "notify" (and notify t)
                         "events" #() "code" (failure-code failure)
                         "state_saved" (and saved t)
                         "message" (format nil "~a~@[ ~a~]~@[ The same failure was already reported.~]"
                                            (failure-text failure)
                                            (let ((step (failure-next-step failure)))
                                              (and (plusp (length step)) step))
                                            (not notify))))))))))

;;; --- the tools ---------------------------------------------------------------------

(nlk:define-contract octoprint-status
  (:doc "Read one OctoPrint 3D printer: job state, connection state, the filename,
completion, the file position, OctoPrint's printTimeLeft estimate and the hotend
(tool0) and bed temperatures. Does not move the printer. include_snapshot saves
one webcam still inside the cell's own folder.")
  ("include_snapshot" :boolean :default nil :doc "save one webcam still; default false"))

(nlk:define-contract octoprint-control
  (:doc "Pause, resume or cancel the configured OctoPrint job. Without confirm it
answers what would be sent and the state now, and sends nothing; with confirm it
reads the state, sends the move, and reads the state again. It sends nothing when
the job is already there. Cancel ends the print and cannot be resumed. Refuses any
other action, G-code or temperature change.")
  ("action" :text :required t :options '("pause" "resume" "cancel")
            :doc "pause, resume or cancel")
  ("confirm" :boolean :default nil :doc "send it; without this nothing is sent"))

(defparameter +watch-schema+
  (list :description
        "Compare this check of the OctoPrint printer with the stored one and
report what changed: printer_disconnected, error, complete, paused (only when a
print was running), cancelled, and stalled (while printing, the file position,
completion and filename did not change for octoprint.stall_minutes). A printer
that cannot be reached is reported once per cause, then once a day, and once when
it answers again. Does not move the printer. Takes no arguments."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "octoprint_watch's schema: the printer's own diff, no arguments.")

(defun status-tool (request)
  (running-settings)
  (handler-case
      (let ((sample (read-sample)))
        (nlk:encode-json-object
         (json-of "ok" t "moved" nil
                  "message" (status-message sample)
                  "next_step" "Pause, resume and cancel go through octoprint_control, which previews and then asks for confirm."
                  "kind" (getf sample :kind)
                  "job_state" (getf sample :job-state)
                  "connection_state" (getf sample :connection-state)
                  "filename" (or (getf sample :filename) :absent)
                  "completion" (or (getf sample :completion) :absent)
                  "filepos" (or (getf sample :filepos) :absent)
                  "print_time_left" (or (getf sample :print-time-left) :absent)
                  "temperatures" (or (getf sample :temperatures) :absent)
                  "temperatures_text" (temperatures-text (getf sample :temperatures))
                  "error_text" (let ((text (getf sample :error-text)))
                                 (if (plusp (length text)) text :absent))
                  "snapshot" (if (octoprint-status-include-snapshot request)
                                 (save-still)
                                 :absent))))
    (printer-failure (failure) (failure-answer failure))))

(defparameter +bodies+ '(("pause" . "{\"command\":\"pause\",\"action\":\"pause\"}")
                          ("resume" . "{\"command\":\"pause\",\"action\":\"resume\"}")
                          ("cancel" . "{\"command\":\"cancel\"}"))
  "The POST /api/job body each action sends, as the original sends it.")

(defparameter +expected-states+ '(("pause" . "Paused") ("resume" . "Printing") ("cancel" . "Operational"))
  "The job state each action should produce.")

(defparameter +past-tense+ '(("pause" . "paused") ("resume" . "resumed") ("cancel" . "cancelled"))
  "What each action did, for the line that reports it.")

(defun approval-line (action origin)
  (let ((body (cdr (assoc action +bodies+ :test #'string=))))
    (format nil "OctoPrint at ~a: POST /api/job ~a. ~a"
            origin body
            (cond ((string= action "pause")
                   "This pauses the current print; it is sent only from Printing or Printing from SD.")
                  ((string= action "resume")
                   "This resumes the paused print from where it stopped; it does not restart from the beginning.")
                  (t "Cancel ends the print; it cannot be resumed afterwards.")))))

(defun send-decision (action sample)
  "send, already, hold, refuse or down, from the job state SAMPLE shows."
  (let ((job (getf sample :job-state))
        (kind (getf sample :kind)))
    (if (member kind '("disconnected" "error") :test #'string=)
        "down"
        (cond
          ((string= action "pause")
           (cond ((member job +printing-states+ :test #'string=) "send")
                 ((string= job "Pausing") "hold")
                 ((string= job "Paused") "already")
                 (t "refuse")))
          ((string= action "resume")
           (cond ((string= job "Paused") "send")
                 ((string= job "Pausing") "hold")
                 ((member job +printing-states+ :test #'string=) "already")
                 (t "refuse")))
          (t (cond ((or (member job +printing-states+ :test #'string=)
                         (member job '("Paused" "Pausing" "Resuming") :test #'string=))
                    "send")
                   ((string= job "Cancelling") "hold")
                   (t "already")))))))

(defun not-sent-message (action decision sample)
  (let ((job (getf sample :job-state)))
    (cond
      ((string= decision "down")
       (format nil "The printer is ~a (job ~a, connection ~a). Nothing was sent. A 409 from /api/printer is not itself a pause."
               (getf sample :kind) job (getf sample :connection-state)))
      ((string= job "Pausing")
       "Job state is Pausing. Nothing was sent. Pausing is a pause in progress, not a failure. Do not send pause or resume until a later read shows Paused or Printing. On OctoPrint 1.11.8 resume runs only when the job is already paused.")
      ((string= job "Cancelling")
       "Job state is Cancelling. Nothing was sent. Cancel was already accepted. Do not send cancel or resume until a later read leaves Cancelling.")
      ((string= decision "already")
       (format nil "Job state is ~a, so ~a was not sent." job action))
      (t (format nil "Job state is ~a, so ~a is not sent from there. Nothing was sent." job action)))))

(defun reached-p (action sample)
  (let ((job (getf sample :job-state))
        (kind (getf sample :kind)))
    (and (not (member kind '("disconnected" "error") :test #'string=))
         (string= job (cdr (assoc action +expected-states+ :test #'string=))))))

(defun settle (action)
  "Read the job again until it shows the move, or give up.
=> (values SAMPLE REACHED)."
  (let ((last nil) (reached nil))
    (dotimes (index +control-reads+)
      (setf last (read-sample))
      (when (reached-p action last)
        (setf reached t)
        (return))
      (unless (= index (1- +control-reads+)) (sleep +control-pause+)))
    (values last reached)))

(defun moved-answer (action decision before after)
  (let ((job (getf after :job-state)))
    (nlk:encode-json-object
     (json-of "ok" t "moved" t "sent" t "action" action
              "job_state" job "previous_state" (getf before :job-state)
              "completion" (or (getf after :completion) :absent)
              "message"
              (if (string= action "cancel")
                  (format nil "Cancel was accepted and a later read shows Operational. The print cannot be resumed. Job state changed from ~a to ~a."
                          (getf before :job-state) job)
                  (format nil "Printer ~a. Job state changed from ~a to ~a."
                          (cdr (assoc action +past-tense+ :test #'string=))
                          (getf before :job-state) job))))))

(defun send-move (action before origin)
  (let ((body (cdr (assoc action +bodies+ :test #'string=))))
    (multiple-value-bind (raw status headers truncated)
        (api-request :post "/api/job" :body body)
      (declare (ignore raw headers truncated))
      (unless (= status 204)
        (printer-fail "http"
                      (format nil "OctoPrint answered HTTP ~d instead of 204, so this was not accepted." status)
                      "Read the state before sending it again."))
      (handler-case
          (multiple-value-bind (after reached) (settle action)
            (if reached
                (moved-answer action nil before after)
                (nlk:encode-json-object
                 (json-of "ok" nil "moved" nil "accepted" t "sent" t "action" action
                          "job_state" (or (getf after :job-state) :absent)
                          "previous_state" (getf before :job-state)
                          "message"
                          (if (and (string= action "resume")
                                   (string= (getf after :job-state) "Operational")
                                   (let ((completion (getf after :completion)))
                                     (and completion (>= completion 100))))
                              "OctoPrint accepted resume (HTTP 204), but the job is Operational at 100%, so the print finished. This is not a successful resume."
                              (format nil "OctoPrint accepted ~a (HTTP 204) for ~a, but the job state is ~a, not ~a. Do not send the command again yet; read the state first."
                                      action origin
                                      (or (getf after :job-state) "unreadable")
                                      (cdr (assoc action +expected-states+ :test #'string=))))))))
        (printer-failure (failure)
          (nlk:encode-json-object
           (json-of "ok" nil "moved" nil "accepted" t "sent" t "action" action
                    "message" (format nil "OctoPrint accepted the command (HTTP 204). A later read failed: ~a"
                                      (failure-text failure))
                    "next_step" "The command may already have taken effect. Read the state before sending it again.")))))))

(defun control-tool (request)
  (running-settings)
  (let ((action (octoprint-control-action request))
        (confirm (octoprint-control-confirm request)))
    (handler-case
        (let ((origin (origin-base)))
          (if (not confirm)
              (let ((sample (read-sample)))
                (nlk:encode-json-object
                 (json-of "ok" t "moved" nil "sent" nil "action" action
                          "job_state" (or (getf sample :job-state) :absent)
                          "kind" (or (getf sample :kind) :absent)
                          "preview" (approval-line action origin)
                          "message" (format nil "~a Job state is ~a. Nothing was sent: call octoprint_control again with action ~s and confirm true."
                                             (approval-line action origin)
                                             (or (getf sample :job-state) "unreadable") action))))
              (let* ((before (read-sample))
                     (decision (send-decision action before)))
                (if (string= decision "send")
                    (send-move action before origin)
                    (nlk:encode-json-object
                     (json-of "ok" nil "moved" nil "sent" nil "action" action
                              "job_state" (or (getf before :job-state) :absent)
                              "kind" (or (getf before :kind) :absent)
                              "decision" decision
                              "message" (not-sent-message action decision before)))))))
      (printer-failure (failure) (failure-answer failure)))))

(defun watch-tool (request)
  (declare (ignore request))
  (running-settings)
  (nlk:encode-json-object (watch-once :advance t)))

;;; --- the operator's command ----------------------------------------------------------

(defun status-text (answer)
  (format nil "~a~@[~%File: ~a~]~%~a"
          (gethash "message" answer)
          (gethash "filename" answer)
          (gethash "temperatures_text" answer)))

(defun run-octoprint (args session-id)
  "The operator's reading of the printer; it never moves it."
  (declare (ignore session-id))
  (running-settings)
  (let* ((text (nlk:trimmed (or args "")))
         (words (and (plusp (length text))
                     (uiop:split-string text :separator '(#\Space #\Tab))))
         (verb (string-downcase (or (first words) "status"))))
    (handler-case
        (cond
          ((string= verb "status")
           (let ((sample (read-sample)))
             (status-text (json-of "message" (status-message sample)
                                   "filename" (or (getf sample :filename) :absent)
                                   "temperatures_text" (temperatures-text (getf sample :temperatures))))))
          ((string= verb "watch") (gethash "message" (watch-once :advance t)))
          ((string= verb "snapshot")
           (let ((answer (save-still)))
             (if (gethash "ok" answer)
                 (format nil "~a ~a" (gethash "message" answer) (or (gethash "path" answer) ""))
                 (format nil "~a" (gethash "message" answer)))))
          ((member verb '("pause" "resume" "cancel") :test #'string=)
           (format nil "/octoprint ~a was not sent: this command does not move the printer. Call octoprint_control with action ~a and confirm true so the move is explicit."
                   verb verb))
          (t "Usage: /octoprint status | watch | snapshot. It reads the printer and never moves it; pause, resume and cancel go through octoprint_control."))
      (printer-failure (failure) (failure-answer failure)))))

;;; --- the background watch -----------------------------------------------------------

(defun watch-tick ()
  "One background check: its events become one standing notice."
  (let ((answer (watch-once :advance t)))
    (if (gethash "notify" answer)
        (nle:notice (format nil "octoprint: ~a" (gethash "message" answer))
                    :level :warning :key "nodecode-octoprint")
        (nle:notice nil :key "nodecode-octoprint"))
    nil))

;;; --- the cell -----------------------------------------------------------------------

(nle:define-cell octoprint
  (:section ("octoprint")
    (:guide "url is the one OctoPrint origin, such as http://printer.local:5000; api_key is a user key, sent only as X-Api-Key; snapshot_url is an optional webcam still on that host; watch_minutes above 0 starts a background watch that reports job events as a notice")
    ("url" :string :required t :doc "the OctoPrint origin, scheme://host:port")
    ("api_key" :secret :required t :doc "sent only as the X-Api-Key header")
    ("snapshot_url" :string :doc "an http(s) webcam still URL on the printer's own host; empty is none")
    ("stall_minutes" :integer :default 10 :min 1
                      :doc "minutes without file progress before a stall is reported")
    ("snapshot_max_bytes" :integer :default 2000000 :min 1
                           :doc "largest webcam still to keep")
    ("watch_minutes" :integer :default 0 :min 0
                      :doc "background check period in minutes; 0 is off"))
  (:tool "octoprint_status" (nlk:contract 'octoprint-status) #'status-tool)
  (:tool "octoprint_control" (nlk:contract 'octoprint-control) #'control-tool)
  (:tool "octoprint_watch" +watch-schema+ #'watch-tool)
  (:help :octoprint "octoprint: one OctoPrint 3D printer - job progress, temperatures, webcam still, pause, resume, cancel" +primer+)
  (:command "octoprint" #'run-octoprint
            :description "Read the OctoPrint printer, or run a watch check"
            :argument-hint "status|watch|snapshot")
  (:start (lambda ()
            (when (plusp (setting :watch-minutes))
              (let ((worker (nlk:worker-start "octoprint-watch" #'watch-tick
                                              :wake (* 60 (setting :watch-minutes)))))
                (nle:on-stop (lambda () (nlk:worker-stop worker))))))))
