;;;; cell.lisp --- the klipper section, three printer tools, /klipper, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's surfaces were three tools, a /klipper-print-watch
;;;; command, a pre_tool_call approval hook, a skill and a cron script. This
;;;; cell keeps the tools, the command and the guidance, folds the approval
;;;; into the tool call itself (a preview, then the same call with confirm),
;;;; and replaces the cron script with a worker that reports events as a
;;;; standing notice while the cell runs.

(in-package #:nodecode-klipper)

(nlk:define-peripheral klipper :not-running t)

(defparameter +json-timeout+ 10)
(defparameter +control-timeout+ 60)
(defparameter +snapshot-timeout+ 15)
(defparameter +json-max-bytes+ 1000000)
(defparameter +snapshot-keep+ 20)

(defparameter +objects-body+
  "{\"objects\":{\"print_stats\":null,\"display_status\":null,\"virtual_sdcard\":null,\"extruder\":null,\"heater_bed\":null,\"webhooks\":null}}"
  "The object query klipper_status sends; every block whole.")

(defparameter +primer+
  "klipper reads one Klipper 3D printer through its Moonraker host, on the
settings' klipper.url. Nothing moves the printer except a confirmed call.

  klipper_status [include_snapshot] [include_files]
      print_stats.state, the filename, progress, the file position, the
      slicer's remaining-time estimate, the extruder (hotend) and bed
      temperatures, and webhooks.state with Klipper's own message. A stopped
      Klipper (shutdown or error) is named as such: print_stats.state is not
      reliable until it restarts. include_files adds the gcode count;
      include_snapshot saves one webcam still into the cell's own folder.
  klipper_control action [confirm]
      action is pause, resume or cancel. Without confirm it answers what
      would be sent and the state now, and sends nothing; with confirm it
      reads the state, sends the move, and reads it again, so moved is a
      fact and not a promise. Any other action, G-code or temperature is
      refused.
  klipper_watch
      compares this check with the last one and reports klippy_shutdown,
      complete, error, paused (only from printing), cancelled, and stalled
      (no file position change for klipper.stall_minutes while printing).
  /klipper status|watch|files|snapshot
      the same readings for the operator; it never moves the printer.

With klipper.watch_minutes above zero a background check runs on that
period and raises the same events as a notice. (help :klipper) answers
this while the cell runs."
  "What (help :klipper) answers while the cell runs.")

;;; --- one Moonraker origin ---------------------------------------------------------

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
  "klipper.url as scheme://host[:port]; signals for anything but one origin."
  (let ((raw (nlk:trimmed (or (setting :url) ""))))
    (when (zerop (length raw))
      (printer-fail "missing_url" "klipper.url is not set."
                    "Point it at one Moonraker origin, such as http://printer.local:7125."))
    (when (find-if (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return #\\))) raw)
      (printer-fail "bad_url" "klipper.url holds a space, a tab or a backslash."
                    "Use scheme://host:port with no path, query, user or password."))
    (let* ((uri (quri:uri raw))
           (scheme (string-downcase (or (quri:uri-scheme uri) "")))
           (host (or (quri:uri-host uri) ""))
           (port (quri:uri-port uri))
           (path (quri:uri-path uri))
           (default (if (string= scheme "https") 443 80)))
      (unless (member scheme '("http" "https") :test #'string=)
        (printer-fail "bad_url" "klipper.url must start with http:// or https://."
                      "Example: http://printer.local:7125"))
      (when (zerop (length host))
        (printer-fail "bad_url" "klipper.url has no host." "Example: http://printer.local:7125"))
      (when (quri:uri-userinfo uri)
        (printer-fail "bad_url" "klipper.url carries a user or password."
                      "Put the API key in klipper.api_key; it is sent only as X-Api-Key."))
      (when (or (and path (plusp (length path)) (not (string= path "/")))
                (quri:uri-query uri) (quri:uri-fragment uri))
        (printer-fail "bad_url" "klipper.url has a path, query or fragment."
                      "Use only scheme://host:port; the cell adds Moonraker's paths itself."))
      (format nil "~a://~a~@[:~d~]" scheme (string-downcase host)
              (and port (/= port default) port)))))

(defun api-headers ()
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (append '(("Accept" . "application/json"))
            (unless (zerop (length key)) (list (cons "X-Api-Key" key))))))

(defun percent-encode (text)
  "TEXT safe in a query value."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          for character = (code-char octet)
          do (if (or (alphanumericp character) (find character "-_.~"))
                 (write-char character out)
                 (format out "%~2,'0X" octet)))))

(defun same-origin-p (url base)
  "Whether URL is under BASE's scheme, host and port."
  (handler-case
      (let* ((one (quri:uri url)) (two (quri:uri base)))
        (and (equal (quri:uri-scheme one) (quri:uri-scheme two))
             (equal (string-downcase (or (quri:uri-host one) ""))
                    (string-downcase (or (quri:uri-host two) "")))
             (eql (quri:uri-port one) (quri:uri-port two))))
    (error () nil)))

(defun api-request (method path &key query body (timeout +json-timeout+) (max-bytes +json-max-bytes+))
  "One Moonraker exchange, bounded; => (values BODY STATUS HEADERS TRUNCATED)."
  (let* ((base (origin-base))
         (url (concatenate 'string base path (or query ""))))
    (unless (same-origin-p url base)
      (printer-fail "off_origin" "The URL left the configured Moonraker origin." "Nothing was sent."))
    (handler-case
        (nlk:http method url
                  :headers (api-headers)
                  :content (cond ((null body) nil)
                                 ((stringp body) body)
                                 (t (nlk:encode-json-object body)))
                  :timeout timeout :max-bytes max-bytes)
      (printer-failure (condition) (error condition))
      (error (condition)
        (printer-fail "network"
                      (format nil "Could not reach Moonraker (~a)."
                              (nlk:one-line (princ-to-string condition) :cap 200))
                      "Check that klipper.url is reachable from this machine and that Moonraker runs.")))))

(defun error-object-text (parsed status)
  (let* ((error (jget parsed "error"))
         (message (or (jstr (jget error "message")) "")))
    (cond ((and (plusp (length message))
                (not (member message '("Unknown" "Unauthorized") :test #'string=)))
           (subseq message 0 (min (length message) 300)))
          (t (format nil "Moonraker returned HTTP ~d." status)))))

(defun api-json (method path &key query body (timeout +json-timeout+))
  "Moonraker's RESULT member; signals PRINTER-FAILURE for anything else."
  (multiple-value-bind (raw status headers truncated)
      (api-request method path :query query :body body :timeout timeout)
    (declare (ignore headers))
    (when truncated
      (printer-fail "too_large"
                    (format nil "Moonraker's answer was over ~d bytes and was discarded." +json-max-bytes+)
                    "Ask for a smaller object query."))
    (let ((parsed (handler-case (nlk:decode-json (or raw #())) (error () nil))))
      (cond
        ((not (hash-table-p parsed))
         (printer-fail "bad_body"
                       (format nil "Moonraker returned HTTP ~d with a body that is not a JSON object." status)
                       "Check that klipper.url points at Moonraker, not another web server."))
        ((jget parsed "error")
         (printer-fail (if (member status '(401 403)) "unauthorized" "moonraker")
                       (error-object-text parsed status)
                       (if (member status '(401 403))
                           "Check klipper.api_key, or leave it empty if this machine is in Moonraker's trusted_clients."
                           "A failed request does not move the printer.")))
        ((not (gethash "result" parsed))
         (printer-fail "moonraker"
                       (format nil "Moonraker answered HTTP ~d without a result." status)
                       "The printer was not changed by a failed request."))
        ((or (< status 200) (>= status 300))
         (printer-fail "moonraker" (format nil "Moonraker returned HTTP ~d." status)
                       "Treat this as a failure."))
        (t (gethash "result" parsed))))))

;;; --- the printer's state -----------------------------------------------------------

(defun metadata-of (filename)
  "PATH's slicer metadata; => (values METADATA ERROR-TEXT)."
  (if (or (zerop (length filename)) (char= (char filename 0) #\/) (search ".." filename))
      (values nil (unless (zerop (length filename))
                    "The filename was not a relative gcode path, so no remaining time was asked for."))
      (handler-case
          (let ((result (api-json :get "/server/files/metadata"
                                   :query (concatenate 'string "?filename=" (percent-encode filename)))))
            (if (hash-table-p result)
                (values result nil)
                (values nil "File metadata was not an object, so no remaining time was computed.")))
        (printer-failure (failure) (values nil (failure-text failure))))))

(defun remaining-seconds (state progress metadata)
  (let ((estimated (and metadata (jnum (jget metadata "estimated_time")))))
    (when (and (member state '("printing" "paused") :test #'string=) estimated progress)
      (max 0.0 (* estimated (- 1.0 progress))))))

(defun json-number (text)
  "TEXT as a JSON number, not a string."
  (nlk:json-verbatim (sb-ext:string-to-octets text :external-format :utf-8)))

(defun json-decimal (value &optional (places 1))
  "VALUE as a JSON number with PLACES decimals, or :ABSENT."
  (if (numberp value)
      (json-number (format nil "~,vF" places value))
      :absent))

(defun json-integer (value)
  "VALUE as a JSON number rounded to a whole one, or :ABSENT."
  (let ((number (jnum value)))
    (if number (round number) :absent)))

(defun heater-of (block)
  "BLOCK's temperature and target, as (values TEMPERATURE TARGET)."
  (values (jnum (jget block "temperature")) (jnum (jget block "target"))))

(defun heater-json (block)
  (multiple-value-bind (temperature target) (heater-of block)
    (json-of "temperature" (json-decimal temperature 1)
             "target" (json-decimal target 1))))

(defun printer-status (&key include-snapshot include-files)
  "The status object a model reads; signals PRINTER-FAILURE."
  (let* ((result (api-json :post "/printer/objects/query" :body +objects-body+))
         (status (jget result "status")))
    (unless (hash-table-p status)
      (printer-fail "bad_body" "Moonraker's object query did not return a status object."))
    (let* ((stats (jget status "print_stats"))
           (virtual (jget status "virtual_sdcard"))
           (display (jget status "display_status"))
           (hooks (jget status "webhooks"))
           (filename (or (jstr (jget stats "filename")) ""))
           (state (jstr (jget stats "state")))
           (progress (or (jnum (jget virtual "progress")) (jnum (jget display "progress"))))
           (klippy (jstr (jget hooks "state")))
           (klippy-message (jstr (jget hooks "state_message"))))
      (multiple-value-bind (metadata metadata-error) (metadata-of filename)
        (let* ((remaining (remaining-seconds state progress metadata))
               (stopped (member klippy +klippy-stopped+ :test #'string=))
               (detail (klippy-detail klippy-message))
               (message (format nil "Printer state ~a, progress ~a, remaining ~a, Klippy ~a.~@[~a~]"
                                (or state "unknown")
                                (if progress (format nil "~,1f%" (* 100.0 progress)) "unknown")
                                (if remaining (format nil "~d seconds (slicer estimate)" (round remaining))
                                    "unknown")
                                (or klippy "unknown")
                                (and stopped
                                     (format nil " Klipper has stopped: ~a. print_stats.state is not reliable until it restarts."
                                             (if (plusp (length detail)) detail "no state_message"))))))
          (let ((body (json-of "ok" t "moved" nil
                                "state" (or state :absent)
                                "filename" (if (plusp (length filename)) filename :absent)
                                "progress" (json-decimal progress 3)
                                "file_position" (json-integer (jget virtual "file_position"))
                                "file_size" (json-integer (jget virtual "file_size"))
                                "remaining_seconds" (if remaining (round remaining) :absent)
                                "remaining_source" (if remaining
                                                        "slicer_estimated_time_times_remaining_fraction"
                                                        :absent)
                                "temperatures" (json-of "extruder" (heater-json (jget status "extruder"))
                                                         "heater_bed" (heater-json (jget status "heater_bed")))
                                "temperatures_text" (temperatures-line status)
                                "klippy_state" (or klippy :absent)
                                "klippy_message" (or klippy-message :absent)
                                "print_message" (or (jstr (jget stats "message")) :absent)
                                "metadata_error" (or metadata-error :absent)
                                "message" message)))
            (when include-files
              (setf (gethash "gcodes" body)
                    (handler-case
                        (let ((files (api-json :get "/server/files/list" :query "?root=gcodes")))
                          (if (vectorp files)
                              (json-of "count" (length files) "complete" t)
                              (json-of "complete" nil "message" "Moonraker's file list was not a list.")))
                      (printer-failure (failure)
                        (json-of "complete" nil "message" (failure-text failure))))))
            (when include-snapshot
              (setf (gethash "snapshot" body) (save-snapshot)))
            body))))))

;;; --- a webcam still, inside the cell's own folder -----------------------------------

(defun snapshot-directory ()
  (uiop:ensure-directory-pathname (merge-pathnames "klipper/snapshots/" (nlk:home))))

(defun stamp-name (extension)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ.~a"
            year month day hour minute second extension)))

(defun image-kind (octets)
  (cond ((and (>= (length octets) 3)
              (= (aref octets 0) #xff) (= (aref octets 1) #xd8) (= (aref octets 2) #xff))
         (values "jpg" "image/jpeg"))
        ((and (>= (length octets) 8) (equalp (subseq octets 0 8) #(137 80 78 71 13 10 26 10)))
         (values "png" "image/png"))
        (t nil)))

(defun prune-snapshots (directory)
  (let ((files (sort (remove-if-not (lambda (path)
                                      (member (string-downcase (or (pathname-type path) ""))
                                              '("jpg" "png") :test #'string=))
                                    (uiop:directory-files directory))
                     #'string< :key #'namestring)))
    (loop for old in (butlast files (min +snapshot-keep+ (length files)))
          do (ignore-errors (delete-file old)))))

(defun save-snapshot ()
  "One webcam still into the cell's folder; => a JSON object saying what happened."
  (handler-case
      (let* ((listed (api-json :get "/server/webcams/list"))
             (webcams (jget listed "webcams"))
             (chosen (and (vectorp webcams)
                          (find-if (lambda (item)
                                     (and (hash-table-p item) (jstr (jget item "snapshot_url"))))
                                   webcams))))
        (if (null chosen)
            (json-of "ok" nil "message" "No webcam with a snapshot URL; nothing was downloaded.")
            (let* ((raw-url (jstr (jget chosen "snapshot_url")))
                   (base (origin-base))
                   (url (if (char= (char raw-url 0) #\/)
                            (concatenate 'string base raw-url)
                            raw-url)))
              (unless (same-origin-p url base)
                (printer-fail "off_origin" "The webcam URL is not on the configured Moonraker origin."
                              "Use a snapshot_url on Moonraker itself; nothing was saved."))
              (multiple-value-bind (raw status headers truncated)
                  (nlk:http :get url :headers (api-headers) :timeout +snapshot-timeout+
                                 :max-bytes (setting :snapshot-max-bytes))
                (declare (ignore headers))
                (cond
                  ((or (< status 200) (>= status 300))
                   (json-of "ok" nil "message" (format nil "Webcam snapshot returned HTTP ~d." status)))
                  (truncated
                   (json-of "ok" nil
                            "message" (format nil "The still is over the ~d byte cap and was discarded."
                                               (setting :snapshot-max-bytes))))
                  (t
                   (multiple-value-bind (extension mime) (image-kind raw)
                     (if (null extension)
                         (json-of "ok" nil "message" "The snapshot was not a JPEG or PNG, so it was not saved.")
                         (let* ((directory (snapshot-directory))
                                (path (merge-pathnames (stamp-name extension) directory)))
                           (ensure-directories-exist path)
                           (with-open-file (out path :direction :output :if-exists :supersede
                                                      :element-type '(unsigned-byte 8))
                             (write-sequence raw out))
                           (prune-snapshots directory)
                           (json-of "ok" t "path" (namestring path) "bytes" (length raw)
                                    "mime" mime "message" "Saved the webcam still."))))))))))
    (printer-failure (failure)
      (json-of "ok" nil "message" (failure-text failure)))))

;;; --- the watch: state between checks ------------------------------------------------

(defun watch-state-path ()
  (merge-pathnames "klipper/watch_state.json" (nlk:home)))

(defun state-to-json (state)
  (json-of "version" (getf state :version)
           "filename" (or (getf state :filename) "")
           "state" (or (getf state :state) "")
           "klippy_state" (or (getf state :klippy-state) "")
           "klippy_stops" (or (getf state :klippy-stops) 0)
           "file_position" (or (getf state :file-position) :absent)
           "progress" (or (getf state :progress) :absent)
           "progress_changed_at" (getf state :progress-changed-at)
           "stall_open" (and (getf state :stall-open) t)
           "generation" (or (getf state :generation) 0)
           "notified" (coerce (or (getf state :notified) '()) 'vector)))

(defun state-from-json (object)
  (let ((notified (jget object "notified")))
    (list :version (jget object "version")
          :filename (or (jstr (jget object "filename")) "")
          :state (or (jstr (jget object "state")) "")
          :klippy-state (or (jstr (jget object "klippy_state")) "")
          :klippy-stops (or (jget object "klippy_stops") 0)
          :file-position (jnum (jget object "file_position"))
          :progress (jnum (jget object "progress"))
          :progress-changed-at (jnum (jget object "progress_changed_at"))
          :stall-open (and (jget object "stall_open") t)
          :generation (or (jget object "generation") 0)
          :notified (if (vectorp notified) (coerce notified 'list) '()))))

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
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (nlk:encode-json-object (state-to-json state)) out))
    t))

(defun watch-once (&key (advance t))
  (running-settings)
  "One watch check; => a JSON object. The printer's failure is an answer here."
  (multiple-value-bind (previous problem) (load-watch-state)
    (if problem
        (json-of "ok" nil "moved" nil "notify" t "events" #() "state_saved" nil
                 "message" (format nil "~a Nothing was compared or stored." problem))
        (handler-case
            (let* ((result (api-json :post "/printer/objects/query" :body +objects-body+))
                   (status (jget result "status")))
              (unless (hash-table-p status)
                (printer-fail "bad_body" "Moonraker's object query did not return a status object."))
              (let ((sample (sample-from-status status)))
                (multiple-value-bind (state events note)
                    (compare previous sample (float (get-universal-time)) (setting :stall-minutes))
                  (let ((saved (when advance (save-watch-state state)))
                        (message (if events
                                     (format nil "~{~a~^ ~}"
                                             (mapcar (lambda (event) (getf event :text)) events))
                                     note)))
                    (json-of "ok" t "moved" nil
                             "notify" (and (plusp (length events)) t)
                             "note" note
                             "events" (coerce events 'vector)
                             "state" (or (getf sample :state) :absent)
                             "progress" (or (getf sample :progress) :absent)
                             "klippy_state" (or (getf sample :klippy-state) :absent)
                             "message" message
                             "state_saved" (and saved t))))))
          (printer-failure (failure)
            (json-of "ok" nil "moved" nil "notify" t "events" #() "state_saved" nil
                     "message" (format nil "~a~@[ ~a~]" (failure-text failure)
                                        (let ((step (failure-next-step failure)))
                                          (and (plusp (length step)) step)))))))))

;;; --- the tools ---------------------------------------------------------------------

(nlk:define-contract klipper-status
  (:doc "Read one Klipper 3D printer through Moonraker: print state, progress, the
slicer's remaining-time estimate, extruder (hotend) and bed temperatures, and
Klipper's own state. Does not move the printer. include_snapshot saves one webcam
still inside the cell's own folder; include_files counts the gcode files.")
  ("include_snapshot" :boolean :default nil :doc "save one webcam still; default false")
  ("include_files" :boolean :default nil :doc "count the gcode files; default false"))

(nlk:define-contract klipper-control
  (:doc "Pause, resume or cancel the configured Klipper printer. Without confirm it
answers what would be sent and the state now, and sends nothing; with confirm it
reads the state, sends the move, and reads the state again. It sends nothing when
the printer is already in that state. Refuses any other action, G-code or
temperature change.")
  ("action" :text :required t :options '("pause" "resume" "cancel")
            :doc "pause, resume or cancel")
  ("confirm" :boolean :default nil :doc "send it; without this nothing is sent"))

(defparameter +watch-schema+
  (list :description
        "Compare this check of the Klipper printer with the stored one and report
what changed: klippy_shutdown (Klipper itself stopped), complete, error, paused
(only when the previous state was printing), cancelled, and stalled (while print-
ing, the file position, progress and filename did not change for klipper.stall_minutes).
Does not move the printer. Takes no arguments."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "klipper_watch's schema: the printer's own diff, no arguments.")

(defun status-tool (request)
  (running-settings)
  (handler-case
      (nlk:encode-json-object
       (printer-status :include-snapshot (and (klipper-status-include-snapshot request) t)
                       :include-files (and (klipper-status-include-files request) t)))
    (printer-failure (failure) (failure-answer failure))))

(defparameter +expected-states+ '(("pause" . "paused") ("resume" . "printing") ("cancel" . "cancelled"))
  "The print_stats.state each action should produce.")

(defparameter +past-tense+ '(("pause" . "paused") ("resume" . "resumed") ("cancel" . "cancelled"))
  "What each action did, for the line that reports it.")

(defun print-state ()
  "print_stats.state now, or NIL when it cannot be read."
  (handler-case
      (let* ((result (api-json :post "/printer/objects/query"
                                :body "{\"objects\":{\"print_stats\":null}}"))
             (stats (jget (jget result "status") "print_stats")))
        (jstr (jget stats "state")))
    (printer-failure () nil)))

(defun control-tool (request)
  (running-settings)
  (let ((action (klipper-control-action request))
        (confirm (klipper-control-confirm request)))
    (handler-case
        (let* ((expected (cdr (assoc action +expected-states+ :test #'string=)))
               (base (origin-base)))
          (if (not confirm)
              (let ((now (print-state)))
                (nlk:encode-json-object
                 (json-of "ok" t "moved" nil "sent" nil "action" action
                          "state" (or now :absent)
                          "message" (format nil "~:(~a~) would be sent to ~a. print_stats.state is ~a. Nothing was sent: call klipper_control again with action ~s and confirm true."
                                             action base (or now "unreadable") action))))
              (let ((before (print-state)))
                (when (null before)
                  (printer-fail "unconfirmed" "print_stats.state could not be read, so nothing was sent."
                                "Call klipper_status. Do not assume the printer moved."))
                (if (string= before expected)
                    (nlk:encode-json-object
                     (json-of "ok" nil "moved" nil "sent" nil "action" action "state" before
                              "message" (format nil "print_stats.state is already ~a, so ~a was not sent."
                                                 before action)))
                    (let ((reply (api-json :post (format nil "/printer/print/~a" action)
                                            :timeout +control-timeout+)))
                      (unless (equal reply "ok")
                        (printer-fail "moonraker"
                                      "Moonraker did not answer ok, so this was not treated as a move."
                                      "Check the printer; do not assume it paused, resumed or cancelled."))
                      (let ((after (print-state)))
                        (nlk:encode-json-object
                         (json-of "ok" (and (equal after expected) t)
                                  "moved" (and (equal after expected) t)
                                  "sent" t "action" action
                                  "state" (or after :absent) "previous_state" before
                                  "message" (if (equal after expected)
                                                 (format nil "Printer ~a. print_stats.state changed from ~a to ~a."
                                                         (or (cdr (assoc action +past-tense+ :test #'string=)) action)
                                                         before after)
                                                 (format nil "Moonraker answered ok for ~a, but print_stats.state is ~a, not ~a. Do not assume the printer moved."
                                                         action (or after "unreadable") expected))))))))))
      (printer-failure (failure) (failure-answer failure)))))

(defun watch-tool (request)
  (declare (ignore request))
  (running-settings)
  (nlk:encode-json-object (watch-once :advance t)))

;;; --- the operator's command ----------------------------------------------------------

(defun show-number (value)
  (if (numberp value) (format nil "~,1F" (float value)) "?"))

(defun temperatures-line (status)
  "One line, from the printer's own numbers."
  (multiple-value-bind (extruder extruder-target) (heater-of (jget status "extruder"))
    (multiple-value-bind (bed bed-target) (heater-of (jget status "heater_bed"))
      (format nil "Extruder ~a/~a C, bed ~a/~a C."
              (show-number extruder) (show-number extruder-target)
              (show-number bed) (show-number bed-target)))))

(defun status-text (body)
  (let ((filename (gethash "filename" body))
        (gcodes (gethash "gcodes" body)))
    (format nil "~a~@[~%File: ~a~]~%~a~@[~%Gcode files: ~a~]"
            (gethash "message" body)
            filename
            (gethash "temperatures_text" body)
            (and gcodes (gethash "count" gcodes)))))

(defun run-klipper (args session-id)
  "The operator's reading of the printer; it never moves it."
  (declare (ignore session-id))
  (running-settings)
  (let* ((text (nlk:trimmed (or args "")))
         (words (and (plusp (length text))
                     (uiop:split-string text :separator '(#\Space #\Tab))))
         (verb (string-downcase (or (first words) "status"))))
    (handler-case
        (cond
          ((string= verb "status") (status-text (printer-status)))
          ((string= verb "files") (status-text (printer-status :include-files t)))
          ((string= verb "watch")
           (format nil "~a" (gethash "message" (watch-once :advance t))))
          ((string= verb "snapshot")
           (let ((answer (save-snapshot)))
             (if (gethash "ok" answer)
                 (format nil "~a ~a" (gethash "message" answer) (or (gethash "path" answer) ""))
                 (format nil "~a" (gethash "message" answer)))))
          ((member verb '("pause" "resume" "cancel") :test #'string=)
           (format nil "/klipper ~a was not sent: this command does not move the printer. Call klipper_control with action ~a and confirm true so the move is explicit."
                   verb verb))
          (t "Usage: /klipper status | files | watch | snapshot. It reads the printer and never moves it; pause, resume and cancel go through klipper_control."))
      (printer-failure (failure) (failure-answer failure)))))

;;; --- the background watch -----------------------------------------------------------

(defun watch-tick ()
  "One background check: its events become one standing notice."
  (let* ((answer (watch-once :advance t))
         (message (gethash "message" answer)))
    (if (gethash "notify" answer)
        (nle:notice (format nil "klipper: ~a" message) :level :warning :key "nodecode-klipper")
        (nle:notice nil :key "nodecode-klipper"))
    nil))

;;; --- the cell -----------------------------------------------------------------------

(nle:define-cell klipper
  (:section ("klipper")
    (:guide "url is the one Moonraker origin, such as http://printer.local:7125; api_key only when this machine is not in Moonraker's trusted_clients; watch_minutes above 0 starts a background watch that reports print events as a notice")
    ("url" :string :required t :doc "the Moonraker origin, scheme://host:port")
    ("api_key" :secret :doc "sent only as the X-Api-Key header")
    ("stall_minutes" :integer :default 10 :min 1
                      :doc "minutes without file progress before a stall is reported")
    ("snapshot_max_bytes" :integer :default 2000000 :min 1
                           :doc "largest webcam still to keep")
    ("watch_minutes" :integer :default 0 :min 0
                      :doc "background check period in minutes; 0 is off"))
  (:tool "klipper_status" (nlk:contract 'klipper-status) #'status-tool)
  (:tool "klipper_control" (nlk:contract 'klipper-control) #'control-tool)
  (:tool "klipper_watch" +watch-schema+ #'watch-tool)
  (:help :klipper "klipper: one Klipper 3D printer over Moonraker - status, temperatures, pause, resume, cancel" +primer+)
  (:command "klipper" #'run-klipper
            :description "Read the Klipper printer, or run a watch check"
            :argument-hint "status|files|watch|snapshot")
  (:start (lambda ()
            (when (plusp (setting :watch-minutes))
              (let ((worker (nlk:worker-start "klipper-watch" #'watch-tick
                                              :wake (* 60 (setting :watch-minutes)))))
                (nle:on-stop (lambda () (nlk:worker-stop worker))))))))
