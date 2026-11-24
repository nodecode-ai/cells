;;;; verbs.lisp --- the four read-only verbs, their shapes, and /nextdns.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin answered four tools with a JSON string each; so does this, over
;;;; the same four endpoints and with the same trimming: a log row keeps
;;;; timestamp, domain, status, the device that asked, the blocklists that
;;;; matched and the protocol, and a broad query is capped so it cannot swamp
;;;; the model's context. Every endpoint is a GET and nothing here writes.

(in-package #:nodecode-nextdns)

;;; --- reading a request -----------------------------------------------------------

(defun asked (value)
  "VALUE as trimmed text, the empty string for NIL."
  (nlk:trimmed (or value "")))

(defun profile-of (asked-profile)
  "The profile the call names, else the section's, else the empty string."
  (let ((given (asked asked-profile)))
    (if (plusp (length given)) given (asked (setting :profile-id)))))

(defun require-profile (profile)
  "PROFILE, or a refusal naming what to set."
  (when (zerop (length profile))
    (fail "no profile: set nextdns.profile_id in the section, or pass profile"))
  profile)

(defun limit-of (wanted ceiling)
  "The limit the API will take: WANTED raised to the floor and capped."
  (max +limit-floor+ (min (or wanted +limit-floor+) ceiling)))

(defun wire-string (object &rest path)
  "The string at PATH in OBJECT, or the empty string."
  (or (apply #'nlk:json-value object :string path) ""))

;;; --- a device name is not the id the API filters on -------------------------------

(defvar *devices* nil
  "PROFILE -> an EQUAL table of lowercased device name -> device id, for one
start. The log endpoint filters on ids, the logs are written with names, so a
name is resolved once and remembered.")

(defun reset-devices ()
  "Start the name table over: what START-CELL does, and what a stop discards."
  (setf *devices* (make-hash-table :test 'equal)))

(defun devices-of (profile)
  "PROFILE's device table, read from the analytics endpoint the first time."
  (unless *devices* (reset-devices))
  (or (gethash profile *devices*)
      (let ((table (make-hash-table :test 'equal)))
        (let ((json (ask! (format nil "/profiles/~a/analytics/devices" (pct-encode profile))
                          (list (cons "limit" "500")))))
          (loop for row across (nlk:json-array json "data")
                for name = (and (hash-table-p row) (nlk:json-value row :string "name"))
                for id = (and (hash-table-p row) (nlk:json-value row :string "id"))
                when (and name id)
                  do (setf (gethash (string-downcase name) table) id)))
        (setf (gethash profile *devices*) table))))

(defun device-id (profile wanted)
  "WANTED as the id the API wants: a name resolved through PROFILE's device
list, an id or __UNIDENTIFIED__ passed through, NIL for no filter."
  (let ((wanted (asked wanted)))
    (cond ((zerop (length wanted)) nil)
          ((and (>= (length wanted) 2) (string= "__" (subseq wanted 0 2))) wanted)
          (t (gethash (string-downcase wanted) (devices-of profile) wanted)))))

;;; --- a log row, trimmed to what triage needs ---------------------------------------

(defun device-label (entry)
  "The device a log row names, or `unidentified'."
  (let ((device (nlk:json-value entry :any "device")))
    (cond ((hash-table-p device)
           (or (nlk:json-value device :string "name")
               (nlk:json-value device :string "id")
               "unidentified"))
          ((and (stringp device) (plusp (length device))) device)
          (t "unidentified"))))

(defun reason-names (entry)
  "The blocklists that matched ENTRY, by name."
  (loop for reason across (nlk:json-array entry "reasons")
        for name = (and (hash-table-p reason) (nlk:json-value reason :string "name"))
        when name collect name))

(defun row-of (entry)
  "ENTRY as one row of the answer."
  (nlk:json-object
   "timestamp" (wire-string entry "timestamp")
   "domain" (wire-string entry "domain")
   "status" (or (nlk:json-value entry :string "status") "default")
   "device" (device-label entry)
   "reasons" (coerce (reason-names entry) 'vector)
   "protocol" (wire-string entry "protocol")
   "encrypted" (nlk:json-value entry :boolean "encrypted")))

(defun log-answer (profile search status device from to limit cursor raw)
  "One page of PROFILE's log, shaped. STATUS empty means no filter."
  (let ((profile (require-profile (profile-of profile))))
    (let* ((json (ask! (format nil "/profiles/~a/logs" (pct-encode profile))
                       (list (cons "limit" (limit-of limit +log-limit-ceiling+))
                             (cons "search" (asked search))
                             (cons "status" (let ((status (asked status)))
                                              (and (plusp (length status)) status)))
                             (cons "device" (device-id profile device))
                             (cons "from" (asked from))
                             (cons "to" (asked to))
                             (cons "cursor" (asked cursor))
                             ;; raw=true is every query, not only the
                             ;; navigational ones: triage wants the picture.
                             (cons "raw" (if raw "true" "false")))))
           (rows (loop for entry across (nlk:json-array json "data")
                       when (hash-table-p entry) collect (row-of entry)))
           (cap (setting :max-rows))
           (kept (if (> (length rows) cap) (subseq rows 0 cap) rows)))
      (nlk:json-object
       "profile" profile
       "count" (length kept)
       "cursor" (wire-string json "meta" "pagination" "cursor")
       "logs" (coerce kept 'vector)
       ;; A value the API did not send is written as JSON false rather than
       ;; left out, so a note is attached only when there is one.
       :opt "note" (when (> (length rows) cap)
                     (format nil "~d more row(s) came back than one answer carries; page with cursor, or raise nextdns.max_rows"
                             (- (length rows) cap)))))))

;;; --- the four tools ---------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's JSON text; every refusal becomes one ERROR line, never an unhandled error."
  `(handler-case (progn ,@body)
     (nextdns-error (condition)
       (nle:failure (format nil "ERROR: ~a" condition)))
     (error (condition)
       (nle:failure (format nil "ERROR: nextdns: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun logs (call)
  "The nextdns_logs tool: the DNS query log of one profile."
  (answering
    (nlk:encode-json-object
     (log-answer (nextdns-logs-profile call)
                  (nextdns-logs-search-term call)
                  (nextdns-logs-status call)
                  (nextdns-logs-device call)
                  (nextdns-logs-from call)
                  (nextdns-logs-to call)
                  (nextdns-logs-limit call)
                  (nextdns-logs-cursor call)
                  (nextdns-logs-raw call)))))

(defun blocked (call)
  "The nextdns_blocked tool: the same log, filtered to blocked queries, so the
blocklist that matched rides every row."
  (answering
    (nlk:encode-json-object
     (log-answer (nextdns-blocked-profile call)
                  (nextdns-blocked-search-term call)
                  "blocked"
                  (nextdns-blocked-device call)
                  (nextdns-blocked-from call)
                  (nextdns-blocked-to call)
                  (nextdns-blocked-limit call)
                  ""
                  t))))

(defun analytics (call)
  "The nextdns_analytics tool: aggregate counts for one metric."
  (answering
    (let* ((profile (require-profile (profile-of (nextdns-analytics-profile call))))
           (metric (let ((metric (asked (nextdns-analytics-metric call))))
                     (if (plusp (length metric)) metric "status")))
           (limit (limit-of (nextdns-analytics-limit call) +analytics-limit-ceiling+))
           (json (ask! (format nil "/profiles/~a/analytics/~a"
                                (pct-encode profile) (pct-encode metric))
                       (list (cons "limit" limit)
                             (cons "from" (asked (nextdns-analytics-from call)))
                             (cons "to" (asked (nextdns-analytics-to call)))
                             (cons "device" (device-id profile (nextdns-analytics-device call))))))
           (data (nlk:json-array json "data")))
      (nlk:encode-json-object
       (nlk:json-object
        "profile" profile
        "metric" metric
        "data" (if (> (length data) limit) (subseq data 0 limit) data))))))

(defun profile-settings (call)
  "The nextdns_profile tool: the profile's settings and the state of its log,
or -- with no profile configured -- the profiles the key can see."
  (answering
    (let ((profile (profile-of (nextdns-profile-profile call))))
      (nlk:encode-json-object
       (if (zerop (length profile))
           (let ((json (ask! "/profiles")))
             (nlk:json-object
              "note" "No default profile configured. Set nextdns.profile_id to one of these."
              "profiles" (coerce (loop for row across (nlk:json-array json "data")
                                        when (hash-table-p row)
                                          collect (nlk:json-object
                                                   "id" (wire-string row "id")
                                                   "name" (wire-string row "name")))
                                  'vector)))
           (let* ((json (ask! (format nil "/profiles/~a" (pct-encode profile))))
                  (data (or (nlk:json-value json :object "data") (nlk:json-object)))
                  (enabled (nlk:json-value data :boolean "settings" "logs" "enabled"))
                  (retention (nlk:json-value data :integer "settings" "logs" "retention")))
             (nlk:json-object
              "profile" profile
              "name" (wire-string data "name")
              "logging_enabled" enabled
              :opt "retention_seconds" retention
              :opt "retention_days" (and retention (/ (round (* retention 10) 86400) 10.0))
              "log_location" (wire-string data "settings" "logs" "location")
              "blocklists" (coerce (loop for entry across (nlk:json-array data "privacy" "blocklists")
                                          when (hash-table-p entry)
                                            collect (wire-string entry "id"))
                                    'vector)
              :opt "note" (unless enabled
                            "Logging is disabled for this profile; the log endpoints will return nothing."))))))))

;;; --- /nextdns ---------------------------------------------------------------------

(defun totals (profile)
  "PROFILE's status totals -- how many queries were default, blocked, relayed
-- as (STATUS . QUERIES) pairs. PROFILE NIL means the section's."
  (let* ((profile (require-profile (profile-of profile)))
         (json (ask! (format nil "/profiles/~a/analytics/status" (pct-encode profile))
                     (list (cons "limit" "100")))))
    (values profile
            (loop for row across (nlk:json-array json "data")
                  when (hash-table-p row)
                    collect (cons (or (nlk:json-value row :string "id") "unknown")
                                  (or (nlk:json-value row :integer "queries") 0))))))

(defun totals-text (profile rows)
  "TOTALS as the few lines an operator reads first."
  (with-output-to-string (out)
    (format out "nextdns: profile ~a~%" profile)
    (dolist (row rows)
      (format out "  ~a: ~d queries~%" (car row) (cdr row)))
    (format out "  total: ~d queries~%" (reduce #'+ rows :key #'cdr :initial-value 0))))

(defun run-command (args session-id)
  "One /nextdns invocation: today's query totals for the configured profile,
or for the profile id given as the argument."
  (declare (ignore session-id))
  (handler-case
      (let ((given (nlk:trimmed (or args ""))))
        (multiple-value-bind (profile rows)
            (totals (if (plusp (length given)) given nil))
          (totals-text profile rows)))
    (nextdns-error (condition) (format nil "nextdns: ~a" condition))))
