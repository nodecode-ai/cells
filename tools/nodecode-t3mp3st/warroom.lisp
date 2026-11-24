;;;; warroom.lisp --- the War Room seam: one HTTP call, and the five verbs behind the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; T3MP3ST is a separate program under its own licence (AGPL-3.0; see
;;;; NOTICE). This cell copies none of it. It speaks to the War Room that
;;;; program serves on loopback -- the same JSON its own UI reads -- so the
;;;; model can start a mission, watch it, read its findings and clear its
;;;; approval gates. Every verb below answers text, because that is what the
;;;; model reads; nothing here holds state, so a restart of either side
;;;; changes nothing a person has to reconcile.

(in-package #:nodecode-t3mp3st)

(nlk:define-peripheral t3mp3st :not-running t)

(defparameter +default-url+ "http://127.0.0.1:3333"
  "Where the War Room answers when the section names nothing else.")

(defparameter +user-agent+ "nodecode-t3mp3st (a Nodecode cell, over the War Room's own HTTP API)"
  "What this cell calls itself on the wire.")

(defparameter +max-bytes+ (* 2 1024 1024)
  "How much of one answer is read: a findings list is the biggest thing here.")

(defparameter +max-rows+ 25
  "Rows a list shows before it says how many it left out.")

(defparameter +controls+ '("stop" "pause" "resume")
  "The mission verbs the War Room serves, each at its own path.")

;;; --- the one network call ----------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub this name."
  (apply #'nlk:http method url arguments))

(defun base-url ()
  "The War Room's root: the section's url, without a trailing slash."
  (let* ((raw (setting :url))
         (text (nlk:trimmed (if (stringp raw) raw ""))))
    (if (plusp (length text))
        (string-right-trim "/" text)
        +default-url+)))

(defun unroutable ()
  "The refusal when nothing answers at the War Room's address."
  (fail "no T3MP3ST War Room answers at ~a. In the T3MP3ST checkout run `npm run server` -- its UI then opens at ~a/ui/ -- and ask again; if it listens elsewhere, set url in the t3mp3st section of ~~/.nodecode/config.jsonc and run (restart-cells \"nodecode-t3mp3st\")."
        (base-url) (base-url)))

(defun body-text (body)
  "BODY -- a string, or the octets MAX-BYTES answers with -- as text."
  (cond ((stringp body) body)
        ((and (vectorp body) (not (stringp body)))
         (or (ignore-errors (sb-ext:octets-to-string body :external-format :utf-8)) ""))
        (t "")))

(defun decode-body (body)
  "BODY as decoded JSON, or NIL when it is not JSON."
  (let ((text (body-text body)))
    (when (plusp (length text))
      (ignore-errors (nlk:decode-json text :whole t)))))

(defun call (method path &key body)
  "One War Room round trip => (values JSON STATUS)."
  (multiple-value-bind (payload status)
      (handler-case
          (http method (concatenate 'string (base-url) path)
                :headers (list (cons "accept" "application/json")
                               (cons "content-type" "application/json")
                               (cons "user-agent" +user-agent+))
                :content (and body (nlk:encode-json-object body))
                :timeout (setting :timeout-seconds)
                :max-bytes +max-bytes+)
        (nlk:turn-cancelled-condition (condition) (error condition))
        (error () (unroutable)))
    (values (decode-body payload) status)))

(defun answered-p (status)
  "Whether STATUS is one a caller can read."
  (and (integerp status) (<= 200 status) (< status 300)))

(defun error-text (json)
  "What the War Room said went wrong, in one line."
  (or (nlk:json-value json :text "error")
      (nlk:json-value json :text "message")
      "no message"))

;;; --- reading JSON the War Room answers ---------------------------------------------

(defun one-line-value (value)
  "VALUE -- any decoded JSON value -- as one line of text."
  ;; The War Room's own shapes move between releases, so the readers below
  ;; never assume one: an object is read for the words they name, and
  ;; whatever else it carries is shown as key=value, bounded, rather than
  ;; dropped.
  (cond ((eq value t) "true")
        ((null value) "none")
        ((stringp value) value)
        ((integerp value) (princ-to-string value))
        ((realp value) (format nil "~,2f" value))
        ((vectorp value) (format nil "~a item~:p" (length value)))
        ((hash-table-p value)
         (let ((parts '()) (shown 0))
           (maphash (lambda (key item)
                      (when (< shown 6)
                        (push (format nil "~a=~a" key (one-line-value item)) parts)
                        (incf shown)))
                    value)
           (format nil "{~{~a~^, ~}}" (nreverse parts))))
        (t (princ-to-string value))))

(defun listed (rows)
  "ROWS as up to +MAX-ROWS+ indented lines, then the count of what is left."
  (with-output-to-string (out)
    (let ((shown 0))
      (dolist (row rows)
        (when (< shown +max-rows+)
          (format out "  ~a~%" row))
        (incf shown))
      (when (> shown +max-rows+)
        (format out "  ... and ~a more~%" (- shown +max-rows+))))))

(defun percent-text (progress)
  "PROGRESS as a percentage: the War Room may count in 0..1 or 0..100."
  (cond ((not (realp progress)) nil)
        ((<= progress 1) (format nil "~d%" (round (* 100 progress))))
        (t (format nil "~d%" (round progress)))))

(defun targets-text (targets)
  "TARGETS -- a decoded array -- as one comma-separated line, or NIL."
  (when (plusp (length targets))
    (format nil "~{~a~^, ~}" (loop for target across targets collect (one-line-value target)))))

(defun operators-text (operators)
  "The operator counts, when the War Room's summary carries them."
  (let ((summary (and operators (nlk:json-value operators :any "summary"))))
    (cond ((null summary) nil)
          ((hash-table-p summary)
           (let ((total (nlk:json-value summary :integer "total")))
             (if total
                 (format nil "~a total, ~a available, ~a busy"
                         total
                         (or (nlk:json-value summary :integer "available") 0)
                         (or (nlk:json-value summary :integer "busy") 0))
                 (one-line-value summary))))
          (t (one-line-value summary)))))

;;; --- what a mission looks like in words --------------------------------------------

(defun render-status (json)
  "The War Room's mission state as the block the model reads."
  (let* ((active (nlk:json-value json :boolean "active"))
         (paused (nlk:json-value json :boolean "paused"))
         (stall (nlk:json-value json :text "stallReason"))
         (mission (nlk:json-value json :object "mission"))
         (name (or (nlk:json-value mission :text "name")
                   (nlk:json-value json :text "name")))
         ;; The server's own ACTIVE flag is the truth; a stopped mission's
         ;; object keeps whatever status it had, so its word is quoted, not
         ;; trusted.
         (reported (nlk:json-value mission :text "status"))
         (state (cond (active (if paused "paused" "running"))
                      ((null mission) nil)
                      (t "not running")))
         (phase (nlk:json-value mission :text "currentPhase"))
         (progress (percent-text (nlk:json-value mission :number "progress")))
         (tick (nlk:json-value json :integer "tickCount"))
         (targets (targets-text (nlk:json-array json "targets")))
         (operators (nlk:json-value json :object "operators"))
         (summary (operators-text operators)))
    (with-output-to-string (out)
      (format out "T3MP3ST War Room at ~a~%" (base-url))
      (if (and (null active) (null mission))
          (format out "  no mission: the server is up and idle~%")
          (progn
            (format out "  mission: ~a~@[ (~a)~]~%" (or name "unnamed") state)
            (when (and (not active) reported (not (string-equal reported "running")))
              (format out "  the War Room's last status for it: ~a~%" reported))
            (when phase (format out "  phase: ~a~%" phase))
            (when progress (format out "  progress: ~a~%" progress))
            (when tick (format out "  ticks: ~a~%" tick))
            (when stall (format out "  stalled: ~a~%" stall))))
      (when targets (format out "  targets: ~a~%" targets))
      (when summary (format out "  operators: ~a~%" summary)))))

(defun mission-id (json)
  "The mission's id, wherever this release of the War Room puts it."
  (or (nlk:json-value json :text "missionId")
      (nlk:json-value json :string "mission" "id")
      (nlk:json-value json :text "id")))

(defun render-launched (targets json)
  "A mission the War Room took."
  (format nil "started~@[ mission ~a~]~%  targets: ~{~a~^, ~}~%  watch it with t3mp3st_status; a gate it hits shows in t3mp3st_approvals."
          (mission-id json) targets))

(defun render-blocked (targets json)
  "A mission an approval gate is holding: the War Room answers this instead of starting."
  ;; The War Room answers a gated launch with HTTP 403 and this body, not with a
  ;; success: the mission exists as a request, nothing has run. So it is read
  ;; before the status is judged, and said as a gate rather than a refusal.
  (let* ((approval (nlk:json-value json :object "approval"))
         (id (or (nlk:json-value approval :text "id") (nlk:json-value json :text "approvalId")))
         (action (nlk:json-value approval :text "action"))
         (target (nlk:json-value approval :text "target"))
         (reason (nlk:json-value approval :text "reason"))
         (next (nlk:json-value json :text "next")))
    (with-output-to-string (out)
      (format out "the mission has NOT started: an approval gate is holding it~%")
      (format out "  targets: ~{~a~^, ~}~%" targets)
      (format out "  gate: ~a~@[ (~a)~]~@[ for ~a~]~%" (or id "unnamed") action target)
      (when reason (format out "  reason: ~a~%" reason))
      (format out "  clear the gate with t3mp3st_approvals action=approve id=~a, then launch again with approval_id=~a. An approval clears the gate for a start; it does not start the mission itself.~%"
              (or id "the gate's id") (or id "the gate's id"))
      (when next (format out "  the War Room's own next step: ~a~%" next)))))

;;; --- the verbs -----------------------------------------------------------------------

(defun status ()
  "What the War Room is doing right now, as text."
  (multiple-value-bind (json code) (call "GET" "/api/mission/status")
    (unless (answered-p code)
      (fail "the War Room answered HTTP ~a: ~a" code (error-text json)))
    (render-status json)))

(defun split-targets (text)
  "TEXT as a list of targets, split on commas and whitespace."
  (remove-if (lambda (item) (zerop (length item)))
             (cl-ppcre:split "[,\\s]+" (or text ""))))

(defun default-mission-name ()
  "A name for a mission nobody named: the minute it started."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second))
    (format nil "Operation ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun launch (targets &key name provider model repo-path approval-id)
  "Start a mission against TARGETS, a string of one or more of them."
  ;; APPROVAL-ID is the gate a previous attempt was held at. The War Room runs
  ;; nothing until a start names one: an approval clears the gate for a start,
  ;; it does not start the mission itself -- so the flow is launch (a 403 with
  ;; a gate), approve, launch again with that id.
  (let ((list (split-targets targets)))
    (when (null list)
      (fail "a mission needs at least one target -- a host, a URL or a CIDR block, as \"10.20.0.5\" or \"app.example, 10.20.0.5\""))
    (let* ((given (nlk:trimmed (or name "")))
           (body (nlk:make-json-object
                  "name" (if (plusp (length given)) given (default-mission-name))
                  "targets" (coerce list 'vector))))
      (flet ((add (key value)
               (let ((text (nlk:trimmed (or value ""))))
                 (when (plusp (length text))
                   (setf (gethash key body) text)))))
        (add "provider" provider)
        (add "model" model)
        (add "repoPath" repo-path)
        (add "approvalId" approval-id))
      (multiple-value-bind (json code) (call "POST" "/api/mission/start" :body body)
        (cond ((or (nlk:json-value json :object "approval")
                   (nlk:json-value json :boolean "blocked"))
               (render-blocked list json))
              ((not (and (integerp code) (< code 400)))
               (fail "the War Room refused the mission (HTTP ~a): ~a" code (error-text json)))
              (t (render-launched list json)))))))

(defun control (action)
  "Stop, pause or resume the running mission."
  (let ((verb (string-downcase (nlk:trimmed (or action "")))))
    (unless (member verb +controls+ :test #'string=)
      (fail "control takes one of ~{~a~^, ~}; it was given ~s" +controls+ action))
    (multiple-value-bind (json code)
        (call "POST" (format nil "/api/mission/~a" verb) :body (nlk:make-json-object))
      (cond ((answered-p code)
             (format nil "~a accepted.~%  ~a" verb (one-line-value json)))
            ;; A verb with nothing to act on is a state, not a failure: the War
            ;; Room answers 404 'No active mission' and means the run is over.
            ((and (integerp code) (= code 404))
             (format nil "~a: no mission is running, so there was nothing to ~a.~%" verb verb))
            (t (fail "the War Room answered HTTP ~a to ~a: ~a" code verb (error-text json)))))))

(defun findings (&key limit)
  "What the mission has found: the vault's findings, and the credentials it records."
  (multiple-value-bind (json code) (call "GET" "/api/mission/findings")
    (unless (answered-p code)
      (fail "the War Room answered HTTP ~a: ~a" code (error-text json)))
    (let* ((items (nlk:json-array json "findings"))
           (credentials (nlk:json-array json "credentials"))
           (rows (loop for item across items collect (one-line-value item)))
           (shown (if (and limit (plusp limit)) (subseq rows 0 (min limit (length rows))) rows)))
      (with-output-to-string (out)
        (format out "~a finding~:p in the mission's vault~%" (length items))
        (write-string (listed shown) out)
        (when (plusp (length credentials))
          (format out "~a credential record~:p (metadata only -- the War Room redacts the secrets)~%"
                  (length credentials)))))))

(defun approval-line (item)
  "One approval request as a line: its id, what it asks for, and its state."
  (format nil "~a  ~a -> ~a  [~a]"
          (or (nlk:json-value item :text "id") "?")
          (or (nlk:json-value item :text "action") "action")
          (or (nlk:json-value item :text "target") "target")
          (or (nlk:json-value item :text "status") "pending")))

(defun url-piece (text)
  "TEXT safe inside one path segment."
  (with-output-to-string (out)
    (loop for character across text
          do (if (or (alphanumericp character) (member character '(#\- #\_ #\. #\~)))
                 (write-char character out)
                 (format out "%~2,'0X" (char-code character))))))

(defun list-approvals ()
  "The approval requests the War Room holds, as text."
  (multiple-value-bind (json code) (call "GET" "/api/approvals")
    (unless (answered-p code)
      (fail "the War Room answered HTTP ~a: ~a" code (error-text json)))
    (let ((items (nlk:json-array json "approvals")))
      (with-output-to-string (out)
        (format out "~a approval request~:p~%" (length items))
        (write-string (listed (loop for item across items collect (approval-line item))) out)
        (when (plusp (length items))
          (format out "clear one with t3mp3st_approvals action=approve id=<its id>; an approval expires, it does not stand.~%"))))))

(defun approvals (&key action id ttl-minutes)
  "List the approval gates, or clear one by id."
  (let ((verb (string-downcase (nlk:trimmed (or action "")))))
    (cond
      ((member verb '("" "list") :test #'string=) (list-approvals))
      ((member verb '("approve" "reject") :test #'string=)
       (let ((which (nlk:trimmed (or id ""))))
         (when (zerop (length which))
           (fail "~a needs the gate's id; t3mp3st_approvals with no action lists them" verb))
         (multiple-value-bind (json code)
             (call "POST" (format nil "/api/approvals/~a/~a" (url-piece which) verb)
                   :body (if (string= verb "approve")
                             (nlk:make-json-object "ttlMinutes" (or ttl-minutes 30)
                                                    "approvedBy" "nodecode")
                             (nlk:make-json-object)))
           (unless (answered-p code)
             (fail "the War Room answered HTTP ~a to ~a: ~a" code verb (error-text json)))
           (format nil "~a: ~a" verb (one-line-value json)))))
      (t (fail "action takes list, approve or reject; it was given ~s" action)))))
