;;;; cell.lisp --- the four verbs, the slash command, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An attempt is begun from a spec -- what would count as done, as files,
;;;; tests and pages -- and checked once: file and page checks are read now,
;;;; a test check runs the command it declares here, bounded, in its workdir.
;;;; What comes out is one sealed receipt, which says VERIFIED only when
;;;; every check passed, FAILED when one requirement did not hold, and
;;;; UNVERIFIED for evidence missing or unobtainable. A sealed attempt is
;;;; never rewritten: fix the work and begin a new attempt.

(in-package #:nodecode-receipts)

;;; --- the manual ---------------------------------------------------------------
;;; (help :receipts) answers it while the cell runs; every request carries the
;;; one line the :HELP clause gives, never this text.

(defparameter +manual+
  "Receipts record what actually passed: the model names, before the work,
what would count as done, and asks for a receipt after it. A receipt is an
immutable snapshot of explicit checks with timestamps, digests and evidence --
not a certification of the whole task.

A spec is JSON: schema_version 1, task_id (a short id, not a path),
workspace_root (an absolute folder), checks (1 to 20), and optionally
allow_loopback_http true for a check against a local server. Each check has an
id and a kind:
  file    path (workspace-relative), and either contains (text the file must
          hold) or json_keys (an array of keys the JSON object must carry).
  test    command, workdir (absolute), expected_exit (0 unless given),
          contains (optional output it must hold), timeout (seconds, 30).
  website url, expected_status, contains (the text the page must hold).

  (receipts:receipts-begin '(:spec ...))   opens an attempt; answer carries
      attempt_id and criteria_digest. One attempt is open per task id at a
      time, and the spec cannot change once begun.
  (receipts:receipts-check '(:attempt_id \"att_...\"))   reads the file and page
      checks, runs the test checks, and seals the receipt. Calling it again on
      a sealed attempt answers the same receipt.
  (receipts:receipts-get '(:task_id \"label\"))   every receipt kept for a task
      id, open or sealed, oldest first.
  (receipts:receipts-export '(:attempt_id ... :format \"html\"))   writes a page
      or a JSON document under the receipts folder and answers its filename.
      public, true unless given as false, drops paths, command lines and raw
      output.

A test check runs its own command: `/receipts TASK-ID [ATTEMPT-ID]` lists the
receipts of a task. The receipts live under the receipts/ folder of the
nodecode home unless the section names another; deleting them removes the
history."
  "What (help :receipts) answers while the cell runs.")

;;; --- what a tool call carries ----------------------------------------------------

(nlk:define-contract begin-request
  (:doc "Begin a receipt attempt from an explicit file, test, and website spec. Creates a new attempt id and criteria digest. Does not run commands.")
  ("spec" :object :required t
   :doc "schema_version 1, task_id, absolute workspace_root, and 1-20 file/test/website checks; allow_loopback_http opts a spec into loopback HTTP for a local demo."))

(nlk:define-contract check-request
  (:doc "Evaluate file and website checks now, run the declared test commands, and store an immutable receipt.")
  ("attempt_id" :text :required t :doc "Attempt id returned by receipts_begin."))

(nlk:define-contract get-request
  (:doc "Read persisted receipts for a task id. Does not certify anything new.")
  ("task_id" :text :required t :doc "Task id from the spec.")
  ("attempt_id" :string :default "" :doc "Optional attempt id."))

(nlk:define-contract export-request
  (:doc "Write a JSON or HTML export into the cell's own storage. The public export omits raw command output and absolute paths. No caller-chosen path.")
  ("attempt_id" :text :required t :doc "Sealed attempt id.")
  ("format" :text :default "json" :slot format-name :doc "json or html.")
  ("public" :boolean :default t :doc "default true: omit raw command output and absolute paths."))

;;; --- one shape of answer -----------------------------------------------------

(defun json-answer (thunk)
  "THUNK's JSON node as text; a refusal is an error document, never a signal
out of a tool call. A verb called with no cell running still signals, so it
says why."
  (receipts-directory)
  (handler-case (json-text (funcall thunk))
    (receipts-error (condition)
      (json-text (jobj "ok" :false "error" (princ-to-string condition))))
    (error (condition)
      (json-text (jobj "ok" :false
                       "error" (format nil "~a: ~a" (type-of condition) condition))))))

;;; --- begin ---------------------------------------------------------------------

(defun begin-attempt (spec)
  "Open an attempt on SPEC and answer the document the tool call returns."
  (let ((root (getf spec :workspace-root)))
    (unless (and (probe-file root) (uiop:directory-pathname-p (probe-file root)))
      (fail "workspace_root is not an existing directory: ~a" root))
    (let ((open (open-attempt-for (getf spec :task-id))))
      (when open
        (fail "an attempt is still open for task ~a (~a); check it, or begin a new task id"
              (getf spec :task-id) (getf open :id))))
    (let* ((attempt (list :id (new-attempt-id)
                          :task-id (getf spec :task-id)
                          :digest (criteria-digest spec)
                          :started-at (nlk:iso-now)
                          :spec spec))
           (saved (save-attempt attempt)))
      (jobj "ok" t
            "attempt_id" (getf saved :id)
            "task_id" (getf saved :task-id)
            "criteria_digest" (getf saved :digest)
            "started_at" (getf saved :started-at)
            "state" "open"
            "terminal_evidence" "a test check runs its command when checked"))))

(defun receipts-begin (request)
  "The receipts_begin tool: open an attempt on the spec REQUEST carries."
  (json-answer (lambda () (begin-attempt (normalize-spec (begin-request-spec request))))))

;;; --- check ---------------------------------------------------------------------

(defun check-row (check result)
  "One check and its RESULT as the receipt's row."
  (jobj "id" (getf check :id)
        "kind" (getf check :kind)
        "state" (getf result :state)
        "reason" (getf result :reason)
        "evidence" (getf result :evidence)
        "provenance" (getf result :provenance)))

(defun receipt-json (attempt results checked-at)
  "ATTEMPT's receipt, its checks built from RESULTS, sealed AT CHECKED-AT."
  (let ((spec (getf attempt :spec)))
    (jobj "schema_version" +schema-version+
          "task_id" (getf attempt :task-id)
          "attempt_id" (getf attempt :id)
          "criteria_digest" (getf attempt :digest)
          "started_at" (getf attempt :started-at)
          "checked_at" checked-at
          "state" (aggregate-state (mapcar (lambda (result) (getf result :state)) results))
          "checks" (jarr (loop for check in (getf spec :checks)
                                for result in results
                                collect (check-row check result)))
          "spec" (spec-json spec)
          "disclosures" (jarr +disclosures+))))

(defun check-attempt (attempt-id)
  "Evaluate ATTEMPT-ID's checks now and seal its receipt; a sealed attempt
answers the receipt it sealed."
  (let ((attempt (read-attempt attempt-id)))
    (unless attempt (fail "attempt not found: ~a" attempt-id))
    (when (getf attempt :receipt-text)
      (return-from check-attempt (getf attempt :receipt-text)))
    (let* ((results (evaluate-checks (getf attempt :spec)))
           (checked-at (nlk:iso-now))
           (receipt (receipt-json attempt results checked-at))
           (text (json-text receipt)))
      (setf (getf attempt :sealed-at) checked-at
            (getf attempt :receipt) receipt
            (getf attempt :receipt-text) text
            (getf attempt :state) (jstring receipt "state"))
      (save-attempt attempt)
      text)))

(defun receipts-check (request)
  "The receipts_check tool: evaluate and seal the attempt REQUEST names."
  (json-answer (lambda () (jraw (check-attempt (nlk:trimmed (check-request-attempt-id request)))))))

;;; --- get -----------------------------------------------------------------------

(defun open-summary (attempt)
  "An attempt not yet sealed, as the receipt GET shows it."
  (jobj "schema_version" +schema-version+
        "task_id" (getf attempt :task-id)
        "attempt_id" (getf attempt :id)
        "criteria_digest" (getf attempt :digest)
        "started_at" (getf attempt :started-at)
        "checked_at" +null+
        "state" "open"
        "spec" (spec-json (getf attempt :spec))))

(defun attempt-document (attempt)
  "ATTEMPT as GET carries it: its sealed receipt as it was sealed, else a
summary that says it is open."
  (if (getf attempt :receipt-text)
      (jraw (getf attempt :receipt-text))
      (open-summary attempt)))

(defun get-attempts (task-id attempt-id)
  "Every attempt kept for TASK-ID, narrowed to ATTEMPT-ID when one is named."
  (require-check-id task-id "task_id")
  (when (plusp (length attempt-id)) (require-check-id attempt-id "attempt_id"))
  (let* ((attempts (list-attempts task-id))
         (chosen (if (plusp (length attempt-id))
                     (remove-if-not (lambda (attempt) (equal attempt-id (getf attempt :id)))
                                    attempts)
                     attempts)))
    (jobj "ok" t
          "task_id" task-id
          "count" (length chosen)
          "attempts" (jarr (mapcar #'attempt-document chosen)))))

(defun receipts-get (request)
  "The receipts_get tool: every receipt kept for a task id."
  (json-answer (lambda ()
                  (get-attempts (nlk:trimmed (get-request-task-id request))
                                (nlk:trimmed (or (get-request-attempt-id request) ""))))))

;;; --- export --------------------------------------------------------------------

(defun export-attempt (attempt-id format public)
  "Write the sealed receipt ATTEMPT-ID as FORMAT -- json or html -- and
answer where it went. PUBLIC drops paths, command lines and raw output."
  (unless (member format '("json" "html") :test #'equal)
    (fail "format must be json or html"))
  (let ((attempt (read-attempt attempt-id)))
    (unless attempt (fail "sealed receipt not found: ~a" attempt-id))
    (unless (getf attempt :receipt-text) (fail "sealed receipt not found: ~a" attempt-id))
    (let* ((text (if (equal format "html")
                     (html-export (getf attempt :receipt) public)
                     (if public (json-export (getf attempt :receipt) t)
                         (getf attempt :receipt-text))))
           (path (write-export attempt-id format text)))
      (jobj "ok" t
            "attempt_id" attempt-id
            "format" format
            "filename" (file-namestring path)
            "bytes" (length (sb-ext:string-to-octets text :external-format :utf-8))
            "public" (if public t :false)))))

(defun receipts-export (request)
  "The receipts_export tool: write a sealed receipt out as json or html."
  (json-answer (lambda ()
                  (export-attempt (nlk:trimmed (export-request-attempt-id request))
                                  (string-downcase (or (export-request-format-name request) "json"))
                                  (export-request-public request)))))

;;; --- /receipts ------------------------------------------------------------------

(defun receipt-line (attempt)
  "One line per attempt, as the slash command lists them."
  (let ((receipt (getf attempt :receipt)))
    (format nil "- ~a ~a checked=~a"
            (getf attempt :id)
            (or (and receipt (jstring receipt "state")) "open")
            (or (getf attempt :sealed-at) "-"))))

(defun run-slash (args session-id)
  "The /receipts command: TASK-ID [ATTEMPT-ID] => that task's receipts."
  (declare (ignore session-id))
  (let ((parts (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))
                       :test #'equal)))
    (cond ((or (null parts) (> (length parts) 2))
           "usage: /receipts <task-id> [attempt-id]")
          (t (handler-case
                 (let* ((task (first parts))
                        (wanted (or (second parts) ""))
                        (attempts (list-attempts (require-check-id task "task_id")))
                        (chosen (if (plusp (length wanted))
                                    (remove-if-not (lambda (attempt)
                                                     (equal wanted (getf attempt :id)))
                                                   attempts)
                                    attempts)))
                   (with-output-to-string (out)
                     (format out "Receipts for ~a: ~d" task (length chosen))
                     (dolist (attempt chosen) (format out "~%~a" (receipt-line attempt)))))
               (receipts-error (condition) (princ-to-string condition)))))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell receipts
  (:section ("receipts")
    (:guide "directory is where the receipts are kept (empty is the nodecode home); read_cap_bytes bounds one file read or page; command_timeout bounds a test check")
    ("directory" :string :default ""
     :doc "folder for the receipts; empty means the receipts folder under the nodecode home")
    ("read_cap_bytes" :integer :default 1048576 :min 1024
     :doc "the most bytes one file read or page response may cover")
    ("command_timeout" :integer :default 60 :min 1
     :doc "the longest a test check may run, in seconds; a check's own timeout is capped by it"))
  (:tool "receipts_begin" (nlk:contract 'begin-request) 'receipts-begin)
  (:tool "receipts_check" (nlk:contract 'check-request) 'receipts-check)
  (:tool "receipts_get" (nlk:contract 'get-request) 'receipts-get)
  (:tool "receipts_export" (nlk:contract 'export-request) 'receipts-export)
  (:help :receipts
   "receipts: begin a spec of file, test and website checks, check it once, and keep the receipt - (help :receipts)"
   +manual+)
  (:command "receipts" 'run-slash
   :description "Receipts for a task: what passed, what failed"
   :argument-hint "TASK-ID [ATTEMPT-ID]"))
