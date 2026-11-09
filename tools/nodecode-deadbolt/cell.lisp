;;;; cell.lisp --- two tools, /deadbolt, the manual, START-CELL.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of __init__.py and writer.py in
;;;; seanebones-lang/deadbolt-hermes-plugin (see NOTICE). The plugin's two
;;;; tools become the same two tools here, with the same names, arguments and
;;;; answers; the plugin's plugin-data directory becomes this cell's review
;;;; directory, and Hermes's profile secret becomes the section's one
;;;; credential.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "deadbolt": {"workspace": "/abs/out", "run_id": "report-001",
;;;;                "sidecar_port": 9782, "admission_token_env": "DEADBOLT_ADMISSION"}

(in-package #:nodecode-deadbolt)

(defparameter +manual+
  "DeadBolt gates ONE new text file on a person's approval, through a DeadBolt 2.0.0
sidecar running beside Nodecode on loopback. The model proposes, a person approves
outside the model, and only then is the file created.
  (deadbolt:prepare \"report.md\" \"the exact text\")
      Saves a private review request: the workspace, the path, the exact content, a nonce
      and a deadline. It grants nothing and creates nothing, and answers the tool's JSON:
      prepared, the request_id, the review_file and its expires_at. The operator reads
      that file with `deadbolt action inspect --file REVIEW-FILE' and approves the
      fingerprint they actually read, outside the agent.
  (deadbolt:commit REQUEST-ID)
      Asks the sidecar for exact admission of that request, then creates the one new
      file. A denial creates nothing. A write that fails after admission answers
      (:ADMITTED T :COMPLETED NIL :OUTCOME \"unknown\"): the operator inspects the
      destination, and the grant is not retried or recreated. The answer is the tool's
      JSON: admitted, completed, and what was written.
  (deadbolt:pending)
      The requests still waiting, newest first: one (ID FILE WRITTEN-AT) a row.
Only what this route creates is protected: an ordinary file tool, a shell, the
browser and MCP servers go around it, so keep Nodecode's own approvals on. It
creates new files only (never overwrite, patch or delete), the parent directory
must exist, and one request is at most 32768 bytes. macOS and Linux.
ERROR: DEADBOLT-ERROR names a refusal: the request is not this run's, a path
leaves the workspace, or the sidecar could not be reached."
  "What (help :deadbolt) answers while the cell runs.")

;;; --- the words of an answer --------------------------------------------------------

(defparameter +stop+
  "Stop. Ask the operator; do not fall back to another file tool."
  "What a denial tells the model.")

(defparameter +after-admission+
  "The operator must inspect the destination. Do not retry or recreate this grant ~
   automatically."
  "What a write that failed after admission tells the model.")

(defparameter +prepare-next+
  "The operator reviews this exact file with deadbolt action inspect, then approves ~
   the fingerprint they read, outside the agent. Preparation is not approval."
  "What a prepared request tells the model to wait for.")

(defun detail-text (condition)
  "CONDITION in one line, the cell's own detail when it is a refusal."
  (if (typep condition 'deadbolt-error)
      (deadbolt-error-detail condition)
      (nlk:one-line (princ-to-string condition) :cap 300)))

(defun prepare-answer (nonce file expires)
  "The answer deadbolt_prepare_write gives: what waits for a person, and where."
  (nlk:encode-json-object
   (nlk:json-object
    "prepared" t
    "request_id" nonce
    "review_file" (namestring file)
    "expires_at" expires
    "output_created" nil
    "next_action" +prepare-next+)))

(defun prepare-refusal (condition)
  "The answer a prepare that could not be saved gives: nothing was created."
  (nlk:encode-json-object
   (nlk:json-object
    "prepared" nil
    "output_created" nil
    "error" "invalid_request_or_configuration"
    "detail" (detail-text condition))))

(defun committed-answer (path bytes)
  "The answer of the one write that was admitted and completed."
  (nlk:encode-json-object
   (nlk:json-object
    "admitted" t
    "completed" t
    "result" (nlk:json-object "path" path "bytes" bytes))))

(defun denied-answer (code)
  "The answer of a denial: no output file exists."
  (nlk:encode-json-object
   (nlk:json-object
    "admitted" nil
    "completed" nil
    "decision" (nlk:json-object "decision" "deny" "code" code)
    "next_action" +stop+)))

(defun unknown-answer (condition)
  "The answer of a write that failed after admission: the grant is spent."
  (nlk:encode-json-object
   (nlk:json-object
    "admitted" t
    "completed" nil
    "outcome" "unknown"
    "detail" (detail-text condition)
    "next_action" +after-admission+)))

(defun unavailable-answer (condition)
  "The answer of a commit that never reached a decision."
  (nlk:encode-json-object
   (nlk:json-object
    "admitted" nil
    "completed" nil
    "error" "request_or_sidecar_unavailable"
    "detail" (detail-text condition))))

;;; --- the verbs --------------------------------------------------------------------

(defun prepare-request (path content)
  "Save one review request for CONTENT at PATH, inside the workspace."
  (let* ((root (workspace-root))
         (text (princ-to-string path)))
    (relative-parts text)
    (multiple-value-bind (envelope nonce)
        (action (run-id) (directory-text root) text content)
      (let ((file (request-file nonce)))
        (write-new-file file (encoded envelope))
        (prepare-answer nonce file (gethash "expires_at" envelope))))))

(define-verb prepare (path content)
  "Prepare a private review request for a new text file: PATH is relative to
  the workspace, CONTENT is the exact text a person is asked to approve. No
  permission is granted and no file is created. => the tool's JSON answer."
  (handler-case (prepare-request path content) (error (condition) (prepare-refusal condition))))

(define-verb commit (request-id)
  "Create the reviewed new text file REQUEST-ID names, but only on the
  sidecar's exact admission. => the tool's JSON answer."
  (handler-case
      (multiple-value-bind (envelope text) (load-request request-id)
        (multiple-value-bind (decision code) (admit text)
          (if (equal decision "allow")
              (handler-case
                  (multiple-value-bind (path bytes) (create-artifact envelope)
                    (committed-answer path bytes))
                (error (condition) (unknown-answer condition)))
              (denied-answer code))))
    (error (condition) (unavailable-answer condition))))

;;; --- what the two tools take --------------------------------------------------------

(nlk:define-contract deadbolt-write-request
  (:doc "Prepare a private exact-action review request for a NEW text file. This grants no permission and does not create the output file. Stop and ask the operator to review it; never approve it yourself.")
  ("path" :text :required t
   :doc "relative path inside the configured workspace; parent directories must exist")
  ("content" :string :required t
   :doc "exact UTF-8 content for operator review"))

(nlk:define-contract deadbolt-commit-request
  (:doc "Create the previously reviewed new text file only if DeadBolt grants exact admission. A denial creates no output. Never automatically retry an admitted/unknown outcome or grant approval through another tool.")
  ("request_id" :text :required t
   :doc "Request ID returned by deadbolt_prepare_write"))

(defun prepare-tool (record)
  "deadbolt_prepare_write: the declaration's members, checked, into PREPARE."
  (prepare (deadbolt-write-request-path record)
           (deadbolt-write-request-content record)))

(defun commit-tool (record)
  "deadbolt_commit_write: the one member, checked, into COMMIT."
  (commit (deadbolt-commit-request-request-id record)))

;;; --- /deadbolt ----------------------------------------------------------------------

(defun status-line ()
  "Where the workspace, the run and the credential stand, and what waits; no network."
  (let ((workspace (setting :workspace))
        (run (setting :run-id))
        (token (admission-token))
        (missing '()))
    (unless workspace (push "workspace" missing))
    (unless (and (stringp run) (plusp (length run))) (push "run_id" missing))
    (unless token (push "admission_token or admission_token_env" missing))
    (if missing
        (format nil "deadbolt: not set up - still needed: ~{~a~^, ~}; (help :deadbolt) says where each goes"
                (nreverse missing))
        (format nil "deadbolt: workspace ~a, run ~a, sidecar 127.0.0.1:~d, ~:[no credential~;credential set~]; ~d waiting in ~a"
                (directory-text workspace) run (setting :sidecar-port) token
                (length (pending)) (namestring (review-directory))))))

(defun pending-line (rows)
  "The waiting review requests, newest first: one ID and its file a line."
  (if (null rows)
      "deadbolt: no review request is waiting"
      (format nil "deadbolt: ~d waiting, newest first~%~{  ~a  ~a~^~%~}"
              (length rows)
              (loop for (id file) in rows
                    append (list id (namestring file))))))

(defun run-slash (args session-id)
  "/deadbolt: where the workspace, the run and the credential stand.
  /deadbolt pending: the review requests still waiting."
  (declare (ignore session-id))
  (let ((typed (string-downcase (nlk:trimmed (or args "")))))
    (cond ((zerop (length typed)) (status-line))
          ((string= typed "pending") (pending-line (pending)))
          (t (format nil "deadbolt: /deadbolt or /deadbolt pending, not ~s" typed)))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell deadbolt
  (:section ("deadbolt")
    (:guide "make a run and a scoped admission credential with the DeadBolt engine and start its sidecar on 127.0.0.1; then point workspace at an existing output directory, name the run, and name the credential here")
    (:one-of "admission_token" "admission_token_env")
    ("workspace" :directory :required t
     :doc "the existing absolute directory new text files go in")
    ("run_id" :string :required t
     :doc "the operator-provisioned DeadBolt run the credential is bound to")
    ("sidecar_port" :integer :default 9782 :min 1
     :doc "the DeadBolt sidecar's loopback TCP port")
    ("admission_token" :secret
     :doc "the run's scoped admission credential, written inline")
    ("admission_token_env" :env
     :doc "the environment variable holding the scoped admission credential")
    ("review_dir" :directory
     :doc "where pending review requests are kept; empty is deadbolt/requests under the nodecode home")
    ("request_ttl" :integer :default 300 :min 1
     :doc "seconds a prepared request stays valid")
    ("max_bytes" :integer :default 32768 :min 512
     :doc "the largest request envelope, in bytes")
    ("sidecar_timeout" :integer :default 5 :min 1
     :doc "seconds one admission call may take"))
  (:help :deadbolt
   "deadbolt:prepare and deadbolt:commit create one reviewed NEW text file through a local DeadBolt sidecar; deadbolt:pending lists what waits"
   +manual+)
  (:tool "deadbolt_prepare_write" (nlk:contract 'deadbolt-write-request) 'prepare-tool)
  (:tool "deadbolt_commit_write" (nlk:contract 'deadbolt-commit-request) 'commit-tool)
  (:command "deadbolt" 'run-slash
   :description "Where the DeadBolt workspace, run and credential stand"
   :argument-hint "pending"
   :session nil))
