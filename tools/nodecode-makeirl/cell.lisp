;;;; cell.lisp --- the manual, the section, the two tools, /makeirl.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verb through the manual: while the cell runs,
;;;; (help :makeirl) answers the release-gate workflow below, and every
;;;; session's help section carries one line naming it.
;;;;
;;;; Config, a sibling top-level key -- README.md has the table, and the
;;;; declaration at the tail of this file is the authority:
;;;;   "makeirl": {"access_token": "...", "timeout_seconds": 120}

(in-package #:nodecode-makeirl)

(defparameter +manual+
  "MakeIRL runs deterministic manufacturing checks on a KiCad 9 project and
answers BLOCKED, REVIEW or READY. It checks rules; it does not prove that the
circuit works, and it covers no firmware, RF, EMC, thermal behaviour or a
manufacturer's own DFM review -- say so whenever you report a verdict.
MakeIRL is reached through the nodecode-makeirl cell, which registers two
tools for you:
  makeirl_tools [filter]        the tools MakeIRL offers right now, with the
                                arguments each takes. Ask it before calling
                                anything: the list comes from the server and
                                changes without a new cell.
  makeirl_call tool, arguments  one of those tools, its arguments inside the
                                arguments object.
Through eval the same two are (makeirl:server-tools) and
(makeirl:call \"release_create_upload\" ((\"product\" . \"blinker\"))). Every
call goes to makeirl.com with the section's token, and uploading a release
sends the project files there.
The release gate: collect the KiCad project files (uploads use flat file
names, so subdirectories are not accepted); release_create_upload with the
product name or id and the base64 file contents answers a releaseId;
release_start queues the run and answers a runId at once; poll
release_status at a sensible interval, since the pipeline takes longer than a
normal request and a pending state is not a failure; release_list_findings
answers the findings, which you group by severity and explain one by one with
the file and the rule it came from. Fix the project in KiCad only when the
person asks, then upload a new release, because a report describes one exact
set of files.
Reports and payment: release_get_artifact_url answers links to the generated
outputs, and those links are private. release_create_quote and
release_create_checkout prepare a purchase: show the price first and hand the
checkout link to the person, and remember that payment happens in the browser,
so never say a report was paid for. generate_board drafts a board from a
description; treat that board as a starting point that still has to pass the
release gate, and never call it ready to fabricate before it has.
ERROR: MAKEIRL-ERROR names what went wrong; when it says no token, tell the
person to set the makeirl section's access_token (or the variable or file it
names) and then (restart-cells \"nodecode-makeirl\")."
  "What (help :makeirl) answers while the cell runs.")

;;; --- the two boundaries the tools cross ---------------------------------------

(nlk:define-contract makeirl-tools
  (:doc "List the tools MakeIRL's MCP server offers right now, with the
arguments each one takes. Ask this before makeirl_call: the list comes from
the server at run time.")
  ("filter" :text :doc "a word to keep only the tools whose name or description carries it; omit for every tool"))

(nlk:define-contract makeirl-call
  (:doc "Call one of MakeIRL's tools by name. KiCad release gate: upload the
project files, start the run, poll it, read its findings. makeirl_tools names
the tool and the arguments it takes.")
  ("tool" :text :required t :doc "the tool name makeirl_tools listed, for example release_create_upload")
  ("arguments" :object :doc "the arguments object that tool takes, exactly as makeirl_tools describes them; omit for none"))

;;; --- what a listing says -------------------------------------------------------

(defun one-line (text)
  "TEXT on one line, for a listing row."
  (let ((line (substitute #\Space #\Newline (or text ""))))
    (if (> (length line) 200) (concatenate 'string (subseq line 0 197) "...") line)))

(defun tool-arguments (tool)
  "TOOL's argument names and types, as one row."
  (let* ((schema (gethash "inputSchema" tool))
         (properties (and (hash-table-p schema) (gethash "properties" schema)))
         (required (and (hash-table-p schema) (gethash "required" schema)))
         (names '()))
    (when (hash-table-p properties)
      (maphash (lambda (name spec)
                 (push (format nil "~a~@[:~a~]~:[~;*~]"
                               name
                               (and (hash-table-p spec) (gethash "type" spec))
                               (and (vectorp required) (find name required :test #'equal)))
                       names))
               properties))
    (when names (format nil " (~{~a~^, ~})" (nreverse names)))))

(defun tool-hint (tool)
  "The read-only or write word TOOL's annotations carry, or NIL."
  (let ((annotations (gethash "annotations" tool)))
    (when (hash-table-p annotations)
      (cond ((gethash "readOnlyHint" annotations) "read-only")
            ((gethash "destructiveHint" annotations) "destructive")
            (t nil)))))

(defun tool-row (tool)
  "One line for TOOL: name, its arguments, its hint, and its description."
  (format nil "  ~a~a~@[ [~a]~]: ~a"
          (gethash "name" tool)
          (or (tool-arguments tool) "")
          (tool-hint tool)
          (one-line (gethash "description" tool))))

(defun tools-text (tools &optional filter)
  "TOOLS, the server's own objects, as the text the model reads, keeping only
the rows whose name or description carries FILTER."
  (let* ((filter (trimmed-text filter))
         (kept (if filter
                   (remove-if-not (lambda (tool)
                                    (and (hash-table-p tool)
                                         (or (search filter (or (gethash "name" tool) "")
                                                     :test #'char-equal)
                                             (search filter (or (gethash "description" tool) "")
                                                     :test #'char-equal))))
                                  tools)
                   (remove-if-not #'hash-table-p tools))))
    (if kept
        (format nil "MakeIRL offers ~d tool~:p~@[ matching ~s~]:~%~{~a~^~%~}~%Call one with makeirl_call."
                (length kept) filter (mapcar #'tool-row kept))
        (format nil "MakeIRL offers no tool~@[ matching ~s~] right now; the cell cannot be signed in, or the server names none."
                filter))))

;;; --- the two tools ------------------------------------------------------------

(defun tools-tool (request)
  "makeirl_tools: the server's tools, filtered when the model names a word."
  (tools-text (server-tools) (makeirl-tools-filter request)))

(defun call-tool (request)
  "makeirl_call: one of the server's tools, by name, with its arguments."
  (call (makeirl-call-tool request) (makeirl-call-arguments request)))

;;; --- /makeirl -----------------------------------------------------------------

(defun status-text ()
  "Where the cell points, and whether it can list anything."
  (handler-case
      (let ((tools (server-tools)))
        (format nil "makeirl: ~d tool~:p at ~a~%~{~a~^~%~}"
                (length tools) (endpoint) (mapcar (lambda (tool) (tool-row tool)) tools)))
    (makeirl-error (condition)
      (format nil "makeirl: ~a" condition))))

(defun makeirl-command (args session-id)
  "The /makeirl command: nothing for where it points and what the server
offers, `tools' for the same list, or a tool name and a JSON object of
arguments to call it."
  (declare (ignore session-id))
  (let* ((text (or (nlk:trimmed (or args "")) ""))
         (space (position #\Space text))
         (verb (if space (subseq text 0 space) text))
         (rest (and space (nlk:trimmed (subseq text (1+ space))))))
    (handler-case
        (cond ((member verb '("" "tools") :test #'string=)
               (if (equal verb "") (status-text) (tools-text (server-tools))))
              ((equal verb "call")
               (if rest
                   (makeirl-command rest nil)
                   "say which tool: /makeirl call release_status {\"releaseId\":\"...\"}"))
              (t (call verb (and rest (plusp (length rest)) rest))))
      (makeirl-error (condition) (format nil "ERROR: ~a" condition)))))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell makeirl
  (:section ("makeirl")
    (:guide "sign in at makeirl.com, then paste the access token here as access_token, or name an environment variable (token_env) or a 0600 file (token_file) holding it; every call and every uploaded project file goes to makeirl.com")
    (:any-of "access_token" "token_env" "token_file")
    ("access_token" :secret
     :doc "the MakeIRL access token, pasted here")
    ("token_env" :env
     :doc "the environment variable holding that token, e.g. MAKEIRL_TOKEN")
    ("token_file" :path
     :doc "or a file holding it, 0600")
    ("endpoint" :string :default "https://makeirl.com/api/mcp"
     :doc "MakeIRL's MCP address")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "seconds one call may wait; a release run is polled, never waited on"))
  (:tool "makeirl_tools" (nlk:contract 'makeirl-tools) 'tools-tool)
  (:tool "makeirl_call" (nlk:contract 'makeirl-call) 'call-tool)
  (:command "makeirl" 'makeirl-command
            :description "MakeIRL: check a KiCad 9 board before manufacturing"
            :argument-hint "tools | <tool> <json arguments>")
  (:help :makeirl "MakeIRL: ERC, DRC, BOM and export checks on a KiCad 9 board before manufacturing" +manual+)
  (:start (lambda ()
            (setf *session* nil)
            (nle:on-stop (lambda () (setf *session* nil)))
            (unless (token-set-p)
              (nle:notice "makeirl: no token yet - sign in at makeirl.com, then set the makeirl section's access_token (or token_env or token_file)"
                          :level :warning :key "nodecode-makeirl")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-makeirl")))))))
