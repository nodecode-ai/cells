;;;; cell.lisp --- an MCP server made into a portable skill: introspect, mine,
;;;; scaffold, validate, install, drift, review.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes plugin with nine mcp_creator_* tools, a
;;;; post_tool_call hook and a /mcp-creator command. This is the same nine
;;;; tools over the same data, with Nodecode's own seams: the mcp.servers
;;;; section of the shared config is where a server is configured, the call
;;;; log is written by a :tool hook, and the package it builds is a knowledge
;;;; cell (a prose define-skill) rather than a Hermes plugin folder.

(in-package #:nodecode-mcp-creator)

;;; --- the manual -------------------------------------------------------------

(defparameter +primer+
  "An MCP server's tools, written down as a skill a later session reads.
The tools are mcp_creator_*, called like any other; each answers JSON.
  servers      what this profile configures (mcp.servers in config.jsonc), and the builds already made
  introspect   connect once to a server, read its tools and their schemas, save the snapshot drift compares against
  mine         what the model actually called and what failed, from this cell's own call log
  convert      one mcp.servers entry as a portable mcp.json entry: ${VAR} secrets, and what a publish build must fix
  scaffold     write the knowledge cell for a server: skill.lisp, SKILL.md, mcp.json, manifest.json
  validate     check a build: the manifest, the system name, the skill, the fingerprint, private detail
  install      copy a build into ~/.nodecode/cells so the next boot loads it
  drift        the tools a server added, removed or changed since the build, and what to do
  review       drift plus usage since the build, and whether a refresh is worth it
A refusal is ERROR: <why>, and it names what to fix. Nothing leaves the
machine: introspection starts the server's own command, or posts to its url,
and every write lands under the data directory."
  "What (help :mcp-creator) answers while the cell runs.")

;;; --- the call log: what the model did with a server ----------------------------

(defun mcp-calls-in (form)
  "The tool names FORM names through the mcp package: (mcp:files/read
:path \"x\") yields files/read. An eval form is how a Nodecode session
reaches an MCP server, so this is where its usage is read."
  (let ((names '())
        (start 0))
    (loop
      for at = (search "mcp:" form :start2 start)
      while at
      do (let* ((from (+ at 4))
                (end (or (position-if (lambda (char)
                                        (member char '(#\Space #\( #\) #\" #\' #\Newline #\Tab)))
                                      form :start from)
                         (length form)))
                (name (subseq form from end)))
           (when (plusp (length name)) (push name names))
           (setf start (max (1+ at) end))))
    (remove-duplicates (nreverse names) :test #'equal)))

(defun observe-tool (op next)
  "A :TOOL point hook: OP is (:name TOOL-NAME :arguments HASH :call-id ID).
One line per MCP call the model made through eval, redacted, in this cell's
own log -- what mcp_creator_mine reads. Nothing to record answers exactly
what the tool did."
  (let ((result (funcall next op)))
    (handler-case
        (when (and (ignore-errors (setting :log-calls))
                   (equal (getf op :name) "eval")
                   (hash-table-p (getf op :arguments)))
          (let ((form (nlk:json-value (getf op :arguments) :string "form")))
            (dolist (name (and form (mcp-calls-in form)))
              (note-call name
                         :ok (not (and (stringp result)
                                       (uiop:string-prefix-p "ERROR:" (string-left-trim '(#\Space) result))))
                         :detail (and (stringp result) (nlk:one-line result :cap 200))))))
      (error () nil))
    result))

;;; --- the command -------------------------------------------------------------

(defun describe-build (build)
  (let* ((manifest (getf build :manifest))
         (name (member-text manifest "name")))
    (format nil "~a (server ~a, ~a, ~a tool~:p)"
            name (member-text manifest "server")
            (or (member-text manifest "built") "never")
            (let ((tools (field manifest "tools")))
              (if (vectorp tools) (length tools) 0)))))

(defun run-mcp-creator (args session-id)
  "One /mcp-creator invocation => the answer's text, and typed alone the
dialog every command answers then: a row per server and per build."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (head (string-downcase (or (first words) "")))
         (rest (format nil "~{~a~^ ~}" (rest words))))
    (cond
      ((string= head "introspect")
       (if (zerop (length rest))
           (values "mcp-creator: introspect needs a server name" (servers-panel))
           (values (format nil "mcp-creator: ~a"
                           (run-introspect (decode-introspect-arguments
                                            (nlk:json-object "server" rest))))
                   (servers-panel))))
      ((string= head "drift")
       (values (format nil "mcp-creator: ~a"
                       (run-drift (decode-drift-arguments (nlk:json-object "server" rest))))
               (builds-panel)))
      ((string= head "review")
       (values (format nil "mcp-creator: ~a"
                       (run-review (decode-review-arguments (nlk:json-object "plugin" rest))))
               (builds-panel)))
      ((member head '("" "status") :test #'string=) (values (status-line) (servers-panel)))
      (t (values (format nil "mcp-creator: ~a is not one of its words (status, introspect, drift, review)"
                         head)
                 (servers-panel))))))

(defun status-line ()
  (let ((servers (server-names))
        (builds (build-folders)))
    (format nil "mcp-creator: ~d server~:p configured~@[, ~d build~:p~]~@[ -- ~{~a~^, ~}~]"
            (length servers) (length builds)
            (mapcar (lambda (build) (member-text (getf build :manifest) "name")) builds))))

(defun servers-panel ()
  (nle:list-dialog
   :title "MCP creator" :summary (status-line)
   :rows (loop for name in (server-names)
               collect (nle:run-row name (format nil "/mcp-creator introspect ~a" name)
                                     :detail (launch-command-text (server-entry name)) :mark "" ))
   :empty-label "No MCP servers configured: mcp.servers in config.jsonc"))

(defun builds-panel ()
  (nle:list-dialog
   :title "MCP creator - builds" :summary (status-line)
   :rows (loop for build in (build-folders)
               collect (nle:run-row (member-text (getf build :manifest) "name")
                                    (format nil "/mcp-creator review ~a"
                                            (member-text (getf build :manifest) "name"))
                                    :detail (describe-build build) :mark ""))
   :empty-label "Nothing built yet: mcp_creator_scaffold writes the first one"))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell mcp-creator
  (:section ("mcp-creator")
    (:guide "no key is needed; introspection starts the servers mcp.servers already names. install_dir defaults to this home's cells folder; log_calls records what the model calls")
    ("data_dir" :path :doc "where snapshots, builds and calls.jsonl live (default: this home's mcp-creator folder)")
    ("install_dir" :path :doc "where install copies a build (default: this home's cells folder)")
    ("timeout_seconds" :integer :default 60 :min 1 :doc "one exchange with a server, in seconds")
    ("max_desc" :integer :default 220 :min 40 :doc "characters kept per tool description")
    ("log_calls" :boolean :default t :doc "record the MCP calls the model makes, redacted and local, for usage mining"))
  (:tool "mcp_creator_servers" (nlk:contract 'servers-arguments) 'run-servers)
  (:tool "mcp_creator_introspect" (nlk:contract 'introspect-arguments) 'run-introspect)
  (:tool "mcp_creator_mine" (nlk:contract 'mine-arguments) 'run-mine)
  (:tool "mcp_creator_convert" (nlk:contract 'convert-arguments) 'run-convert)
  (:tool "mcp_creator_scaffold" (nlk:contract 'scaffold-arguments) 'run-scaffold)
  (:tool "mcp_creator_validate" (nlk:contract 'validate-arguments) 'run-validate)
  (:tool "mcp_creator_install" (nlk:contract 'install-arguments) 'run-install)
  (:tool "mcp_creator_drift" (nlk:contract 'drift-arguments) 'run-drift)
  (:tool "mcp_creator_review" (nlk:contract 'review-arguments) 'run-review)
  (:command "mcp-creator" 'run-mcp-creator
            :description "MCP servers: what is configured, what was built, and drift since"
            :argument-hint "STATUS | INTROSPECT SERVER | DRIFT SERVER | REVIEW PLUGIN")
  (:help :mcp-creator "mcp-creator: an MCP server's tools as a skill a later session reads - (mcp-creator:drift \"files\")" +primer+)
  (:hook :tool #'observe-tool)
  (:start (lambda ()
            (let* ((shared (ignore-errors (nle:read-shared-config)))
                   (mcp (and (hash-table-p shared) (gethash "mcp" shared))))
              (setf *config* shared
                    *servers* (or (and (hash-table-p mcp) (gethash "servers" mcp))
                                   (ignore-errors (nle:config-get '("mcp" "servers")))))
              (setf *data-dir* (uiop:ensure-directory-pathname
                                (or (ignore-errors (setting :data-dir))
                                    (nlk:home "mcp-creator"))))
              (ensure-directories-exist (merge-pathnames "x" (builds-root)))
              (nle:on-stop (lambda ()
                             (setf *config* nil *servers* nil *data-dir* nil)))))))
