;;;; cell.lisp --- the tools, the section, /super-artifacts, and (help :super-artifacts).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Super-Artifacts/plugins (see NOTICE). The original was a
;;;; portable package: an MCP server entry and one skill. Here the server's
;;;; tools are the cell's own -- each one a tools/call over the same transport
;;;; -- the skill is the manual topic below, and the sign-in the host used to
;;;; offer is /super-artifacts signin.

(in-package #:nodecode-super-artifacts)

;;; --- what the model is told -------------------------------------------------------

(defparameter +primer+
  "Super Artifacts publishes a page, a form or a tracker as a live private URL that opens on a
phone, and reads back what visitors submitted.
  super_whoami {}                                             -> which account it publishes as
  super_publish {slug, title, html, visibility?}               -> the URL it went live at
  super_data {artifact}                                        -> the rows visitors submitted
  super_artifacts {artifact?, open?}                           -> what is published, or one of them
  super_tool {tool, arguments?}                                -> any other tool the server offers
Call super_whoami first and tell the person, once, which account it names. Publish with
visibility \"private\" and never widen who can open an artifact -- a share, a public visibility, a
wider project -- unless the person asked for that artifact in this conversation. The same slug
publishes a new version at the same URL: say the URL on a line of its own every time, and keep one
stable slug per subject so each update is a version. Report the rows that are there; never invent
one. When the server refuses a call as unauthorised, the person has to sign in: /super-artifacts
signin prints a code to approve, and needs no config edit from the model.
The other tools the server offers -- super_upload for images and video, super_templates for a page
to start from, super_comments, super_data_query, super_share, super_help for the rest -- are all
reached through super_tool, by their own names."
  "What (help :super-artifacts) answers while the cell runs: the original's skill, in the cell's
voice.")

;;; --- the tools' schemas -----------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:make-json-object "type" type
                                                                           "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *whoami-schema*
  (tool-schema "Name the Super Artifacts account this cell would publish to. Call it first, and tell the person which account it names."
                '())
  "The super_whoami tool.")

(defparameter *publish-schema*
  (tool-schema "Publish an HTML page, form or tracker as a live private URL and answer that URL. The same slug publishes a new version at the same URL."
                '(("slug" "string" "the stable name of this artifact, one per subject" t)
                  ("title" "string" "the title the page and the gallery show" t)
                  ("html" "string" "the complete standalone HTML of the page" t)
                  ("visibility" "string" "\"private\" (default) or \"public\"; keep it private unless asked")))
  "The super_publish tool.")

(defparameter *data-schema*
  (tool-schema "Read what visitors submitted to one artifact's form, as rows. Report what is there; never invent a row."
                '(("artifact" "string" "the slug or URL of the artifact" t)
                  ("limit" "integer" "how many rows at most")))
  "The super_data tool.")

(defparameter *artifacts-schema*
  (tool-schema "List the artifacts this account has published, or read one of them."
                '(("artifact" "string" "the slug or URL of one artifact to read")
                  ("open" "boolean" "read a private artifact without changing who can open it")))
  "The super_artifacts tool.")

(defparameter *tool-schema*
  (tool-schema "Call any other tool the Super Artifacts server offers, by its own name: super_upload, super_templates, super_comments, super_data_query, super_share, super_artifact_set, super_help and the rest."
                '(("tool" "string" "the server's own tool name, e.g. super_templates" t)
                  ("arguments" "object" "the arguments that tool takes, as an object")))
  "The super_tool passthrough.")

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: super-artifacts: ~a" condition)))))

(defun argument (args name)
  (and (hash-table-p args) (gethash name args)))

(defun present (value)
  "VALUE when the wire sent it, NIL when it is absent or JSON null."
  (and (not (eq value :null)) (not (null value)) value))

(defun forwarded (args names)
  "ARGS with only NAMES kept, as the argument object a tools/call carries."
  (apply #'nlk:make-json-object
         (loop for name in names
               for value = (present (argument args name))
               when value append (list name value))))

(defun text-argument (args name &key (required t))
  "ARGS's NAME as trimmed non-empty text, or a refusal naming it."
  (let ((value (present (argument args name))))
    (cond ((and (stringp value) (plusp (length (nlk:trimmed value)))) (nlk:trimmed value))
          (required (fail "~a is required" name))
          (t nil))))

(defun tool-whoami (args)
  "super_whoami: the account the server publishes to."
  (declare (ignore args))
  (answering (call-tool "super_whoami" nil)))

(defun tool-publish (args)
  "super_publish: the URL the page went live at."
  (answering
    (let ((slug (text-argument args "slug"))
          (title (text-argument args "title"))
          (html (text-argument args "html"))
          (visibility (or (text-argument args "visibility" :required nil) "private")))
      (when (string= (nlk:trimmed html) "") (fail "html is required"))
      (format nil "~a~%~%slug ~a, ~a. Publish the same slug again to make a new version at the same URL."
              (call-tool "super_publish"
                         (nlk:make-json-object "slug" slug "title" title
                                                "html" html "visibility" visibility))
              slug visibility))))

(defun tool-data (args)
  "super_data: the rows one artifact collected."
  (answering (call-tool "super_data" (forwarded args '("artifact" "limit")))))

(defun tool-artifacts (args)
  "super_artifacts: what is published, or one artifact."
  (answering (call-tool "super_artifacts" (forwarded args '("artifact" "open")))))

(defun tool-call-any (args)
  "super_tool: any tool the server offers, by its own name."
  (answering
    (let ((tool (text-argument args "tool")))
      (unless (uiop:string-prefix-p "super_" tool)
        (fail "tool must be one of the server's own names, which start super_: ~s" tool))
      (call-tool tool (present (argument args "arguments"))))))

;;; --- the operator's command ------------------------------------------------------

(defun run-command (args session-id)
  "One /super-artifacts invocation: nothing or `status' for what is ready,
`signin' to approve a code, `cancel' to stop waiting."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab)))
             (action (string-downcase (or (first words) ""))))
        (cond
          ((member action '("" "status") :test #'string=) (status-line))
          ((member action '("signin" "sign-in" "login") :test #'string=) (begin-signin))
          ((string= action "cancel") (cancel-signin))
          (t (format nil "/super-artifacts takes signin, status or cancel; it read ~s"
                     (nlk:one-line (or args ""))))))
    (super-artifacts-error (condition) (format nil "/super-artifacts: ~a" condition))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell super-artifacts
  (:section ("super-artifacts")
    (:guide "leave base_url as it is; run /super-artifacts signin and approve the code, or set key_env to the variable holding a key from the dashboard; /super-artifacts status says what is ready; then ask the model to publish a page or read one back")
    ("base_url" :string :default "https://superartifacts.app/mcp"
     :doc "the Super Artifacts MCP server (https, or http on loopback)")
    ("key_env" :env :default "SUPER_ARTIFACTS_KEY"
     :doc "the environment variable holding an API key, when no sign-in has saved a token")
    ("key_file" :path
     :doc "a file holding an API key; read before the variable, and before a saved token")
    ("state_dir" :path
     :doc "the folder holding a signed-in token: <home>/super-artifacts/ unless named here")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one request's ceiling, in seconds"))
  (:help :super-artifacts
         "super-artifacts: publish a page, form or tracker as a live private URL, and read back what visitors submitted"
         +primer+)
  (:command "super-artifacts" 'run-command
            :description "Super Artifacts: sign in, or say what is configured"
            :argument-hint "[signin | status | cancel]")
  (:tool "super_whoami" *whoami-schema* 'tool-whoami)
  (:tool "super_publish" *publish-schema* 'tool-publish)
  (:tool "super_data" *data-schema* 'tool-data)
  (:tool "super_artifacts" *artifacts-schema* 'tool-artifacts)
  (:tool "super_tool" *tool-schema* 'tool-call-any)
  (:start (lambda ()
            (setf *pending* nil *initialized* nil *session-id* nil *token* nil *retrying* nil)
            (nle:on-stop (lambda ()
                           (stop-poller)
                           (setf *pending* nil *initialized* nil *session-id* nil *token* nil)
                           (nle:notice nil :key (notice-key)))))))
