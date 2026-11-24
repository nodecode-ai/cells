;;;; cell.lisp --- the two tools, /artifact-relay, and the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of eloktev/hermes-artifact-relay (see NOTICE). The original
;;;; registered two tools and a prose skill with a Hermes context, and a CLI
;;;; command that signed in. Here the same two tools are registered off the
;;;; cell's section, the skill is (help :artifact-relay) -- one line of which
;;;; rides every request -- and the CLI's setup and status are
;;;; /artifact-relay's.

(in-package #:nodecode-artifact-relay)

;;; --- what the model is told -------------------------------------------------------

(defparameter +primer+
  "Artifact Relay publishes a long result -- a report, a document, a standalone page -- as a
private link someone opens in a browser, and reads one back by its URL or its id.
  artifact_publish {title, content, summary?, format?, expires_days?}   -> the viewer URL
  artifact_read {url}                                                    -> the original source
When to publish: when an answer would be too long, or too much of a document, to sit in the
transcript, or when the operator asks for a link. Put the whole of it in content and keep the
reply to a few lines and the URL. format is markdown (the default) or html: an html artifact is
a page of its own, so write it whole and standalone. expires_days runs 0..3650, 30 unless given,
and a publish is refused past 5 MiB. Never publish a secret, a token or a private key: a link is
a link.
artifact_read takes the viewer URL or the bare id; a URL on another origin is refused before the
token leaves, so a foreign URL is read only when the section's base_url names that service.
The artifact-relay section names the service (base_url), the token (the variable token_env
names, or token_file), and whether a publish names the session it came from
(include_provenance, off by default). /artifact-relay status says what is ready;
/artifact-relay setup signs in to the hosted control plane with a device code."
  "What (help :artifact-relay) answers while the cell runs: the original's skill, in the cell's
voice.")

;;; --- the tools' schemas -----------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool
schema, as the original wrote its two."
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

(defparameter *publish-schema*
  (tool-schema
   "Publish private Markdown or standalone HTML to the configured Artifact Relay and return its viewer URL. Never include secrets."
   '(("title" "string" "Artifact title" t)
     ("content" "string" "Complete Markdown or standalone HTML source" t)
     ("summary" "string" "One line the relay lists the artifact under")
     ("format" "string" "\"markdown\" (default) or \"html\"")
     ("expires_days" "integer" "How long the link lives, 0 to 3650 days (default 30)")))
  "The artifact_publish tool, as the original's schema had it.")

(defparameter *read-schema*
  (tool-schema
   "Read original Markdown or HTML from a configured Artifact Relay URL. Foreign-origin URLs are rejected before authentication."
   '(("url" "string" "Artifact URL or id" t)))
  "The artifact_read tool, as the original's schema had it.")

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: artifact-relay: ~a" condition)))))

(defun argument (args name)
  (and (hash-table-p args) (gethash name args)))

(defun tool-publish (args)
  "artifact_publish: the viewer URL, and what the relay kept."
  (answering
    (multiple-value-bind (json id url)
        (publish :title (argument args "title")
                 :content (argument args "content")
                 :summary (argument args "summary")
                 :format (argument args "format")
                 :expires-days (argument args "expires_days"))
      (declare (ignore json))
      (format nil "Published ~s: ~a~%id ~a, ~a~@[, expires in ~d days~]. The link is as private as the relay: never publish a secret.~@[~%~a~]"
              (text-field (argument args "title") "title" +max-title+)
              url id
              (format-field (argument args "format"))
              (expiry-field (argument args "expires_days"))
              (text-field (argument args "summary") "summary" +max-summary+)))))

(defun tool-read (args)
  "artifact_read: one artifact's source back."
  (answering
    (multiple-value-bind (json id)
        (read-artifact (argument args "url"))
      (format nil "Artifact ~a~@[ ~s~]~@[ (~a)~]~%~%~a"
              id
              (json-text json "title")
              (json-text json "format")
              (json-text json "content")))))

;;; --- the operator's command ------------------------------------------------------

(defun run-command (args session-id)
  "One /artifact-relay invocation: nothing or `status' for what is ready,
`setup [URL]' to sign in with a device code, `cancel' to stop waiting."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab)))
             (action (string-downcase (or (first words) ""))))
        (cond
          ((member action '("" "status") :test #'string=) (status-line))
          ((string= action "setup") (begin-setup (second words)))
          ((string= action "cancel") (cancel-setup))
          (t (format nil "/artifact-relay takes setup [URL], status or cancel; it read ~s"
                     (nlk:one-line (or args ""))))))
    ;; a command answers words, never a backtrace: the operator reads why in the shell
    (artifact-relay-error (condition) (format nil "/artifact-relay: ~a" condition))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell artifact-relay
  (:section ("artifact-relay")
    (:guide "set base_url to the relay's own origin (https, or http on loopback) and token_env to the variable holding its API token, or leave both to /artifact-relay setup, which learns them from the control plane; /artifact-relay status says what is ready; then the model publishes with artifact_publish and reads with artifact_read")
    ("base_url" :string
     :doc "the relay's origin, e.g. https://relay.example; empty until the section or a finished setup names one")
    ("control_plane" :string :default "https://relay.lok-labs.com"
     :doc "where /artifact-relay setup asks for a device code")
    ("token_env" :env :default "ARTIFACT_RELAY_API_TOKEN"
     :doc "the environment variable holding the relay's API token, when no token file is kept")
    ("token_file" :path
     :doc "a file holding the token; /artifact-relay setup writes <state_dir>/token, which is read here too")
    ("state_dir" :path
     :doc "the folder holding the token and the saved service: <home>/artifact-relay/ unless named here")
    ("include_provenance" :boolean :default nil
     :doc "whether a publish names the session it came from; off, nothing about this machine rides along")
    ("timeout_seconds" :integer :default 90 :min 1
     :doc "one request's ceiling, in seconds"))
  (:help :artifact-relay
         "artifact-relay: publish long Markdown or HTML as a private link, and read its source back"
         +primer+)
  (:command "artifact-relay" 'run-command
            :description "Artifact Relay: sign in, or say what is configured"
            :argument-hint "[setup [URL] | status | cancel]")
  (:tool "artifact_publish" *publish-schema* 'tool-publish)
  (:tool "artifact_read" *read-schema* 'tool-read)
  (:start (lambda ()
            (setf *pending* nil)
            (nle:on-stop (lambda ()
                           (stop-poller)
                           (setf *pending* nil)
                           (nle:notice nil :key (notice-key)))))))
