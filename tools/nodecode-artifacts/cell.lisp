;;;; cell.lisp --- the six artifact tools, /artifacts, and the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of dmmeteo/hermes-artifacts (see NOTICE). The original
;;;; registered six tools and a prose skill with a Hermes context; here the
;;;; six tools are registered off the cell's section and the skill is
;;;; (help :artifacts), one line of which rides every request.

(in-package #:nodecode-artifacts)

;;; --- what the model is told -------------------------------------------------------

(defparameter +primer+
  "Artifacts is one private library shared by every session on this machine: finished documents
go in, and a guest link comes out only when the operator asks for one.
  artifact_publish {title, path | content, kind?}      -> id, slug, /artifacts/<slug>
  artifact_list {query?, kind?, profile?, limit?, cursor?} -> metadata, newest first
  artifact_get {artifact_id}                            -> metadata, shares, text preview
  artifact_download {artifact_id, path, overwrite?}     -> the original bytes on disk
  artifact_share {artifact_id, expires_in_hours?}       -> a guest URL, shown once
  artifact_revoke_share {share_id}                      -> that one link, dead
Publishing is private and creates no link. Formats are html (self-contained: inline CSS and JS,
no external URLs), markdown, svg, png and webp; a path is taken by its extension and inline
content must say kind. Call artifact_share only when the user asks for a link, and give the URL
back as it came: it is the grant, and it is returned once. Links expire (default 7 days, at most
8760 hours) and are revoked by share.id, never by the URL. Artifacts are immutable: change one by
publishing a new one. Never build a guest URL yourself. source_profile is a label, not an owner.
Errors read {error {code, message}}: not_configured (set base_url and a token), service_unreachable,
unauthorized, not_found, too_large, invalid_arguments."
  "(help :artifacts) answers this.")

;;; --- schemas ------------------------------------------------------------------

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

(defparameter *publish-schema*
  (tool-schema
   "Save a finished document to the private artifact library. Publishing creates no guest link. Never publish a secret."
   '(("title" "string" "Human-readable title, at most 200 characters")
     ("path" "string" "A local file the service takes by its extension (html, htm, md, markdown, svg, png, webp)")
     ("content" "string" "Inline html, markdown or svg text, instead of a path")
     ("kind" "string" "html, markdown, svg, png or webp; inferred from a path, required for content")))
  "artifact_publish, as the original's schema had it.")

(defparameter *list-schema*
  (tool-schema
   "List the private artifact library, newest first, with optional filters and pagination."
   '(("query" "string" "Case-insensitive title search")
     ("kind" "string" "Only artifacts of this kind")
     ("profile" "string" "Only artifacts published under this source_profile label")
     ("limit" "integer" "How many to list, 1 to 100 (default 20)")
     ("cursor" "string" "next_cursor from a previous call")))
  "artifact_list.")

(defparameter *get-schema*
  (tool-schema
   "Read one artifact's metadata, its guest links and a text preview for html, markdown and svg."
   '(("artifact_id" "string" "Artifact id, as artifact_publish or artifact_list returned it" t)))
  "artifact_get.")

(defparameter *download-schema*
  (tool-schema
   "Write an artifact's original bytes to a local file, atomically; refuses to replace one unless overwrite is true."
   '(("artifact_id" "string" "Artifact id" t)
     ("path" "string" "Destination file path" t)
     ("overwrite" "boolean" "Replace an existing file")))
  "artifact_download.")

(defparameter *share-schema*
  (tool-schema
   "Create one guest link for one artifact. Only when the user asks to share: anyone with the URL can open it until it expires or is revoked."
   '(("artifact_id" "string" "Artifact id" t)
     ("expires_in_hours" "number" "Lifetime in hours; 168 (7 days) unless given, at most 8760")))
  "artifact_share.")

(defparameter *revoke-schema*
  (tool-schema
   "Revoke one guest link by its share id. Other links to the same artifact keep working."
   '(("share_id" "string" "share.id from artifact_share or artifact_get" t)))
  "artifact_revoke_share.")

;;; --- arguments ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: artifacts: ~a" condition)))))

(defun argument (args name)
  (and (hash-table-p args) (gethash name args)))

(defun argument-text (args name)
  (present-text (argument args name)))

(defun required-text (args name)
  (or (argument-text args name) (fail "~a is required" name)))

(defun kind-of (value &key (required nil))
  "VALUE as one of the five kinds, or NIL when absent and not required."
  (let ((kind (and (stringp value) (string-downcase (nlk:trimmed value)))))
    (cond ((and kind (plusp (length kind)))
           (unless (member kind +kinds+ :test #'string=)
             (fail "kind must be one of ~{~a~^, ~}, not ~s" +kinds+ value))
           kind)
          (required (fail "kind is required: one of ~{~a~^, ~}" +kinds+))
          (t nil))))

(defun content-type-of (kind)
  (or (cdr (assoc kind +media-types+ :test #'string=)) "application/octet-stream"))

(defun source-of (args)
  "WHAT to publish: (values OCTETS KIND TITLE-FALLBACK), from exactly one of
path and content, as the original read it."
  (let* ((path (argument-text args "path"))
         (content (argument args "content"))
         (kind (kind-of (argument args "kind"))))
    (when (and path content) (fail "give exactly one of path or content, not both"))
    (cond
      (path
       (let* ((file (nlk:expand-home path))
              (extension (string-downcase (or (pathname-type file) "")))
              (inferred (cdr (assoc extension +kinds-by-extension+ :test #'string=))))
         (unless inferred
           (fail "~s is not a supported file: use html, htm, md, markdown, svg, png or webp" path))
         (when (and kind (not (string= kind inferred)))
           (fail "kind ~a does not match ~s, which is ~a" kind path inferred))
         (unless (and (probe-file file) (not (uiop:directory-exists-p file)))
           (fail "no such file: ~a" file))
         (let ((bytes (file-bytes file)))
           (when (> (length bytes) +max-upload-bytes+)
             (fail "~a is ~:d bytes; the service takes at most ~:d"
                   file (length bytes) +max-upload-bytes+))
           (values bytes inferred (pathname-name file)))))
      ((stringp content)
       (let ((kind (or kind (fail "inline content needs kind: html, markdown or svg"))))
         (unless (member kind +text-kinds+ :test #'string=)
           (fail "inline content may be html, markdown or svg; publish a ~a by path" kind))
         (let ((bytes (utf8 content)))
           (when (> (length bytes) +max-upload-bytes+)
             (fail "content is ~:d bytes; the service takes at most ~:d"
                   (length bytes) +max-upload-bytes+))
           (values bytes kind nil))))
      (t (fail "give a path or inline content, one of them")))))

(defun title-of (args fallback)
  (let ((title (argument-text args "title")))
    (cond (title
           (when (> (length title) 200) (fail "title is longer than 200 characters"))
           title)
          (fallback (substitute #\Space #\_ (string-downcase fallback)))
          (t (fail "title is required when publishing inline content")))))

(defun session-label ()
  "The running turn's session id, when there is one: a provenance label."
  (let ((turn (ignore-errors (nle:turn))))
    (present-text (getf turn :session-id))))

(defun artifact-of (json)
  "The artifact object JSON carries -- bare, or under \"artifact\"."
  (let ((nested (and (hash-table-p json) (gethash "artifact" json))))
    (if (hash-table-p nested) nested json)))

(defun artifact-line (artifact)
  (format nil "~a  ~a  ~a~@[  /artifacts/~a~]~@[  ~d share(s)~]"
          (or (json-text artifact "id") "?")
          (or (json-text artifact "kind") "?")
          (or (json-text artifact "title") "(untitled)")
          (json-text artifact "slug")
          (nlk:json-value artifact :integer "active_shares")))

;;; --- the tools ------------------------------------------------------------------

(defun tool-publish (args)
  "artifact_publish: save the bytes, answer where they landed."
  (answering
    (multiple-value-bind (bytes kind fallback) (source-of args)
      (let* ((title (title-of args fallback))
             (profile (or (present-text (setting :source-profile)) "nodecode"))
             (json (request :post "/artifacts"
                            :params (list (cons "title" title)
                                          (cons "kind" kind)
                                          (cons "source_profile" profile)
                                          (cons "source_session_id" (session-label)))
                            :content bytes
                            :content-type (content-type-of kind)))
             (artifact (artifact-of json)))
        (format nil "Published ~s privately (~a, ~:d bytes).~%id ~a, slug ~a, library path ~@[/artifacts/~a~].~%No guest link exists yet: call artifact_share only if the user asks to share it."
                title kind (length bytes)
                (json-text artifact "id") (json-text artifact "slug")
                (json-text artifact "slug"))))))

(defun tool-list (args)
  "artifact_list: the library, newest first."
  (answering
    (let* ((limit (let ((value (argument args "limit")))
                    (cond ((null value) 20)
                          ((integerp value) (max 1 (min value 100)))
                          (t (fail "limit must be a whole number, not ~s" value)))))
           (json (request :get "/artifacts"
                          :params (list (cons "q" (argument-text args "query"))
                                        (cons "kind" (kind-of (argument args "kind")))
                                        (cons "profile" (argument-text args "profile"))
                                        (cons "limit" (princ-to-string limit))
                                        (cons "cursor" (argument-text args "cursor")))))
           (items (let ((value (nlk:json-value json :array "items")))
                    (if (vectorp value) (coerce value 'list) value))))
      (with-output-to-string (out)
        (if (null items)
            (format out "No artifacts in the library~@[ matching ~s~] yet."
                    (argument-text args "query"))
            (format out "~d artifact~:p, newest first:~%~{  ~a~^~%~}"
                    (length items) (mapcar #'artifact-line items)))
        (let ((cursor (json-text json "next_cursor")))
          (when (present-text cursor)
            (format out "~%more: pass cursor ~a" cursor)))))))

(defun tool-get (args)
  "artifact_get: one artifact's metadata, its links, and a text preview."
  (answering
    (let* ((id (required-text args "artifact_id"))
           (json (request :get (format nil "/artifacts/~a" (segment id))))
           (artifact (artifact-of json))
           (kind (json-text artifact "kind"))
           (shares (let ((value (nlk:json-value json :array "shares")))
                     (if (vectorp value) (coerce value 'list) value))))
      (with-output-to-string (out)
        (format out "~a [~a] ~s~@[  slug ~a~]~@[  ~d bytes~]~%library path /artifacts/~a"
                (or (json-text artifact "id") id)
                (or kind "?")
                (or (json-text artifact "title") "(untitled)")
                (json-text artifact "slug")
                (nlk:json-value artifact :integer "byte_length")
                (or (json-text artifact "slug") ""))
        (if (null shares)
            (format out "~%no guest links: none has been created yet")
            (format out "~%guest links:~%~{  ~a ~a~@[ until ~a~]~^~%~}"
                    (loop for share in shares
                          collect (json-text share "id")
                          collect (or (json-text share "status") "?")
                          collect (json-text share "expires_at"))))
        (when (member kind +text-kinds+ :test #'string=)
          (format out "~%~%preview:~%~a"
                  (preview-of id)))))))

(defun preview-of (id)
  "The first +PREVIEW-CHARS+ characters of a text artifact's bytes."
  (let* ((bytes (download-bytes (format nil "/artifacts/~a/download" (segment id))))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (if (> (length text) +preview-chars+)
        (format nil "~a~%... (~:d more characters)" (subseq text 0 +preview-chars+)
                (- (length text) +preview-chars+))
        text)))

(defun tool-download (args)
  "artifact_download: the original bytes, written where the caller said."
  (answering
    (let ((id (required-text args "artifact_id"))
          (path (required-text args "path")))
      (let ((target (nlk:expand-home path)))
        (when (and (probe-file target) (not (argument args "overwrite")))
          (fail "~a exists; pass overwrite true to replace it" target))
        (let ((bytes (download-bytes (format nil "/artifacts/~a/download" (segment id)))))
          (nlk:write-file-atomically target bytes)
          (format nil "Wrote ~:d bytes to ~a~%sha256 ~a"
                  (length bytes) target (nlk:sha256-text bytes)))))))

(defun hours-of (args)
  "expires_in_hours as a positive finite number, or NIL when absent."
  (let ((value (argument args "expires_in_hours")))
    (when (and value (not (and (stringp value) (zerop (length (nlk:trimmed value))))))
      (let ((hours (cond ((numberp value) (coerce value 'double-float))
                         ((ignore-errors (parse-number (nlk:trimmed value))))
                         (t (fail "expires_in_hours must be a number of hours, not ~s" value)))))
        (unless (and hours (plusp hours) (<= hours +max-hours+))
          (fail "expires_in_hours must be more than 0 and at most ~d, not ~s" +max-hours+ value))
        hours))))

(defun parse-number (text)
  (let ((mark (position-if (lambda (c) (member c '(#\. #\e #\E))) text)))
    (if mark
        (let ((*read-default-float-format* 'double-float)
              (*read-eval* nil))
          (multiple-value-bind (value at) (ignore-errors (read-from-string text))
            (and (realp value) (= at (length text)) (coerce value 'double-float))))
        (ignore-errors (parse-integer text)))))

(defun tool-share (args)
  "artifact_share: one guest URL, shown once."
  (answering
    (let* ((id (required-text args "artifact_id"))
           (hours (hours-of args))
           (json (request :post (format nil "/artifacts/~a/shares" (segment id))
                          :json (if hours
                                    (nlk:make-json-object "expires_in_seconds"
                                                           (max 1 (round (* hours 3600))))
                                    (nlk:make-json-object))))
           (share (and (hash-table-p json) (gethash "share" json)))
           (url (json-text json "url")))
      (unless (present-text url) (fail "the service's share answer has no guest URL"))
      (format nil "Guest link for ~a:~%~a~%share.id ~a~@[ until ~a~]. The URL is shown only now; keep the share id to revoke it. Anyone holding it can open this one artifact until then."
              id url
              (or (json-text share "id") "?")
              (json-text share "expires_at")))))

(defun tool-revoke (args)
  "artifact_revoke_share: one link, dead."
  (answering
    (let* ((id (required-text args "share_id"))
           (json (request :delete (format nil "/shares/~a" (segment id))))
           (share (and (hash-table-p json) (gethash "share" json))))
      (format nil "Guest link ~a is now ~a. Other links to the same artifact keep working."
              id (or (json-text share "status") "revoked")))))

;;; --- the operator's command ------------------------------------------------------

(defun status-line ()
  "What is configured, in one line. It signals while the cell is not running."
  (format nil "Artifacts: ~a, publishing as ~a. Six tools: publish, list, get, download, share, revoke."
          (origin-text (base-origin))
          (or (present-text (setting :source-profile)) "nodecode")))

(defun run-command (args session-id)
  "One /artifacts invocation: nothing for the newest few, `status' for the
section in force, any other words as a title search."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab)))
             (action (string-downcase (or (first words) ""))))
        (cond ((string= action "status") (status-line))
              ((string= action "") (concatenate 'string (status-line) (format nil "~%~%") (tool-list nil)))
              (t (tool-list (nlk:make-json-object "query" (format nil "~{~a~^ ~}" words))))))
    (artifacts-error (condition) (format nil "/artifacts: ~a" condition))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell artifacts
  (:section ("artifacts")
    (:guide "point base_url at the artifact service you run (its /internal/v1 API), name the variable holding its machine credential in token_env, or a file in token_file; /artifacts status says what is ready; then the model publishes with artifact_publish and shares only when asked")
    ("base_url" :string
     :doc "the artifact service's origin, e.g. http://127.0.0.1:8080; https unless it is this machine")
    ("token_env" :env :default "HERMES_ARTIFACTS_TOKEN"
     :doc "the environment variable holding the service's machine credential")
    ("token_file" :path
     :doc "a file holding the credential; read before token_env is")
    ("source_profile" :string :default "nodecode"
     :doc "the attribution label a publish carries; a label and a filter, never an owner")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one request's ceiling, in seconds"))
  (:help :artifacts
         "artifacts: a private artifact library - publish, find, download, and hand out expiring guest links"
         +primer+)
  (:command "artifacts" 'run-command
            :description "What the artifact library holds, and whether it is configured"
            :argument-hint "[status | words to search titles for]")
  (:tool "artifact_publish" *publish-schema* 'tool-publish)
  (:tool "artifact_list" *list-schema* 'tool-list)
  (:tool "artifact_get" *get-schema* 'tool-get)
  (:tool "artifact_download" *download-schema* 'tool-download)
  (:tool "artifact_share" *share-schema* 'tool-share)
  (:tool "artifact_revoke_share" *revoke-schema* 'tool-revoke)
  (:start (lambda ()
            (unless (present-text (setting :base-url))
              (nle:notice "artifacts: set base_url in the artifacts section to the artifact service's URL" :key "nodecode-artifacts"))
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-artifacts"))))))
