;;;; cell.lisp --- FlipLetters' split-flap boards, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Terahertz-Inc/flipletters-mcp (see NOTICE). FlipLetters
;;;; turns text written in flipcode -- a small text format for split-flap and
;;;; flip-dot departure boards -- into a validated board and a rendered MP4.
;;;; Its thirteen tools are the hosted server's own, at the section's url:
;;;; this file declares the arguments each one documents and passes them
;;;; through, and adds flipletters_tools, which lists what the live server
;;;; offers so a model reads the server's schema before it calls a name.
;;;;
;;;; The flow the original's skill spells out, and the primer repeats: read
;;;; the reference, draft, validate until clean, then render or share only
;;;; when the operator asked, because a render spends the key's export quota.

(in-package #:nodecode-flipletters)

(nlk:define-peripheral flipletters :not-running t)

(defparameter +primer+
  "FlipLetters designs split-flap and flip-dot board videos (the old train-station departure
board, with its mechanical sound) from flipcode, a small text format, and renders them to MP4.
Its tools are the FlipLetters server's, at the section's url; every call needs the field's key.
  flipletters_flipcode_reference {}                     the flipcode cheat sheet: directives,
      board types, size limits, transitions. Read it before writing a board.
  flipletters_example_board {template}                   a complete valid board for one type
      (minimal, trainboard/station, artboard, flipdot, marquee, announcement, arena).
  flipletters_validate_board {code}                      parse a draft: valid or not, warnings,
      the normalized code, the board's summary, a Studio share link. Do this before rendering.
  flipletters_template_url {board?, grid?, title?, accent?, out?, sound?, speed?, slides?,
      flip?, hold?}                                      a readable Studio URL, one query
      parameter each; slides is an array of strings, | between rows.
  flipletters_create_animation {text, template?}         text to a finished MP4 in one call:
      it renders under the key owner's plan (free renders carry a watermark) and counts against
      the render quota; not idempotent. Markdown is not supported in the board's text.
  flipletters_render_board {code, repeat?, wait?}        a cloud render of a flipcode document;
      wait polls up to about 45 seconds, then use flipletters_render_status {jobId}.
  flipletters_list_teams {}  flipletters_share_to_team {teamId, name, code}
  flipletters_list_workspaces {}  flipletters_list_postiz_destinations {workspaceId}
      flipletters_list_postiz_integrations {workspaceId, destinationId}
      flipletters_share_to_postiz {workspaceId, destinationId, integrationId, date,
      type?, content?, mediaUrl?, fileName?, providerType?}   schedule, draft or post the
      MP4 through the operator's own Postiz channels.
  flipletters_tools {refresh?}                           what the server offers this key now
Every answer is text: URLs and warnings are the server's words. Errors are ERROR: with the
reason the server gave. Draft and validate freely; render, share and post only when the operator
asked, since they spend the key owner's quota and publish to their channels."
  "What (help :flipletters) answers while the cell runs.")

(defparameter +description-chars+ 200
  "How much of one tool's description the tool list repeats.")

;;; --- what a call answers ----------------------------------------------------------

(defun result-text (result)
  "RESULT as the text the model reads: the text blocks the server sent, joined,
else its structured content as JSON."
  (let* ((texts (loop for block across (nlk:json-array result "content")
                      when (and (hash-table-p block)
                                (equal "text" (nlk:json-value block :string "type")))
                        collect (nlk:json-value block :string "text")))
         (text (and texts (format nil "~{~a~^~%~}" texts))))
    (or (and text (plusp (length text)) text)
        (let ((structured (nlk:json-value result :object "structuredContent")))
          (and structured (nlk:encode-json-object structured)))
        "(FlipLetters answered nothing)")))

(defun arguments (pairs)
  "PAIRS, (KEY . VALUE), as the arguments object of one tools/call: a NIL
VALUE is a member the call left out, so nothing is sent for it; T and :false
ride as JSON's true and false."
  (apply #'nlk:make-json-object
         (loop for (key . value) in pairs
               when value append (list key value))))

(defun remote (name &rest pairs)
  "NAME called on the FlipLetters server with PAIRS. => the text the model reads."
  (multiple-value-bind (result error) (invoke name (arguments pairs))
    (when error (fail "~a" error))
    (result-text result)))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (flipletters-error (condition)
       (nle:failure (format nil "ERROR: ~a" (flipletters-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: FlipLetters: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun tools-text (refresh)
  "The server's tools as the lines the model reads: name, description, and
the arguments the schema marks required."
  (multiple-value-bind (tools error) (tools-result refresh)
    (when error (fail "~a" error))
    (if (null tools)
        "FlipLetters offers this key no tools."
        (with-output-to-string (out)
          (format out "FlipLetters offers ~d tools; each takes the arguments it names:" (length tools))
          (loop for tool in tools
                for required = (required-arguments tool)
                do (format out "~%  ~a -- ~a~@[  (required: ~{~a~^, ~})~]"
                           (tool-name tool)
                           (nlk:clip (nlk:one-line (tool-description tool)) +description-chars+)
                           required))))))

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract flipletters-tools
  (:doc "List the tools the FlipLetters server offers this key, with the arguments each requires. Call this when a name below is refused or a board needs a tool this cell does not name.")
  ("refresh" :boolean :default nil
   :doc "ask the server again instead of answering from the copy this process already has"))

(defun no-argument-schema (description)
  "DESCRIPTION as the tool schema of a tool with no arguments: the server
lists these three with an empty property set."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

(defparameter +reference-schema+
  (no-argument-schema
   "The flipcode cheat sheet: directives, board types and size limits, transition modes, and flip-native effect recipes. Read it before writing a board so the code is valid on the first try.")
  "The flipletters_flipcode_reference tool, which takes no arguments.")

(defparameter +list-teams-schema+
  (no-argument-schema
   "The teams the key owner belongs to, with their ids: the ids flipletters_share_to_team takes.")
  "The flipletters_list_teams tool, which takes no arguments.")

(defparameter +list-workspaces-schema+
  (no-argument-schema
   "The key owner's agency projects (workspaces), with the ids the Postiz tools take.")
  "The flipletters_list_workspaces tool, which takes no arguments.")

(nlk:define-contract flipletters-example-board
  (:doc "A complete, valid flipcode document for a board type: a known-good starting point to edit rather than writing one from scratch.")
  ("template" :text :required t
   :doc "board type or alias: minimal, trainboard, station, artboard, art, flipdot, dots, marquee, desk, announcement, poster, arena, lightbox"))

(nlk:define-contract flipletters-validate-board
  (:doc "Parse a flipcode document: whether it is valid, the warnings, the normalized code, the board's summary, and a Studio share URL that opens the exact board. Draft, validate, fix warnings, then give the operator the share link.")
  ("code" :text :required t
   :doc "a complete flipcode document (read flipletters_flipcode_reference first)"))

(nlk:define-contract flipletters-template-url
  (:doc "Build a human-readable FlipLetters Studio URL where every query parameter mirrors one flipcode line: for short text boards whose address should stay legible. For full fidelity or paint grids use flipletters_validate_board's share link.")
  ("board" :text :doc "board type or alias, e.g. station")
  ("grid" :text :doc "ROWSxCOLS, e.g. 8x34")
  ("title" :text :doc "the board's title")
  ("accent" :text :doc "a hex color, # optional")
  ("out" :text :doc "the canvas: vertical, square, 1200x630@25, ...")
  ("sound" :text :doc "the mechanical sound the board plays")
  ("speed" :text :slot flip-speed :doc "the flip speed")
  ("slides" :array :of :string
   :doc "one entry per slide, | between rows: \"EGGS BENEDICT|FLAT WHITE\"")
  ("flip" :text :doc "the transition, commas for spaces: cascade,9,0.2,0.4")
  ("hold" :text :doc "how long a slide holds, in seconds"))

(nlk:define-contract flipletters-create-animation
  (:doc "One call from plain text to a FlipLetters video: builds a board on a text template, checks the text against the content rules, cloud-renders an MP4 under the key owner's plan and returns its mp4Url, its shareUrl, the jobId and the status. Not idempotent: every call starts a new render and counts against the plan's quota, so poll flipletters_render_status rather than calling again.")
  ("text" :text :required t
   :doc "what the board should say, e.g. \"I love sourdough\"; a newline forces a line break")
  ("template" :text
   :doc "board type: minimal (the default), trainboard, marquee, announcement, arena; aliases like station, desk, poster and lightbox are read too"))

(nlk:define-contract flipletters-render-board
  (:doc "Start a cloud MP4 render of a flipcode document under the key owner's plan: free renders carry a watermark and every render counts against the export quota. wait polls until the render is done, about 45 seconds at most; past that, poll flipletters_render_status with its jobId.")
  ("code" :text :required t
   :doc "a complete flipcode document")
  ("repeat" :integer
   :doc "loop the segment this many times; 1 when unset")
  ("wait" :boolean :default t
   :doc "poll until the render is done; false answers as soon as the job is queued"))

(nlk:define-contract flipletters-render-status
  (:doc "Poll one render job: its status, and its MP4 URL once it is done.")
  ("job_id" :text :required t
   :doc "the job id a render answered with"))

(nlk:define-contract flipletters-share-to-team
  (:doc "Share a board into a team's library, under the name the team sees it by.")
  ("team_id" :text :required t
   :doc "a team id from flipletters_list_teams")
  ("name" :text :required t
   :doc "the display name the board takes in the team library")
  ("code" :text :required t
   :doc "a complete flipcode document, validated first"))

(nlk:define-contract flipletters-list-postiz-destinations
  (:doc "The Postiz destinations attached to one project; a destination id is what the integration list and a share name.")
  ("workspace_id" :text :required t
   :doc "a workspace id from flipletters_list_workspaces"))

(nlk:define-contract flipletters-list-postiz-integrations
  (:doc "The social channels connected to one project's Postiz account: the integration ids a share names.")
  ("workspace_id" :text :required t
   :doc "a workspace id from flipletters_list_workspaces")
  ("destination_id" :text :required t
   :doc "a destination id from flipletters_list_postiz_destinations"))

(nlk:define-contract flipletters-share-to-postiz
  (:doc "Upload a rendered board's MP4 and schedule, draft or post it through the operator's own connected social channels. It publishes to accounts they own, so do it only when they asked, and read the date back to them.")
  ("workspace_id" :text :required t
   :doc "a FlipLetters project id")
  ("destination_id" :text :required t
   :doc "a Postiz destination id on that project")
  ("integration_id" :text :required t
   :doc "a Postiz channel id on that destination")
  ("date" :text :required t
   :doc "when it goes out, a UTC ISO datetime, e.g. 2026-08-14T15:00:00.000Z")
  ("type" :text :slot post-type
   :doc "schedule (the default), draft or now")
  ("content" :text :doc "the caption or post body")
  ("media_url" :text :doc "the HTTPS URL of the MP4 a cloud render answered with")
  ("file_name" :text :doc "a local renders/*.mp4 name, when the Studio host holds the file")
  ("provider_type" :text :doc "the Postiz provider type, e.g. tiktok, x, linkedin"))

;;; --- the tools -------------------------------------------------------------------

(defun tools-tool (call)
  "The flipletters_tools tool: what the server offers this key."
  (answering (tools-text (flipletters-tools-refresh call))))

(defun reference-tool (call)
  "The flipletters_flipcode_reference tool: the cheat sheet."
  (declare (ignore call))
  (answering (remote "flipletters_flipcode_reference")))

(defun example-board-tool (call)
  "The flipletters_example_board tool: one valid board of a type."
  (answering (remote "flipletters_example_board"
                      (cons "template" (flipletters-example-board-template call)))))

(defun validate-board-tool (call)
  "The flipletters_validate_board tool: a draft parsed, with its warnings and link."
  (answering (remote "flipletters_validate_board"
                      (cons "code" (flipletters-validate-board-code call)))))

(defun template-url-tool (call)
  "The flipletters_template_url tool: a readable Studio URL."
  (answering (remote "flipletters_template_url"
                      (cons "board" (flipletters-template-url-board call))
                      (cons "grid" (flipletters-template-url-grid call))
                      (cons "title" (flipletters-template-url-title call))
                      (cons "accent" (flipletters-template-url-accent call))
                      (cons "out" (flipletters-template-url-out call))
                      (cons "sound" (flipletters-template-url-sound call))
                      (cons "speed" (flipletters-template-url-speed call))
                      (cons "slides" (flipletters-template-url-slides call))
                      (cons "flip" (flipletters-template-url-flip call))
                      (cons "hold" (flipletters-template-url-hold call)))))

(defun create-animation-tool (call)
  "The flipletters_create_animation tool: text to a rendered MP4."
  (answering (remote "flipletters_create_animation"
                      (cons "text" (flipletters-create-animation-text call))
                      (cons "template" (flipletters-create-animation-template call)))))

(defun render-board-tool (call)
  "The flipletters_render_board tool: one flipcode document rendered."
  (answering (remote "flipletters_render_board"
                      (cons "code" (flipletters-render-board-code call))
                      (cons "repeat" (flipletters-render-board-repeat call))
                      (cons "wait" (if (flipletters-render-board-wait call) t :false)))))

(defun render-status-tool (call)
  "The flipletters_render_status tool: one job polled."
  (answering (remote "flipletters_render_status"
                      (cons "jobId" (flipletters-render-status-job-id call)))))

(defun list-teams-tool (call)
  "The flipletters_list_teams tool: the key owner's teams."
  (declare (ignore call))
  (answering (remote "flipletters_list_teams")))

(defun share-to-team-tool (call)
  "The flipletters_share_to_team tool: a board into a team library."
  (answering (remote "flipletters_share_to_team"
                      (cons "teamId" (flipletters-share-to-team-team-id call))
                      (cons "name" (flipletters-share-to-team-name call))
                      (cons "code" (flipletters-share-to-team-code call)))))

(defun list-workspaces-tool (call)
  "The flipletters_list_workspaces tool: the key owner's projects."
  (declare (ignore call))
  (answering (remote "flipletters_list_workspaces")))

(defun list-postiz-destinations-tool (call)
  "The flipletters_list_postiz_destinations tool: a project's destinations."
  (answering (remote "flipletters_list_postiz_destinations"
                      (cons "workspaceId" (flipletters-list-postiz-destinations-workspace-id call)))))

(defun list-postiz-integrations-tool (call)
  "The flipletters_list_postiz_integrations tool: a destination's channels."
  (answering (remote "flipletters_list_postiz_integrations"
                      (cons "workspaceId" (flipletters-list-postiz-integrations-workspace-id call))
                      (cons "destinationId" (flipletters-list-postiz-integrations-destination-id call)))))

(defun share-to-postiz-tool (call)
  "The flipletters_share_to_postiz tool: a board's MP4 to social channels."
  (answering (remote "flipletters_share_to_postiz"
                      (cons "workspaceId" (flipletters-share-to-postiz-workspace-id call))
                      (cons "destinationId" (flipletters-share-to-postiz-destination-id call))
                      (cons "integrationId" (flipletters-share-to-postiz-integration-id call))
                      (cons "date" (flipletters-share-to-postiz-date call))
                      (cons "type" (flipletters-share-to-postiz-post-type call))
                      (cons "content" (flipletters-share-to-postiz-content call))
                      (cons "mediaUrl" (flipletters-share-to-postiz-media-url call))
                      (cons "fileName" (flipletters-share-to-postiz-file-name call))
                      (cons "providerType" (flipletters-share-to-postiz-provider-type call)))))

;;; --- /flipletters -----------------------------------------------------------------

(defun status-text ()
  "What /flipletters answers alone: where it points, whether a key is set, and
how many of the server's tools this process already knows."
  (format nil "FlipLetters: ~a~%key: ~:[not set~;set~] (~a)~%tools: ~:[not asked yet~;~:*~d known~]~%~
/flipletters tools - what the server offers; /flipletters reference - the flipcode cheat sheet; ~
/flipletters refresh - ask for the tool list again"
          (setting :url)
          (and (api-key) t)
          (or (setting :api-key-env) "api_key")
          (length *tools*)))

(defun run-command (args session-id)
  "/flipletters alone: where the cell points and whether it has a key;
tools: the server's tool list; reference: the flipcode cheat sheet."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member word '("" "status") :test #'string=) (status-text))
          ((member word '("tools" "list") :test #'string=) (answering (tools-text nil)))
          ((string= word "refresh") (answering (tools-text t)))
          ((string= word "reference") (answering (remote "flipletters_flipcode_reference")))
          (t "usage: /flipletters - where it points and whether a key is set; /flipletters tools - the server's tools; /flipletters refresh - ask again; /flipletters reference - the flipcode cheat sheet"))))

;;; --- the declaration --------------------------------------------------------------

(defun api-key-of (plist)
  "The key PLIST carries: api_key inline, else the variable api_key_env names."
  (flet ((nonblank (text)
           (let ((text (and (stringp text) (nlk:trimmed text))))
             (and text (plusp (length text)) text))))
    (or (nonblank (getf plist :api-key))
        (let ((name (nonblank (getf plist :api-key-env))))
          (and name (nonblank (ignore-errors (environment-value name))))))))

(defun read-settings (values table)
  "The declared VALUES, checked once: a url with no scheme and a section with
no key anywhere are refused by name, as the setup wizard would have them."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (let ((root (string-right-trim "/" (or (getf plist :url) ""))))
      (unless (or (uiop:string-prefix-p "https://" root) (uiop:string-prefix-p "http://" root))
        (nlk:config-error "url ~s has no http:// or https:// scheme: set it to https://app.flipletters.com/mcp/v1"
                          root))
      (setf (getf plist :url) root))
    (unless (api-key-of plist)
      (nlk:config-error "no FlipLetters API key: put the flk_ key from FlipLetters Studio -> Account -> API keys & agents in the environment variable ~a, or write it here as api_key, then (restart-cells \"nodecode-flipletters\")"
                        (or (getf plist :api-key-env) "FLIPLETTERS_API_KEY")))
    plist))

(defun install ()
  "A start makes no session: the first call does, and a restart forgets it."
  (forget-session)
  (nle:on-stop #'forget-session))

(nle:define-cell flipletters
  (:section ("flipletters")
    (:guide "put the flk_ key from FlipLetters Studio -> Account -> API keys & agents in the environment variable api_key_env names (or write it as api_key); url is the hosted server; next step: ask for a board, then flipletters_validate_board before a render")
    ("api_key" :secret
     :doc "the FlipLetters API key (flk_...), written here")
    ("api_key_env" :env :default "FLIPLETTERS_API_KEY"
     :doc "the environment variable holding the key, when it is not written above")
    ("url" :string :default "https://app.flipletters.com/mcp/v1"
     :doc "the server's Streamable HTTP endpoint")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one exchange's ceiling, in seconds"))
  (:settings #'read-settings)
  (:start #'install)
  (:help :flipletters
         "flipletters: split-flap and flip-dot board videos from flipcode - validate, render, share"
         +primer+)
  (:tool "flipletters_tools" (nlk:contract 'flipletters-tools) #'tools-tool)
  (:tool "flipletters_flipcode_reference" +reference-schema+ #'reference-tool)
  (:tool "flipletters_example_board" (nlk:contract 'flipletters-example-board) #'example-board-tool)
  (:tool "flipletters_validate_board" (nlk:contract 'flipletters-validate-board) #'validate-board-tool)
  (:tool "flipletters_template_url" (nlk:contract 'flipletters-template-url) #'template-url-tool)
  (:tool "flipletters_create_animation" (nlk:contract 'flipletters-create-animation) #'create-animation-tool)
  (:tool "flipletters_render_board" (nlk:contract 'flipletters-render-board) #'render-board-tool)
  (:tool "flipletters_render_status" (nlk:contract 'flipletters-render-status) #'render-status-tool)
  (:tool "flipletters_list_teams" +list-teams-schema+ #'list-teams-tool)
  (:tool "flipletters_share_to_team" (nlk:contract 'flipletters-share-to-team) #'share-to-team-tool)
  (:tool "flipletters_list_workspaces" +list-workspaces-schema+ #'list-workspaces-tool)
  (:tool "flipletters_list_postiz_destinations" (nlk:contract 'flipletters-list-postiz-destinations)
         #'list-postiz-destinations-tool)
  (:tool "flipletters_list_postiz_integrations" (nlk:contract 'flipletters-list-postiz-integrations)
         #'list-postiz-integrations-tool)
  (:tool "flipletters_share_to_postiz" (nlk:contract 'flipletters-share-to-postiz) #'share-to-postiz-tool)
  (:command "flipletters" 'run-command
            :description "FlipLetters: the endpoint, the server's tools, and the flipcode reference"
            :argument-hint "tools|refresh|reference"))
