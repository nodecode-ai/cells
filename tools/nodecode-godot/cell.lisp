;;;; cell.lisp --- godot: a live Godot 4 editor, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Tormen28/argos (see NOTICE). The original is a
;;;; Python MCP server in front of a Godot editor addon: the addon opens a
;;;; loopback WebSocket, publishes its port and a per-editor token under the
;;;; project's .argos/ folder, and answers one JSON request at a time. This
;;;; cell speaks that wire itself -- read .argos/port and .argos/token, open
;;;; the bridge, send {id, token, action, ...}, read {ok, id, result|error,
;;;; detail} -- and registers the editor verbs the original's skill names.

(defpackage #:nodecode-godot
  (:use #:cl)
  (:export #:godot-error))

(in-package #:nodecode-godot)

(nlk:define-peripheral godot :not-running t)

;;; --- the endpoint ---------------------------------------------------------

(defun non-empty (value)
  "VALUE when it is a string with something in it, trimmed, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun project ()
  "The Godot project root the section names, without a trailing separator."
  (let ((given (non-empty (setting :project))))
    (when given
      (string-right-trim '(#\/ #\\)
                         (uiop:native-namestring (uiop:ensure-directory-pathname given))))))

(defun bridge-file (name)
  "The text of the addon's NAME file under the project's .argos/ folder."
  (let* ((root (project))
         (path (and root
                    (merge-pathnames (format nil ".argos/~a" name)
                                     (uiop:ensure-directory-pathname root)))))
    (when (and path (probe-file path))
      (string-trim '(#\Space #\Tab #\Newline #\Return) (uiop:read-file-string path)))))

(defun bridge-port ()
  "The port the editor published, or the one the section pins."
  (or (let ((pinned (setting :port))) (and (integerp pinned) (plusp pinned) pinned))
      (let ((text (bridge-file "port")))
        (and text (ignore-errors (parse-integer text :junk-allowed t))))))

(defun bridge-token ()
  "The token the editor published, or the one the section pins."
  (or (non-empty (setting :token)) (bridge-file "token")))

(defun answer-seconds ()
  (or (setting :timeout-seconds) 10))

(defun endpoint ()
  "The bridge URL of a running editor."
  (let ((port (bridge-port)))
    (unless (and port (<= 1 port 65535))
      (fail "no bridge port~@[: enable the argos addon in ~a and open that project in Godot, so it writes .argos/port~]: set \"port\" in the godot section"
            (project)))
    (format nil "ws://127.0.0.1:~d/" port)))

(defun check-config ()
  "Refuse a section that names no bridge, naming the member to set."
  (let ((root (project)))
    (cond ((and root (not (uiop:directory-exists-p (uiop:ensure-directory-pathname root))))
           (error 'nlk:config-refusal
                  :detail (format nil "godot: ~a is not a folder: \"project\" is the Godot project root, the one holding project.godot" root)))
          ((and (null root) (null (bridge-port)))
           (error 'nlk:config-refusal
                  :detail "godot needs \"project\": the Godot project root that has the argos addon (it holds .argos/port and .argos/token), or a \"port\" when the editor runs elsewhere"))
          (t t))))

(defun describe-endpoint ()
  "The one line /godot opens with: which project, which bridge."
  (format nil "godot: ~a~%project: ~a~%bridge: ~a"
          (if (bridge-port) "a bridge port is published" "no bridge port yet")
          (or (project) "not set")
          (if (bridge-port) (endpoint) "not set")))

;;; --- the wire -------------------------------------------------------------

(defstruct (bridge (:constructor make-bridge (send recv close)))
  "One open bridge: SEND one text frame, RECV the next message or NIL once
the editor closes, CLOSE it. A test builds one over a scripted answer."
  (send nil :type function)
  (recv nil :type function)
  (close nil :type function))

(defvar *bridge-factory* nil
  "How one exchange gets its bridge: a function of URL answering a BRIDGE.
NIL means the real one, OPEN-BRIDGE; a test binds a scripted one.")

(defvar *request-id* 0
  "The id of the last request this run sent; every request one more.")

(defun frame-text (message)
  "MESSAGE as text, whether the socket handed back a string or octets."
  (if (stringp message)
      message
      (sb-ext:octets-to-string (coerce message '(vector (unsigned-byte 8)))
                                :external-format :utf-8)))

(defun receive-text (mailbox url)
  "The next message of MAILBOX, or NIL once the socket reports a close."
  (let ((message (sb-concurrency:receive-message mailbox)))
    (cond ((and message (eq :message (first message))) (second message))
          ((and message (eq :error (first message)))
           (fail "the bridge at ~a failed: ~a" url (second message)))
          (t nil))))

(defun open-bridge (url)
  "A bridge to URL over the addon's loopback WebSocket, connected."
  (let* ((ws (wsd:make-client url))
         (mailbox (sb-concurrency:make-mailbox :name "nodecode-godot bridge")))
    (wsd:on :message ws
            (lambda (message)
              (sb-concurrency:send-message mailbox (list :message (frame-text message)))))
    (wsd:on :close ws
            (lambda (&key code reason)
              (declare (ignore reason))
              (sb-concurrency:send-message mailbox (list :close code))))
    (wsd:on :error ws
            (lambda (condition)
              (sb-concurrency:send-message mailbox (list :error (princ-to-string condition)))))
    (handler-case (sb-sys:with-deadline (:seconds (answer-seconds)) (wsd:start-connection ws))
      (error (condition)
        (ignore-errors (nlk:sever-websocket ws))
        (fail "the editor's bridge at ~a did not open: ~a" url condition)))
    (make-bridge (lambda (text) (wsd:send ws text))
                 (lambda () (receive-text mailbox url))
                 (lambda () (nlk:sever-websocket ws)))))

(defun decode-frame (text url)
  "TEXT as one decoded message; a frame that is not JSON is a failure."
  (handler-case (nlk:decode-json text)
    (error ()
      (fail "the bridge at ~a answered text that is not JSON: ~a"
            url (subseq text 0 (min 160 (length text)))))))

(defun exchange (url text)
  "TEXT's answer from the bridge at URL, or a failure naming what went wrong."
  (let ((bridge (funcall (or *bridge-factory* #'open-bridge) url)))
    (unwind-protect
         (handler-case
             (sb-sys:with-deadline (:seconds (answer-seconds))
               (funcall (bridge-send bridge) text)
               (loop repeat 64
                     for reply = (funcall (bridge-recv bridge))
                     for object = (and reply (decode-frame reply url))
                     when object return object
                     unless reply
                       do (fail "the editor at ~a closed the bridge before answering" url)))
           (sb-sys:deadline-timeout ()
             (fail "the editor at ~a did not answer within ~a s" url (answer-seconds))))
      (ignore-errors (funcall (bridge-close bridge))))))

(defun request-object (id action payload)
  "The one frame one request sends: the id, the token, the action, the payload."
  (let ((object (if (hash-table-p payload) (nlk:copy-json-object payload) (nlk:make-json-object)))
        (token (bridge-token)))
    (setf (gethash "id" object) id
          (gethash "action" object) action)
    (if token
        (setf (gethash "token" object) token)
        (setf (gethash "token" object) ""))
    object))

(defun rpc (action &optional payload)
  "One ARGOS exchange: ACTION carrying PAYLOAD, answered with its RESULT object."
  (let* ((id (incf *request-id*))
         (reply (exchange (endpoint) (nlk:encode-json-object (request-object id action payload)))))
    (unless (nlk:json-value reply :boolean "ok")
      (fail "~a was refused: ~a~@[ (~a)~]"
            action
            (or (nlk:json-value reply :string "error") "error")
            (nlk:json-value reply :string "detail")))
    (or (nlk:json-value reply :object "result") (nlk:make-json-object))))

(defparameter *answer-limit* 24000
  "How much of one answer the model reads before it is cut.")

(defun render (result)
  "RESULT as the indented JSON text the model reads."
  (let ((text (nlk:pretty-json result)))
    (if (<= (length text) *answer-limit*)
        text
        (format nil "~a~%... ~d more characters; ask for a subtree or a smaller max_nodes"
                (subseq text 0 *answer-limit*) (- (length text) *answer-limit*)))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (render (progn ,@body))
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: godot: ~a" condition)))))

;;; --- the tools ------------------------------------------------------------

(defun property-object (type doc)
  "One property of a tool schema: TYPE with DOC, an array of strings for
\"array\" and an array of objects for \"objects\"."
  (let ((object (nlk:make-json-object "description" doc)))
    (cond ((equal type "string") (setf (gethash "type" object) "string"))
          ((equal type "integer") (setf (gethash "type" object) "integer"))
          ((equal type "number") (setf (gethash "type" object) "number"))
          ((equal type "boolean") (setf (gethash "type" object) "boolean"))
          ((equal type "object") (setf (gethash "type" object) "object"))
          ((equal type "array")
           (setf (gethash "type" object) "array"
                 (gethash "items" object) (nlk:make-json-object "type" "string")))
          ((equal type "objects")
           (setf (gethash "type" object) "array"
                 (gethash "items" object) (nlk:make-json-object "type" "object"))))
    object))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (property-object type doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defun forwarded (args names)
  "ARGS' members NAMES holds, as the request payload this action sends."
  (let ((object (nlk:make-json-object)))
    (when (hash-table-p args)
      (dolist (name names object)
        (multiple-value-bind (value present) (gethash name args)
          (when present (setf (gethash name object) value)))))))

(defmacro define-tool (schema-name function action description &body properties)
  "Define SCHEMA-NAME and FUNCTION-NAME: a Nodecode tool that forwards to
ACTION on the editor, with the arguments the original's tools name."
  `(progn
     (defparameter ,schema-name (tool-schema ,description ',properties))
     (defun ,function (args)
       ,(format nil "The ~a tool: one ARGOS exchange with the live editor." action)
       (answering (rpc ,action (forwarded args ',(mapcar #'first properties)))))))

(define-tool *ping-schema* tool-ping "ping"
  "Check that the editor answers at all, through the argos addon in it."
  )

(define-tool *status-schema* tool-status "status"
  "Editor status: Godot version, project path, edited scene, playing flag and port."
  )

(define-tool *tree-schema* tool-tree "scene.get_tree"
  "JSON node tree of the edited scene: name, type, relative path, script and position per node. max_nodes and max_depth trim it, and truncated says they did."
  ("max_nodes" "integer" "Most nodes to return; 2000 by default, 20000 at most." nil)
  ("max_depth" "integer" "Deepest level to return; 12 by default, 64 at most." nil))

(define-tool *open-schema* tool-open "scene.open"
  "Open a scene in the editor, given its res:// path."
  ("path" "string" "The scene to open, as res://path/scene.tscn." t))

(define-tool *save-schema* tool-save "scene.save"
  "Save the edited scene to disk."
  )

(define-tool *reload-schema* tool-reload "scene.reload"
  "Reload the edited scene from disk, dropping unsaved edits."
  )

(define-tool *new-schema* tool-new "scene.new"
  "Create a new scene at a res:// path and open it."
  ("path" "string" "Where to save the new scene, as res://path/scene.tscn." t))

(define-tool *create-schema* tool-create "node.create"
  "Create a node of a Godot class under a parent in the edited scene, as one undoable action. props takes the property names Godot uses, and @vec2:x,y and @color:r,g,b,a for vectors and colours."
  ("parent" "string" "The parent node's path relative to the scene root, or '' for the root." nil)
  ("class" "string" "The Godot class to instantiate, such as Node2D or Sprite2D." t)
  ("name" "string" "The node's name." nil)
  ("props" "object" "Properties to set on the new node, by Godot property name." nil))

(define-tool *instantiate-schema* tool-instantiate "node.instantiate"
  "Instance a saved scene under a parent in the edited scene."
  ("scene" "string" "The scene to instance, as res://path/scene.tscn." t)
  ("parent" "string" "The parent node's path relative to the scene root, or '' for the root." nil)
  ("name" "string" "The instance's name." nil))

(define-tool *reparent-schema* tool-reparent "node.reparent"
  "Move a node under another parent in the edited scene."
  ("path" "string" "The node's path relative to the scene root." t)
  ("parent" "string" "The new parent's path relative to the scene root, or '' for the root." t)
  ("keep_transform" "boolean" "Keep the node's world transform." nil))

(define-tool *get-schema* tool-get "node.get_prop"
  "Read one property of a node; vectors and colours come back as @vec2: and @color:."
  ("path" "string" "The node's path relative to the scene root." t)
  ("prop" "string" "The Godot property name." t))

(define-tool *set-schema* tool-set "node.set_prop"
  "Set one property of a node, as one undoable action. Accepts @vec2:x,y and @color:r,g,b,a."
  ("path" "string" "The node's path relative to the scene root." t)
  ("prop" "string" "The Godot property name." t)
  ("value" nil "The value to set: a number, string, boolean, or @vec2:/@color: text." t))

(define-tool *delete-schema* tool-delete "node.delete"
  "Delete a node from the edited scene, undoably; the scene root itself cannot be deleted."
  ("path" "string" "The node's path relative to the scene root." t))

(define-tool *batch-schema* tool-batch "action.batch"
  "Apply up to 64 node edits as one undo group: node.create, node.instantiate, node.get_prop, node.set_prop, node.delete and node.reparent. A nested batch or an undo inside it is refused."
  ("name" "string" "The undo group's name, 80 characters at most." nil)
  ("on_error" "string" "abort (the default) stops at the first failed op; continue carries on." nil)
  ("ops" "objects" "The operations, each an object with its own action and arguments." t))

(define-tool *undo-schema* tool-undo "undo.undo"
  "Undo the editor's last action, ARGOS edits included."
  )

(define-tool *redo-schema* tool-redo "undo.redo"
  "Redo the last undone action."
  )

(define-tool *anim-list-schema* tool-anim-list "anim.list"
  "List the AnimationPlayer and AnimationTree nodes under a path, or the whole edited scene. Read-only."
  ("path" "string" "The node to list from, relative to the scene root; omit for the whole scene." nil))

(define-tool *anim-get-schema* tool-anim-get "anim.get"
  "Read one AnimationPlayer (names, length, loop mode, track count) or one AnimationTree (active flag, state names). Read-only."
  ("path" "string" "The AnimationPlayer or AnimationTree's path relative to the scene root." t))

(define-tool *anim-play-schema* tool-anim-play "anim.play"
  "Play a named animation on an AnimationPlayer, or travel to a state on an AnimationTree. Playback only: it changes nothing saved."
  ("path" "string" "The AnimationPlayer or AnimationTree's path relative to the scene root." t)
  ("name" "string" "The animation or state name." nil)
  ("custom_speed" "number" "Playback speed as a multiple." nil)
  ("from_end" "boolean" "Play the animation backwards from its end." nil))

(define-tool *anim-stop-schema* tool-anim-stop "anim.stop"
  "Stop an AnimationPlayer and seek to 0, or deactivate an AnimationTree."
  ("path" "string" "The AnimationPlayer or AnimationTree's path relative to the scene root." t)
  ("seek_zero" "boolean" "false keeps the current pose on an AnimationPlayer." nil))

(define-tool *fs-scan-schema* tool-fs-scan "fs.scan"
  "Rescan the project's files after they were written from outside the editor."
  )

(defparameter *play-schema*
  (tool-schema
   "Play the game: the project's main scene, or the scene in scene_path. Playing runs that project's own GDScript."
   '(("scene_path" "string" "A res:// scene to play instead of the project's main scene." nil))))

(defparameter *stop-schema*
  (tool-schema
   "Stop the running game."
   '()))

(defparameter *capture-schema*
  (tool-schema
   "Capture a PNG of the 2D editor viewport, or of a rendered scene when scene_path is passed. The editor needs a window. out is res://....png or .argos/captures/....png; with session the file goes under .argos/captures/<session>/ and an omitted name takes the next frame_NNN.png. No absolute paths, no '..', no overwrite."
   '(("out" "string" "The PNG to write, as res://....png or .argos/captures/....png, or a bare name when session is set." nil)
  ("session" "string" "The folder under .argos/captures/ to write into, [A-Za-z0-9_-]{1,64}." nil)
  ("scene_path" "string" "A res:// scene to render instead of the editor viewport." nil)
  ("w" "integer" "The PNG's width in pixels, 16 to 4096." nil)
  ("h" "integer" "The PNG's height in pixels, 16 to 4096." nil))))

(define-tool *raw-schema* tool-raw-schema "raw"
  "Escape hatch: send any ARGOS action with a JSON payload, for what no other tool covers."
  ("action" "string" "The ARGOS action to send, such as scene.get_tree." t)
  ("payload" "object" "The action's own arguments, if any." nil))

(defun tool-play (args)
  "Play the project's main scene, or the scene named in scene_path."
  (answering
   (let ((scene (nlk:json-value args :string "scene_path")))
     (if (and scene (plusp (length scene)))
         (rpc "run.play_scene" (nlk:make-json-object "path" scene))
         (rpc "run.play_main")))))

(defun tool-stop (args)
  "Stop the running game."
  (declare (ignore args))
  (answering (rpc "run.stop")))

(defun tool-capture (args)
  "Capture a PNG: the 2D editor viewport, or a rendered scene when scene_path is passed."
  (answering
   (let* ((scene (nlk:json-value args :string "scene_path"))
          (payload (forwarded args '("out" "session" "w" "h"))))
     (if (and scene (plusp (length scene)))
         (progn (setf (gethash "path" payload) scene)
                (rpc "capture.scene" payload))
         (rpc "capture.editor_2d" payload)))))

(defun tool-raw (args)
  "Escape hatch: send any ARGOS action with a JSON payload."
  (answering
   (let ((action (nlk:json-value args :string "action")))
     (unless (non-empty action)
       (fail "godot_raw needs \"action\": the ARGOS action to send"))
     (rpc action (nlk:json-value args :object "payload")))))

(defparameter *primer*
  "godot drives a live Godot 4 editor through the argos addon in it. Use the
editor for the scene it has open: create nodes, set properties, batch those
edits into one undo group, play animations, capture PNGs, press play. Edit .gd,
.tscn and project.godot on disk when the change is source, then call
godot_fs_scan. Prefer godot_batch, godot_anim_* and godot_capture over
godot_raw. Captures are PNG only, at res://....png or .argos/captures/....png,
never absolute and never over an existing file. Playing a scene runs that
project's own GDScript, so only do it in a project you trust."
  "What (help :godot) says about using the editor.")

(defun godot-version (status)
  "The Godot build STATUS names, as one line."
  (or (nlk:json-value status :string "godot" "string")
      (let ((version (nlk:json-value status :object "godot")))
        (and version
             (format nil "~a.~a.~a"
                     (or (nlk:json-value version :integer "major") "?")
                     (or (nlk:json-value version :integer "minor") "?")
                     (or (nlk:json-value version :integer "patch") "?"))))
      "unknown"))

(defun status-line ()
  "The report /godot answers with: the endpoint and what the editor says of itself."
  (running-settings)
  (handler-case
      (let ((status (rpc "status")))
        (format nil "~a~%editor: Godot ~a~%scene: ~a~%playing: ~a~%~
                     tools: godot_ping, godot_status, godot_scene_tree, ~
                     godot_scene_open/save/reload/new, godot_node_create/instantiate/~
                     reparent/get/set/delete, godot_batch, godot_undo, godot_redo, ~
                     godot_anim_list/get/play/stop, godot_play, godot_stop, godot_capture, ~
                     godot_fs_scan, godot_raw"
                (describe-endpoint)
                (godot-version status)
                (let ((scene (nlk:json-value status :string "edited_scene")))
                  (if (and scene (plusp (length scene))) scene "none open"))
                (if (nlk:json-value status :boolean "is_playing") "yes" "no")))
    (error (condition)
      (format nil "~a~%no answer: ~a" (describe-endpoint) condition))))

(defun run-godot-command (args session-id)
  "One /godot invocation: the endpoint and what the editor says of itself."
  (declare (ignore args session-id))
  (status-line))

;;; --- the cell -------------------------------------------------------------

(nle:define-cell godot
  (:section ("godot")
    (:guide "point \"project\" at the Godot project root that has the argos addon enabled; the cell reads .argos/port and .argos/token from it while the editor is open; next step: /godot shows the editor")
    ("project" :string :default ""
     :doc "the Godot project root the argos addon runs in (the folder holding project.godot)")
    ("port" :integer :default 0 :min 0
     :doc "the bridge port, when it is not the one the editor publishes under .argos/port (0 reads that file)")
    ("token" :secret
     :doc "the bridge token, when it is not the one the editor publishes under .argos/token")
    ("timeout_seconds" :integer :default 10 :min 1
     :doc "one exchange's ceiling"))
  (:help :godot "godot: the live Godot 4 editor - scene tree, nodes, batches, animation, play and screenshots, through the argos addon" *primer*)
  (:command "godot" 'run-godot-command
            :description "Show the editor the godot cell is attached to")
  (:tool "godot_ping" *ping-schema* 'tool-ping)
  (:tool "godot_status" *status-schema* 'tool-status)
  (:tool "godot_scene_tree" *tree-schema* 'tool-tree)
  (:tool "godot_scene_open" *open-schema* 'tool-open)
  (:tool "godot_scene_save" *save-schema* 'tool-save)
  (:tool "godot_scene_reload" *reload-schema* 'tool-reload)
  (:tool "godot_scene_new" *new-schema* 'tool-new)
  (:tool "godot_node_create" *create-schema* 'tool-create)
  (:tool "godot_node_instantiate" *instantiate-schema* 'tool-instantiate)
  (:tool "godot_node_reparent" *reparent-schema* 'tool-reparent)
  (:tool "godot_node_get" *get-schema* 'tool-get)
  (:tool "godot_node_set" *set-schema* 'tool-set)
  (:tool "godot_node_delete" *delete-schema* 'tool-delete)
  (:tool "godot_batch" *batch-schema* 'tool-batch)
  (:tool "godot_undo" *undo-schema* 'tool-undo)
  (:tool "godot_redo" *redo-schema* 'tool-redo)
  (:tool "godot_anim_list" *anim-list-schema* 'tool-anim-list)
  (:tool "godot_anim_get" *anim-get-schema* 'tool-anim-get)
  (:tool "godot_anim_play" *anim-play-schema* 'tool-anim-play)
  (:tool "godot_anim_stop" *anim-stop-schema* 'tool-anim-stop)
  (:tool "godot_play" *play-schema* 'tool-play)
  (:tool "godot_stop" *stop-schema* 'tool-stop)
  (:tool "godot_capture" *capture-schema* 'tool-capture)
  (:tool "godot_fs_scan" *fs-scan-schema* 'tool-fs-scan)
  (:tool "godot_raw" *raw-schema* 'tool-raw)
  (:start (lambda ()
            (check-config)
            (setf *request-id* 0)
            (nle:on-stop (lambda () (setf *request-id* 0))))))
