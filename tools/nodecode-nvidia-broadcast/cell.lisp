;;;; cell.lisp --- NVIDIA Broadcast's ten tools, over its local MCP gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NVIDIA Broadcast 2.3 and newer runs a streamable-HTTP MCP server on
;;;; 127.0.0.1 (the first free port from 18100, 18100 by default) that drives
;;;; the app: camera, microphone and speaker effects, Studio Voice profiles,
;;;; device selection, camera resolution, and effects on local media files.
;;;; Nothing leaves loopback and there is no key to find.
;;;;
;;;; This cell is the MCP client and the ten tools: one POST per JSON-RPC
;;;; message, the Mcp-Session-Id the initialize answer issues echoed on every
;;;; later one, and the answer read out of whichever body came back - one
;;;; JSON object, or the server-sent events that carry it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "nvidia-broadcast": {"url": "http://127.0.0.1:18100/gateway"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from NousResearch's Hermes Agent plugin (see NOTICE): the ten
;;;; tools, their arguments and answer shapes, and the guidance the original
;;;; skill gave the model.

(in-package #:nodecode-nvidia-broadcast)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "NVIDIA Broadcast is available through the nodecode-nvidia-broadcast cell: ten tools, which a
person can also call from eval in the nvidia-broadcast package. They drive the NVIDIA
Broadcast app on this machine over its local MCP gateway; nothing leaves loopback. The app must
be installed at 2.3 or newer and running in the desktop session, or no tools answer.
  get_broadcast_state  {effect_type}
      Current effects as effectId -> {enabled, ...params}, grouped by camera, microphone and
      speaker. Read-only. warnings[] marks a section whose effects are on but whose device is
      unselected. On connect the gateway's instructions already list the effectIds, so you do
      not need a discovery call to act.
  set_effects  {effects, action}
      Turn effects on or off and tune them in ONE call: effects is an array of
      {effectId, enabled, params}, each effectId at most once, params nested under \"params\"
      (numeric values are 0-1; modes are enums). Its answer carries the resulting state, so
      never follow it with get_broadcast_state only to confirm. Or send only
      action \"restore_previous_live\" to restore the Live snapshot that file processing
      suspended.
  describe_effects  {effect_id, section}
      What each effect does, its guidance, GPU and beta badges and its params with ranges. For
      explaining or weighing a tradeoff, not for acting.
  list_devices  {kind}
      The cameras, microphones and speakers, the active one, and for cameras the current
      resolution plus availableResolutions. Call it fresh every time: ids change between turns.
      Never reuse a deviceId from an earlier turn.
  set_active_device  {kind, device_id, width, height, frame_rate}
      Switch the active device. For a camera, pass width and height (and frameRate) too, to
      switch device and resolution in ONE call: a camera switch restarts the stream.
  set_camera_resolution  {device_id, width, height, frame_rate}
      Change the active camera's resolution only. Prefer set_active_device when the device is
      switching anyway.
  submit_file_processing  {input_path, effects, batch_id, output_folder, output_path}
      Queue one local media file for processing with the same effects. The answer carries an id
      and a batchId: keep the batchId and send it with every other file of the batch.
  get_file_processing_jobs  {job_ids, batch_id, limit}
      One job, or the recent ones, as a snapshot. Do not poll it in a loop.
  wait_for_processing_update  {batch_id, job_ids, cursor, timeout_ms}
      The one way to wait: it wakes for job-status and queue-pause changes. Take a batch
      snapshot without cursor, then pass the returned cursor for the next change. completed
      means every file completed; mixed means terminal outcomes differ and mixed is not
      success.
  control_file_processing_jobs  {action, job_ids, all}
      pause or resume the queue, cancel or retry jobs. Cancellation is final until the user
      asks again; a queue the user paused is resumed only after asking them.
Reading the answers, the way the gateway reports them: success false carries failed[] with a
reason (timeout, unavailable, device_not_selected, file_not_found) and skipped[] with the
effect it conflicts with - report which effects did not apply, never the batch as done.
requiresUserInput true means stop and ask the user: ambiguous_frame_rate (with the options)
means ask which frame rate, device_not_selected means ask them to select that device, and
file_not_found means ask for a real path. An error -32602 means the arguments were wrong and
will stay wrong; an error -32603 with retryable true means the app was busy, so back off and
retry. A -33201 or failed[].reason device_not_selected means that effect's camera, microphone
or speaker is not selected: name the device type and ask the user to select one. When the
gateway does not answer at all, NVIDIA Broadcast is not running (or not installed, or older
than 2.3): say so, and do not guess at a device or an effect."
  "What (help :nvidia-broadcast) answers while the cell runs.")

;;; --- what the gateway speaks ----------------------------------------------------

(defparameter +protocol-version+ "2025-06-18"
  "The MCP protocol version this client announces in initialize.")

(defparameter +client-name+ "nodecode-nvidia-broadcast"
  "The clientInfo name this cell gives the gateway.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (nvidia-broadcast-error (condition)
       (nle:failure (format nil "ERROR: ~a" (nvidia-broadcast-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: NVIDIA Broadcast: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun blank (text)
  "TEXT trimmed when it is a non-empty string, else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun arg-object (&rest pairs)
  "A JSON object of PAIRS, with the members no call needs left out: NIL, an
empty string, an empty array. JSON false is NIL too, so a member the model
answered false is sent as absent, which every gateway field treats the same."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          when (and value
                    (not (and (stringp value) (zerop (length (nlk:trimmed value)))))
                    (not (and (vectorp value) (zerop (length value)))))
            do (setf (gethash key object) value))
    object))

(defun array-of (value)
  "VALUE as a JSON array, or NIL when it is absent or empty."
  (cond ((null value) nil)
        ((and (vectorp value) (zerop (length value))) nil)
        ((and (listp value) (null value)) nil)
        ((vectorp value) value)
        ((listp value) (coerce value 'vector))
        (t (vector value))))

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the URL trimmed and checked: one endpoint, on
loopback, which is what the gateway publishes."
  (declare (ignore table))
  (let* ((url (nlk:trimmed (or (getf values :url) "")))
         (url (string-right-trim "/" url))
         (plist (copy-list values)))
    (unless (plusp (length url))
      (nlk:config-error "url is empty: set it to the gateway's endpoint, http://127.0.0.1:18100/gateway unless the app reports another port"))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "url ~s has no http:// or https:// scheme: set it to the whole endpoint, e.g. \"http://127.0.0.1:18100/gateway\"" url))
    (setf (getf plist :url) url)
    plist))

;;; --- the MCP client -------------------------------------------------------------

(defvar *session-id* nil
  "The Mcp-Session-Id the initialize answer issued, or NIL before one has.")

(defvar *initialized* nil
  "Whether this cell has initialized a session with the gateway.")

(defvar *request-id* 0
  "The last JSON-RPC id this cell sent; every request takes the next one.")

(defun reset-session ()
  "Forget the session: the next call initializes a new one."
  (setf *session-id* nil
        *initialized* nil))

(defun header-value (headers name)
  "NAME's value from the response headers: dexador's table, or an alist."
  (cond ((hash-table-p headers) (gethash name headers))
        ((consp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun note-session-id (headers)
  "Keep the session id a response issued, if it issued one."
  (let ((session (header-value headers "mcp-session-id")))
    (when (and (stringp session) (plusp (length session)))
      (setf *session-id* session))))

(defun request-headers ()
  "The headers every message carries: MCP's, and the session's once one exists."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . +protocol-version+))
          (when *session-id*
            (list (cons "mcp-session-id" *session-id*)))))

(defun sse-payloads (text)
  "The `data:' payload of every server-sent event in TEXT, oldest first."
  (let ((payloads '())
        (data '()))
    (flet ((flush ()
             (when data
               (push (format nil "~{~a~^~%~}" (nreverse data)) payloads)
               (setf data '()))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (let ((line (string-right-trim '(#\Return) line)))
          (cond ((uiop:string-prefix-p "data:" line)
                 (push (string-left-trim " " (subseq line 5)) data))
                ((zerop (length (nlk:trimmed line))) (flush)))))
      (flush))
    (nreverse payloads)))

(defun response-messages (body headers)
  "The JSON-RPC messages BODY carries: one object, a batch array, or the
events of a server-sent stream."
  (let* ((text (cond ((stringp body) body)
                     ((streamp body) (or (ignore-errors (nlk:body-text body)) ""))
                     (t "")))
         (type (string-downcase (or (header-value headers "content-type") ""))))
    (cond ((search "event-stream" type)
           (loop for payload in (sse-payloads text)
                 for value = (ignore-errors (nlk:decode-json payload))
                 when (hash-table-p value) collect value))
          ((zerop (length (nlk:trimmed text))) '())
          (t (let ((value (ignore-errors (nlk:decode-json text))))
               (cond ((hash-table-p value) (list value))
                     ((vectorp value)
                      (loop for item across value when (hash-table-p item) collect item))
                     (t '())))))))

(defun post-message (payload &key (timeout (setting :timeout)))
  "POST one JSON-RPC message to the gateway => (values MESSAGES STATUS). A
dial that fails is a refusal naming the endpoint, since the usual cause is
that NVIDIA Broadcast is not running."
  (handler-case
      (multiple-value-bind (body status headers)
          (nlk:http :post (setting :url)
                    :headers (request-headers)
                    :content (nlk:encode-json-object payload)
                    :timeout timeout
                    :connect-timeout (min 10 timeout))
        (note-session-id headers)
        (values (response-messages body headers) status))
    (error (condition)
      (fail "the NVIDIA Broadcast gateway at ~a did not answer: ~a. Is NVIDIA Broadcast 2.3 or newer running? ~a"
            (setting :url)
            (nlk:one-line (princ-to-string condition))
            "The tools appear while the app runs, and the gateway listens on loopback only."))))

(defun error-text (error)
  "A JSON-RPC error object's words."
  (or (blank (nlk:json-value error :string "message"))
      (nlk:encode-json-object error)))

(defun stale-session-error-p (error)
  "Whether ERROR is the gateway saying the session it was given is gone."
  (let ((text (string-downcase (error-text error))))
    (and (search "session" text) t)))

(defun rpc (method params &key (timeout (setting :timeout)) (retry t))
  "One JSON-RPC request => its result object. A session the gateway no longer
knows is re-initialized once; any other error is a refusal carrying the
gateway's own words."
  (let ((id (incf *request-id*)))
    (multiple-value-bind (messages status)
        (post-message (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)
                      :timeout timeout)
      (let ((answer (find id messages :key (lambda (message) (nlk:json-value message :integer "id")))))
        (cond
          ((null answer)
           (if (and retry (member status '(400 404)))
               (progn (reset-session)
                      (initialize :timeout timeout)
                      (rpc method params :timeout timeout :retry nil))
               (fail "~a got no answer from the gateway (HTTP ~a): ~a"
                     method status
                     "the app may still be starting; ask again in a few seconds")))
          (t (let ((error (nlk:json-value answer :object "error")))
               (when error
                 (if (and retry (stale-session-error-p error))
                     (progn (reset-session)
                            (initialize :timeout timeout)
                            (return-from rpc (rpc method params :timeout timeout :retry nil)))
                     (fail "the gateway answered ~a with error ~a: ~a"
                           method (nlk:json-value error :integer "code") (error-text error))))
               (nlk:json-value answer :object "result"))))))))

(defun initialize (&key (timeout (setting :timeout)))
  "Open an MCP session: initialize, then the initialized notification."
  (setf *initialized* nil)
  (rpc "initialize"
       (nlk:json-object "protocolVersion" +protocol-version+
                         "capabilities" (nlk:make-json-object)
                         "clientInfo" (nlk:json-object "name" +client-name+
                                                         "version" "0.1.0"))
       :timeout timeout :retry nil)
  (post-message (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized")
                :timeout timeout)
  (setf *initialized* t))

(defun ensure-session (&key (timeout (setting :timeout)))
  (unless *initialized*
    (initialize :timeout timeout)))

(defun tool-text (result)
  "RESULT's text, as the model reads it: the content items, or the
structuredContent when the gateway sent no text. isError makes it a failure."
  (let* ((content (nlk:json-array result "content"))
         (texts (and (vectorp content)
                     (loop for item across content
                           for text = (nlk:json-value item :string "text")
                           when (and text (plusp (length text))) collect text)))
         (structured (nlk:json-value result :object "structuredContent"))
         (text (cond (texts (format nil "~{~a~^~%~}" texts))
                     (structured (nlk:encode-json-object structured))
                     (t ""))))
    (if (nlk:json-value result :boolean "isError")
        (nle:failure (format nil "ERROR: ~a"
                              (if (plusp (length (nlk:trimmed text)))
                                  text
                                  "the gateway reported a failed call")))
        text)))

(defun call-tool (name arguments &key (timeout (setting :timeout)))
  "One MCP tools/call, by the gateway's own tool NAME, => its text."
  (ensure-session :timeout timeout)
  (tool-text (rpc "tools/call"
                  (nlk:json-object "name" name "arguments" arguments)
                  :timeout timeout)))

;;; --- the ten tools --------------------------------------------------------------

(defun broadcast-state (call)
  "get_broadcast_state: the current effects, grouped by section."
  (answering
    (call-tool "get_broadcast_state"
               (arg-object "effectType" (blank (state-read-effect-type call))))))

(defun describe-effects (call)
  "describe_effects: what the effects do, and the params each takes."
  (answering
    (call-tool "describe_effects"
               (arg-object "effectId" (blank (effects-describe-effect-id call))
                           "section" (blank (effects-describe-section call))))))

(defun set-effects (call)
  "set_effects: one call for every effect to change, or the Live restore."
  (answering
    (let ((action (blank (effects-set-action call)))
          (effects (array-of (effects-set-effects call))))
      (cond (action (call-tool "set_effects" (arg-object "action" action)))
            (effects (call-tool "set_effects" (arg-object "effects" effects)))
            (t (refuse "set_effects needs either an effects array or action \"restore_previous_live\""))))))

(defun list-devices (call)
  "list_devices: the cameras, microphones and speakers, and the active one."
  (answering
    (call-tool "list_devices"
               (arg-object "kind" (or (blank (device-list-kind call)) "all")))))

(defun set-active-device (call)
  "set_active_device: switch a device, and a camera's resolution with it."
  (answering
    (call-tool "set_active_device"
               (arg-object "kind" (blank (device-set-kind call))
                           "deviceId" (blank (device-set-device-id call))
                           "width" (device-set-width call)
                           "height" (device-set-height call)
                           "frameRate" (device-set-frame-rate call)))))

(defun set-camera-resolution (call)
  "set_camera_resolution: change the active camera's size."
  (answering
    (call-tool "set_camera_resolution"
               (arg-object "deviceId" (blank (resolution-set-device-id call))
                           "width" (resolution-set-width call)
                           "height" (resolution-set-height call)
                           "frameRate" (resolution-set-frame-rate call)))))

(defun submit-file (call)
  "submit_file_processing: queue one local media file."
  (answering
    (call-tool "submit_file_processing"
               (arg-object "_version" 1
                           "inputPath" (blank (file-submit-input-path call))
                           "effects" (array-of (file-submit-effects call))
                           "batchId" (blank (file-submit-batch-id call))
                           "outputFolder" (blank (file-submit-output-folder call))
                           "outputPath" (blank (file-submit-output-path call))))))

(defun file-jobs (call)
  "get_file_processing_jobs: one job or the recent ones."
  (answering
    (call-tool "get_file_processing_jobs"
               (arg-object "_version" 1
                           "jobIds" (array-of (file-jobs-job-ids call))
                           "batchId" (blank (file-jobs-batch-id call))
                           "limit" (file-jobs-limit call)))))

(defun wait-for-update (call)
  "wait_for_processing_update: the one wait for file processing."
  (answering
    (let* ((timeout-ms (file-wait-timeout-ms call))
           (seconds (+ 10 (if timeout-ms (ceiling timeout-ms 1000) 0))))
      (call-tool "wait_for_processing_update"
                 (arg-object "_version" 1
                             "batchId" (blank (file-wait-batch-id call))
                             "jobIds" (array-of (file-wait-job-ids call))
                             "cursor" (blank (file-wait-cursor call))
                             "timeoutMs" timeout-ms)
                 :timeout (max (setting :timeout) seconds)))))

(defun control-jobs (call)
  "control_file_processing_jobs: pause, resume, cancel or retry."
  (answering
    (call-tool "control_file_processing_jobs"
               (arg-object "_version" 1
                           "action" (blank (file-control-action call))
                           "jobIds" (array-of (file-control-job-ids call))
                           "all" (file-control-all call)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell nvidia-broadcast
  (:section ("nvidia-broadcast")
    (:guide "NVIDIA Broadcast 2.3 or newer on Windows, running in your desktop session; there is no key, because the gateway answers on loopback; url is the gateway's endpoint and only needs setting when the app bound another port (18101-18109); next step: ask for the camera effects")
    ("url" :string :default "http://127.0.0.1:18100/gateway"
     :doc "the gateway's endpoint: its default port is 18100, and the app takes the first free port from there")
    ("timeout" :integer :default 60 :min 1
     :doc "how long one request to the gateway may take, in seconds"))
  (:settings #'read-settings)
  (:help :nvidia-broadcast
         "nvidia-broadcast: get_broadcast_state, set_effects, describe_effects, list_devices, set_active_device, set_camera_resolution, submit_file_processing, get_file_processing_jobs, wait_for_processing_update and control_file_processing_jobs drive NVIDIA Broadcast over its local MCP gateway"
         *manual*)
  (:tool "get_broadcast_state" (nlk:contract 'state-read) #'broadcast-state)
  (:tool "describe_effects" (nlk:contract 'effects-describe) #'describe-effects)
  (:tool "set_effects" (nlk:contract 'effects-set) #'set-effects)
  (:tool "list_devices" (nlk:contract 'device-list) #'list-devices)
  (:tool "set_active_device" (nlk:contract 'device-set) #'set-active-device)
  (:tool "set_camera_resolution" (nlk:contract 'resolution-set) #'set-camera-resolution)
  (:tool "submit_file_processing" (nlk:contract 'file-submit) #'submit-file)
  (:tool "get_file_processing_jobs" (nlk:contract 'file-jobs) #'file-jobs)
  (:tool "wait_for_processing_update" (nlk:contract 'file-wait) #'wait-for-update)
  (:tool "control_file_processing_jobs" (nlk:contract 'file-control) #'control-jobs))
