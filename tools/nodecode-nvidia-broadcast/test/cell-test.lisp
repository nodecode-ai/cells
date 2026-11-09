;;;; cell-test.lisp --- the nvidia-broadcast cell against a stubbed gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NVIDIA Broadcast's gateway is a stubbed NLK:HTTP: it speaks JSON-RPC
;;;; back, issues a session id from initialize, echoes nothing but what the
;;;; test asked for, and never dials loopback. Each tool is called the way a
;;;; model's call reaches it - through the real dispatch, contract and all -
;;;; so the schema the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "nvidia-broadcast" "NVIDIA-BROADCAST-CELL-"
  :start nodecode-nvidia-broadcast:start-cell
  :defaults (("url" "http://127.0.0.1:18100/gateway")))

;;; --- the stubbed gateway ----------------------------------------------------------

(defvar *gateway-tools* nil
  "TOOL-NAME -> a function of ARGUMENTS answering the text the tool replies with.")

(defvar *gateway-is-error* nil
  "When true, every tools/call answers isError true, as a failed call does.")

(defvar *gateway-events* nil
  "When true, answers arrive as server-sent events, the streamable transport's other body.")

(defun json-headers (&optional session)
  "A response header table, with the session id the initialize answer issues."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table)
          (if *gateway-events* "text/event-stream" "application/json"))
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun rpc-body (id result)
  "The JSON-RPC answer carrying RESULT for ID, as the wire carries it."
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun tool-result (text)
  "A tools/call result object holding TEXT, and isError when the test says so."
  (let ((result (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text)))))
    (when *gateway-is-error* (setf (gethash "isError" result) t))
    result))

(defun event-body (text)
  "TEXT as the body of one server-sent event."
  (format nil "event: message~%data: ~a~%~%" text))

;;; --- the stub, and reading back what the cell sent --------------------------------

(defmacro with-gateway ((asked) &body body)
  "BODY with NLK:HTTP answering as NVIDIA Broadcast's gateway does. ASKED
collects (METHOD URL CONTENT HEADERS), oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (let* ((content (getf options :content))
                                          (payload (nlk:decode-json content))
                                          (id (nlk:json-value payload :integer "id"))
                                          (rpc (nlk:json-value payload :string "method")))
                                     (push (list method url content (getf options :headers)) ,log)
                                     (cond
                                       ((equal rpc "initialize")
                                        (values (rpc-body id (nlk:json-object
                                                               "protocolVersion" "2025-06-18"
                                                               "capabilities" (nlk:make-json-object)
                                                               "serverInfo" (nlk:json-object
                                                                             "name" "nvidia-broadcast")))
                                                200 (json-headers "session-1")))
                                       ((equal rpc "notifications/initialized")
                                        (values "" 202 (json-headers "session-1")))
                                       ((equal rpc "tools/call")
                                        (let* ((params (nlk:json-value payload :object "params"))
                                               (name (nlk:json-value params :string "name"))
                                               (arguments (or (nlk:json-value params :object "arguments")
                                                              (nlk:make-json-object)))
                                               (tool (cdr (assoc name *gateway-tools* :test #'equal)))
                                               (text (if tool
                                                         (funcall tool arguments)
                                                         (error "the cell called ~a, which this test did not stub" name)))
                                               (body (rpc-body id (tool-result text))))
                                          (if *gateway-events*
                                              (values (event-body body) 200 (json-headers "session-1"))
                                              (values body 200 (json-headers "session-1")))))
                                       (t (error "unexpected JSON-RPC method ~a" rpc)))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun tool-calls (asked)
  "The tools/call entries of ASKED, oldest first."
  (remove-if-not (lambda (entry)
                   (equal "tools/call"
                          (nlk:json-value (nlk:decode-json (third entry)) :string "method")))
                 asked))

(defun sent-arguments (entry)
  "The arguments object ENTRY's tool call carried."
  (nlk:json-value (nlk:decode-json (third entry)) :object "params" "arguments"))

(defun sent-headers (entry)
  "ENTRY's request headers, as an alist lookup."
  (fourth entry))

(defun header-of (entry name)
  (cdr (assoc name (sent-headers entry) :test #'string-equal)))

(defun wire-answer (id tool arguments &optional (context (compiled-context '())))
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments context)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "nvidia-broadcast"
  (:config "url" "http://127.0.0.1:18100/gateway")
  (:help :nvidia-broadcast)
  (:refused ("url" "127.0.0.1:18100/gateway") ("timeout" 0)))

(deftest nvidia-broadcast-cell-registers-the-ten-tools ()
  ;; The original plugin's tool set, by the names its skill taught the model.
  (with-cell-stop ((nvidia-broadcast-start))
    (dolist (name '("get_broadcast_state" "describe_effects" "set_effects" "list_devices"
                    "set_active_device" "set_camera_resolution" "submit_file_processing"
                    "get_file_processing_jobs" "wait_for_processing_update"
                    "control_file_processing_jobs"))
      (is-present (tool (nle::find-tool name)) (format nil "~a is registered" name)))))

;;; --- reading the effects ------------------------------------------------------------

(defparameter +state+
  "{\"camera\":{\"background_blur\":{\"enabled\":true,\"strength\":0.7}},\"mic\":{\"noise_removal\":{\"enabled\":true}}}"
  "The state object the gateway answers get_broadcast_state with.")

(deftest nvidia-broadcast-cell-reads-the-state
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "get_broadcast_state" (lambda (arguments) (declare (ignore arguments)) +state+)))))
      (with-gateway (asked)
        (let ((answer (wire-answer "nb-1" "get_broadcast_state" "{\"effect_type\":\"camera\"}")))
          (is (search "background_blur" answer) "the state comes back as the gateway said it")
          (is (equal "camera" (nlk:json-value (sent-arguments (first (tool-calls asked))) :string "effectType"))
              "effect_type is mapped onto the gateway's effectType")
          (is (string= "session-1" (header-of (first (tool-calls asked)) "mcp-session-id"))
              "every call after initialize echoes the session id it issued")
          (is (equal "initialize" (nlk:json-value (nlk:decode-json (third (first asked))) :string "method"))
              "and initialize is the first thing the cell sends"))))))

(deftest nvidia-broadcast-cell-reads-a-server-sent-answer
  ;; The streamable transport may answer in events rather than one object.
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "get_broadcast_state" (lambda (arguments) (declare (ignore arguments)) +state+))))
          (*gateway-events* t))
      (with-gateway (asked)
        (is (search "background_blur" (wire-answer "nb-2" "get_broadcast_state" "{}"))
            "an event-stream answer is read to its data payload")))))

(deftest nvidia-broadcast-cell-reports-a-failed-call
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "get_broadcast_state" (lambda (arguments) (declare (ignore arguments)) "device_not_selected"))))
          (*gateway-is-error* t))
      (with-gateway (asked)
        (declare (ignore asked))
        (is (search "ERROR: device_not_selected" (wire-answer "nb-3" "get_broadcast_state" "{}"))
            "an isError answer is a failure, not an answer")))))

;;; --- setting effects ------------------------------------------------------------------

(deftest nvidia-broadcast-cell-sets-effects-in-one-call
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "set_effects" (lambda (arguments) (nlk:encode-json-object arguments))))))
      (with-gateway (asked)
        (let* ((arguments "{\"effects\":[{\"effectId\":\"noise_removal\",\"enabled\":true},{\"effectId\":\"background_blur\",\"enabled\":true,\"params\":{\"strength\":0.7}}]}")
               (answer (wire-answer "nb-4" "set_effects" arguments))
               (sent (sent-arguments (first (tool-calls asked)))))
          (is (= 2 (length (nlk:json-array sent "effects"))) "both effects ride one call")
          (is (search "noise_removal" answer) "and the answer carries what the gateway applied"))))))

(deftest nvidia-broadcast-cell-restores-the-live-snapshot
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "set_effects" (lambda (arguments) (nlk:json-value arguments :string "action"))))))
      (with-gateway (asked)
        (is (equal "restore_previous_live"
                   (wire-answer "nb-5" "set_effects" "{\"action\":\"restore_previous_live\"}"))
            "the action alone is the whole call")
        (is (null (nlk:json-value (sent-arguments (first (tool-calls asked))) :string "effects"))
            "and no effects array is sent with it")))))

(deftest nvidia-broadcast-cell-refuses-an-empty-effect-set
  (with-cell-stop ((nvidia-broadcast-start))
    (with-gateway (asked)
      (is (search "ERROR:" (wire-answer "nb-6" "set_effects" "{}"))
          "neither effects nor an action is refused before anything is sent")
      (is (null (tool-calls asked)) "and the gateway is never called"))))

;;; --- devices and resolution ------------------------------------------------------------

(deftest nvidia-broadcast-cell-lists-every-device-by-default
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "list_devices" (lambda (arguments) (nlk:encode-json-object arguments))))))
      (with-gateway (asked)
        (wire-answer "nb-7" "list_devices" "{}")
        (is (equal "all" (nlk:json-value (sent-arguments (first (tool-calls asked))) :string "kind"))
            "an absent kind asks for all three sections")))))

(deftest nvidia-broadcast-cell-switches-a-camera-and-its-size-at-once
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "set_active_device" (lambda (arguments) (nlk:encode-json-object arguments))))))
      (with-gateway (asked)
        (wire-answer "nb-8" "set_active_device"
                     "{\"kind\":\"camera\",\"device_id\":\"cam-2\",\"width\":1920,\"height\":1080,\"frame_rate\":60}")
        (let ((sent (sent-arguments (first (tool-calls asked)))))
          (is (equal "cam-2" (nlk:json-value sent :string "deviceId")) "the id is the gateway's deviceId")
          (is (= 1920 (nlk:json-value sent :integer "width")))
          (is (= 60 (nlk:json-value sent :integer "frameRate"))
              "and the frame rate rides the same call, never guessed by the cell"))))))

(deftest nvidia-broadcast-cell-sets-a-resolution
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "set_camera_resolution" (lambda (arguments) (nlk:encode-json-object arguments))))))
      (with-gateway (asked)
        (wire-answer "nb-9" "set_camera_resolution" "{\"device_id\":\"cam-2\",\"width\":1280,\"height\":720}")
        (let ((sent (sent-arguments (first (tool-calls asked)))))
          (is (= 1280 (nlk:json-value sent :integer "width")))
          (is (not (nth-value 1 (gethash "frameRate" sent)))
              "a size with no frame rate sends none, so the gateway can say ambiguous_frame_rate"))))))

;;; --- file processing ---------------------------------------------------------------------

(deftest nvidia-broadcast-cell-submits-a-file-with-the-version
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "submit_file_processing" (lambda (arguments) (nlk:encode-json-object arguments))))))
      (with-gateway (asked)
        (wire-answer "nb-10" "submit_file_processing"
                     "{\"input_path\":\"C:/Users/me/Videos/a.mp4\",\"effects\":[{\"effectId\":\"background_blur\",\"params\":{\"strength\":0.6}}]}")
        (let ((sent (sent-arguments (first (tool-calls asked)))))
          (is (= 1 (nlk:json-value sent :integer "_version")) "every file tool carries _version 1")
          (is (equal "C:/Users/me/Videos/a.mp4" (nlk:json-value sent :string "inputPath")))
          (is (= 1 (length (nlk:json-array sent "effects")))))))))

(deftest nvidia-broadcast-cell-waits-with-a-cursor
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "wait_for_processing_update" (lambda (arguments) (nlk:encode-json-object arguments))))))
      (with-gateway (asked)
        (wire-answer "nb-11" "wait_for_processing_update"
                     "{\"batch_id\":\"b-1\",\"cursor\":\"c-7\",\"timeout_ms\":30000}")
        (let ((sent (sent-arguments (first (tool-calls asked)))))
          (is (equal "b-1" (nlk:json-value sent :string "batchId")))
          (is (equal "c-7" (nlk:json-value sent :string "cursor")))
          (is (= 30000 (nlk:json-value sent :integer "timeoutMs"))))))))

(deftest nvidia-broadcast-cell-controls-the-queue
  (with-cell-stop ((nvidia-broadcast-start))
    (let ((*gateway-tools* (list (cons "control_file_processing_jobs" (lambda (arguments) (nlk:encode-json-object arguments))))))
      (with-gateway (asked)
        (wire-answer "nb-12" "control_file_processing_jobs" "{\"action\":\"pause\"}")
        (let ((sent (sent-arguments (first (tool-calls asked)))))
          (is (equal "pause" (nlk:json-value sent :string "action")))
          (is (= 1 (nlk:json-value sent :integer "_version"))))))))

;;; --- no gateway at all ----------------------------------------------------------------------

(deftest nvidia-broadcast-cell-says-so-when-the-gateway-is-down ()
  (with-cell-stop ((nvidia-broadcast-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                               (declare (ignorable method url options))
                               (error "connection refused"))
      (let ((answer (wire-answer "nb-13" "get_broadcast_state" "{}")))
        (is (search "ERROR:" answer) "a refused dial is a failure the model can read")
        (is (search "NVIDIA Broadcast" answer) "naming the app the user has to start")
        (is (search "did not answer" answer) "and the endpoint that did not answer")))))
