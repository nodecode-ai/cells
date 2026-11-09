;;;; cell-test.lisp --- lifecycle, the handshake, the four tools, the plan, /unity.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing here needs an editor, a
;;;; Unity install or the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, a missing url and a wrong one refused), the
;;;; handshake and its session id, the four tools through the seam the model
;;;; calls them by, the plan that runs once and stops at its first failure, the
;;;; path refusals, and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defparameter +test-url+ "http://127.0.0.1:8080/mcp"
  "The endpoint every test's section names.")

(defparameter +test-tools+
  "[{\"name\":\"manage_scene\",\"description\":\"Scene ops: open, save, hierarchy\"},\
    {\"name\":\"manage_gameobject\",\"description\":\"Create and modify GameObjects\"}]"
  "The two tools the stubbed editor advertises.")

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values TEXT STATUS HEADERS).")

(defmacro with-unity-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials an editor."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected unity request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun unity-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun unity-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun unity-request-method (request)
  "REQUEST's JSON-RPC method, or NIL for a notification-less POST."
  (nlk:json-value (unity-request-body request) :string "method"))

(defun unity-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun unity-rpc-methods ()
  "The JSON-RPC methods the editor was asked, in order."
  (remove nil (mapcar #'unity-request-method (unity-asked))))

(defun tools-call-count ()
  (count "tools/call" (unity-rpc-methods) :test #'equal))

(defun rpc-answer (result)
  "One JSON-RPC success answer carrying RESULT, as text."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":~a}" result))

(defun fake-editor (&key (failing '()) (sse nil))
  "An editor that handshakes, lists +TEST-TOOLS+, and echoes each tools/call
back as its args; a tool in FAILING answers with an isError result instead."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((body (nlk:decode-json (getf args :content)))
                 (name (nlk:json-value body :string "method"))
                 (params (nlk:json-value body :object "params")))
            (cond
              ((equal name "initialize")
               (let ((answer (rpc-answer "{\"protocolVersion\":\"2025-06-18\",\"serverInfo\":{\"name\":\"MCP for Unity\"}}")))
                 (values (if sse
                             (format nil "event: message~%data: ~a~%~%" answer)
                             answer)
                         200 '(("mcp-session-id" . "session-1")))))
              ((equal name "notifications/initialized") (values "" 202 nil))
              ((equal name "tools/list")
               (values (rpc-answer (format nil "{\"tools\":~a}" +test-tools+)) 200 nil))
              ((equal name "tools/call")
               (let ((tool (nlk:json-value params :text "name")))
                 (if (member tool failing :test #'equal)
                     (values (rpc-answer
                              (format nil "{\"content\":[{\"type\":\"text\",\"text\":\"~a could not find the object\"}],\"isError\":true}"
                                      tool))
                             200 nil)
                     (values (rpc-answer
                              (format nil "{\"content\":[{\"type\":\"text\",\"text\":\"~a ok with ~a\"}]}"
                                      tool (or (nlk:json-value (nlk:json-value params :object "arguments")
                                                                :text "action")
                                               "no action")))
                             200 nil))))
              (t (error "unexpected method ~a" name)))))))

(defun unity-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "unity-call" name arguments)))

(defun unity-command (arguments)
  "The /unity command with ARGUMENTS, as the operator reads it."
  (nodecode-unity::run-command arguments nil))

(defun plan-id-of (text)
  "The plan id in the plan TEXT unity_plan answered."
  (let* ((start (+ 5 (or (search "plan " text) 0)))
         (end (or (search ":" text :start2 start) (length text))))
    (string-trim '(#\Space) (subseq text start end))))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "unity" "UNITY-CELL-" :start nodecode-unity:start-cell)

(define-cell-lifecycle-tests "unity"
  (:fixture with-unity-wire)
  (:config "url" +test-url+)
  (:help :unity)
  (:command "unity")
  (:running
   (is (nle::find-tool "unity_tools") "the tool list is registered")
   (is (nle::find-tool "unity_call") "and one call")
   (is (nle::find-tool "unity_plan") "and the plan")
   (is (nle::find-tool "unity_apply") "and its apply")
   (let ((ours (count-if (lambda (entry) (eql 0 (search "unity_" (first entry)))) nle::*tools*)))
     (is (= 4 ours) "four of them, and no others"))
   (is (equal +test-url+ (nodecode-unity::endpoint)) "the section's endpoint is what it dials"))
  (:stopped
   (is (null (nle::find-tool "unity_plan")) "stop takes them back")
   (is (null (nle::find-tool "unity_apply"))))
  (:refused ("timeout_seconds" 1)
            ("plan_ttl_minutes" 0)
            ("max_chars" 10)
            ("url" "http://192.168.1.5:8080/mcp")))

(deftest unity-cell-a-start-with-no-url-refuses-and-installs-nothing (with-unity-wire)
  (let ((text (refusal-text nlk:config-refusal (unity-start))))
    (is-carrying text "url" "MCP for Unity")
    (is (null (nle::find-tool "unity_call")) "nothing was installed")
    (is (null (unity-asked)) "and nothing was sent")))

;;; --- the handshake and the tool list ---------------------------------------------

(deftest unity-cell-the-tools-list-is-asked-once-with-its-session (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor)
    (let ((answer (unity-call "unity_tools" "{}")))
      (is (search "manage_scene - Scene ops" answer) "every tool reaches the model, one line each")
      (is (search "manage_gameobject" answer) "including the second")
      (is (equal '("initialize" "notifications/initialized" "tools/list") (unity-rpc-methods)) "one handshake, then the list")
      (is (equal "session-1" (unity-request-header (third (unity-asked)) "mcp-session-id")) "and the session id rides every call after it"))
    (unity-call "unity_tools" "{}")
    (is (= 3 (length (unity-asked))) "a second look is served from the cell, not the editor")))

(deftest unity-cell-an-sse-handshake-is-read-like-a-json-one (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor :sse t)
    (let ((answer (unity-call "unity_tools" "{}")))
      (is (search "manage_scene" answer) "the event's data line is the answer")
      (is (equal "session-1" (getf nodecode-unity::*session* :id)) "and its headers carried the session"))))

;;; --- one call ---------------------------------------------------------------------

(deftest unity-cell-a-call-carries-its-tool-and-its-arguments (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor)
    (let ((answer (unity-call "unity_call"
                               "{\"tool\": \"manage_scene\", \"arguments\": {\"action\": \"get_hierarchy\"}}")))
      (is (search "manage_scene" answer) "the editor's answer comes back")
      (is (search "get_hierarchy" answer) "with the arguments the model sent")
      (let* ((request (car (last (unity-asked))))
             (params (nlk:json-value (unity-request-body request) :object "params")))
        (is (equal "manage_scene" (nlk:json-value params :text "name")) "the tool name on the wire")
        (is (equal "get_hierarchy" (nlk:json-value (nlk:json-value params :object "arguments") :text "action")) "and its arguments are the object the model sent")))))

(deftest unity-cell-a-call-the-editor-refused-is-the-tool-s-error (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor :failing '("manage_gameobject"))
    (let ((answer (unity-call "unity_call"
                               "{\"tool\": \"manage_gameobject\", \"arguments\": {\"name\": \"Coin\"}}")))
      (is (search "ERROR: " answer) "a refusal is the tool result, not a condition up the stack")
      (is (search "manage_gameobject could not find the object" answer) "in the editor's own words"))))

;;; --- the plan ---------------------------------------------------------------------

(deftest unity-cell-a-plan-shows-every-step-and-sends-nothing (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor)
    (let ((plan (unity-call "unity_plan"
                             "{\"note\": \"a coin the player collects\", \"steps\": [{\"tool\": \"manage_script\", \"arguments\": {\"path\": \"Assets/Scripts/Coin.cs\"}}, {\"tool\": \"manage_scene\", \"arguments\": {\"path\": \"Assets/Scenes/Main.unity\"}}]}")))
      (is (search "a coin the player collects" plan) "the note is at the top")
      (is (search "1. manage_script" plan) "the first step in full")
      (is (search "Assets/Scripts/Coin.cs" plan) "with the path it names")
      (is (search "2. manage_scene" plan) "and the second")
      (is (search "Nothing has run yet" plan) "and that nothing ran")
      (is (search "unity_apply" plan) "and the call that runs it")
      (is (null (unity-asked)) "making a plan sends nothing")
      (is (plusp (length (plan-id-of plan))) "it answers a plan id"))))

(deftest unity-cell-a-plan-refuses-a-path-outside-the-project (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor)
    (let ((answer (unity-call "unity_plan"
                               "{\"steps\": [{\"tool\": \"manage_script\", \"arguments\": {\"path\": \"Assets/../../etc/passwd\"}}]}")))
      (is (search "ERROR: " answer) "the plan is refused")
      (is (search "climbs out of the project" answer) "naming why")
      (is (null (unity-asked)) "and nothing was sent"))
    (let ((answer (unity-call "unity_plan"
                               "{\"steps\": [{\"tool\": \"manage_asset\", \"arguments\": {\"path\": \"Assets/Keys/id_rsa\"}}]}")))
      (is (search "key or credential file" answer) "a key file is refused too"))
    (let ((answer (unity-call "unity_plan"
                               "{\"steps\": [{\"tool\": \"manage_asset\", \"arguments\": {\"path\": \"/etc/hosts\"}}]}")))
      (is (search "absolute path" answer) "and an absolute one"))
    (let ((answer (unity-call "unity_plan"
                               "{\"steps\": [{\"tool\": \"manage_editor\", \"arguments\": {\"menu_path\": \"File/Quit\"}}]}")))
      (is (search "drops unsaved work" answer) "and File/Quit"))))

(deftest unity-cell-a-plan-runs-once-in-order-and-stops-at-its-first-failure (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor :failing '("manage_scene"))
    (let* ((plan (unity-call "unity_plan"
                              "{\"steps\": [{\"tool\": \"manage_script\", \"arguments\": {\"path\": \"Assets/A.cs\"}}, {\"tool\": \"manage_scene\", \"arguments\": {\"path\": \"Assets/Main.unity\"}}, {\"tool\": \"manage_script\", \"arguments\": {\"path\": \"Assets/B.cs\"}}]}"))
           (id (plan-id-of plan)))
      (let ((answer (unity-call "unity_apply" (format nil "{\"plan_id\": \"~a\"}" id))))
        (is (search "FAILED at step 2 of 3" answer) "the failing step is named")
        (is (search "could not find the object" answer) "with the editor's reason")
        (is (search "1 step did not run" answer) "and the rest were not sent")
        (is (= 2 (tools-call-count)) "two steps were sent, not three"))
      (let ((again (unity-call "unity_apply" (format nil "{\"plan_id\": \"~a\"}" id))))
        (is (search "ERROR: " again) "a plan runs once")
        (is (search "already used" again) "and says so when it is asked again"))
      (is (= 2 (tools-call-count)) "the second apply sent nothing"))))

(deftest unity-cell-only-the-newest-plan-can-run (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor)
    (let* ((first (unity-call "unity_plan"
                               "{\"steps\": [{\"tool\": \"manage_script\", \"arguments\": {\"path\": \"Assets/A.cs\"}}]}"))
           (older (plan-id-of first)))
      (unity-call "unity_plan"
                   "{\"steps\": [{\"tool\": \"manage_script\", \"arguments\": {\"path\": \"Assets/B.cs\"}}]}")
      (let ((answer (unity-call "unity_apply" (format nil "{\"plan_id\": \"~a\"}" older))))
        (is (search "replaced by a newer plan" answer) "the older plan is refused")
        (is (zerop (tools-call-count)) "and nothing was sent")))))

;;; --- the operator's command --------------------------------------------------------

(deftest unity-cell-the-command-shows-what-the-editor-answers (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (fake-editor)
    (let ((text (unity-command "")))
      (is-carrying text ("unity: http://127.0.0.1:8080/mcp" "what it is connected to") ("manage_scene" "and the tools it found"))
      (is (search "unity_plan" text) "and how to plan a change"))
    (let ((text (unity-command "gameobject")))
      (is (search "manage_gameobject" text) "a word keeps the matching tool")
      (is (null (search "manage_scene" text)) "and leaves the rest out"))))

(deftest unity-cell-the-command-says-why-when-no-editor-answers (with-unity-wire)
  (with-cell-stop ((unity-start "url" +test-url+))
    (setf *reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (values "bad gateway" 502 nil)))
    (let ((text (unity-command "")))
      (is (search "not reachable" text) "the operator is told the editor did not answer")
      (is (search "MCP for Unity" text) "and what to open"))))
