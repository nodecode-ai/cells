;;;; cell-test.lisp --- the NVIDIA App cell against a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the cell's one network seam
;;;; (NODECODE-NVIDIA-APP::*HTTP*) is bound to a canned server, and the app's
;;;; published server.json is a file a test writes in a scratch directory.
;;;; What is proved: the lifecycle every cell owns; that a call is a real
;;;; JSON-RPC exchange -- a handshake with its notification, then tools/call
;;;; with the published bearer token and the session the server answered;
;;;; that the app's published address wins over a stale one in the section;
;;;; that a section with no address at all refuses the start; that the
;;;; restricted Overlay workflow is enforced (a toggle needs enable, a save
;;;; and a screenshot take none, desktop capture is not offered, only rtx_dvc
;;;; is a filter); that a laptop feature goes out in one legal shape; and
;;;; that a tool's own failure reads as ERROR: and that /nvidia answers.

(in-package #:nodecode.test)

(define-test-slice "nvidia-app" "NVIDIA-APP-CELL-" :start nodecode-nvidia-app:start-cell)

;;; --- the canned server --------------------------------------------------------------

(defvar *nvapp-calls* nil
  "Every request the cell made, newest first: (:method :url :headers :request).")

(defvar *nvapp-text* "ok" "What a tools/call answers as its one text block.")

(defvar *nvapp-failed* nil "True makes the answered tool call report its own failure.")

(defvar *nvapp-status* 200 "The status a tools/call answers.")

(defvar *nvapp-tools* '("nvapp_client_get_driver_status" "nvapp_overlay_get_status")
  "What tools/list advertises.")

(defun nvapp-server-json (&key (url "http://app.test:47985/mcp") (token "published-token"))
  "The app's published file, in a directory of its own: its path as a string."
  (let* ((directory (merge-pathnames (format nil "nvapp-~a/" (random 1000000))
                                      (uiop:ensure-directory-pathname
                                       (uiop:temporary-directory))))
         (path (merge-pathnames "server.json" directory)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (nlk:encode-json-object
                     (let ((object (nlk:json-object "url" url)))
                       (when token (setf (gethash "token" object) token))
                       object))
                    out))
    (namestring path)))

(defun nvapp-missing-json ()
  "A path where no NVIDIA App published anything."
  (namestring (merge-pathnames (format nil "nvapp-absent-~a/server.json" (random 1000000))
                                (uiop:ensure-directory-pathname (uiop:temporary-directory)))))

(defun nvapp-headers (&key session sse)
  (append (list (cons "content-type" (if sse "text/event-stream" "application/json")))
          (when session (list (cons "mcp-session-id" session)))))

(defvar *nvapp-sse* nil "True answers as an event stream rather than a JSON body.")

(defun nvapp-body (json)
  (if *nvapp-sse* (format nil "event: message~%data: ~a~%~%" json) json))

(defvar *nvapp-expire-once* nil
  "True makes the next tools/call answer the session-expired error an NVIDIA App
restart leaves under a call, and clears itself so the retry answers.")

(defun nvapp-answer (method url headers content)
  "The cell's one network call, answered from the canned server."
  (let* ((request (and content (plusp (length content)) (nlk:decode-json content)))
         (name (nlk:json-value request :string "method"))
         (id (nlk:json-value request :any "id")))
    (push (list :method method :url url :headers headers :request request) *nvapp-calls*)
    (cond
      ((equal name "initialize")
       (values (nvapp-body (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "result" (nlk:json-object
                                                        "protocolVersion" "2025-06-18"
                                                        "serverInfo" (nlk:json-object
                                                                      "name" "NVIDIA App")))))
               200 (nvapp-headers :session "sess-1")))
      ((equal name "notifications/initialized") (values "" 202 (nvapp-headers)))
      ((equal name "tools/list")
       (values (nvapp-body (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "result" (nlk:json-object
                                                        "tools" (map 'vector
                                                                     (lambda (one)
                                                                       (nlk:json-object
                                                                        "name" one
                                                                        "description" "an NVIDIA App tool"))
                                                                     *nvapp-tools*)))))
               *nvapp-status* (nvapp-headers)))
      ((and (equal name "tools/call") *nvapp-expire-once*)
       (setf *nvapp-expire-once* nil)
       (values (nvapp-body (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "error" (nlk:json-object
                                                        "code" -32000
                                                        "message" "transport session expired: outcome unknown"))))
               200 (nvapp-headers)))
      ((equal name "tools/call")
       (values (nvapp-body (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "result" (nlk:json-object
                                                        "content" (vector (nlk:json-object
                                                                           "type" "text"
                                                                           "text" *nvapp-text*))
                                                        "isError" (and *nvapp-failed* t)))))
               *nvapp-status* (nvapp-headers :sse *nvapp-sse*)))
      (t (values (nvapp-body (nlk:encode-json-object
                               (nlk:json-object "jsonrpc" "2.0" "error"
                                                (nlk:json-object "code" -32601
                                                                 "message" "no such method"))))
                 200 (nvapp-headers))))))

(defmacro with-nvapp-wire (&body body)
  "BODY with the cell's one network call answered from the canned server."
  `(let ((*nvapp-calls* '()) (*nvapp-text* "ok") (*nvapp-failed* nil)
         (*nvapp-status* 200) (*nvapp-sse* nil) (*nvapp-expire-once* nil))
     (with-stubbed-fdefinition (nodecode-nvidia-app::http (method url &key headers content &allow-other-keys)
       (nvapp-answer method url headers content))
       ,@body)))

;;; --- reading the canned server's traffic ---------------------------------------------

(defun nvapp-request (method &optional tool)
  "The newest request of METHOD (a tools/call for TOOL when given)."
  (find-if (lambda (one)
             (and (equal method (nlk:json-value (getf one :request) :string "method"))
                  (or (null tool)
                      (equal tool (nlk:json-value (getf one :request) :string "params" "name")))))
           *nvapp-calls*))

(defun nvapp-sent (tool)
  "The arguments the newest tools/call for TOOL carried."
  (let ((call (nvapp-request "tools/call" tool)))
    (and call (nlk:json-value (getf call :request) :object "params" "arguments"))))

(defun nvapp-tool (name arguments)
  "The registered tool NAME called as the dispatch seam calls it."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry) arguments)))

(defun nvapp-header (call name)
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun nvapp-json (&rest pairs)
  "PAIRS as the decoded object a tool call carries: JSON through the wire."
  (nlk:decode-json (nlk:encode-json-object (apply #'nlk:make-json-object pairs))))

;;; --- lifecycle ------------------------------------------------------------------------

(define-cell-lifecycle-tests "nvidia-app"
  (:fixture with-nvapp-wire)
  (:config "url" "http://app.test:47985/mcp" "token" "nvapp-test-token")
  (:help :nvidia-app)
  (:command "nvidia")
  (:running
   (is (nle::find-tool "nvapp_overlay_capture") "the Overlay tools are registered")
   (is (nle::find-tool "nvapp_client_get_driver_status") "and the app's own")
   (is (= 30 (nodecode-nvidia-app::setting :timeout-seconds)) "the declared default")
   (is (null nodecode-nvidia-app::*session*) "nothing was dialled before a call"))
  (:stopped
   (is (null (nle::find-tool "nvapp_overlay_capture")) "stop unregisters the tools"))
  (:refused ("server_json" (nvapp-missing-json))
            ("url" "localhost:47985/mcp")
            ("timeout_seconds" 1))
  (:idle nodecode-nvidia-app:nvidia-app-error (nodecode-nvidia-app:tools)))

(deftest nvidia-app-cell-a-section-with-no-address-refuses ()
  ;; The app publishes its address; without the app the folder says so once,
  ;; on its REFUSED line, instead of failing on every call.
  (with-nvapp-wire
    (is (signals-error nlk:config-refusal
                       (nvidia-app-start "server_json" (nvapp-missing-json))))
    (is (search "is not published"
                (handler-case (progn (nvidia-app-start "server_json" (nvapp-missing-json)) "")
                  (nlk:config-refusal (condition)
                    (nlk:config-refusal-detail condition)))) "the refusal names the file it looked for")
    (is (null (nle::find-tool "nvapp_overlay_capture")) "a refused start installed nothing")
    (is (null *nvapp-calls*) "and dialled nothing")))

;;; --- one call is a real exchange ----------------------------------------------------

(deftest nvidia-app-cell-a-call-carries-the-published-token-and-session ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "server_json" (nvapp-server-json)))
      (setf *nvapp-text* "GeForce Game Ready 581.15 is installed.")
      (is (equal "GeForce Game Ready 581.15 is installed."
                 (nvapp-tool "nvapp_client_get_driver_status" (nlk:json-object))) "the app's text is the answer")
      (let ((call (nvapp-request "tools/call" "nvapp_client_get_driver_status")))
        (is-present (call call) "a tools/call went out"
          (is (equal "Bearer published-token" (nvapp-header call "authorization")) "with the token the app published")
          (is (equal "sess-1" (nvapp-header call "mcp-session-id")) "and the session the handshake answered")))
      (is (= 1 (count "initialize" *nvapp-calls*
                      :key (lambda (one) (nlk:json-value (getf one :request) :string "method"))
                      :test #'equal)) "one handshake for the life of the start")
      (is (nvapp-request "notifications/initialized") "and the notification the protocol requires")
      (is (equal "http://app.test:47985/mcp" (getf (first *nvapp-calls*) :url)) "every request goes to the published address"))))

(deftest nvidia-app-cell-the-published-address-wins-over-a-stale-one ()
  ;; NVIDIA App picks a new port and token when it restarts; a section that
  ;; pinned the old pair must not keep dialling it.
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "server_json" (nvapp-server-json :url "http://fresh.test:50001/mcp"
                                                                       :token "fresh-token")
                                      "url" "http://stale.test:47985/mcp"
                                      "token" "stale-token"))
      (nvapp-tool "nvapp_overlay_get_status" (nlk:json-object))
      (let ((call (nvapp-request "tools/call")))
        (is (equal "http://fresh.test:50001/mcp" (getf call :url)) "the fresh address")
        (is (equal "Bearer fresh-token" (nvapp-header call "authorization")) "and its token")))))

(deftest nvidia-app-cell-an-event-stream-answer-parses-like-a-plain-one ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp"))
      (setf *nvapp-sse* t *nvapp-text* "Recording started.")
      (is (equal "Recording started."
                 (nvapp-tool "nvapp_overlay_capture"
                              (nvapp-json "action" "toggle_recording" "enable" t))) "the data: line carried the answer"))))

(deftest nvidia-app-cell-a-session-the-app-forgot-is-handed-again ()
  ;; An app restart under a call: the server forgets the session, the cell
  ;; handshakes once more with whatever the app now publishes.
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp" "token" "nvapp-test-token"))
      (setf nodecode-nvidia-app::*handshaked* t
            nodecode-nvidia-app::*session* "sess-gone"
            *nvapp-expire-once* t
            *nvapp-text* "Instant Replay is on.")
      (is (equal "Instant Replay is on."
                 (nvapp-tool "nvapp_overlay_get_status" (nlk:json-object))) "the read was repeated after the handshake")
      (is (= 2 (count "tools/call" *nvapp-calls*
                      :key (lambda (one) (nlk:json-value (getf one :request) :string "method"))
                      :test #'equal)) "once refused, once answered")
      (is (= 1 (count "initialize" *nvapp-calls*
                      :key (lambda (one) (nlk:json-value (getf one :request) :string "method"))
                      :test #'equal)) "with exactly one new handshake"))))

;;; --- the restricted Overlay workflow -------------------------------------------------

(deftest nvidia-app-cell-a-toggle-needs-its-target-state ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp"))
      (is (search "takes enable"
                  (refusal-text nodecode-nvidia-app:nvidia-app-error
                                (nvapp-tool "nvapp_overlay_capture"
                                             (nvapp-json "action" "toggle_recording")))) "a toggle without enable is refused, not guessed")
      (is (null *nvapp-calls*) "and nothing was dialled to find out")
      (nvapp-tool "nvapp_overlay_capture" (nvapp-json "action" "toggle_recording" "enable" nil))
      (let ((sent (nvapp-sent "nvapp_overlay_capture")))
        (is-present (sent sent) "the explicit state went out"
          (is (search "\"enable\":false" (nlk:encode-json-object sent)) "as the false the caller asked for")))
      (nvapp-tool "nvapp_overlay_capture" (nvapp-json "action" "toggle_recording" "enable" t))
      (is (search "\"enable\":true"
                  (nlk:encode-json-object (nvapp-sent "nvapp_overlay_capture"))) "and true as true"))))

(deftest nvidia-app-cell-save-and-screenshot-take-no-enable ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp"))
      (is (search "no enable"
                  (refusal-text nodecode-nvidia-app:nvidia-app-error
                                (nvapp-tool "nvapp_overlay_capture"
                                             (nvapp-json "action" "save_instant_replay" "enable" t)))) "save_instant_replay takes no enable")
      (nvapp-tool "nvapp_overlay_capture" (nvapp-json "action" "capture_screenshot"))
      (let ((sent (nvapp-sent "nvapp_overlay_capture")))
        (is-present (sent sent) "a screenshot needs only its action"
          (is (null (gethash "enable" sent)) "and carries no enable")))
      (is (search "capture_screenshot"
                  (refusal-text nodecode-nvidia-app:nvidia-app-error
                                (nvapp-tool "nvapp_overlay_capture"
                                             (nvapp-json "action" "toggle_desktop_capture" "enable" t)))) "desktop capture is not offered"))))

(deftest nvidia-app-cell-only-rtx-dvc-is-a-filter ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp"))
      (nvapp-tool "nvapp_overlay_configure_filters" (nvapp-json "filter" "rtx_dvc" "enable" t))
      (is (search "rtx_dvc" (nlk:encode-json-object (nvapp-sent "nvapp_overlay_configure_filters"))) "the one filter the workflow authorizes")
      (is (search "filter is"
                  (refusal-text nodecode-nvidia-app:nvidia-app-error
                                (nvapp-tool "nvapp_overlay_configure_filters"
                                             (nvapp-json "filter" "rtx_hdr" "enable" t)))) "another filter is refused")
      (is (search "takes enable"
                  (refusal-text nodecode-nvidia-app:nvidia-app-error
                                (nvapp-tool "nvapp_overlay_configure_stats" (nvapp-json)))) "statistics visibility needs its state"))))

;;; --- the app's own tools --------------------------------------------------------------

(deftest nvidia-app-cell-an-application-name-must-be-exact ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp"))
      (is (search "game is missing"
                  (refusal-text nodecode-nvidia-app:nvidia-app-error
                                (nvapp-tool "nvapp_client_launch_application" (nvapp-json)))) "launching without a game is refused")
      (nvapp-tool "nvapp_client_launch_application" (nvapp-json "game" "Example Game"))
      (is (equal "Example Game"
                 (nlk:json-value (nvapp-sent "nvapp_client_launch_application") :string "game")))
      (nvapp-tool "nvapp_client_get_applications" (nvapp-json "game" "Example Game"))
      (let ((sent (nvapp-sent "nvapp_client_get_applications")))
        (is (equal "Example Game" (nlk:json-value sent :string "game")))
        (is (null (gethash "preset" sent)) "an absent preset is not sent")))))

(deftest nvidia-app-cell-a-laptop-feature-goes-out-in-one-legal-shape ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp"))
      (nvapp-tool "nvapp_client_set_laptop_feature"
                  (nvapp-json "feature" "whisper_mode" "enable" t "fan_volume" "Quiet"))
      (let ((sent (nlk:encode-json-object (nvapp-sent "nvapp_client_set_laptop_feature"))))
        (is (search "\"whisperMode\"" sent) "the global shape names the feature")
        (is (search "\"enabled\":true" sent) "its state")
        (is (search "\"fanVolume\":\"Quiet\"" sent) "and the fan profile")
        (is (not (search "game" sent)) "with no game"))
      (nvapp-tool "nvapp_client_set_laptop_feature"
                  (nvapp-json "feature" "battery_boost" "game" "Example Game" "use_global" t))
      (is (search "\"batteryBoost\":{\"useGlobal\":true}" (nlk:encode-json-object
                                                           (nvapp-sent "nvapp_client_set_laptop_feature"))) "the application shape that follows the global setting carries no enabled")
      (is (search "fan_volume"
                  (refusal-text nodecode-nvidia-app:nvidia-app-error
                                (nvapp-tool "nvapp_client_set_laptop_feature"
                                             (nvapp-json "feature" "whisper_mode" "game" "Example Game"
                                                          "use_global" nil "enable" t
                                                          "fan_volume" "Quiet")))) "an application-scoped Whisper Mode takes no fan profile"))))

(deftest nvidia-app-cell-failures-read-as-failures ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "url" "http://app.test:47985/mcp"))
      (setf *nvapp-text* "overlay_not_available" *nvapp-failed* t)
      (is (equal "ERROR: overlay_not_available"
                 (nvapp-tool "nvapp_overlay_get_status" (nlk:json-object))) "the app's own failure is the answer, marked ERROR: with its code")
      (setf *nvapp-failed* nil *nvapp-status* 500)
      (is (search "500" (refusal-text nodecode-nvidia-app:nvidia-app-error
                                       (nvapp-tool "nvapp_overlay_get_status" (nlk:json-object)))) "a refused transport is a refusal naming the status"))))

(deftest nvidia-app-cell-the-token-never-rides-in-a-failure ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "server_json" (nvapp-server-json)))
      (setf *nvapp-status* 401)
      (let ((text (refusal-text nodecode-nvidia-app:nvidia-app-error
                                 (nvapp-tool "nvapp_client_get_driver_status" (nlk:json-object)))))
        (is (not (search "published-token" text)) "the token is not in the message")))))

;;; --- /nvidia ---------------------------------------------------------------------------

(deftest nvidia-app-cell-the-slash-command-answers-without-the-network ()
  (with-nvapp-wire
    (with-cell-stop ((nvidia-app-start "server_json" (nvapp-server-json)))
      (is-carrying (text (cell-entry "nodecode-nvidia-app" "nvidia" ""))
        "http://app.test:47985/mcp"
        "token published or set")
      (is (null *nvapp-calls*) "the connection line dials nothing")
      (is-carrying (text (cell-entry "nodecode-nvidia-app" "nvidia" "tools"))
        "2 tools"
        "nvapp_client_get_driver_status")
      (is (nvapp-request "tools/list") "/nvidia tools asks the app for its own list")
      (is-carrying (text (cell-entry "nodecode-nvidia-app" "nvidia" "status"))
        "Overlay:"
        "Driver:")
      (is (nvapp-request "tools/call" "nvapp_overlay_get_status") "status reads the Overlay")
      (is (nvapp-request "tools/call" "nvapp_client_get_driver_status") "and the driver"))))
