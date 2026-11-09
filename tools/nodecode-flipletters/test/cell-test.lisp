;;;; cell-test.lisp --- lifecycle, the tools through stubbed HTTP, /flipletters, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start with a key in the environment, a key written inline, a section with
;;;; no key refused by the variable's name, a url without a scheme refused, a
;;;; timeout under its floor refused), the MCP handshake the tools ride
;;;; (initialize, its notification, the session id echoed, a dropped session
;;;; made again), the arguments each tool sends and the answer it builds, and
;;;; /flipletters with and without a word. Every request is a stubbed
;;;; NLK:HTTP, so nothing reaches FlipLetters or the network.

(in-package #:nodecode.test)

;;; --- fixtures ------------------------------------------------------------------

(defvar *flipletters-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *flipletters-answer-fn* nil
  "What the stubbed NLK:HTTP answers: (METHOD URL ARGS) => (values BODY STATUS HEADERS).")

(defvar *flipletters-env* '(("FLIPLETTERS_API_KEY" . "flk_test"))
  "What the stubbed UIOP:GETENV answers, so a key in the machine's own
environment never reaches a test that did not ask for one.")

(defmacro with-flipletters-wire (&body body)
  "BODY with NLK:HTTP and UIOP:GETENV stubbed: a request is logged in
*FLIPLETTERS-REQUESTS* and answered by *FLIPLETTERS-ANSWER-FN*, and the only
environment is *FLIPLETTERS-ENV*."
  `(let ((*flipletters-requests* '())
         (*flipletters-answer-fn* (lambda (method url args)
                                     (declare (ignore method url args))
                                     (error "unexpected FlipLetters request")))
         (*flipletters-env* (copy-tree *flipletters-env*)))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *flipletters-requests*)
                                (funcall *flipletters-answer-fn* method url args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (cdr (assoc name *flipletters-env* :test #'string=)))
         ,@body))))

(defun flipletters-asked ()
  "The stubbed requests, oldest first."
  (reverse *flipletters-requests*))

(defun flipletters-rpc (request)
  "REQUEST's JSON-RPC body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun flipletters-rpc-method (request)
  "REQUEST's JSON-RPC method, and for a call its tool name after a slash."
  (let* ((message (and request (flipletters-rpc request)))
         (method (nlk:json-value message :string "method")))
    (if (equal method "tools/call")
        (format nil "tools/call ~a" (nlk:json-value message :string "params" "name"))
        method)))

(defun flipletters-arguments (request)
  "REQUEST's tools/call arguments, or NIL for another message."
  (nlk:json-value (flipletters-rpc request) :object "params" "arguments"))

(defun flipletters-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defparameter +handshake+
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{}}}"
  "What the initialize request is answered with.")

(defun flipletters-headers ()
  "The response headers of a stubbed answer: a JSON body and a session id."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table) "application/json"
          (gethash "mcp-session-id" table) "sess-1")
    table))

(defun flipletters-text-result (text)
  "TEXT as the body of a tools/call answer carrying one content block."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"content\":[{\"type\":\"text\",\"text\":~a}]}}"
          (nlk:encode-json-object text)))

(defun flipletters-handler (answer)
  "The stub from here on: the handshake and its notification, then ANSWER, a
function of the JSON-RPC method => (values BODY STATUS)."
  (setf *flipletters-answer-fn*
        (lambda (method url args)
          (declare (ignore method url))
          (let ((rpc (nlk:json-value (nlk:decode-json (getf args :content)) :string "method")))
            (cond ((equal rpc "initialize") (values +handshake+ 200 (flipletters-headers)))
                  ((equal rpc "notifications/initialized") (values "" 202 (flipletters-headers)))
                  (t (multiple-value-bind (body status) (funcall answer rpc)
                       (values body (or status 200) (flipletters-headers)))))))))

(defun flipletters-answer-text (text)
  "The stub from here on: every tools/call answers TEXT as its one content block."
  (flipletters-handler (lambda (rpc)
                         (declare (ignore rpc))
                         (values (flipletters-text-result text) 200))))

(defun flipletters-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun flipletters-call-request (name)
  "The tools/call request of the test for tool NAME, or NIL."
  (find (format nil "tools/call ~a" name) (flipletters-asked)
        :key #'flipletters-rpc-method :test #'equal))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "flipletters" "FLIPLETTERS-CELL-")

(defun flipletters-config (&rest pairs)
  "A whole shared config with one `flipletters' section built from PAIRS."
  (nlk:json-object "flipletters" (apply #'nlk:make-json-object pairs)))

(defun flipletters-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-flipletters:start-cell (apply #'flipletters-config pairs)))

(define-cell-lifecycle-tests "flipletters"
  (:fixture with-flipletters-wire)
  (:help :flipletters)
  (:command "flipletters")
  (:running (is (nle::find-tool "flipletters_validate_board") "the validate tool is registered")
            (is (nle::find-tool "flipletters_create_animation") "the one-call render is registered")
            (is (nle::find-tool "flipletters_tools") "the tool list is registered")
            (is (equal "https://app.flipletters.com/mcp/v1" (nodecode-flipletters::setting :url)) "the section's default endpoint")
            (is (equal "flk_test" (nodecode-flipletters::api-key)) "the key is read from the variable")
            (is (= 60 (nodecode-flipletters::setting :timeout-seconds)) "the default ceiling"))
  (:stopped (is (null (nle::find-tool "flipletters_validate_board")) "stop unregisters the tools")
            (is (null (nle::find-tool "flipletters_tools")) "both of them"))
  (:refused ("timeout_seconds" 4)
            ("url" "app.flipletters.com")
            ("api_key" nil))
  (:idle nodecode-flipletters:flipletters-error (nodecode-flipletters::status-text)))

;;; --- the section ----------------------------------------------------------------

(deftest flipletters-cell-a-section-with-no-key-refuses-by-the-variable-s-name ()
  (with-flipletters-wire
    (let ((*flipletters-env* '()))
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (flipletters-start)))
                   ("FLIPLETTERS_API_KEY" "the refusal names the variable")
                   ("FlipLetters Studio" "and says where the key comes from"))
      (is (null (nle::find-tool "flipletters_validate_board")) "a refused start installed nothing"))))

(deftest flipletters-cell-a-key-written-inline-is-read-from-the-section ()
  (with-flipletters-wire
    (let ((*flipletters-env* '()))
      (with-cell-stop ((flipletters-start "api_key" "flk_inline"))
        (is (equal "flk_inline" (nodecode-flipletters::api-key)) "api_key is the key")))))

;;; --- the handshake and one call ------------------------------------------------

(deftest flipletters-cell-a-validate-answers-the-server-s-text ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-answer-text "{\"valid\":true,\"warnings\":[],\"shareUrl\":\"https://app.flipletters.com/s/abc\"}")
      (let ((text (flipletters-tool "flipletters_validate_board"
                                     (nlk:json-object "code" "OUT: HELLO"))))
        (is (search "https://app.flipletters.com/s/abc" text) "the share link is the server's words")
        (let* ((requests (flipletters-asked))
               (call (flipletters-call-request "flipletters_validate_board")))
          (is (= 3 (length requests)) "initialize, its notification, the call")
          (is (equal "initialize" (flipletters-rpc-method (first requests))) "the session is initialized first")
          (is (equal "notifications/initialized" (flipletters-rpc-method (second requests))) "and acknowledged")
          (is (eq call (third requests)) "the third is the call, not another handshake")
          (is (equal "https://app.flipletters.com/mcp/v1" (second call)) "the section's endpoint")
          (is (equal "Bearer flk_test" (flipletters-header call "authorization")) "the key rides as a bearer")
          (is (equal "application/json, text/event-stream" (flipletters-header call "accept")) "MCP's accept")
          (is (equal "OUT: HELLO" (nlk:json-value (flipletters-arguments call) :string "code")) "the code rides as the argument")
          (is (equal "sess-1" (flipletters-header (third requests) "mcp-session-id")) "the session id the handshake issued is echoed")
          (is (equal "2025-06-18" (flipletters-header (third requests) "mcp-protocol-version"))))))))

(deftest flipletters-cell-create-animation-carries-its-text-and-template ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-answer-text "{\"mp4Url\":\"https://cdn.example.com/b.mp4\",\"status\":\"done\"}")
      (let ((text (flipletters-tool "flipletters_create_animation"
                                     (nlk:json-object "text" "NOW BOARDING" "template" "trainboard")))
            (arguments (flipletters-arguments
                        (flipletters-call-request "flipletters_create_animation"))))
        (is (search "https://cdn.example.com/b.mp4" text) "the MP4 URL is answered")
        (is (equal "NOW BOARDING" (nlk:json-value arguments :string "text")))
        (is (equal "trainboard" (nlk:json-value arguments :string "template")))))))

(deftest flipletters-cell-a-render-sends-its-boolean-and-leaves-out-what-was-not-given ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-answer-text "queued")
      (flipletters-tool "flipletters_render_board" (nlk:json-object "code" "OUT: HI"))
      (let ((arguments (flipletters-arguments
                        (flipletters-call-request "flipletters_render_board"))))
        (is (eq t (nlk:json-value arguments :boolean "wait")) "wait defaults to polling")
        (is (null (nth-value 1 (nlk:json-value arguments :integer "repeat"))) "an unset member is not sent"))
      (flipletters-tool "flipletters_render_board"
                         (nlk:json-object "code" "OUT: HI" "wait" t "repeat" 3))
      (let ((arguments (flipletters-arguments (car (last (flipletters-asked))))))
        (is (eq t (gethash "wait" arguments)) "wait stays a JSON true")
        (is (= 3 (nlk:json-value arguments :integer "repeat")) "and the repeat rides")))))

(deftest flipletters-cell-a-share-names-the-ids-the-server-asked-for ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-answer-text "posted")
      (flipletters-tool "flipletters_share_to_postiz"
                         (nlk:json-object "workspace_id" "w1" "destination_id" "d1"
                                          "integration_id" "i1" "date" "2026-08-14T15:00:00.000Z"
                                          "content" "flip!"))
      (let ((arguments (flipletters-arguments
                        (flipletters-call-request "flipletters_share_to_postiz"))))
        (is (equal "w1" (nlk:json-value arguments :string "workspaceId")) "the ids are camelCase on the wire")
        (is (equal "2026-08-14T15:00:00.000Z" (nlk:json-value arguments :string "date")))
        (is (equal "flip!" (nlk:json-value arguments :string "content")))
        (is (null (nth-value 1 (nlk:json-value arguments :string "type"))) "an unset type is not sent")))))

(deftest flipletters-cell-a-refused-answer-is-a-repair-not-a-crash ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-handler
       (lambda (rpc)
         (declare (ignore rpc))
         (values "{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32001,\"message\":\"invalid api key\"}}"
                 200)))
      (let ((text (flipletters-tool "flipletters_list_teams" (nlk:json-object))))
        (is (search "ERROR: FlipLetters refused tools/call" text) text)
        (is (search "invalid api key" text) text))
      (flipletters-handler (lambda (rpc) (declare (ignore rpc)) (values "{}" 401)))
      (let ((text (flipletters-tool "flipletters_list_teams" (nlk:json-object))))
        (is (search "ERROR: FlipLetters: the server answered 401" text) text)
        (is (search "the key was refused" text) text)))))

(deftest flipletters-cell-a-dropped-session-is-made-again ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-answer-text "ok")
      (is (equal "ok" (flipletters-tool "flipletters_list_teams" (nlk:json-object))))
      (is (= 3 (length (flipletters-asked))) "one handshake and one call")
      (let ((dropped nil))
        (flipletters-handler
         (lambda (rpc)
           (declare (ignore rpc))
           (if dropped
               (values (flipletters-text-result "again") 200)
               (progn (setf dropped t) (values "{}" 404)))))
        (is (equal "again" (flipletters-tool "flipletters_list_teams" (nlk:json-object))) "a 404 on the call makes a new session and asks again")
        (is (= 7 (length (flipletters-asked))) "handshake, notification, the first call, the 404, then a fresh three")))))

(deftest flipletters-cell-the-tool-list-names-what-the-server-offers ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-handler
       (lambda (rpc)
         (declare (ignore rpc))
         (values "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"flipletters_validate_board\",\"description\":\"Validate a board\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"code\":{\"type\":\"string\"}},\"required\":[\"code\"]}},{\"name\":\"flipletters_list_teams\",\"description\":\"Teams\",\"inputSchema\":{\"type\":\"object\",\"properties\":{}}}]}}"
                 200)))
      (let ((text (flipletters-tool "flipletters_tools" (nlk:json-object))))
        (is (search "2 tools" text) text)
        (is (search "flipletters_validate_board" text))
        (is (search "required: code" text) "the schema's required arguments are named"))
      (is (find "tools/list" (flipletters-asked) :key #'flipletters-rpc-method :test #'equal) "one tools/list")
      (is (search "flipletters_list_teams" (flipletters-tool "flipletters_tools" (nlk:json-object))))
      (is (= 1 (count "tools/list" (flipletters-asked)
                      :key #'flipletters-rpc-method :test #'equal)) "the list is kept for this process"))))

;;; --- /flipletters ---------------------------------------------------------------

(deftest flipletters-cell-the-command-says-where-it-points ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (let ((text (cell-entry "nodecode-flipletters" "flipletters" "")))
        (is (search "FlipLetters: https://app.flipletters.com/mcp/v1" text))
        (is (search "key: set" text) "the key is reported as set, never printed")
        (is (search "/flipletters tools" text) "and the words it takes are named")
        (is (null (flipletters-asked)) "a status read asks the server nothing"))
      (is (search "usage:" (cell-entry "nodecode-flipletters" "flipletters" "nonsense"))))))

(deftest flipletters-cell-the-command-prints-the-reference ()
  (with-flipletters-wire
    (with-cell-stop ((flipletters-start))
      (flipletters-answer-text "# flipcode: OUT: ...")
      (is (search "# flipcode" (cell-entry "nodecode-flipletters" "flipletters" "reference")))
      (is (find "tools/call flipletters_flipcode_reference" (flipletters-asked)
                :key #'flipletters-rpc-method :test #'equal)))))
