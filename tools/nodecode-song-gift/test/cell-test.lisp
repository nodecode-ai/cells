;;;; cell-test.lisp --- lifecycle, the four tools and /song-gift, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches the catalog and
;;;; nothing touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, wrong values and an empty endpoint refused),
;;;; the MCP exchange it speaks (initialize, then the catalog's own tool), the
;;;; four tools through the seam the model calls them by -- their tool name,
;;;; their arguments, their headers and their answer -- and the operator's
;;;; command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-song-gift-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials the catalog."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected song-gift request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun song-gift-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun song-gift-request-body (request)
  "REQUEST's JSON body, decoded."
  (let ((content (getf (cddr request) :content)))
    (and (stringp content) (nlk:decode-json content))))

(defun song-gift-method (request)
  "The JSON-RPC method REQUEST carried."
  (nlk:json-value (song-gift-request-body request) :text "method"))

(defun song-gift-call-requests ()
  "The stubbed tools/call requests, oldest first."
  (remove-if-not (lambda (request) (equal "tools/call" (song-gift-method request)))
                 (song-gift-asked)))

(defun song-gift-params (request)
  "The params object a stubbed JSON-RPC REQUEST carried."
  (nlk:json-value (song-gift-request-body request) :object "params"))

(defun song-gift-arguments (request)
  "The arguments object a stubbed tools/call REQUEST carried."
  (nlk:json-value (song-gift-params request) :object "arguments"))

(defun song-gift-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun song-gift-headers (&optional (content-type "application/json") session)
  "A decoded header table, as NLK:HTTP answers one."
  (let ((table (make-hash-table :test 'equal)))
    (when content-type (setf (gethash "content-type" table) content-type))
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun song-gift-result (text &key errorp)
  "An MCP tool result whose one text block is TEXT."
  (nlk:encode-json-object
   (nlk:make-json-object "content"
                          (vector (nlk:make-json-object "type" "text" "text" text))
                          "isError" (and errorp t))))

(defun song-gift-serving (result)
  "A server that answers the handshake and gives RESULT -- an encoded MCP result -- to any tool."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((body (nlk:decode-json (getf args :content)))
                 (rpc (nlk:json-value body :text "method"))
                 (id (nlk:json-value body :integer "id")))
            (cond
              ((equal rpc "initialize")
               (values "{\"jsonrpc\": \"2.0\", \"result\": {\"protocolVersion\": \"2025-06-18\"}}"
                       200 (song-gift-headers "application/json" "session-1")))
              ((equal rpc "notifications/initialized")
               (values "" 202 (song-gift-headers)))
              ((equal rpc "tools/call")
               (values (format nil "{\"jsonrpc\": \"2.0\", \"id\": ~d, \"result\": ~a}" id result)
                       200 (song-gift-headers "application/json" "session-1")))
              (t (error "unexpected JSON-RPC method ~a" rpc)))))))

(defun song-gift-answer (text)
  "The server answers TEXT as its one text block, whatever tool is called."
  (song-gift-serving (song-gift-result text)))

(defun song-gift-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "song-gift-call" name arguments)))

(defparameter +catalog+
  "{\"occasions\": [{\"name\": \"birthday\", \"label\": \"Birthday\",      \"tip\": \"name the year you met and one habit only you know\"},      {\"name\": \"anniversary\", \"label\": \"Anniversary\", \"tip\": \"the first trip, in detail\"}],      \"count\": 2}"
  "An occasion answer in the shape the plugin's README describes.")

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "song-gift" "SONG-GIFT-CELL-" :start nodecode-song-gift:start-cell)

(define-cell-lifecycle-tests "song-gift"
  (:fixture with-song-gift-wire)
  (:config "timeout_seconds" 45)
  (:help :song-gift)
  (:command "song-gift")
  (:running
   (is (nle::find-tool "list_occasions") "list_occasions is registered")
   (is (nle::find-tool "list_styles") "and list_styles")
   (is (nle::find-tool "guide_story_and_lyrics") "and guide_story_and_lyrics")
   (is (nle::find-tool "get_pricing_options") "and get_pricing_options")
   (is (equal "https://customsonggift.org/api/mcp" (nodecode-song-gift::setting :endpoint)) "pointing at the plugin's own catalog"))
  (:stopped
   (is (null (nle::find-tool "list_occasions")) "stop unregisters the tools")
   (is (null (nle::find-tool "get_pricing_options")) "every one of them"))
  (:refused ("endpoint" "")
            ("endpoint" "customsonggift.org/api/mcp")
            ("timeout_seconds" 0)))

(deftest song-gift-cell-a-blank-endpoint-says-what-to-set (with-song-gift-wire)
  ;; The one setting a call cannot work without: an empty endpoint refuses at
  ;; start, in words that name the member and the catalog's own address.
  (let ((text (refusal-text nlk:config-refusal (song-gift-start "endpoint" ""))))
    (is-carrying text "no endpoint" "endpoint")
    (is (search "https://customsonggift.org/api/mcp" text) "the refusal names the address to keep"))
  (let ((text (refusal-text nlk:config-refusal (song-gift-start "endpoint" "catalog.test"))))
    (is (search "http://" text) "and what a good address looks like"))
  (is (null (nle::find-tool "list_occasions")) "and nothing was installed"))

;;; --- the exchange -----------------------------------------------------------------

(deftest song-gift-cell-a-tool-call-handshakes-then-asks-the-catalog (with-song-gift-wire)
  (with-cell-stop ((song-gift-start))
    (song-gift-answer +catalog+)
    (let ((answer (song-gift-call "list_occasions" "{}")))
      (let ((requests (song-gift-asked)))
        (is (equal '("initialize" "notifications/initialized" "tools/call")
                   (mapcar #'song-gift-method requests)) "one call is initialize, initialized, tools/call")
        (is (equal "https://customsonggift.org/api/mcp" (second (first requests))) "to the plugin's endpoint")
        (is (equal "session-1" (song-gift-request-header (third requests) "mcp-session-id")) "with the session id initialize issued"))
      (is (equal "list_occasions" (nlk:json-value (song-gift-params (first (song-gift-call-requests)))
                                                    :text "name")) "the catalog's own tool name")
      (is (search "occasions: " answer) "a JSON answer is laid out by member")
      (is (search "birthday" answer) "with the occasion it named")
      (is (search "one habit only you know" answer) "and its storytelling tip")
      (is (search "count: 2" answer) "and how many there were"))))

;;; --- the four tools ---------------------------------------------------------------

(deftest song-gift-cell-styles-asks-for-the-occasion-only-when-one-is-named (with-song-gift-wire)
  (with-cell-stop ((song-gift-start))
    (song-gift-answer "piano ballad: slow, one voice, the words in front")
    (let ((answer (song-gift-call "list_styles" "{\"occasion\": \"anniversary\"}")))
      (is (search "piano ballad" answer) "a plain text answer is the text itself")
      (let ((request (first (song-gift-call-requests))))
        (is (equal "list_styles" (nlk:json-value (song-gift-params request) :text "name")) "the style tool")
        (is (equal "anniversary" (nlk:json-value (song-gift-arguments request) :text "occasion")) "carrying the occasion the call named"))))
  (with-cell-stop ((song-gift-start))
    (song-gift-answer "{}")
    (song-gift-call "list_styles" "{}")
    (let ((arguments (song-gift-arguments (car (last (song-gift-call-requests))))))
      (is (null (nlk:json-value arguments :text "occasion")) "and nothing the call left out"))))

(deftest song-gift-cell-the-guide-carries-the-memories-it-was-given (with-song-gift-wire)
  (with-cell-stop ((song-gift-start))
    (song-gift-answer "{\"structure\": \"verse, chorus, verse\", \"details\": [\"a name\", \"a place\"]}")
    (let ((answer (song-gift-call "guide_story_and_lyrics"
                                  "{\"occasion\": \"mothers_day\", \"recipient\": \"my mother\", \"memories\": [\"the kitchen radio\", \"her blue coat\"]}")))
      (let* ((request (first (song-gift-call-requests)))
             (arguments (song-gift-arguments request))
             (memories (nlk:json-value arguments :array "memories")))
        (is (equal "guide_story_and_lyrics" (nlk:json-value (song-gift-params request) :text "name")) "the guidance tool")
        (is (equal "mothers_day" (nlk:json-value arguments :text "occasion")) "the occasion")
        (is (equal "my mother" (nlk:json-value arguments :text "recipient")) "who it is for")
        (is (equal "the kitchen radio" (and memories (aref memories 0))) "and the memories, in order"))
      (is (search "structure: verse, chorus, verse" answer) "the advice laid out by member")
      (is (search "1. a name" answer) "with its detail list"))))

(deftest song-gift-cell-pricing-is-the-catalog-s-and-an-error-is-said-so (with-song-gift-wire)
  (with-cell-stop ((song-gift-start))
    (song-gift-answer "{\"starting_price\": \"9.90 USD\", \"includes\": [\"keepsake page\", \"MP3\"]}")
    (let ((answer (song-gift-call "get_pricing_options" "{}")))
      (is (search "9.90 USD" answer) "the price the catalog states")
      (is (equal "get_pricing_options"
                 (nlk:json-value (song-gift-params (first (song-gift-call-requests))) :text "name")) "through the pricing tool"))
    (song-gift-serving (song-gift-result "the catalog is being rebuilt" :errorp t))
    (let ((answer (song-gift-call "list_occasions" "{}")))
      (is (search "ERROR: " answer) "an error answer is marked as one")
      (is (search "being rebuilt" answer) "and carries the server's own words"))))

(deftest song-gift-cell-an-extra-member-goes-through-as-the-server-names-it (with-song-gift-wire)
  (with-cell-stop ((song-gift-start))
    (song-gift-answer "{}")
    (song-gift-call "list_occasions" "{\"extra\": {\"locale\": \"de\"}}")
    (let ((arguments (song-gift-arguments (first (song-gift-call-requests)))))
      (is (equal "de" (nlk:json-value arguments :text "locale")) "extra's member reaches the catalog unchanged"))))

(deftest song-gift-cell-a-refused-status-is-the-model-s-repair (with-song-gift-wire)
  (with-cell-stop ((song-gift-start))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method url args))
            (values "{\"jsonrpc\": \"2.0\", \"id\": 3, \"error\": {\"code\": -32603, \"message\": \"catalog unavailable\"}}"
                    503 (song-gift-headers))))
    (let ((answer (song-gift-call "list_styles" "{}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "catalog unavailable" answer) "and carries the server's own words"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest song-gift-cell-the-command-asks-or-says-what-it-is-connected-to (with-song-gift-wire)
  (with-cell-stop ((song-gift-start))
    (let ((status (nodecode-song-gift::run-command "" nil)))
      (is (search "https://customsonggift.org/api/mcp" status) "no argument reports the connection")
      (is (search "/song-gift styles" status) "and how to ask for a part of the catalog")
      (is (null (song-gift-asked)) "and sends nothing"))
    (song-gift-answer +catalog+)
    (let ((answer (nodecode-song-gift::run-command "  styles  anniversary  " nil)))
      (is (search "birthday" answer) "a word runs that part of the catalog")
      (let ((arguments (song-gift-arguments (first (song-gift-call-requests)))))
        (is (equal "anniversary" (nlk:json-value arguments :text "occasion")) "the occasion trimmed, as typed")))
    (let ((usage (nodecode-song-gift::run-command "songs" nil)))
      (is (search "usage: /song-gift" usage) "an unknown word says what the command takes"))))
