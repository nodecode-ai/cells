;;;; cell-test.lisp --- lifecycle, the two tools, /island, the session, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, every HTTP exchange stubbed:
;;;; that a start puts the manual, /island and the two tools on and a stop takes
;;;; them back; that a call forwards the world's tool and answers its text, and
;;;; an errored tool answers ERROR:; that the tool list reads names and
;;;; descriptions and narrows by filter; that one session is opened and reused
;;;; and an event-stream answer is read like a JSON one; that a call without a
;;;; tool name, a blank endpoint and a timeout under its floor are refused; and
;;;; that a world that is not running says so in words.

(in-package #:nodecode.test)

(define-test-slice "island" "ISLAND-CELL-" :start nodecode-island:start-cell)

;;; --- the stubbed world -------------------------------------------------------

(defvar *sent* '()
  "What the stubbed world received, newest first: (MESSAGE HEADERS) per POST.")

(defvar *world-up* t
  "NIL stands in for a game that is not running: every exchange signals.")

(defvar *stream* nil
  "T when tools/list answers as a server-sent event stream rather than one body.")

(defun json-object (&rest pairs)
  "A JSON object of PAIRS, as the cell builds one."
  (apply #'nlk:make-json-object pairs))

(defun reply (id result)
  "One JSON-RPC answer as the JSON text the world would send."
  (nlk:encode-json-object (json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-block (text)
  "One content block carrying TEXT."
  (json-object "type" "text" "text" text))

(defun world-answer (message)
  "What the stubbed world answers MESSAGE: (values TEXT STATUS HEADERS)."
  (let ((method (gethash "method" message))
        (id (gethash "id" message)))
    (cond
      ((equal method "initialize")
       (values (reply id (json-object "protocolVersion" "2025-06-18"
                                      "serverInfo" (json-object "name" "agent-inception")))
               200 '(("mcp-session-id" . "world-1"))))
      ((equal method "notifications/initialized")
       (values "" 202 nil))
      ((equal method "tools/list")
       (let ((text (reply id (json-object "tools"
                                          (vector (json-object "name" "look"
                                                               "description" "Look around the island.")
                                                  (json-object "name" "join"
                                                               "description" "Join a world as a villager."))))))
         (if *stream*
             (values (format nil "event: message~%data: ~a~%~%" text) 200 nil)
             (values text 200 nil))))
      ((equal method "tools/call")
       (let* ((params (gethash "params" message))
              (name (gethash "name" params)))
         (if (equal name "till")
             (values (reply id (json-object "content" (vector (text-block "no wheat here"))
                                            "isError" t))
                     200 nil)
             (values (reply id (json-object "content"
                                            (vector (text-block (format nil "you are on a meadow (~a)" name)))))
                     200 nil))))
      (t (values "{}" 200 nil)))))

(defun stub-http (method url &rest args)
  "The stubbed nlk:http: record the exchange, then answer it as the world would."
  (declare (ignore method url))
  (let* ((body (getf args :content))
         (message (nlk:decode-json body)))
    (push (list message (getf args :headers)) *sent*)
    (unless *world-up* (error "connection refused"))
    (world-answer message)))

(defun posted (method)
  "The messages the stubbed world received for METHOD, newest first."
  (loop for (message nil) in *sent*
        when (equal method (gethash "method" message)) collect message))

(defmacro with-island-world ((&key (up t) stream) &body body)
  "Run BODY inside a started cell talking to the stubbed world; stopped on unwind."
  `(with-saved-globals (nle:*tools* nodecode-island::*world* nodecode-island::*answers*)
     (let ((*sent* '()) (*world-up* ,up) (*stream* ,stream))
       (with-stubbed-fdefinition (nlk:http (method url &rest args) (apply #'stub-http method url args))
         (with-cell-stop ((island-start))
           ,@body)))))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "island"
  (:help :island)
  (:command "island")
  (:running (is (equal "http://localhost:5173/mcp" (nodecode-island::setting :world-url)) "the local world is the default")
            (is (= 20 (nodecode-island::setting :timeout-seconds)))
            (is (nle::find-tool "island") "the island tool is registered")
            (is (nle::find-tool "island_tools") "and the list tool"))
  (:stopped (is (null (nle::find-tool "island")) "stop unregisters the tools"))
  (:refused ("world_url" 42) ("timeout_seconds" 0))
  (:idle island:island-error (island:call "look") (island:tools)))

;;; --- the two tools -------------------------------------------------------------

(deftest island-cell-a-call-forwards-the-tool-and-answers-its-text ()
  (with-island-world ()
    (is (equal "you are on a meadow (look)" (island:call "look")) "the world's text is the answer")
    (let* ((params (gethash "params" (first (posted "tools/call")))))
      (is (equal "look" (gethash "name" params)) "the tool was forwarded by name")
      (is (hash-table-p (gethash "arguments" params)) "with an empty argument object")
      (is (zerop (hash-table-count (gethash "arguments" params)))))))

(deftest island-cell-a-tool-that-refused-answers-error ()
  (with-island-world ()
    (is (equal "ERROR: no wheat here" (island:call "till")) "the world's isError becomes the model's ERROR")))

(deftest island-cell-the-tool-list-reads-names-and-narrows-by-filter ()
  (with-island-world ()
    (let ((all (island:tools)))
      (is (search "2 tools at http://localhost:5173/mcp" all))
      (is (search "look - Look around the island." all))
      (is (search "join - Join a world as a villager." all)))
    (is-carrying (answer (island:tools "villager"))
      ("1 tool at http://localhost:5173/mcp") ("join - Join a world as a villager.")
      (:absent "look - Look around the island." "the filter keeps only what names it"))))

;;; --- the session ---------------------------------------------------------------

(deftest island-cell-one-session-is-opened-and-reused ()
  (with-island-world ()
    (island:call "look")
    (island:call "look")
    (is (= 1 (length (posted "initialize"))) "one initialize for the run")
    (is (equal "world-1"
               (cdr (assoc "mcp-session-id" (second (first *sent*)) :test #'string-equal))) "the second call carries the session the first opened")))

(deftest island-cell-an-event-stream-answer-reads-like-a-json-one ()
  (with-island-world (:stream t)
    (is (search "look - Look around the island." (island:tools)) "the last event's data is the answer")))

;;; --- the command ---------------------------------------------------------------

(deftest island-cell-the-command-says-where-the-world-is ()
  (with-island-world ()
    (is-carrying (answer (nodecode-island::run-slash nil nil))
      ("island: http://localhost:5173/mcp answers 2 tools") ("look, join")))
  (with-island-world (:up nil)
    (is-carrying (answer (nodecode-island::run-slash nil nil))
      ("did not answer" "is the game running there?"))))

;;; --- the boundaries ------------------------------------------------------------

(deftest island-cell-a-call-without-a-tool-name-is-refused ()
  (is (signals-error nlk:contract-refusal
        (nodecode-island::decode-island-call (json-object))) "the contract's required tool is the gate"))

(deftest island-cell-a-world-that-is-not-running-says-so ()
  (with-island-world (:up nil)
    (is-carrying (answer (handler-case (island:call "look")
                           (island:island-error (condition) (princ-to-string condition))))
      ("did not answer" "is the game running there?")
      ("http://localhost:5173/mcp" "it names where it looked"))))
