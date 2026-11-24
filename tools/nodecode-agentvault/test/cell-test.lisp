;;;; cell-test.lisp --- the lifecycle, and the two tools through the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every answer here comes through the seam a model or a shell would use:
;;;; AV-CALL-TOOL dispatches a tool exactly as a provider's tool call does, and
;;;; /vault runs the function the command runs. Nothing dials anything and no
;;;; app is installed: the one spawn in the cell is stubbed, so the "server" is
;;;; a string the test wrote, and the frames the cell sent are read back out of
;;;; a stream the stub was handed.

(in-package #:nodecode.test)

(define-test-slice "agentvault" "AGENTVAULT-CELL-" :start nodecode-agentvault:start-cell)

(define-cell-lifecycle-tests "agentvault"
  (:config "token" "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
  (:help :vault)
  (:command "vault")
  (:refused ("client" "valhalla") ("timeout_seconds" 1))
  (:idle nodecode-agentvault:agentvault-error (nodecode-agentvault:search "auth")))

;;; --- the stub server ----------------------------------------------------------------

(defparameter +av-token+ "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  "A binding token of the shape the app issues: 64 hexadecimal characters.")

(defvar *av-sent* nil
  "Each launch the stub made, newest first, as (ARGV ENVIRONMENT).")

(defun av-json-string (text)
  "TEXT as a JSON string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char char out))))
    (write-char #\" out)))

(defun av-answer (text &key error)
  "What the server answers for one tools/call: TEXT in its one content part."
  (format nil "{\"content\":[{\"type\":\"text\",\"text\":~a}],\"isError\":~a}"
          (av-json-string text) (if error "true" "false")))

(defun av-replies (&rest answers)
  "The frames a stub server writes: the handshake's two, then ANSWERS."
  (let ((id 0))
    (with-output-to-string (out)
      (flet ((reply (result)
               (format out "{\"jsonrpc\":\"2.0\",\"id\":~d,\"result\":~a}~%" (incf id) result)))
        (reply "{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"serverInfo\":{\"name\":\"vault\",\"version\":\"1\"}}")
        (reply "{\"tools\":[{\"name\":\"search_sessions\"},{\"name\":\"get_session\"}]}")
        (dolist (answer answers) (reply answer))))))

(defmacro with-agentvault ((requests &rest answers) &body body)
  "BODY with the cell started on a good token and the launch stubbed: the
\"server\" writes ANSWERS after its handshake, and REQUESTS is the stream the
frames this cell sent can be read back from with AV-FRAMES."
  `(let* ((script (av-replies ,@answers))
          (,requests (make-string-output-stream))
          (*av-sent* nil)
          (nodecode-agentvault::*launch*
            (lambda (argv &key environment &allow-other-keys)
              (push (list argv environment) *av-sent*)
              (values 4242 (make-string-input-stream script) nil nil ,requests))))
     (with-cell-stop ((agentvault-start "token" +av-token+))
       ,@body)))

(defun av-frames (stream)
  "The frames written to STREAM so far, decoded, oldest first."
  (let ((text (get-output-stream-string stream)))
    (loop for line in (uiop:split-string text :separator '(#\Newline))
          when (plusp (length (nlk:trimmed line)))
            collect (nlk:decode-json line))))

(defun av-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun av-args (&rest pairs)
  "An arguments body, as a provider sends one."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun av-call-frame (frames)
  "The tools/call frame among FRAMES."
  (find "tools/call" frames :test #'equal :key (lambda (frame) (gethash "method" frame))))

(defun av-call-arguments (frames)
  "The arguments of the tools/call among FRAMES."
  (gethash "arguments" (gethash "params" (av-call-frame frames))))

(defun av-call-name (frames)
  "The tool name of the tools/call among FRAMES."
  (gethash "name" (gethash "params" (av-call-frame frames))))

;;; --- the tools ------------------------------------------------------------------------

(deftest agentvault-cell-search-tool ()
  (with-agentvault (requests (av-answer "1 session: pinned_uri vault://s-1@3"))
    (is (search "pinned_uri" (av-call-tool "search_sessions" (av-args "query" "auth token" "limit" 3))) "the server's text is the tool's answer")
    (let ((frames (av-frames requests)))
      (is (equal "search_sessions" (av-call-name frames)) "the tool the app names")
      (let ((arguments (av-call-arguments frames)))
        (is (equal "auth token" (gethash "query" arguments)) "the query, as it was typed")
        (is (eql 3 (gethash "limit" arguments)) "the limit the call asked for"))
      (is (null (gethash "harness" (av-call-arguments frames))) "no filter for a member the call left alone"))))

(deftest agentvault-cell-session-tool ()
  (with-agentvault (requests (av-answer "the exact revision, whole"))
    (is (search "the exact revision" (av-call-tool "get_session"
                                                   (av-args "session_id" "s-1" "revision" 7))) "get_session answers the server's text")
    (let ((arguments (av-call-arguments (av-frames requests))))
      (is (equal "s-1" (gethash "session_id" arguments)) "the session the search returned")
      (is (eql 7 (gethash "revision" arguments)) "the revision copied as it was, never replaced"))))

(deftest agentvault-cell-an-error-answer-is-the-models-text ()
  (with-agentvault (requests (av-answer "query_too_broad" :error t))
    (is (search "query_too_broad" (av-call-tool "search_sessions" (av-args "query" "the"))) "the server's own error code reaches the model")
    (is (search "ERROR:" (av-call-tool "search_sessions" (av-args "query" "the"))) "and reads as a failed call")))

(deftest agentvault-cell-no-token-refuses ()
  (with-cell-stop ((agentvault-start))
    (is (search "binding token" (av-call-tool "search_sessions" (av-args "query" "auth"))) "a call without a token says which one to paste")
    (is (null *av-sent*) "and nothing was launched")))

;;; --- the environment the app is started with ------------------------------------------

(deftest agentvault-cell-launches-the-apps-server ()
  (with-agentvault (requests (av-answer "ok"))
    (av-call-tool "search_sessions" (av-args "query" "auth"))
    (destructuring-bind (argv environment) (first *av-sent*)
      (is (search "/Contents/Resources/runtime/node" (first argv)) "the app's own Node runtime")
      (is (search "/Contents/Resources/app/src/mcp/server.js" (second argv)) "the app's own MCP server")
      (is (member (format nil "VAULT_MCP_TOKEN=~a" +av-token+) environment :test #'equal) "the binding token, from the section and nowhere else")
      (is (member "VAULT_MCP_HARNESS=hermes" environment :test #'equal) "the client binding the token belongs to"))))

;;; --- /vault -----------------------------------------------------------------------------

(deftest agentvault-cell-command ()
  (with-agentvault (requests (av-answer "found from the command"))
    (is (search "found from the command" (nodecode-agentvault::run-command "auth" "s-1")) "/vault with words searches")
    (is (search "binding token set" (nodecode-agentvault::run-command "" "s-1")) "/vault typed alone says what the section holds, without a call")))
