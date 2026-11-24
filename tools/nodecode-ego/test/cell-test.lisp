;;;; cell-test.lisp --- lifecycle, and the tools and the command through a stubbed relay.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs the ego relay, a Mac, a token or the network: NODECODE-EGO::*POST*
;;;; is bound to a stub that answers the MCP handshake and one tool per test.
;;;; What is under test is what this cell owns -- the request it builds, the
;;;; session it keeps, and how each answer reads.

(in-package #:nodecode.test)

(define-test-slice "ego" "EGO-CELL-" :start nodecode-ego:start-cell)

(define-cell-lifecycle-tests "ego"
  (:config "token" "test-token")
  (:command "ego")
  (:help :ego)
  (:running
   (is (nle::find-tool "ego_get_page_text") "the read tool is registered")
   (is (nle::find-tool "ego_save_page_markdown") "the save tool is registered")
   (is (nle::find-tool "ego_navigate") "the navigate tool is registered")
   (is (nle::find-tool "ego_list_tabs") "the tab list is registered")
   (is (nle::find-tool "ego_status") "the status tool is registered"))
  (:stopped
   (is (null (nle::find-tool "ego_get_page_text")) "and stop takes the read tool back"))
  (:refused ("token_env" "")
            ("timeout_seconds" 0)
            ("timeout_seconds" "soon"))
  (:idle nodecode-ego:ego-error (nodecode-ego:status-text)))

;;; --- the stubbed relay ---------------------------------------------------------------

(defparameter *page-json*
  "{\"label\": \"p1\", \"title\": \"Example Domain\", \"url\": \"https://example.org/\", \"text\": \"Hello page.\", \"truncated\": false}"
  "ego_get_page_text's answer for one short page.")

(defparameter *status-json*
  "{\"online\": true, \"egoVersion\": \"1.2.3\", \"egoReachable\": true, \"egoError\": null, \"capabilities\": [\"fetch_images\"]}"
  "ego_status's answer for a daemon whose browser answered.")

(defparameter *tabs-json*
  "{\"tabs\": [{\"label\": \"p1\", \"title\": \"Example Domain\", \"url\": \"https://example.org/\", \"active\": true}], \"spaceId\": \"space-1\"}"
  "ego_list_tabs' answer for one agent tab.")

(defparameter *save-json*
  "{\"path\": \"/notes/example.md\", \"chars\": 42, \"images_ok\": 1, \"images_failed\": 0, \"images_pending\": 0}"
  "ego_save_page_markdown's answer for one note that landed.")

(defvar *answers* nil
  "Tool name -> the answer text the stubbed relay gives it.")

(defvar *calls* nil
  "Every request this test made, newest first: (METHOD TOOL ARGUMENTS HEADERS).")

(defun ego-object (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          when value do (setf (gethash key table) value))
    table))

(defun ego-encode (value)
  (nlk:encode-json-object value))

(defun ego-reply (id result)
  (ego-encode (ego-object "jsonrpc" "2.0" "id" id "result" result)))

(defun ego-tool-reply (id text)
  (ego-reply id (ego-object "content" (vector (ego-object "type" "text" "text" text))
                             "isError" nil)))

(defun ego-stub (url headers body timeout)
  "The relay's side of the one call this cell makes, by method and by tool."
  (declare (ignore url timeout))
  (let* ((message (cell-json body))
         (method (gethash "method" message))
         (params (gethash "params" message))
         (tool (and (hash-table-p params) (gethash "name" params))))
    (push (list method tool (and tool (gethash "arguments" params)) headers) *calls*)
    (cond
      ((equal method "initialize")
       (values (ego-reply (gethash "id" message)
                           (ego-object "protocolVersion" "2025-06-18"
                                       "capabilities" (ego-object)
                                       "serverInfo" (ego-object "name" "ego")))
               200 '(("mcp-session-id" . "session-1") ("content-type" . "application/json"))))
      ((equal method "notifications/initialized")
       (values "" 202 nil))
      ((equal method "tools/call")
       (let ((answer (cdr (assoc tool *answers* :test #'equal))))
         (values (ego-tool-reply (gethash "id" message) (or answer "{}"))
                 200 '(("content-type" . "application/json")))))
      (t (values "" 200 nil)))))

(defmacro with-ego ((&rest pairs) &body body)
  "BODY with the cell started on the stubbed relay and one answer per tool."
  `(let ((*answers* (list (cons "ego_status" *status-json*)
                           (cons "ego_list_tabs" *tabs-json*)
                           (cons "ego_get_page_text" *page-json*)
                           (cons "ego_save_page_markdown" *save-json*)
                           (cons "ego_navigate" *page-json*)))
         (*calls* nil)
         (nodecode-ego::*post* #'ego-stub))
     (with-cell-stop ((ego-start "token" "test-token" ,@pairs))
       ,@body)))

(defun ego-arguments (tool)
  "The arguments the last call of TOOL carried."
  (third (find tool *calls* :key #'second :test #'equal)))

;;; --- the tools -----------------------------------------------------------------------

(deftest ego-cell-reads-a-page-through-the-relay ()
  (with-ego ()
    (let ((text (nodecode-ego:page-text :url "https://example.org/")))
      (is (search "Hello page." text) "the page's text is the answer")
      (is (search "Example Domain - https://example.org/" text) "with its title and url above it")
      (let ((sent (gethash "url" (ego-arguments "ego_get_page_text")))
            (bearer (cdr (assoc "authorization" (fourth (first *calls*)) :test #'string-equal))))
        (is (equal "https://example.org/" sent) "the url rides the call, so opening and reading are one call")
        (is (find "initialize" *calls* :key #'first :test #'equal) "over a session that was opened")
        (is (equal "Bearer test-token" bearer) "with the section's token as its bearer")))))

(deftest ego-cell-saves-and-lists ()
  (with-ego ()
    (let ((saved (nodecode-ego:save-text :url "https://example.org/"
                                         :subdir "notes" :slug "example")))
      (is (search "/notes/example.md" saved) "a save answers where the note landed")
      (is (search "42 characters" saved) "and what it holds")
      (let ((sent (gethash "subdir" (ego-arguments "ego_save_page_markdown"))))
        (is (equal "notes" sent) "subdir rides the call")))
    (let ((tabs (nodecode-ego:tabs-text)))
      (is (search "Example Domain" tabs) "the tab list names the agent's tab")
      (is (search "(active)" tabs) "and marks the active one"))))

(deftest ego-cell-status-and-command ()
  (with-ego ()
    (let ((status (nodecode-ego:status-text)))
      (is (search "ego lite answered" status) "status says the browser answered")
      (is (search "1.2.3" status) "with the version it answered with"))
    (multiple-value-bind (text dialog) (cell-entry "nodecode-ego" "ego" "status")
      (declare (ignore dialog))
      (is (search "ego lite answered" text) "/ego answers the same report"))))

(deftest ego-cell-a-relay-that-does-not-answer-refuses ()
  (let ((nodecode-ego::*post* (lambda (url headers body timeout)
                                (declare (ignore url headers body timeout))
                                (error "Connection refused"))))
    (with-cell-stop ((ego-start "token" "test-token"))
      (let ((text (refusal-text nodecode-ego:ego-error (nodecode-ego:status-text))))
        (is (search "did not answer" text) "the refusal says the relay did not answer")
        (is (search "is the relay running" text) "and names the next step")))))
