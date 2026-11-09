;;;; cell-test.lisp --- the tools over a stubbed transport, the sign-in, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs the network: every request goes through
;;;; NODECODE-SUPER-ARTIFACTS::*HTTP*, a stub that records the call it was
;;;; handed and answers what the test chose. What is under test is what this
;;;; cell owns -- the JSON-RPC it composes, the SSE and JSON answers it reads,
;;;; the arguments each tool forwards, the words a refusal carries, and the
;;;; device flow that ends with a token file at 0600.

(eval-when (:load-toplevel :execute) (require :sb-posix))

(in-package #:nodecode.test)

(define-test-slice "super-artifacts" "SUPER-ARTIFACTS-CELL-" :start nodecode-super-artifacts:start-cell)

(define-cell-lifecycle-tests "super-artifacts"
  (:config "base_url" "https://superartifacts.app/mcp" "state_dir" (namestring (scratch-dir))
           "key_file" (namestring (scratch-key)))
  (:help :super-artifacts)
  (:command "super-artifacts")
  (:running
   (is (nle::find-tool "super_whoami") "the whoami tool is registered")
   (is (nle::find-tool "super_publish") "the publish tool is registered")
   (is (nle::find-tool "super_data") "the read-back tool too")
   (is (nle::find-tool "super_artifacts") "and the listing")
   (is (nle::find-tool "super_tool") "and the passthrough")
   (let ((endpoint (nodecode-super-artifacts::endpoint-text)))
     (is (equal "https://superartifacts.app/mcp" endpoint) "the section's server is what a call goes to")))
  (:stopped
   (is (null (nle::find-tool "super_publish")) "stop takes every tool back"))
  (:refused ("timeout_seconds" 0) ("timeout_seconds" "soon"))
  (:idle nodecode-super-artifacts:super-artifacts-error (nodecode-super-artifacts::status-line)))

;;; --- the fixture ------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL HEADERS BODY), newest first.")

(defvar *answer* nil
  "What the stubbed transport answers: a function of METHOD, URL and the
request's BODY text, answering (VALUES TEXT STATUS HEADERS).")

(defvar *scratch* 0
  "How many scratch folders this run has made, so each test's is its own.")

(defvar *dir* nil
  "The scratch folder of the fixture running now.")

(defun scratch-dir ()
  "A fresh state folder under the run's home."
  (let ((dir (uiop:ensure-directory-pathname
              (nlk:home (format nil "test-super-artifacts/~d/" (incf *scratch*))))))
    (ensure-directories-exist dir)
    dir))

(defun scratch-key ()
  "A file holding one key, the way the section's key_file names it."
  (let ((path (merge-pathnames "key" (scratch-dir))))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string "test-key" out))
    path))

(defun answer-json (json &optional (status 200) headers)
  "One stubbed answer: JSON text, a status and headers."
  (values json status headers))

(defun serve (function &key (status 200) headers)
  "Make every stubbed request answer STATUS with the text FUNCTION makes of
its BODY -- which it reads as decoded JSON."
  (setf *answer* (lambda (method url body)
                   (declare (ignore method url))
                   (values (funcall function body) status headers))))

(defmacro with-server (() &body body)
  "BODY with the cell started on a scratch folder and a stubbed transport."
  `(let* ((*dir* (scratch-dir))
          (*requests* '())
          (*answer* (lambda (method url body)
                      (declare (ignore method url body))
                      (error "unexpected request: the test scripted none")))
          (nodecode-super-artifacts::*http*
            (lambda (method url &rest options)
              (push (list method url (getf options :headers) (getf options :content)) *requests*)
              (let ((content (getf options :content)))
                (funcall *answer* method url
                         (if (and (stringp content)
                                  (uiop:string-prefix-p "{" (nlk:trimmed content)))
                             (nlk:decode-json content)
                             content))))))
     (with-cell-stop ((super-artifacts-start "base_url" "https://superartifacts.app/mcp"
                                              "state_dir" (namestring *dir*)
                                              "key_file" (namestring (merge-pathnames "key" *dir*))))
       (with-open-file (out (merge-pathnames "key" *dir*) :direction :output
                                                          :if-exists :supersede
                                                          :if-does-not-exist :create)
         (write-string "test-key" out))
       ,@body)))

(defun asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun request-header (request name)
  (cdr (assoc name (third request) :test #'string-equal)))

(defun request-body (request)
  "A request's body as decoded JSON."
  (nlk:decode-json (fourth request)))

(defun body-text (request key)
  "The string under KEY in a request's body, or NIL."
  (nlk:json-value (request-body request) :string key))

(defun method-of (request)
  (body-text request "method"))

(defun args (&rest pairs)
  (apply #'nlk:make-json-object pairs))

(defun initialize-result ()
  "The handshake answer the transport expects."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"serverInfo\":{\"name\":\"super-artifacts\",\"version\":\"1\"}}}"))

(defun tool-answer (text)
  "One tools/call result carrying TEXT."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"content\":[{\"type\":\"text\",\"text\":~a}],\"isError\":false}}"
          (nlk:encode-json-object text)))

(defun sse (json)
  "JSON as the event stream the transport may answer with instead."
  (format nil "event: message~%data: ~a~%~%" json))

(defun routing (tools &optional (initialize (initialize-result)))
  "A stub answer for a whole session: the handshake, its notice, and TOOLS
answering a tools/call by the tool's name."
  (lambda (body)
    (let ((method (and (hash-table-p body) (nlk:json-value body :string "method"))))
      (cond
        ((equal method "initialize") initialize)
        ((equal method "notifications/initialized") "")
        ((equal method "tools/call")
         (let* ((params (gethash "params" body))
                (name (nlk:json-value params :string "name")))
           (or (gethash name tools)
               (format nil "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"no such tool\"}],\"isError\":true}}"))))
        (t (error "unexpected method: ~a" method))))))

;;; --- the tools -------------------------------------------------------------------

(deftest super-artifacts-cell-publishes-over-one-tools-call ()
  (with-server ()
    (serve (routing (let ((table (make-hash-table :test 'equal)))
                      (setf (gethash "super_publish" table)
                            (tool-answer "https://superartifacts.app/a/northlight/supper-club"))
                      table)))
    (let ((answer (nodecode-super-artifacts::tool-publish
                   (args "slug" "supper-club" "title" "Supper club" "html" "<h1>Menu</h1>"))))
      (is (search "https://superartifacts.app/a/northlight/supper-club" answer) "the tool answers the URL the page went live at")
      (is (search "supper-club" answer) "and names the slug a later version uses"))
    (let ((calls (asked)))
      (is (= 3 (length calls)) "a handshake, its notice and one tool call")
      (is (equal "initialize" (method-of (first calls))) "the handshake comes first")
      (is (equal "notifications/initialized" (method-of (second calls))) "then the transport's notice")
      (is (equal "tools/call" (method-of (third calls))) "then the call")
      (is (equal "Bearer test-key" (request-header (third calls) "authorization")) "with the key_file's key")
      (is (search "text/event-stream" (request-header (third calls) "accept")) "asking for either answer the server chooses")
      (let* ((params (gethash "params" (request-body (third calls))))
             (arguments (gethash "arguments" params)))
        (is (equal "super_publish" (nlk:json-value params :string "name")) "the server's own tool name")
        (is (equal "<h1>Menu</h1>" (nlk:json-value arguments :string "html")) "the whole page rides the arguments")
        (is (equal "private" (nlk:json-value arguments :string "visibility")) "private unless asked")))))

(deftest super-artifacts-cell-reads-an-event-stream-answer ()
  (with-server ()
    (setf *answer* (let ((router (routing (let ((table (make-hash-table :test 'equal)))
                                            (setf (gethash "super_data" table)
                                                  (tool-answer "9 rows: Ada, evening; Bo, early"))
                                            table))))
                     (lambda (method url body)
                       (declare (ignore method url))
                       (let ((text (funcall router body)))
                         (values (if (search "\"result\"" text) (sse text) text) 200 nil)))))
    (let ((answer (nodecode-super-artifacts::tool-data (args "artifact" "supper-club"))))
      (is (search "9 rows" answer) "the rows come back out of the event stream")
      (is (search "Ada" answer) "with what visitors typed"))
    (let* ((call (third (asked)))
           (arguments (gethash "arguments" (gethash "params" (request-body call)))))
      (is (equal "supper-club" (nlk:json-value arguments :string "artifact")) "the artifact is what the tool forwards")
      (is (null (nlk:json-value arguments :string "limit")) "and a member the call left out is not sent"))))

(deftest super-artifacts-cell-passes-another-tool-through ()
  (with-server ()
    (serve (routing (let ((table (make-hash-table :test 'equal)))
                      (setf (gethash "super_templates" table)
                            (tool-answer "3 templates: supper club, proof, plan"))
                      table)))
    (let ((answer (nodecode-super-artifacts::tool-call-any
                   (args "tool" "super_templates" "arguments" (args "request" "supper club")))))
      (is (search "supper club" answer) "the server's own answer is what the model reads"))
    (let* ((call (third (asked)))
           (params (gethash "params" (request-body call))))
      (is (equal "super_templates" (nlk:json-value params :string "name")) "the named tool is the one called")
      (is (equal "supper club" (nlk:json-value (gethash "arguments" params) :string "request")) "with the arguments it was given"))))

(deftest super-artifacts-cell-refuses-a-call-the-server-refused ()
  (with-server ()
    (serve (lambda (body)
             (declare (ignore body))
             (format nil "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Not connected to a Super Artifacts account.\"}],\"isError\":true}}")))
    (let ((answer (nodecode-super-artifacts::tool-whoami nil)))
      (is (search "ERROR:" answer) "a refused tool call answers as a failure")
      (is (search "Not connected" answer) "carrying the server's own words")
      (is (search "/super-artifacts signin" answer) "and what the model should tell the person"))))

(deftest super-artifacts-cell-names-the-setting-that-is-missing ()
  (with-server ()
    (serve (routing (make-hash-table :test 'equal)))
    (delete-file (merge-pathnames "key" *dir*))
    (let ((*token* nil))
      (let ((answer (nodecode-super-artifacts::tool-whoami nil)))
        (is (search "ERROR:" answer) "a call with no key is a refusal")
        (is (search "SUPER_ARTIFACTS_KEY" answer) "naming the variable to set")
        (is (null (asked)) "and nothing was sent")))
    (is (search "not ready" (nodecode-super-artifacts::status-line)) "status says so too")))

;;; --- the sign-in -----------------------------------------------------------------

(deftest super-artifacts-cell-signs-in-with-a-device-code ()
  (with-server ()
    (let ((router (routing (let ((table (make-hash-table :test 'equal)))
                             (setf (gethash "super_whoami" table)
                                   (tool-answer "account northlight"))
                             table))))
      (setf *answer* (lambda (method url body)
                       (declare (ignore method))
                       (cond
                         ((search "/oauth/register" url)
                          (values "{\"client_id\":\"client-1\"}" 201 nil))
                         ((search "/oauth/device_authorization" url)
                          (values "{\"device_code\":\"dev-1\",\"user_code\":\"ABCD-EFGH\",\"verification_uri_complete\":\"https://superartifacts.app/device?code=ABCD-EFGH\",\"expires_in\":600,\"interval\":5}"
                                  200 nil))
                         ((search "/oauth/token" url)
                          (values "{\"access_token\":\"minted-token\",\"refresh_token\":\"refresh-1\",\"expires_in\":3600}"
                                  200 nil))
                         (t (values (funcall router body) 200 nil))))))
    ;; the poll is driven by hand, one attempt at a time, so no thread races the script
    (with-stubbed-fdefinition (nlk:worker-start (name step &rest options)
                               nil)
      (let ((answer (nodecode-super-artifacts::begin-signin)))
        (is (search "ABCD-EFGH" answer) "the operator is given the code to type")
        (is (search "https://superartifacts.app/device?code=ABCD-EFGH" answer) "the product's own page"))
      (is (eq :stop (nodecode-super-artifacts::poll-attempt)) "an approval ends the poll")
      (let* ((path (nodecode-super-artifacts::token-path))
             (kept (uiop:read-file-string path))
             (mode (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path))))))
        (is (search "minted-token" kept) "the token the approval bought is kept")
        (is (equal #o600 mode) "private: only its owner can read it"))
      (setf *requests* '())
      (delete-file (merge-pathnames "key" *dir*))
      (let ((answer (nodecode-super-artifacts::tool-whoami nil)))
        (is (search "northlight" answer) "and the next call uses it"))
      (let ((call (first (asked))))
        (is (equal "Bearer minted-token" (request-header call "authorization")) "on the wire")))))

(deftest super-artifacts-cell-refuses-a-server-that-is-not-https ()
  (with-server ()
    (serve (routing (make-hash-table :test 'equal)))
    (let ((nodecode-super-artifacts::*super-artifacts*
            (list* :base-url "http://elsewhere.example/mcp"
                   nodecode-super-artifacts::*super-artifacts*)))
      (let ((answer (nodecode-super-artifacts::tool-whoami nil)))
        (is (search "ERROR:" answer) "a plain-http server elsewhere is a refusal")
        (is (search "base_url" answer) "naming the member")
        (is (null (asked)) "and nothing was sent")))))
