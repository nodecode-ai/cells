;;;; cell.lisp --- the PhotoFresco editor, its operations, and the files around them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The editor runs in the browser; this cell is the link beside it. Three
;;;; tools: CONNECT opens a browser window of the connector's own and asks the
;;;; person there to allow this agent; OPS reads the editor's session, its open
;;;; documents and the operations the grants allow, and runs one of them; FILE
;;;; moves a local image in (doc.open) and an export out (export.artifact,
;;;; read back in chunks, checked against its SHA-256).
;;;;
;;;; The editor enforces the grants on every call, not this cell: what is
;;;; listed here is documentation. The original plugin resolved one MCP tool
;;;; per editor operation; a cell offers the same operations through one
;;;; executor, the catalog the editor answers being the authority.

(in-package #:nodecode-photofresco)

(nlk:define-peripheral photofresco :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "PhotoFresco is a browser image editor this cell drives through the
photofresco_connect, photofresco_ops and photofresco_file tools; in Lisp the
same calls answer as photofresco:connect, photofresco:status,
photofresco:documents, photofresco:operations, photofresco:execute,
photofresco:open-file and photofresco:export-file. A refusal is
PHOTOFRESCO-ERROR with its reason.
  (photofresco:connect)            open the editor in a browser window of its
      own and ask the person there to allow this agent; then, in that window,
      click Allow. The answer names the session and its grants.
  (photofresco:status)             the session: documents, grants, budget.
  (photofresco:documents)          open tabs, their ids, revisions, controllers.
  (photofresco:operations)         the operations the grants allow now, each
      with its parameters: read this before running one.
  (photofresco:execute \"layer.add\" :params (json-object \"name\" \"Sky\"))
      one operation; a refusal is the editor's answer, not an exception.
Use render.preview to see the result, doc.get_state to orient. Spend and
external-write operations ask the person in the editor the first time and
are then allowed within the budget they set there."
  "What (help :photofresco) answers while the cell runs.")

;;; --- state ----------------------------------------------------------------------

(defvar *link* nil
  "The open browser link, or NIL: set by CONNECT, cleared by DISCONNECT.")

(defvar *open-editor* #'open-editor
  "The seam a test stubs; the real one launches a browser of the connector's.")

(defun object (&rest keys-and-values)
  (apply #'json-object keys-and-values))

(defun require-link ()
  (or *link*
      (fail "no PhotoFresco session yet; run photofresco_connect first")))

(defun pairing-code ()
  (format nil "~4,'0d-~4,'0d" (random 10000) (random 10000)))

(defun new-request-id ()
  (format nil "req-~d-~d" (get-universal-time) (random 1000000)))

;;; --- the channel ----------------------------------------------------------------

(defun page-call (link method &key params session-key (tries 4000))
  "One request to the editor channel; its response, or a refusal naming why."
  (unless (and (link-key link) (link-context-id link))
    (fail "the editor's connector channel is not open"))
  (let* ((id (incf (link-seq link)))
         (envelope (object "v" 1 "key" (link-key link) "id" id "method" method)))
    (when session-key (setf (gethash "session_key" envelope) session-key))
    (when params (setf (gethash "params" envelope) params))
    (let* ((ack (funcall (link-send link) link "Runtime.callFunctionOn"
                          (object "functionDeclaration" *deliver*
                                  "executionContextId" (link-context-id link)
                                  "arguments" (vector (object "value" envelope))
                                  "returnByValue" t "silent" t)
                          (link-cdp-session link)))
           (verdict (nlk:json-value ack :string "result" "value")))
      (unless (equal verdict "accepted")
        (fail "the editor channel did not take ~a (~a)" method (or verdict "no answer"))))
    (let ((message (wait-message link id tries)))
      (unless message
        (fail "the editor did not answer ~a in time" method))
      (if (nlk:json-value message :boolean "ok")
          (nlk:json-value message :any "result")
          (fail "the editor refused ~a: ~a (~a)" method
                (or (nlk:json-value message :string "error" "message") "no message")
                (or (nlk:json-value message :string "error" "code") "no code"))))))

(defun wait-message (link id tries)
  "The queued response to ID, pumping the link until it lands."
  (loop repeat tries
        do (let ((found (find id (link-queue link)
                              :key (lambda (message) (nlk:json-value message :integer "id"))
                              :test #'eql)))
             (when found
               (setf (link-queue link) (remove found (link-queue link)))
               (return found)))
           (funcall (link-pump link) link)
        finally (return nil)))

;;; --- the public calls -----------------------------------------------------------

(defun disconnect ()
  "Forget the link; the editor window stays open for the person there."
  (when *link*
    (ignore-errors (funcall (link-close *link*) *link*))
    (setf *link* nil))
  "disconnected")

(defun connect (&key client document capabilities budget-credits)
  "Open the editor in the connector's own browser window, ask the person there
for a session, and answer it once allowed."
  (let* ((link (or (and *link* (link-key link) *link*) (funcall *open-editor*)))
         (access (when (or capabilities budget-credits)
                   (let ((grant (object "capabilities" (or capabilities #())))
                         )
                     (when budget-credits
                       (setf (gethash "budget_credits" grant) budget-credits))
                     grant)))
         (params (object "client" (or client (setting :client) "Nodecode")
                         "surface" "mcp"
                         "connector" "nodecode-photofresco 0.1.0"
                         "code" (pairing-code))))
    (setf *link* link)
    (when document (setf (gethash "document" params) document))
    (when access (setf (gethash "access" params) access))
    (let ((result (page-call link "pair" :params params)))
      (setf (link-session-key link) (nlk:json-value result :string "session_key"))
      result)))

(defun call (method &key params)
  "One of the session's own methods: status, documents, attach, release,
operations, outcome, request_access, close."
  (let ((link (require-link)))
    (page-call link method :params params
               :session-key (or (link-session-key link)
                                (fail "no paired editor session; run photofresco_connect")))))

(defun status () (call "status"))
(defun documents () (call "documents"))
(defun operations () (call "operations"))

(defun execute (op &key params request-id expected-revision)
  "Run one editor operation; the editor's answer, refusals included."
  (let ((request (object "request_id" (or request-id (new-request-id)) "op" op)))
    (when params (setf (gethash "params" request) params))
    (when expected-revision (setf (gethash "expected_revision" request) expected-revision))
    (call "execute" :params request)))

;;; --- files ---------------------------------------------------------------------

(defun granted-root (path kinds)
  "PATH when a folder setting of one of KINDS covers it, else a refusal."
  (let* ((absolute (namestring (uiop:ensure-absolute-pathname path)))
         (roots (loop for kind in kinds
                      append (let ((value (setting kind)))
                               (cond ((vectorp value) (coerce value 'list))
                                     ((listp value) value)
                                     (t (list value)))))))
    (or (find-if (lambda (root)
                   (let ((prefix (namestring (uiop:ensure-directory-pathname root))))
                     (and (<= (length prefix) (length absolute))
                          (string-equal prefix (subseq absolute 0 (length prefix))))))
                 roots)
        (fail "~a is in no granted folder; name one in the ~a setting"
              absolute (substitute #\_ #\- (string-downcase (string (first kinds))))))))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun open-file (path &key name mime-type)
  "Open a local image in a new editor tab; needs the Edit grant."
  (require-link)
  (granted-root path '(:read-roots))
  (let ((params (object "name" (or name (file-namestring path))
                        "data_base64" (cl-base64:usb8-array-to-base64-string (read-octets path)))))
    (when mime-type (setf (gethash "mime_type" params) mime-type))
    (let ((response (execute "doc.open" :params params)))
      (unless (nlk:json-value response :boolean "ok")
        (fail "the editor would not open ~a: ~a" path
              (or (nlk:json-value response :string "error") "no reason")))
      response)))

(defun export-file (path &key (format "png") layer-id overwrite)
  "Export the session document to PATH (png, psd or pfd); needs Read and Export."
  (require-link)
  (granted-root path '(:write-roots))
  (let* ((params (object "format" format))
         (artifact (progn (when layer-id (setf (gethash "layer_id" params) layer-id))
                          (let ((response (execute "export.artifact" :params params)))
                            (unless (nlk:json-value response :boolean "ok")
                              (fail "the editor would not export: ~a"
                                    (or (nlk:json-value response :string "error") "no reason")))
                            (nlk:json-value response :any "result")))))
    (let ((bytes (retrieve-artifact artifact)))
      (when (and (probe-file path) (not overwrite))
        (fail "~a exists; pass overwrite to replace it" path))
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
        (write-sequence bytes out))
      (list :path (namestring path) :bytes (length bytes)
            :artifact (nlk:json-value artifact :string "artifact_id")
            :format (nlk:json-value artifact :string "format")))))

(defun retrieve-artifact (artifact)
  "ARTIFACT's bytes, chunk by chunk, each checked against its declared length."
  (let* ((expected (or (nlk:json-value artifact :integer "byte_length") 0))
         (id (nlk:json-value artifact :string "artifact_id"))
         (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (offset 0))
    (loop
      for response = (execute "artifact.read" :params (object "artifact_id" id "offset" offset))
      do (unless (nlk:json-value response :boolean "ok")
           (fail "artifact.read failed: ~a" (or (nlk:json-value response :string "error") "no reason")))
         (let* ((chunk (nlk:json-value response :any "result"))
                (data (cl-base64:base64-string-to-usb8-array
                       (or (nlk:json-value chunk :string "data_base64") ""))))
           (loop for byte across data do (vector-push-extend byte out))
           (incf offset (length data))
           (when (or (nlk:json-value chunk :boolean "eof")
                     (and (plusp expected) (>= offset expected)))
             (return out)))
      finally (return out))))

;;; --- the tools -------------------------------------------------------------------

(defun schema (description properties &optional required)
  "One tool schema: a JSON object of PROPERTIES, REQUIRED naming what must be there."
  (let ((object (object "type" "object" "properties" (apply #'object properties))))
    (when required (setf (gethash "required" object) required))
    (list :description description :parameters object)))

(defparameter +schema-connect+
  (schema "Open PhotoFresco, a browser image editor, in a window of its own and ask the person there to allow this agent. Answers the session once allowed."
          (list "document" (object "type" "string"
                                    "description" "an editor document id, or 'active' (default) or 'none'")
                "capabilities" (object "type" "array" "items" (object "type" "string")
                                        "description" "what to ask for: read, edit, export, external-write, spend")
                "budget_credits" (object "type" "integer" "minimum" 1
                                          "description" "credits to ask for with the spend capability"))))

(defparameter +schema-ops+
  (schema "PhotoFresco's editor session: read its status, its documents or the operations it allows, and run one operation (layer.add, adjust.add, render.preview, export.artifact...)."
          (list "action" (object "type" "string"
                                  "enum" #("status" "documents" "operations" "execute" "outcome")
                                  "description" "what to read, or execute to run op")
                "op" (object "type" "string"
                              "description" "the operation's canonical name, e.g. layer.add")
                "params" (object "type" "object"
                                  "description" "the operation's own parameters")
                "request_id" (object "type" "string"
                                     "description" "1-128 characters of A-Z a-z 0-9 . _ : -")
                "expected_revision" (object "type" "integer" "minimum" 0
                                             "description" "refuse unless the document is at this revision"))
          #("action")))

(defparameter +schema-file+
  (schema "Move an image between a granted local folder and the PhotoFresco editor: open a local file in a tab, or export the document to a file (png, psd or pfd)."
          (list "action" (object "type" "string" "enum" #("open" "export"))
                "path" (object "type" "string"
                                "description" "the file, under a folder the settings grant")
                "format" (object "type" "string" "enum" #("png" "psd" "pfd"))
                "overwrite" (object "type" "boolean"
                                     "description" "replace an existing file")
                "layer_id" (object "type" "string"
                                    "description" "export one layer"))
          #("action" "path")))

(defun answer-text (result)
  "A JSON answer as the one line a tool and a command both give."
  (typecase result
    (string result)
    (hash-table (nlk:encode-json-object result))
    (t (format nil "~a" result))))

(defun tool-connect (arguments)
  (let ((result (connect
                 :document (nlk:json-value arguments :string "document")
                 :capabilities (nlk:json-value arguments :array "capabilities")
                 :budget-credits (nlk:json-value arguments :integer "budget_credits"))))
    (format nil "Connected to PhotoFresco. ~a" (answer-text result))))

(defun tool-ops (arguments)
  (let ((action (or (nlk:json-value arguments :string "action") "status")))
    (cond
      ((string-equal action "status") (answer-text (status)))
      ((string-equal action "documents") (answer-text (documents)))
      ((string-equal action "operations") (answer-text (operations)))
      ((string-equal action "outcome")
       (answer-text (call "outcome" :params (object "request_id"
                                                  (or (nlk:json-value arguments :string "request_id")
                                                      (fail "outcome needs the request_id"))))))
      ((string-equal action "execute")
       (let ((op (or (nlk:json-value arguments :string "op") (fail "execute needs an op"))))
         (answer-text (execute op
                            :params (nlk:json-value arguments :any "params")
                            :request-id (nlk:json-value arguments :string "request_id")
                            :expected-revision (nlk:json-value arguments :integer "expected_revision")))))
      (t (fail "~s is no action; use status, documents, operations, execute or outcome" action)))))

(defun tool-file (arguments)
  (let ((action (nlk:json-value arguments :string "action"))
        (path (or (nlk:json-value arguments :string "path") (fail "this needs a path"))))
    (cond
      ((string-equal action "open")
       (format nil "Opened ~a in the editor." (answer-text (open-file path))))
      ((string-equal action "export")
       (let ((written (export-file path
                                   :format (or (nlk:json-value arguments :string "format") "png")
                                   :layer-id (nlk:json-value arguments :string "layer_id")
                                   :overwrite (nlk:json-value arguments :boolean "overwrite"))))
         (format nil "Exported ~a bytes to ~a." (getf written :bytes) (getf written :path))))
      (t (fail "~s is no file action; use open or export" action)))))

(defun run-slash (args session-id)
  "One /photofresco invocation: status, documents, operations, or an op to run."
  (declare (ignore session-id))
  (let ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space))))
    (cond
      ((null words) "photofresco: status, documents, operations, connect, or an operation name to run")
      ((string-equal (first words) "connect") (format nil "~a" (connect)))
      ((string-equal (first words) "status") (answer-text (status)))
      ((string-equal (first words) "documents") (answer-text (documents)))
      ((string-equal (first words) "operations") (answer-text (operations)))
      (t (answer-text (execute (first words)))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell photofresco
  (:section ("photofresco")
    (:guide "read_roots and write_roots are the only folders the editor may touch; the browser is a Chrome, Edge or Chromium already installed")
    ("client" :string :default "Nodecode" :doc "the agent name shown in the editor's Allow prompt")
    ("editor_url" :string :default "https://photofresco.com/app/" :doc "the editor this link opens; its origin is pinned")
    ("browser" :string :default "" :doc "path to a Chrome/Edge/Chromium executable; empty finds one")
    ("wait_seconds" :integer :default 180 :min 30 :doc "how long the Allow prompt waits")
    ("read_roots" :list :doc "folders the editor may open local files from")
    ("write_roots" :list :doc "folders exports may be written to"))
  (:tool "photofresco_connect" +schema-connect+ 'tool-connect)
  (:tool "photofresco_ops" +schema-ops+ 'tool-ops)
  (:tool "photofresco_file" +schema-file+ 'tool-file)
  (:command "photofresco" 'run-slash
            :description "PhotoFresco: the editor's session and operations"
            :argument-hint "STATUS|DOCUMENTS|OPERATIONS|CONNECT|OP")
  (:help :photofresco
         "photofresco: edit images in a PhotoFresco editor window you approve - (photofresco:execute \"layer.add\" :params ...)"
         +primer+)
  (:start (lambda ()
            (setf *link* nil)
            (nle:on-stop (lambda () (setf *link* nil))))))
