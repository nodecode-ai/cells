;;;; support.lisp --- the FXMacroData test slice: a stubbed service, no network.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the same nodecode.test registry every cell's do
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; FXMACRODATA-CELL- name prefix; RUN-FXMACRODATA-TESTS runs exactly that
;;;; slice.
;;;;
;;;; The one seam that reaches outside is NLK:HTTP, the only way this cell
;;;; touches the network. WITH-API replaces it with a small stand-in for both
;;;; wires -- the REST routes these tools call, and the three-request MCP
;;;; handshake -- plus a record of every exchange, so a test asserts on the
;;;; request and nothing dials.

(in-package #:nodecode.test)

(define-test-slice "fxmacrodata" "FXMACRODATA-CELL-"
    :start nodecode-fxmacrodata:start-cell)

;;; --- the canned service ---------------------------------------------------------------

(defparameter +endpoint+ "https://api.test/fxm"
  "The REST origin a test's section names.")

(defparameter +mcp+ "https://mcp.test/mcp"
  "The MCP endpoint a test's section names.")

(defparameter +key-env+ "NODECODE_FXMACRODATA_TEST_KEY"
  "A variable nothing sets: the key in these tests is the one the section carries.")

(defparameter +no-key-env+ "NODECODE_FXMACRODATA_TEST_ABSENT"
  "The variable a test names when no key must be found anywhere.")

(defvar *calls* '()
  "Every (METHOD URL ARGUMENTS) the stub took, newest first.")

(defvar *answers* '()
  "((MARKER . FUNCTION) ...): FUNCTION answers (VALUES TEXT STATUS HEADERS) for the
newest request whose URL or body holds MARKER.")

(defun script (marker function)
  "Answer every request whose URL or content holds MARKER with FUNCTION for this test."
  (push (cons marker function) *answers*))

(defun response-headers (&optional (session nil))
  "What the service answers with, as an alist."
  (append (list (cons "Content-Type" "application/json"))
          (when session (list (cons "Mcp-Session-Id" session)))))

(defun json-body (object)
  (nlk:encode-json-object object))

(defun path-of (url)
  "URL without the endpoint and without its query."
  (let* ((path (if (uiop:string-prefix-p +endpoint+ url)
                   (subseq url (length +endpoint+))
                   url))
         (query (position #\? path)))
    (if query (subseq path 0 query) path)))

(defun query-of (url)
  "URL's query string as an alist of (NAME . VALUE), as the service reads it."
  (let ((query (position #\? url)))
    (when query
      (loop for pair in (uiop:split-string (subseq url (1+ query)) :separator "&")
            for equals = (position #\= pair)
            when equals
              collect (cons (subseq pair 0 equals) (subseq pair (1+ equals)))))))

(defun rpc-result (&optional (content "ok"))
  "One MCP tools/call result, with the structured content the original kept."
  (nlk:json-object
   "jsonrpc" "2.0" "id" 2
   "result" (nlk:json-object
             "content" (vector (nlk:json-object "type" "text" "text" content))
             "structuredContent" (nlk:json-object "data" (vector (nlk:json-object "ok" t)))
             "isError" nil)))

(defun mcp-answer (content)
  "The three answers one MCP tool call takes: initialize, the notification, the call."
  (let ((body (getf content :content)))
    (cond ((and body (search "notifications/initialized" body))
           (values "" 202 (response-headers "sess-1")))
          ((and body (search "initialize" body))
           (values (json-body (nlk:json-object "jsonrpc" "2.0" "id" 1
                                                "result" (nlk:json-object
                                                          "protocolVersion" "2025-03-26"
                                                          "capabilities" (nlk:json-object))))
                   200 (response-headers "sess-1")))
          (t (values (json-body (rpc-result)) 200 (response-headers "sess-1"))))))

(defun history-page (url)
  "One page of indicator history, paged the way the service pages it."
  (let* ((offset (or (cdr (assoc "offset" (query-of url) :test #'string=)) "0"))
         (rows (if (string= offset "0")
                   (vector (nlk:json-object "indicator" "policy_rate" "value" 4.5)
                           (nlk:json-object "indicator" "policy_rate" "value" 4.25))
                   (vector (nlk:json-object "indicator" "policy_rate" "value" 4.0))))
         (more (string= offset "0")))
    (json-body (nlk:json-object
                "data" rows
                "dataset_version" "v1"
                "pagination" (nlk:json-object "has_more" more
                                               "returned_count" (length rows)
                                               "offset" (parse-integer offset)
                                               "next_offset" (if more (+ 2 (parse-integer offset)) nil))
                "notices" (vector)))))

(defun answer-for (method url content)
  "What the stand-in service says to one exchange."
  (cond
    ((and (eq method :post) (search "mcp" url))
     (mcp-answer (list :content content)))
    ((search "/v1/data_catalogue/" url)
     (json-body (nlk:json-object "data" (nlk:json-object
                                          "currency" "USD"
                                          "indicators" (vector (nlk:json-object "slug" "policy_rate"))))))
    ((search "/v1/announcements/" url)
     (history-page url))
    ((search "/v1/calendar/" url)
     (json-body (nlk:json-object
                 "data" (vector (nlk:json-object "indicator" "cpi" "release_date" "2026-01-15"
                                                  "source_url" "https://publisher.test/cpi"))
                 "freemium_window" (nlk:json-object "message" "The free tier shows the last 12 months."))))
    ((search "/v1/forex/" url)
     (json-body (nlk:json-object "data" (nlk:json-object "base" "USD" "quote" "EUR" "rate" 0.92))))
    ((search "/v1/research/panel" url)
     (json-body (nlk:json-object "data" (vector (nlk:json-object "panel" t)))))
    (t (json-body (nlk:json-object "data" (nlk:json-object "ok" t))))))

(defun stub-http (method url &rest arguments)
  "The stubbed NLK:HTTP: the stand-in service, and a record of every exchange."
  (let ((content (getf arguments :content)))
    (push (list method url arguments) *calls*)
    (let ((scripted (find-if (lambda (entry)
                               (or (search (car entry) url)
                                   (and (stringp content) (search (car entry) content))))
                             *answers*)))
      (if scripted
          (funcall (cdr scripted) method url content)
          (multiple-value-bind (text status headers) (answer-for method url content)
            (if status
                (values text status headers)
                (values text 200 (response-headers))))))))

(defmacro with-api (&body body)
  "Run BODY with NLK:HTTP stubbed and the stub's own state cleared."
  `(with-saved-globals (*calls* *answers*)
     (setf *calls* '() *answers* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'stub-http method url arguments))
       ,@body)))

(defmacro with-fxm ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as the `fxmacrodata' section, both
wires stubbed, and stopped on unwind."
  `(with-api
     (with-cell-stop ((fxmacrodata-start
                       "base_url" +endpoint+ "mcp_url" +mcp+ ,@pairs))
       ,@body)))

;;; --- asking the cell what a model would ---------------------------------------------

(defun call (name &optional (arguments "{}"))
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun newest-call ()
  (first *calls*))

(defun newest-url ()
  (second (newest-call)))

(defun newest-headers ()
  (getf (third (newest-call)) :headers))

(defun header (name &optional (headers (newest-headers)))
  (cond ((hash-table-p headers)
         (or (gethash name headers)
             (loop for key being the hash-keys of headers
                   when (string-equal key name) return (gethash key headers))))
        (t (cdr (assoc name headers :test #'string-equal)))))

(defun exchanges-to (fragment)
  "Every exchange whose URL holds FRAGMENT, newest first."
  (remove-if-not (lambda (exchange) (search fragment (second exchange))) *calls*))

;;; --- the arguments a tool's schema asks for -------------------------------------------

(defun sample-value (name property)
  "One value PROPERTY accepts, for the required member NAME: a test calls every tool."
  (let ((enum (and (hash-table-p property) (gethash "enum" property)))
        (type (and (hash-table-p property) (gethash "type" property))))
    (cond ((and (vectorp enum) (plusp (length enum))) (aref enum 0))
          ((and (stringp name) (search "date" name)) "2026-01-02")
          ((equal type "integer") 1)
          ((equal type "number") 1)
          ((equal type "boolean") t)
          ((equal type "array") (vector))
          ((equal type "object") (make-hash-table :test #'equal))
          (t "USD"))))

(defun minimal-arguments (name)
  "The smallest argument object NAME's schema accepts: one value per required member."
  (let* ((schema (getf (nodecode-fxmacrodata::tool-schema name) :parameters))
         (required (and (hash-table-p schema) (gethash "required" schema)))
         (properties (and (hash-table-p schema) (gethash "properties" schema)))
         (object (make-hash-table :test #'equal)))
    (when (vectorp required)
      (loop for member across required
            do (setf (gethash member object)
                     (sample-value member (and (hash-table-p properties)
                                               (gethash member properties))))))
    (nlk:encode-json-object object)))
