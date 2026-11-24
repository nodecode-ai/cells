;;;; support.lisp --- the test slice: a stubbed Qrblox MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; QRBLOX-CELL- name prefix; RUN-QRBLOX-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-QRBLOX::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a local JSON-RPC
;;;; server written here: it answers initialize, tools/list and tools/call from
;;;; the request's own method, records every call, and never dials.

(in-package #:nodecode.test)

(define-test-slice "qrblox" "QRBLOX-CELL-" :start nodecode-qrblox:start-cell)

;;; --- the canned server ------------------------------------------------------------

(defparameter +key+ "sk-qrblox-test-key"
  "The key a test's section carries.")

(defparameter +session+ "sess-qrblox-1"
  "The session id the stub hands at initialize.")

(defparameter +server-tools+
  (list (cons "qrblox.ping" "Confirm reachability and identity")
        (cons "qrblox.merchant.get_profile" "The merchant workspace attached to the key")
        (cons "qrblox.qr.list" "List recent QR codes")
        (cons "qrblox.qr.create" "Create a draft QR code")
        (cons "qrblox.audio.create" "Draft an audio code")
        (cons "qrblox.menu.create" "Build a menu")
        (cons "qrblox.menu.list" "List menus")
        (cons "qrblox.menu.add_item" "Extend a menu")
        (cons "qrblox.challenge.create" "Scan streaks and loyalty campaigns")
        (cons "qrblox.analytics.scan_summary" "Total scans and the by-day series"))
  "The tool list the stub's tools/list answers: the server's own names.")

;;; --- the stub ---------------------------------------------------------------------

(defvar *calls* nil
  "Every request the stub answered, newest first: (:url :headers :body :method).")

(defvar *answer* nil
  "A function of one call answering (values TEXT STATUS HEADERS), or NIL for the
stub's own JSON-RPC server.")

(defun envelope (id result)
  "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key errorp)
  "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                   "isError" (and errorp t)))

(defun call-result (body)
  "What the stub's tools/call answers: the tool's name and arguments back."
  (let ((tool (nlk:json-value (nlk:json-value body :object "params") :string "name"))
        (arguments (nlk:json-value (nlk:json-value body :object "params") :object "arguments")))
    (text-result (format nil "~a answered ~a" tool
                         (if arguments (nlk:encode-json-object arguments) "{}")))))

(defun tools-result ()
  "What the stub's tools/list answers: the server's own names and descriptions."
  (nlk:json-object
   "tools"
   (coerce (loop for (name . description) in +server-tools+
                  collect (nlk:json-object "name" name "description" description))
           'vector)))

(defun session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) +session+)
    headers))

(defun stub-http (method url &key headers content timeout connect-timeout)
  (declare (ignore timeout connect-timeout))
  (let* ((body (and (stringp content) (ignore-errors (nlk:decode-json content))))
         (call (list :method method :url url :headers headers :body body)))
    (push call *calls*)
    (if *answer*
      (funcall *answer* call)
      (let ((name (nlk:json-value body :text "method"))
            (id (nlk:json-value body :integer "id")))
        (cond
          ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 (session-headers) nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/list")
           (values (envelope id (tools-result)) 200 nil nil))
          ((equal name "tools/call")
           (values (envelope id (call-result body)) 200 nil nil))
          (t (values "{}" 404 nil nil)))))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-qrblox::http
                                (method url &key headers content timeout connect-timeout)
                                (stub-http method url :headers headers :content content
                                                 :timeout timeout :connect-timeout connect-timeout))
       ,@body)))

;;; --- starting it, calling it ---------------------------------------------------------

(defun qrblox-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'qrblox-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-qrblox ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((qrblox-run-start ,@pairs))
       ,@body)))

(defun qrblox-called (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (princ-to-string (nle::message-content (execute-wire-call "qrblox-call-1" name arguments))))

;;; --- reading the request a call made --------------------------------------------------

(defun qrblox-tool-call (&optional (index 0))
  "The INDEX'th newest tools/call the cell sent, as (:METHOD :URL :HEADERS :BODY)."
  (nth index (remove-if-not (lambda (call)
                               (equal "tools/call" (nlk:json-value (getf call :body) :text "method")))
                             *calls*)))

(defun qrblox-sent-tool (&optional (index 0))
  "The server tool name the INDEX'th newest tools/call named."
  (let ((call (qrblox-tool-call index)))
    (and call (nlk:json-value (nlk:json-value (getf call :body) :object "params")
                               :string "name"))))

(defun qrblox-sent-arguments (&optional (index 0))
  "The arguments object of the INDEX'th newest tools/call."
  (let ((call (qrblox-tool-call index)))
    (and call (nlk:json-value (nlk:json-value (getf call :body) :object "params")
                               :object "arguments"))))

(defun qrblox-request-header (call name)
  "CALL's header NAME, case-insensitively."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun qrblox-method-call (method &optional (index 0))
  "The INDEX'th newest request of JSON-RPC METHOD, or NIL."
  (nth index (remove-if-not (lambda (call)
                               (equal method (nlk:json-value (getf call :body) :text "method")))
                             *calls*)))
