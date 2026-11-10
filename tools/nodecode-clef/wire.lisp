;;;; wire.lisp --- Clef's hosted MCP server, spoken the way a client speaks it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was an MCP package: mcp.json named one streamable-HTTP
;;;; endpoint, https://clefdrills.com/mcp, and the harness offered whatever
;;;; tools the server listed. This is the same conversation one layer down --
;;;; JSON-RPC 2.0 over HTTP POST, tools/call with the catalog's own member
;;;; names -- so the cell reaches the same four tools with no Python, no MCP
;;;; client and nothing fetched at run time.
;;;;
;;;; A tool answers a JSON-RPC result carrying STRUCTUREDCONTENT, which is
;;;; what this reads; the content text is the fallback, and ISERROR makes a
;;;; refusal carrying the server's own sentence. The one seam a test stubs is
;;;; HTTP; nothing else here touches the network, a file or the environment.

(in-package #:nodecode-clef)

(defparameter +endpoint+ "https://clefdrills.com/mcp"
  "Clef's public MCP endpoint, as the plugin's mcp.json named it.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision the endpoint speaks, sent as a request header.")

(defparameter +detail-chars+ 600
  "How much of a failed answer the refusal quotes.")

(defvar *request-id* 0
  "Each request's JSON-RPC id; a reply is matched by nothing, so they only
need to differ.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun clipped (text)
  "TEXT at most +DETAIL-CHARS+ characters, with a note when it was cut."
  (let ((text (princ-to-string (or text ""))))
    (if (<= (length text) +detail-chars+)
        text
        (format nil "~a..." (subseq text 0 +detail-chars+)))))

(defun http (url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS), every status a value."
  (nlk:http :post url :headers headers :content content :timeout (or timeout 30)))

(defun json-payload (body)
  "BODY as the JSON text it carries: a server-sent event's last `data:' line
when the server answered that way, else BODY trimmed."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (princ-to-string (or body "")))))
    (if (or (uiop:string-prefix-p "data:" text)
            (search (format nil "~%data:") text))
        (let ((payload nil))
          (dolist (line (uiop:split-string text :separator '(#\Newline)))
            (when (uiop:string-prefix-p "data:" line)
              (setf payload (string-trim '(#\Space #\Tab #\Return #\Newline)
                                          (subseq line (length "data:"))))))
          (or payload ""))
        text)))

(defun object-of (text where)
  "TEXT decoded as one JSON object, or a refusal naming WHERE."
  (let ((value (ignore-errors (nlk:decode-json (json-payload text)))))
    (unless (hash-table-p value)
      (fail "~a returned no JSON object: ~a" where (clipped text)))
    value))

(defun block-text (result)
  "What an MCP answer's text blocks say, joined by lines."
  (let ((parts '()))
    (dolist (block (coerce (or (nlk:json-value result :array "content") #()) 'list))
      (let ((text (nlk:json-value block :string "text")))
        (when (and text (plusp (length text))) (push text parts))))
    (format nil "~{~a~^~%~}" (nreverse parts))))

(defun json-rpc (endpoint method params &key timeout)
  "METHOD called on ENDPOINT with PARAMS => the answer's `result' object.
A refused exchange, a non-JSON body and a JSON-RPC error member each signal
CLEF-ERROR carrying the server's own sentence."
  (let* ((request (nlk:encode-json-object
                   (nlk:json-object "jsonrpc" "2.0"
                                    "id" (incf *request-id*)
                                    "method" method
                                    "params" params)))
         (body nil)
         (status nil))
    (multiple-value-setq (body status)
      (http endpoint
            :headers (list (cons "content-type" "application/json")
                           (cons "accept" "application/json, text/event-stream")
                           (cons "mcp-protocol-version" +protocol-version+))
            :content request
            :timeout timeout))
    (when (>= status 400)
      (fail "Clef's MCP server answered HTTP ~d~@[ ~a~]"
            status (let ((text (string-trim '(#\Space #\Newline #\Return)
                                            (princ-to-string (or body "")))))
                     (and (plusp (length text)) (clipped text)))))
    (let* ((answer (object-of body "Clef's MCP server"))
           (error (nlk:json-value answer :object "error")))
      (when error
        (fail "Clef's MCP server refused ~a: ~a"
              method
              (or (nlk:json-value error :string "message") (clipped (block-text error)))))
      (or (nlk:json-value answer :object "result")
          (fail "Clef's MCP server answered ~a with no result" method)))))

(defun call-tool (endpoint name arguments &key timeout)
  "NAME called on ENDPOINT's MCP server with ARGUMENTS, a JSON object =>
what it answered: its STRUCTUREDCONTENT when the server sends one, else its
text block decoded, else NIL."
  (let* ((result (json-rpc endpoint "tools/call"
                            (nlk:json-object "name" name "arguments" arguments)
                            :timeout timeout))
         (text (block-text result)))
    (when (nlk:json-value result :boolean "isError")
      (fail "~a: ~a" name (if (plusp (length text)) text "the server refused the call")))
    (or (nlk:json-value result :object "structuredContent")
        (and (plusp (length text)) (ignore-errors (nlk:decode-json text)))
        (fail "~a answered nothing" name))))

;;; --- values an answer carries -----------------------------------------------------

(defparameter +note-names+
  #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B")
  "The twelve pitch classes, spelled with sharps, for naming a MIDI value.")

(defun midi-name (midi)
  "MIDI's name as this cell writes it -- C4 for 60 -- or NIL when MIDI is
not a MIDI value."
  (when (and (integerp midi) (<= 0 midi 127))
    (format nil "~a~d" (aref +note-names+ (mod midi 12)) (- (floor midi 12) 1))))

(defun link-line (object)
  "The browser tool an answer links when it names one, else NIL."
  (let ((link (nlk:json-value object :string "browserTool")))
    (and link (plusp (length link)) (format nil "browser tool: ~a" link))))
