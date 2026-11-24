;;;; mcp.lisp --- Meme Studio's endpoint, one tools/call, and the answer it gives.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Meme Studio is a hosted MCP server: one Streamable HTTP endpoint that
;;;; answers JSON-RPC. Everything the cell knows about it lives here: the one
;;;; POST, the envelope, the content items and the picture one of them
;;;; carries. CELL.LISP is the only caller.
;;;;
;;;; The peer is the endpoint the section names and nothing else. The joke,
;;;; the captions and any picture the person gave go there (that is what a
;;;; meme is made of); nothing else is read, written or spawned, and no key is
;;;; asked. The meme itself is not stored on the server: the edit_url the
;;;; answer carries holds it.

(in-package #:nodecode-meme-studio)

;;; --- the endpoint, and the one call -----------------------------------------

(defparameter +user-agent+
  "nodecode-meme-studio/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "Who the server sees asking: the cell and where it comes from, and nothing
about the operator.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision the call names. The server takes tools/call without a
handshake and answers this one; the header says which the cell speaks.")

(defparameter +image-types+
  '(("image/png" . "png") ("image/jpeg" . "jpg") ("image/gif" . "gif")
    ("image/webp" . "webp"))
  "The picture types a tool answer may carry, and the file each is written as.")

(defun endpoint ()
  "The MCP endpoint the section names."
  (setting :endpoint))

(defun answer-limit ()
  "The most one answer may carry: a final GIF in base64, and the text beside
it. The server caps a picture it accepts at max_image_bytes, and base64 spends
four bytes on three."
  (+ (* 2 (setting :max-image-bytes)) (* 64 1024)))

(defun post (request)
  "The cell's one network call: REQUEST, a JSON-RPC object's text, POSTed to
the endpoint. Answers (values BODY-TEXT STATUS). Tests stub this name and
never touch the network."
  (multiple-value-bind (body status)
      (nlk:http :post (endpoint)
                :headers `(("content-type" . "application/json")
                           ("accept" . "application/json, text/event-stream")
                           ("mcp-protocol-version" . ,+protocol-version+)
                           ("user-agent" . ,+user-agent+))
                :content request
                :timeout (setting :timeout-seconds)
                :max-bytes (answer-limit))
    (values (if (stringp body) body (nlk:decode-octets body)) status)))
;;; --- what comes back --------------------------------------------------------

(defun first-line (text)
  "TEXT's first line, cut to something a refusal can carry."
  (let* ((end (position #\Newline text))
         (line (subseq text 0 (or end (length text)))))
    (subseq line 0 (min 200 (length line)))))

(defun sse-data (text)
  "TEXT when it is a JSON body, else the last event's data in the server-sent
event stream it is. A Streamable HTTP server may answer either, so both come
back as the one body to decode."
  (let ((start (position-if-not (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return))) text)))
    (if (and start (char= #\{ (char text start)))
        text
        (let ((events '()) (lines '()))
          (flet ((flush ()
                   (when lines
                     (push (format nil "~{~a~^~%~}" (nreverse lines)) events)
                     (setf lines '()))))
            (dolist (line (uiop:split-string text :separator '(#\Newline)))
              (cond ((zerop (length line)) (flush))
                    ((and (>= (length line) 5) (string= "data:" line :end2 5))
                     (push (string-left-trim " " (subseq line 5)) lines))))
            (flush))
          (or (first events) text)))))

(defun rpc (method params)
  "One JSON-RPC request to the endpoint => its RESULT object. A transport
error, a refused status and an error envelope all FAIL here, with what the
server said."
  (let ((request (nlk:make-json-object "jsonrpc" "2.0" "id" 1 "method" method)))
    (when params
      (setf (gethash "params" request) params))
    (multiple-value-bind (body status) (post (nlk:encode-json-object request))
      (unless (eql status 200)
        (fail "Meme Studio answered HTTP ~a~@[: ~a~]"
              status (and (stringp body) (plusp (length body)) (first-line body))))
      (let ((envelope (ignore-errors (nlk:decode-json (sse-data (or body ""))))))
        (unless (hash-table-p envelope)
          (fail "Meme Studio answered something that is not JSON"))
        (let ((error (gethash "error" envelope)))
          (when error
            (fail "Meme Studio refused ~a: ~a" method
                  (or (and (hash-table-p error) (nlk:json-value error :string "message"))
                      (princ-to-string error)))))
        (let ((result (gethash "result" envelope)))
          (unless (hash-table-p result)
            (fail "Meme Studio answered no result for ~a" method))
          result)))))

(defun call (tool arguments)
  "TOOL's result from the server: ARGUMENTS, the table the model sent."
  (rpc "tools/call" (nlk:make-json-object
                      "name" tool
                      "arguments" (or arguments (nlk:make-json-object)))))

;;; --- the answer, as the model reads it ----------------------------------------

(defun save-picture (data mime)
  "(values PATH BYTES) of one image item's DATA written under output_dir,
never over another file there."
  (let* ((octets (or (ignore-errors (cl-base64:base64-string-to-usb8-array data))
                     (fail "Meme Studio sent a picture that is not base64")))
         (type (or (cdr (assoc mime +image-types+ :test #'string-equal)) "png"))
         (directory (setting :output-dir))
         (file (loop for attempt from 0
                     for candidate = (merge-pathnames
                                      (format nil "meme-~d~@[-~d~].~a" (get-universal-time)
                                              (and (plusp attempt) attempt) type)
                                      directory)
                     unless (probe-file candidate)
                       return candidate)))
    (ensure-directories-exist file)
    (alexandria:write-byte-vector-into-file octets file :if-exists :supersede)
    (values (namestring file) (length octets))))

(defun answer-text (result)
  "The text one tool's RESULT is to the model: its text items in order, each
picture saved under output_dir and named on a line of its own, and the server's
own error flag in front. Nothing is retyped, so an edit_url or a caption the
server returned comes back exactly as it sent it."
  (let ((blocks '()))
    (loop for item across (nlk:json-array result "content")
          for type = (nlk:json-value item :string "type")
          do (cond
               ((equal type "text")
                (let ((text (nlk:json-value item :string "text")))
                  (when text (push text blocks))))
               ((equal type "image")
                (let ((data (nlk:json-value item :string "data"))
                      (mime (nlk:json-value item :string "mimeType")))
                  (when data
                    (multiple-value-bind (path bytes) (save-picture data mime)
                      (push (format nil "Picture: ~a (~a, ~d bytes). Show it with ![meme](~a), or open it."
                                    path (or mime "image") bytes path)
                            blocks)))))))
    (let ((text (format nil "~{~a~^~%~}" (nreverse blocks))))
      (cond ((zerop (length text)) "ERROR: Meme Studio answered nothing")
            ((nlk:json-value result :boolean "isError") (format nil "ERROR: ~a" text))
            (t text)))))
