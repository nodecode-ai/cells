;;;; relay.lisp --- the loopback chat-completions endpoint a round is sent to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nodecode's chat lane builds the round's body as it does for any
;;;; OpenAI-compatible provider, and the catalog row this cell publishes
;;;; points its base at a listener on 127.0.0.1. The relay takes that POST,
;;;; turns it into one agy prompt (agy.lisp), and streams the answer back as
;;;; SSE chunks the chat lane's own fold reads: text, tool_calls, finish,
;;;; usage. The model list is served at the same port, so the core's live
;;;; /models listing sees what agy serves.

(in-package #:nodecode-antigravity-agy)

(defparameter +head-limit+ (* 64 1024)
  "The most bytes a request line and its headers may take.")

(defparameter +body-limit+ (* 64 1024 1024)
  "The most bytes a round's body may take.")

(defparameter +read-seconds+ 60
  "How long one connection may take to deliver its request.")

(defstruct (relay (:copier nil))
  (listener nil)
  (port 0 :type integer)
  (thread nil)
  (stopping nil))

(defvar *relay* nil
  "The running listener, or NIL while the cell is stopped.")

(defun relay-base ()
  "The base a round is sent to: the relay's address, which the catalog row
publishes as the provider's api."
  (if *relay*
      (format nil "http://127.0.0.1:~d" (relay-port *relay*))
      "http://127.0.0.1:1"))

(defun start-relay ()
  "Listen on a free loopback port and serve it."
  (unless *relay*
    (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :backlog 16
                                                         :element-type '(unsigned-byte 8)))
           (relay (make-relay :listener listener :port (usocket:get-local-port listener))))
      (setf (relay-thread relay)
            (bt2:make-thread (lambda () (relay-loop relay)) :name "antigravity-agy relay"))
      (setf *relay* relay)
      relay)))

(defun stop-relay ()
  "Stop serving and listening; every later round finds no relay."
  (let ((relay *relay*))
    (when relay
      (setf *relay* nil
            (relay-stopping relay) t)
      (ignore-errors (usocket:socket-close (relay-listener relay)))
      (ignore-errors (bt2:join-thread (relay-thread relay))))))

(defun relay-loop (relay)
  "Accept connections until the relay is stopped, one thread each, so a round
that waits on agy never blocks a listing."
  (handler-case
      (loop until (relay-stopping relay)
            do (when (usocket:wait-for-input (relay-listener relay) :timeout 0.1 :ready-only t)
                 (let ((connection (usocket:socket-accept (relay-listener relay)
                                                          :element-type '(unsigned-byte 8))))
                   (bt2:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (sb-sys:with-deadline (:seconds (* 10 +read-seconds+))
                                 (handle-connection (usocket:socket-stream connection)))
                             (error () nil))
                        (ignore-errors (usocket:socket-close connection))))
                    :name "antigravity-agy round"))))
    (error () nil)))

;;; --- HTTP ---------------------------------------------------------------------

(defun read-head (stream)
  "The request line and headers STREAM delivers, as text, or NIL at EOF."
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (unless byte (return-from read-head nil))
             (vector-push-extend byte buffer)
             (when (> (fill-pointer buffer) +head-limit+)
               (error "request head over ~d bytes" +head-limit+))
          until (let ((end (fill-pointer buffer)))
                  (and (>= end 4)
                       (= 13 (aref buffer (- end 4))) (= 10 (aref buffer (- end 3)))
                       (= 13 (aref buffer (- end 2))) (= 10 (aref buffer (- end 1))))))
    (sb-ext:octets-to-string buffer :external-format :latin-1)))

(defun parse-head (text)
  "(values METHOD TARGET HEADERS) of the request head TEXT."
  (let* ((lines (remove "" (ppcre:split "\\r\\n" text) :test #'string=))
         (request (uiop:split-string (first lines) :separator " ")))
    (values (first request) (second request)
            (loop for line in (rest lines)
                  for colon = (position #\: line)
                  when colon
                    collect (cons (subseq line 0 colon)
                                  (string-trim '(#\Space #\Tab) (subseq line (1+ colon))))))))

(defun header (headers name)
  "The value of header NAME in HEADERS, any case."
  (cdr (assoc name headers :test #'string-equal)))

(defun read-body (stream headers)
  "The octets HEADERS' Content-Length announces."
  (let ((length (ignore-errors (parse-integer (or (header headers "content-length") "")))))
    (unless (and length (<= 0 length +body-limit+))
      (error "a request without a usable Content-Length"))
    (let ((body (make-array length :element-type '(unsigned-byte 8))))
      (unless (= length (read-sequence body stream))
        (error "a request ended before its ~d bytes" length))
      body)))

(defun respond (stream status reason body &key (content-type "application/json"))
  "Answer STATUS with BODY and close the exchange."
  (let ((octets (sb-ext:string-to-octets body :external-format :utf-8)))
    (write-sequence
     (sb-ext:string-to-octets
      (format nil "HTTP/1.1 ~d ~a~c~cContent-Type: ~a~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c"
              status reason #\Return #\Linefeed content-type #\Return #\Linefeed (length octets)
              #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
      :external-format :latin-1)
     stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun handle-connection (stream)
  "Read one request: a round's POST, the model listing, or nothing."
  (multiple-value-bind (method target headers) (parse-head (or (read-head stream) (return-from handle-connection)))
    (cond
      ((and (equal method "POST") (uiop:string-prefix-p "/chat/completions" target))
       (handle-round stream (read-body stream headers)))
      ((and (equal method "GET") (uiop:string-prefix-p "/models" target))
       (respond stream 200 "OK" (models-body)))
      ((member method '("GET" "HEAD") :test #'equal)
       (respond stream 200 "OK" "{}"))
      (t (respond stream 404 "Not Found" "{\"error\":{\"message\":\"no such path\"}}")))))

;;; --- the model listing ---------------------------------------------------------

(defun models-body ()
  "The models the section offers, in the listing shape the core reads."
  (with-output-to-string (out)
    (write-string "{\"object\":\"list\",\"data\":[" out)
    (loop for id in (model-ids)
          for first = t then nil
          do (unless first (write-char #\, out))
             (format out "{\"id\":~a,\"object\":\"model\",\"owned_by\":\"antigravity\"}" (json-string id)))
    (write-string "]}" out)))

;;; --- the round ------------------------------------------------------------------

(defun agy-round (request)
  "One round: BODY as the chat request it is, (values CONTENT CALLS USAGE)."
  (let* ((model (or (nlk:json-value request :string "model") (first (model-ids))))
         (messages (or (nlk:json-value request :array "messages") #()))
         (tools (nlk:json-value request :array "tools"))
         (prompt (concatenate 'string (conversation-text messages)
                              (string #\Newline)
                              "Answer as the assistant, in the style the system prompt asks for.")))
    (multiple-value-bind (text usage)
        (run-agy model (system-prompt messages tools) prompt
                 :timeout (or (ignore-errors (setting :timeout-seconds)) 300))
      (multiple-value-bind (content calls) (split-calls text)
        (values content calls usage)))))

(defun sse-write (stream payload)
  "One SSE event, flushed."
  (write-sequence
   (sb-ext:string-to-octets (format nil "data: ~a~c~c~c~c" payload #\Return #\Linefeed #\Return #\Linefeed)
                             :external-format :utf-8)
   stream)
  (finish-output stream))

(defun chunk-json (model delta finish)
  "A chat-completion chunk carrying DELTA and FINISH."
  (format nil "{\"id\":\"chatcmpl-agy\",\"object\":\"chat.completion.chunk\",\"created\":~d,\"model\":~a,\"choices\":[{\"index\":0,\"delta\":~a,\"finish_reason\":~a}]}"
          (get-universal-time) (json-string model) delta (if finish (json-string finish) "null")))

(defun usage-json (usage)
  "The terminal usage chunk, in the chat wire's shape."
  (let* ((prompt (getf usage :prompt 0))
         (completion (getf usage :completion 0)))
    (format nil "{\"id\":\"chatcmpl-agy\",\"object\":\"chat.completion.chunk\",\"choices\":[],\"usage\":{\"prompt_tokens\":~d,\"completion_tokens\":~d,\"total_tokens\":~d,\"prompt_tokens_details\":{\"cached_tokens\":~d},\"completion_tokens_details\":{\"reasoning_tokens\":~d}}}"
            prompt completion (+ prompt completion) (getf usage :cached 0) (getf usage :reasoning 0))))

(defun tool-delta (calls)
  "CALLS as one delta's tool_calls array."
  (with-output-to-string (out)
    (write-string "{\"tool_calls\":[" out)
    (loop for (name . arguments) in calls
          for index from 0
          do (unless (zerop index) (write-char #\, out))
             (format out "{\"index\":~d,\"id\":\"call_agy_~d_~d\",\"type\":\"function\",\"function\":{\"name\":~a,\"arguments\":~a}}"
                     index (get-universal-time) index (json-string name) (json-string arguments)))
    (write-string "]}" out)))

(defun stream-round (stream text)
  "Run the round for BODY and stream it back as SSE chunks."
  (let ((model nil))
    (handler-case
        (multiple-value-bind (content calls usage)
            (progn (setf model (or (nlk:json-value (nlk:decode-json text) :string "model") "agy"))
                   (agy-round (nlk:decode-json text)))
          (write-sequence (sb-ext:string-to-octets "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: close\r\n\r\n" :external-format :latin-1) stream)
          (finish-output stream)
          (when (and content (plusp (length content)))
            (sse-write stream (chunk-json model (format nil "{\"content\":~a}" (json-string content)) nil)))
          (if calls
              (sse-write stream (chunk-json model (tool-delta calls) "tool_calls"))
              (sse-write stream (chunk-json model "{}" "stop")))
          (sse-write stream (usage-json usage))
          (sse-write stream "[DONE]"))
      (agy-error (condition)
        (respond stream 502 "Bad Gateway"
                 (format nil "{\"error\":{\"message\":~a,\"type\":\"agy_error\",\"code\":502}}"
                         (json-string (agy-message condition)))))
      (error (condition)
        (respond stream 502 "Bad Gateway"
                 (format nil "{\"error\":{\"message\":~a,\"type\":\"agy_error\",\"code\":502}}"
                         (json-string (princ-to-string condition))))))))

(defun handle-round (stream octets)
  "Answer one chat-completions POST off OCTETS."
  (stream-round stream (sb-ext:octets-to-string octets :external-format :utf-8)))
