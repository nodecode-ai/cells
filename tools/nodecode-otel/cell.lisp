;;;; cell.lisp --- the section, the two hook points, the three tools and /otel.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Hermes' observability plugin exports a session as OpenTelemetry spans.
;;;; Nodecode already records every fact a session produces, so this cell does
;;;; the exporting half: a tool call, a model round, a round's usage and a
;;;; failed turn each become one span, in the OpenTelemetry semantic
;;;; conventions, posted as OTLP/HTTP JSON to whichever collector the
;;;; operator names — or kept in one local file when none is named.
;;;;
;;;; Nothing is dialled until an endpoint is set, and then the only address
;;;; is that endpoint: no discovery, no self-update, no second host.
;;;;
;;;; Config, a top-level key:
;;;;   "otel": {"endpoint": "http://localhost:6006/v1/traces",
;;;;            "content_capture": "preview",
;;;;            "api_key_env": "PHOENIX_API_KEY"}

(in-package #:nodecode-otel)

(nlk:define-peripheral otel :not-running t)

;;; --- the exporter -------------------------------------------------------------

(defvar *pending* '()
  "Spans made since the last successful export, newest first, bounded.")

(defvar *last-export* nil
  "The newest attempt: (:ok COUNT) or (:failed . REASON).")

(defvar *recorded* 0
  "Spans this run recorded, kept and sent alike.")

(defvar *exported* 0
  "Spans this run's endpoints accepted.")

(defvar *post* 'post-json
  "The POST the exporter is asked with, a function of URL, BODY and HEADERS
answering true on 2xx, else (values NIL REASON); a test binds it.")

(defparameter +queue-limit+ 2000
  "How many spans may wait for one export before the oldest are dropped.")
(defparameter +attribute-limit+ 500
  "How long one mark's detail may be, so a span stays small.")

(defun endpoint ()
  "The OTLP/HTTP traces URL this cell posts to, or NIL for local only."
  (let ((url (ignore-errors (setting :endpoint))))
    (and (stringp url) (plusp (length url)) url)))

(defun expand-env (text)
  "TEXT with every ${NAME} replaced by the environment's value, an unset one empty."
  (with-output-to-string (out)
    (loop with index = 0
          while (< index (length text))
          do (let ((open (search "${" text :start2 index)))
               (cond
                 ((null open)
                  (write-string text out :start index)
                  (setf index (length text)))
                 (t
                  (write-string text out :start index :end open)
                  (let ((close (position #\} text :start (1+ open))))
                    (cond
                      (close
                       (write-string (or (uiop:getenv (subseq text (+ open 2) close)) "") out)
                       (setf index (1+ close)))
                      (t
                       (write-string text out :start open)
                       (setf index (length text)))))))))))

(defun export-headers ()
  "The request headers: JSON, the section's own lines, and the bearer token
the api_key_env names."
  (let ((headers (list (cons "content-type" "application/json"))))
    (dolist (line (ignore-errors (setting :headers)))
      (let ((colon (and (stringp line) (position #\: line))))
        (when (and colon (plusp colon))
          (push (cons (string-downcase (string-trim " " (subseq line 0 colon)))
                      (string-trim " " (expand-env (subseq line (1+ colon)))))
                headers))))
    (let* ((variable (ignore-errors (setting :api-key-env)))
           (token (and variable (uiop:getenv variable))))
      (when (and token (plusp (length token)))
        (push (cons "authorization" (format nil "Bearer ~a" token)) headers)))
    headers))

(defun post-json (url body headers)
  "POST BODY to URL with HEADERS => T on 2xx, else (values NIL REASON)."
  (handler-case
      (multiple-value-bind (answer status)
          (nlk:http :post url :content body
                            :timeout (setting :timeout-seconds)
                            :connect-timeout (setting :timeout-seconds)
                            :headers headers)
        (declare (ignore answer))
        (if (< status 300) t (values nil (format nil "HTTP ~d" status))))
    (error (condition) (values nil (princ-to-string condition)))))

(defun record-span (span)
  "Keep SPAN: in the local file when the section says so, in the export
queue when an endpoint is set. Never signals."
  (incf *recorded*)
  (when (ignore-errors (setting :local-store))
    (ignore-errors (store-span span)))
  (when (endpoint)
    (push span *pending*)
    (when (> (length *pending*) +queue-limit+)
      (setf *pending* (subseq *pending* 0 +queue-limit+))))
  span)

(defun flush-spans ()
  "POST every waiting span in one OTLP request => (values SENT REASON)."
  (let ((url (endpoint)))
    (cond
      ((null url) (values 0 "no endpoint set: spans stay in the local file"))
      ((null *pending*) (values 0 nil))
      (t
       (multiple-value-bind (sent reason)
           (funcall *post* url
                    (nlk:encode-json-object (otlp-body (reverse *pending*)))
                    (export-headers))
         (if sent
             (let ((count (length *pending*)))
               (setf *pending* nil
                     *last-export* (list :ok count))
               (incf *exported* count)
               (values count nil))
             (progn (setf *last-export* (list :failed reason))
                    (values 0 reason))))))))

(defun export-step ()
  "What the exporter thread runs: send what waits, if anything does."
  (when *pending*
    (ignore-errors (flush-spans))))

;;; --- a tool call ------------------------------------------------------------------

(defun tool-answer-text (answer)
  "ANSWER, a tool body's return, as the text the span describes."
  (typecase answer
    (string answer)
    (null "")
    (t (with-output-to-string (out) (princ answer out)))))

(defun argument-text (arguments)
  "A call's arguments as JSON text, so a preview reads like the call."
  (when arguments
    (handler-case (if (hash-table-p arguments)
                      (nlk:encode-json-object arguments)
                      (princ-to-string arguments))
      (error () nil))))

(defun on-tool-call (op next)
  "The :TOOL point: one CLIENT span per tool call, then the call's own answer."
  (let* ((started (now-ns))
         (name (or (getf op :name) "tool"))
         (turn (ignore-errors (nle:turn)))
         (arguments (clip (argument-text (getf op :arguments)))))
    (let ((answer (funcall next op)))
      (nlk:with-handlers ((error () answer))
        (let* ((text (tool-answer-text answer))
               (failed (eql 0 (search "ERROR" text))))
          (record-span
           (make-span :name (format nil "tool ~a" name)
                      :kind :client
                      :start-ns started
                      :end-ns (now-ns)
                      :trace-id (trace-id-for turn)
                      :span-id (new-span-id)
                      :status (if failed "error" "ok")
                      :attrs (attrs "gen_ai.operation.name" "execute_tool"
                                     "gen_ai.tool.name" name
                                     "nodecode.tool.call_id" (getf op :call-id)
                                     "nodecode.tool.arguments" arguments
                                     "nodecode.tool.result" (clip text)
                                     "nodecode.tool.result_chars" (length text)
                                     "nodecode.session.id" (getf turn :session-id)
                                     "nodecode.turn.id" (getf turn :turn-id))))))
      answer)))

;;; --- a turn's durable facts ---------------------------------------------------------

(defun trace-round (turn body)
  "One completed model round as a span: which model, how long to the first
token, how long the stream ran."
  (let* ((ended (now-ns))
         (stream (nlk:json-value body :integer "stream-duration-ms"))
         (ttft (nlk:json-value body :integer "ttft-ms"))
         (model (nlk:json-value body :string "model"))
         (provider (nlk:json-value body :string "provider")))
    (record-span
     (make-span :name (format nil "chat ~@[~a/~]~a" provider (or model "model"))
                :kind :client
                :start-ns (- ended (* 1000000 (or stream ttft 0)))
                :end-ns ended
                :trace-id (trace-id-for turn)
                :span-id (new-span-id)
                :attrs (attrs "gen_ai.operation.name" "chat"
                               "gen_ai.request.model" model
                               "gen_ai.provider.name" provider
                               "nodecode.provider.round" (nlk:json-value body :integer "provider-round")
                               "nodecode.ttft_ms" ttft
                               "nodecode.stream_ms" stream
                               "nodecode.transport_attempts" (nlk:json-value body :integer "transport-attempts")
                               "nodecode.session.id" (getf turn :session-id)
                               "nodecode.turn.id" (getf turn :turn-id))))))

(defun trace-usage (turn body)
  "One round's usage as a span: the token counts and the cost the turn loop
already recorded — the numbers a Usage view sums."
  (let* ((now (now-ns))
         (model (nlk:json-value body :string "model"))
         (provider (nlk:json-value body :string "provider")))
    (record-span
     (make-span :name (format nil "usage ~@[~a/~]~a" provider (or model "model"))
                :kind :internal
                :start-ns now
                :end-ns now
                :trace-id (trace-id-for turn)
                :span-id (new-span-id)
                :attrs (attrs "gen_ai.operation.name" "chat"
                               "gen_ai.request.model" model
                               "gen_ai.provider.name" provider
                               "gen_ai.usage.input_tokens" (nlk:json-value body :integer "input-tokens")
                               "gen_ai.usage.output_tokens" (nlk:json-value body :integer "output-tokens")
                               "gen_ai.usage.cached_input_tokens" (nlk:json-value body :integer "cached-input-tokens")
                               "gen_ai.response.finish_reason" (nlk:json-value body :string "finish-reason")
                               "nodecode.cost_usd" (nlk:json-value body :number "cost_usd")
                               "nodecode.session.id" (getf turn :session-id)
                               "nodecode.turn.id" (getf turn :turn-id))))))

(defun trace-failure (turn body)
  "A failed turn as one error span, carrying the reason."
  (let* ((now (now-ns))
         (detail (clip (nlk:json-value body :string "detail"))))
    (record-span
     (make-span :name "turn failed"
                :kind :internal
                :start-ns now
                :end-ns now
                :trace-id (trace-id-for turn)
                :span-id (new-span-id)
                :status "error"
                :detail detail
                :attrs (attrs "nodecode.session.id" (getf turn :session-id)
                               "nodecode.turn.id" (getf turn :turn-id))))))

(defun trace-fact (op)
  "A durable frame's fact as a span, when it is a round, its usage or a
failure. The fact rides under the frame's \"event\": its type, its payload,
and a metadata envelope carrying the turn id."
  (let* ((payload (getf op :payload))
         (frame (and (hash-table-p payload) (nlk:json-value payload :object "frame")))
         (event (and frame (nlk:json-value frame :object "event"))))
    (when event
      (let ((kind (nlk:json-value event :string "type"))
            (body (nlk:json-value event :object "payload"))
            (turn (list :session-id (getf op :session-id)
                        :turn-id (or (nlk:json-value event :string "metadata" "turn_id")
                                     (nlk:json-value frame :string "turn_id")))))
        (when (hash-table-p body)
          (cond ((equal kind "turn.provider_request") (trace-round turn body))
                ((equal kind "turn.usage") (trace-usage turn body))
                ((equal kind "turn.failed") (trace-failure turn body))))))))

(defun on-frame (op next)
  "The :FRAME point: the three durable facts above become spans, every other
frame passes untouched, and a failure here never fails the frame."
  (let ((received (funcall next op)))
    (when (getf op :durable-p)
      (nlk:with-handlers ((error () nil))
        (trace-fact op)))
    received))

;;; --- the tools -------------------------------------------------------------------

(defparameter +status-schema+
  (list :description
        "What this session's OpenTelemetry export is doing: the endpoint it posts spans to or that it keeps them locally, how much text rides along, how many spans were recorded, sent and are waiting, and how the last export went."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "otel_status takes no arguments.")

(nlk:define-contract span-query
  (:doc "Read back the spans kept on this machine, newest first: one line each with its time, name, how long it took and how it went. Filter by a word in the span name — `tool eval', `chat', `usage', `mark' — to look at one kind. This reads the local file only; the endpoint has its own view.")
  ("limit" :integer :default 20
           :doc "how many spans to read, newest first")
  ("match" :string :default ""
           :doc "a word the span name must hold; empty reads every span"))

(nlk:define-contract mark
  (:doc "Put one marker span on this session's trace, so a person reading the spans later sees where the session was at a milestone — a decision, a fix proved, a deploy. Use it once for a moment worth finding later, not every step.")
  ("name" :text :required t
          :doc "what this moment is, a few words")
  ("detail" :string :default ""
           :doc "one sentence of context: what was decided or proved"))

(defun otel-status (&optional arguments)
  "The otel_status tool: one paragraph on what is exported and where."
  (declare (ignore arguments))
  (let ((url (endpoint)))
    (format nil "otel: ~a; capture ~a; ~d spans recorded this run, ~d waiting to send, ~d sent; ~a"
            (if url
                (format nil "posting spans to ~a" url)
                "keeping spans locally (no endpoint set)")
            (capture)
            *recorded*
            (length *pending*)
            *exported*
            (cond ((null *last-export*) "no export tried yet")
                  ((eq (first *last-export*)
                       :ok)
                   (format nil "the last export carried ~d spans" (second *last-export*)))
                  (t (format nil "the last export failed: ~a" (second *last-export*)))))))

(defun spans-text (limit match)
  "The newest LIMIT spans whose name holds MATCH, as lines, or why there are none."
  (let ((rows (store-rows :limit (max 1 limit) :match match)))
    (if (null rows)
        (format nil "no spans kept yet~@[ matching ~s~]: this cell writes one per tool call, model round and mark"
                (and (plusp (length match)) match))
        (format nil "~{~a~^~%~}" (mapcar #'row-line rows)))))

(defun otel-spans (query)
  "The otel_spans tool: the local file, newest first."
  (spans-text (span-query-limit query) (span-query-match query)))

(defun otel-mark (mark)
  "The otel_mark tool: one marker span, kept and queued like any other."
  (let* ((turn (ignore-errors (nle:turn)))
         (now (now-ns))
         (name (mark-name mark))
         (detail (mark-detail mark))
         (detail (if (> (length detail) +attribute-limit+)
                     (subseq detail 0 +attribute-limit+)
                     detail)))
    (record-span (make-span :name (format nil "mark ~a" name)
                            :kind :internal
                            :start-ns now
                            :end-ns now
                            :trace-id (trace-id-for turn)
                            :span-id (new-span-id)
                            :attrs (attrs "nodecode.mark.detail" (clip detail)
                                           "nodecode.session.id" (getf turn :session-id)
                                           "nodecode.turn.id" (getf turn :turn-id))))
    (format nil "marked ~s: one span, ~a"
            name
            (if (endpoint) "kept locally and queued for the endpoint" "kept in the local file"))))

;;; --- /otel ------------------------------------------------------------------------

(defun send-text ()
  "The /otel send answer: what went, and where that leaves the cell."
  (multiple-value-bind (sent reason) (flush-spans)
    (format nil "~a~@[ (~a)~]. ~a"
            (if (plusp sent) (format nil "sent ~d spans" sent) "nothing to send")
            reason
            (otel-status))))

(defun run-otel (args session-id)
  "One /otel invocation => the answer's text, and typed alone the dialog it
answers then: the newest spans, sending now, clearing the local file."
  (declare (ignore session-id))
  (let ((args (nlk:trimmed (or args ""))))
    (cond
      ((or (string= args "") (string= args "status"))
       (values (otel-status)
               (nle:list-dialog
                :title "OpenTelemetry"
                :summary (one-line (otel-status) 120)
                :rows (list (nle:ask-row "Recent spans" "How many" "/otel spans"
                                          :placeholder "20" :verb "show")
                            (nle:ask-row "Send now" "Press Enter" "/otel send"
                                          :verb "send")))))
      ((uiop:string-prefix-p "spans" args)
       (values (spans-text (or (ignore-errors
                                 (parse-integer (subseq args (length "spans")) :junk-allowed t))
                               20)
                            "")
               nil))
      ((string= args "send") (values (send-text) nil))
      ((string= args "clear")
       (values (format nil "cleared ~d spans from the local file" (clear-store)) nil))
      (t (values (format nil "otel: ~s is not a verb; try /otel, /otel spans 20, /otel send or /otel clear"
                         args)
                 nil)))))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell otel
  (:section ("otel")
    (:guide "no endpoint keeps every span in a local file; an OTLP/HTTP endpoint posts them to a collector you run; content_capture says how much of a tool argument, a result or a turn's failure rides along")
    ("endpoint" :string :default ""
     :doc "OTLP/HTTP traces URL, e.g. http://localhost:6006/v1/traces; empty keeps spans on this machine")
    ("headers" :list
     :doc "extra request headers, each \"Name: value\"; ${VAR} reads the environment")
    ("api_key_env" :env
     :doc "environment variable holding a bearer token for the endpoint")
    ("service_name" :string :default "nodecode"
     :doc "the service.name every span carries")
    ("content_capture" :choice :options '("full" "preview" "off") :default "preview"
     :doc "how much text rides along: everything, a preview, or none")
    ("preview_chars" :integer :default 1200 :min 0
     :doc "how many characters one preview keeps")
    ("local_store" :boolean :default t
     :doc "keep spans in a local file, whether or not an endpoint is set")
    ("max_spans" :integer :default 500 :min 1
     :doc "how many spans the local file keeps")
    ("flush_seconds" :integer :default 5 :min 1
     :doc "how often waiting spans go out")
    ("timeout_seconds" :integer :default 10 :min 1
     :doc "one export's ceiling"))
  ;; Two things the declaration cannot say: an endpoint that is not a web
  ;; address, and a section that names nowhere for a span to go. Both refuse
  ;; at start, so the operator reads one sentence instead of a silent drop.
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((url (getf values :endpoint)))
                 (when (and url (plusp (length url))
                            (not (or (uiop:string-prefix-p "http://" url)
                                     (uiop:string-prefix-p "https://" url))))
                   (nlk:config-error
                    "otel.endpoint is ~s: an OTLP/HTTP endpoint is a web address, http:// or https://"
                    url))
                 (when (and (not (getf values :local-store))
                            (or (null url) (zerop (length url))))
                   (nlk:config-error
                    "otel.endpoint is empty and otel.local_store is false: a span would go nowhere; set one of them"))
                 values)))
  (:tool "otel_status" +status-schema+ 'otel-status)
  (:tool "otel_spans" (nlk:contract 'span-query) 'otel-spans)
  (:tool "otel_mark" (nlk:contract 'mark) 'otel-mark)
  (:command "otel" 'run-otel
            :description "OpenTelemetry: what was traced, and where it went"
            :argument-hint "[status|spans N|send|clear]")
  (:hook :tool #'on-tool-call)
  (:hook :frame #'on-frame)
  (:start (lambda ()
            (setf *pending* nil
                  *last-export* nil
                  *recorded* 0
                  *exported* 0
                  *stored* (length (or (ignore-errors (store-lines)) '())))
            (when (endpoint)
              (let ((worker (nlk:worker-start "otel-export" #'export-step
                                              :wake (setting :flush-seconds))))
                (nle:on-stop (lambda () (nlk:worker-stop worker))))))))
