;;;; span.lisp --- one span, the local file it lands in, and its OTLP shape.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A span is a plain struct: the name, the times, the ids that join it to
;;;; its turn's other spans, and an alist of attributes in the OpenTelemetry
;;;; semantic conventions (`gen_ai.tool.name', `gen_ai.request.model',
;;;; `gen_ai.usage.input_tokens') so a Phoenix, Jaeger or Langfuse view reads
;;;; the model columns it expects instead of a private dialect.
;;;;
;;;; What is captured is read off the section at the moment the span is made:
;;;; `content_capture' is off (no text at all), preview (the first
;;;; `preview_chars') or full (the whole of it).

(in-package #:nodecode-otel)

;;; --- identity and time ---------------------------------------------------------

(defun now-ns ()
  "The wall clock in nanoseconds, which is what OTLP stamps a span with."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000000000) (* microseconds 1000))))

(defun random-hex (digits)
  "DIGITS lowercase hex characters from the OS generator."
  (let ((text (make-string digits)))
    (dotimes (index digits text)
      (setf (char text index) (char "0123456789abcdef" (random 16))))))

(defun new-trace-id () (random-hex 32))
(defun new-span-id () (random-hex 16))

(defvar *traces* (make-hash-table :test #'equal)
  "One trace id per turn, so a turn's tool calls, rounds and marks join.
A key is (SESSION-ID . TURN-ID); the table is a bounded cache, not a record.")

(defun trace-id-for (turn)
  "The trace id of TURN, a plist as NLE:TURN answers, made on first use."
  (let ((key (cons (getf turn :session-id) (getf turn :turn-id))))
    (when (> (hash-table-count *traces*) 512)
      (clrhash *traces*))
    (or (gethash key *traces*)
        (setf (gethash key *traces*) (new-trace-id)))))

;;; --- what a text is, as the section allows ---------------------------------------

(defun capture ()
  "The content_capture in force: off, preview or full."
  (or (ignore-errors (setting :content-capture)) "preview"))

(defun clip (text)
  "TEXT as content_capture allows: nothing when off, the whole of it when
full, its first preview_chars characters when preview."
  (let ((text (typecase text
                (null nil)
                (string text)
                (t (with-output-to-string (out) (princ text out))))))
    (case (capture)
      ("off" nil)
      ("full" text)
      (t (let ((limit (or (ignore-errors (setting :preview-chars)) 0)))
           (when (and text (plusp limit) (> (length text) limit))
             (concatenate 'string (subseq text 0 limit) "…"))
           (when (and text (or (not (plusp limit)) (<= (length text) limit)))
             text))))))

(defun one-line (text &optional (limit 200))
  "TEXT on one line, at most LIMIT characters: what a table row can say."
  (when text
    (let ((text (substitute #\Space #\Newline (substitute #\Space #\Tab text))))
      (if (> (length text) limit)
          (concatenate 'string (subseq text 0 limit) "…")
          text))))

(defun attrs (&rest pairs)
  "PAIRS as an attribute alist, `(key value ...)', dropping an absent value."
  (loop for (key value) on pairs by #'cddr
        when (and key value) collect (cons key value)))

(defun attr (attributes key)
  (cdr (assoc key attributes :test #'string=)))

;;; --- the span --------------------------------------------------------------------

(defstruct (span (:constructor make-span) (:conc-name span-))
  "One finished unit of work, as the local store keeps it and OTLP carries it."
  name
  (kind :internal)
  start-ns
  end-ns
  trace-id
  span-id
  (attrs '())
  (status "ok")
  detail)

(defun span-object (span)
  "SPAN as the JSON object the local file holds, one per line."
  (nlk:json-object
   "name" (span-name span)
   "trace_id" (span-trace-id span)
   "span_id" (span-span-id span)
   "start_ns" (princ-to-string (span-start-ns span))
   "end_ns" (princ-to-string (span-end-ns span))
   "status" (span-status span)
   "detail" (span-detail span)
   "attrs" (apply #'nlk:make-json-object
                   (loop for (key . value) in (span-attrs span)
                         append (list key (typecase value
                                            (string value)
                                            (number value)
                                            (t (princ-to-string value))))))))

(defun row-string (row key)
  (let ((value (and (hash-table-p row) (gethash key row))))
    (and value (princ-to-string value))))

;;; --- the local file ---------------------------------------------------------------

(defun store-path ()
  "The one file this cell keeps: spans, oldest first, under the home."
  (nlk:home "otel/spans.jsonl"))

(defvar *stored* 0
  "How many spans the local file holds, as this run counts them.")

(defun store-span (span)
  "Append SPAN to the local file, trimming it to max_spans."
  (let ((path (store-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create)
      (write-line (nlk:encode-json-object (span-object span)) out))
    (incf *stored*)
    (when (> *stored* (setting :max-spans))
      (trim-store))))

(defun store-lines ()
  "The local file's lines, oldest first."
  (let ((path (store-path)))
    (when (probe-file path)
      (with-open-file (in path :if-does-not-exist nil)
        (loop for line = (read-line in nil)
              while line collect line)))))

(defun trim-store ()
  "Rewrite the local file with its newest max_spans spans."
  (let* ((path (store-path))
         (kept (last (store-lines) (setting :max-spans))))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (line kept) (write-line line out)))
    (setf *stored* (length kept))
    kept))

(defun read-store ()
  "The local file as decoded span objects, oldest first."
  (loop for line in (store-lines)
        for object = (ignore-errors (nlk:decode-json line))
        when (hash-table-p object) collect object))

(defun clear-store ()
  "Forget every span kept locally => how many lines went."
  (let ((path (store-path))
        (lines (store-lines)))
    (when (probe-file path) (ignore-errors (delete-file path)))
    (setf *stored* 0)
    (length lines)))

(defun store-rows (&key (limit 20) (match ""))
  "The local file's spans newest first, at most LIMIT, those whose name holds MATCH."
  (let* ((rows (reverse (read-store)))
         (found (if (and match (plusp (length match)))
                    (remove-if-not (lambda (row)
                                     (search (string-downcase match)
                                             (string-downcase (or (row-string row "name") ""))))
                                   rows)
                    rows)))
    (if (> (length found) limit) (subseq found 0 limit) found)))

(defun clock-time (nanoseconds)
  "NANOSECONDS since the epoch as the local clock's hh:mm:ss, or NIL."
  (when nanoseconds
    (let ((seconds (floor nanoseconds 1000000000)))
      (multiple-value-bind (second minute hour) (decode-universal-time seconds)
        (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))))

(defun row-line (row)
  "One span object as one line a person reads: when, what, how long, how it went."
  (let* ((start (ignore-errors (parse-integer (or (row-string row "start_ns") "") :junk-allowed t)))
         (end (ignore-errors (parse-integer (or (row-string row "end_ns") "") :junk-allowed t)))
         (attributes (gethash "attrs" row))
         (subject (and (hash-table-p attributes)
                       (or (gethash "gen_ai.tool.name" attributes)
                           (gethash "gen_ai.request.model" attributes)
                           (gethash "nodecode.mark.detail" attributes))))
         (milliseconds (and start end (round (- end start) 1000000)))
         (status (row-string row "status")))
    (format nil "~@[~a ~]~a~@[ (~a)~]~@[ ~dms~]~@[ ~a~]"
            (clock-time start)
            (or (row-string row "name") "?")
            (and subject (one-line (princ-to-string subject) 60))
            milliseconds
            (unless (equal status "ok") status))))

;;; --- OTLP ---------------------------------------------------------------------------

(defun otlp-attribute (key value)
  "KEY and VALUE as one OTLP KeyValue."
  (nlk:json-object
   "key" key
   "value" (typecase value
             (string (nlk:json-object "stringValue" value))
             (integer (nlk:json-object "intValue" (princ-to-string value)))
             (real (nlk:json-object "doubleValue" (coerce value 'double-float)))
             (t (nlk:json-object "boolValue" (and value t))))))

(defun otlp-status (span)
  (nlk:json-object "code" (if (equal (span-status span) "ok") 1 2)
                    "message" (or (span-detail span) "")))

(defun otlp-span (span)
  "SPAN in the OTLP/JSON trace shape, gen_ai attributes intact."
  (nlk:json-object
   "traceId" (span-trace-id span)
   "spanId" (span-span-id span)
   "name" (span-name span)
   "kind" (if (eq (span-kind span) :client) 3 1)
   "startTimeUnixNano" (princ-to-string (span-start-ns span))
   "endTimeUnixNano" (princ-to-string (span-end-ns span))
   "attributes" (coerce (loop for (key . value) in (span-attrs span)
                               collect (otlp-attribute key value))
                        'vector)
   "status" (otlp-status span)))

(defun otlp-body (spans)
  "SPANS as one OTLP/HTTP export request body."
  (nlk:json-object
   "resourceSpans"
   (vector
    (nlk:json-object
     "resource"
     (nlk:json-object
      "attributes"
      (vector (otlp-attribute "service.name"
                               (or (ignore-errors (setting :service-name)) "nodecode"))
              (otlp-attribute "telemetry.sdk.name" "nodecode-otel")
              (otlp-attribute "telemetry.sdk.language" "lisp")))
     "scopeSpans"
     (vector (nlk:json-object
              "scope" (nlk:json-object "name" "nodecode-otel")
              "spans" (coerce (mapcar #'otlp-span spans) 'vector)))))))
