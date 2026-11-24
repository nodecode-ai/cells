;;;; cell.lisp --- what a turn sends to Langfuse, and when.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Langfuse (https://langfuse.com) is an open-source LLM observability
;;;; platform. This cell mirrors the original Hermes plugin's shape onto the
;;;; seams Nodecode has: one trace per turn, one generation per provider
;;;; round, one span per tool call, and a batch posted to the project's own
;;;; ingestion endpoint when the turn ends.
;;;;
;;;; The seams, and why these:
;;;;   :frame  every durable fact passes through the :FRAME point
;;;;           (NLE:FRAME-FACT reads one out) — turn.usage carries the
;;;;           round's provider, model, token counts and the priced cost the
;;;;           core already computed, turn.completed / turn.failed /
;;;;           turn.cancelled say the turn is over, which is when a batch
;;;;           goes.
;;;;   :tool   the around shape of one tool call, so a span gets the
;;;;           arguments, the result and the wall time without parsing text.
;;;;
;;;; Nothing here fails a turn: the frame hook runs as a watcher, the tool
;;;; hook records inside HANDLER-CASE, and an unreachable Langfuse keeps the
;;;; batch in memory with the refusal said as one standing notice.

(in-package #:nodecode-langfuse)

;;; --- what the cell is ---------------------------------------------------------

(defparameter +trace-name+ "Nodecode turn")
(defparameter +generation-name+ "LLM call")
(defparameter +span-name-fallback+ "tool")
(defparameter +ingestion-path+ "/api/public/ingestion")
(defparameter +default-base-url+ "https://cloud.langfuse.com")
(defparameter +post-timeout+ 10
  "Seconds one batch may take, connect and read alike.")
(defparameter +held-limit+ 128
  "How many turns' batches are held in memory before the oldest is dropped.")
(defparameter +capture-modes+ '("sanitized" "metadata" "full")
  "sanitized redacts and truncates; metadata keeps structure only; full is raw.")

(defvar *traces* nil
  "TURN-ID -> TRACE for the turns this start has seen, newest last, or NIL
while the cell is stopped.")
(defvar *last-error* nil
  "Why the last batch was refused, or NIL.")
(defvar *current* nil
  "The trace the newest fact on this thread belongs to: the tool hook's
fallback when it runs outside a turn's own thread.")
(defvar *operator* nil
  "Bound true while /langfuse runs, so a verb may say more than a hook.")

;;; --- settings, read leniently ------------------------------------------------
;;; (SETTING ...) is the declaration's own reader and signals for a key the
;;; declaration does not carry. These wrappers only cover the case a function
;;; is called while the cell is stopped (a test, an eval form): the default
;;; stands in then, and the declaration's value stands in every other time.

(defun option (key fallback)
  "KEY's setting, FALLBACK when the cell is stopped."
  (let ((value (ignore-errors (setting key))))
    (if (null value) fallback value)))

(defun capture-mode () (option :capture "sanitized"))
(defun max-chars () (option :max-chars 12000))
(defun max-depth () (option :max-depth 4))
(defun base-url () (string-right-trim "/" (option :base-url +default-base-url+)))

;;; --- ids, clocks, bytes -------------------------------------------------------

(defun random-hex (length)
  "LENGTH random lowercase hex characters."
  (let ((out (make-string-output-stream)))
    (dotimes (i length) (format out "~(~x~)" (random 16)))
    (get-output-stream-string out)))

(defun fresh-id () (random-hex 32))

(defun now () (nlk:unix-now))

(defun stamp (&optional (at (now)))
  "AT, seconds since the epoch, as the UTC ISO-8601 Langfuse takes."
  (nlk:iso-time at))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64 (text)
  "TEXT's UTF-8 octets, base64 as RFC 4648 writes them."
  (let* ((octets (nlk:utf-8-octets text))
         (length (length octets))
         (out (make-string-output-stream)))
    (loop for i from 0 below length by 3
          for b0 = (aref octets i)
          for b1 = (and (< (+ i 1) length) (aref octets (+ i 1)))
          for b2 = (and (< (+ i 2) length) (aref octets (+ i 2)))
          for n = (logior (ash b0 16) (ash (or b1 0) 8) (or b2 0))
          do (write-char (char +base64-alphabet+ (ldb (byte 6 18) n)) out)
             (write-char (char +base64-alphabet+ (ldb (byte 6 12) n)) out)
             (write-char (if b1 (char +base64-alphabet+ (ldb (byte 6 6) n)) #\=) out)
             (write-char (if b2 (char +base64-alphabet+ (ldb (byte 6 0) n)) #\=) out))
    (get-output-stream-string out)))

(defun basic-auth (public-key secret-key)
  "The Authorization header value Langfuse wants: Basic, the two keys joined
by a colon."
  (format nil "Basic ~a" (base64 (format nil "~a:~a" public-key secret-key))))

(defun auth-headers ()
  (list (cons "authorization" (basic-auth (option :public-key "")
                                          (option :secret-key "")))))

(defun ingestion-url ()
  "The project's ingestion endpoint: the base URL and its one path."
  (format nil "~a~a" (base-url) +ingestion-path+))

;;; --- capture: redact, truncate, or omit --------------------------------------
;;; The original plugin's `sanitized' default, carried over: pattern-based
;;; defense in depth applied BEFORE truncation, so a secret near the cut is
;;; still recognized. It is not a DLP guarantee, and README.md says so.

(defparameter +secret-prefixes+
  '("sk-" "pk-" "ghp_" "gho_" "github_pat_" "xoxb-" "xoxp-" "hf_" "AIza"
    "shpat_" "npm_" "sk_live_" "rk_live_")
  "How a pasted credential starts. A word starting so and long enough is
replaced whole.")

(defparameter +secret-names+
  '("password" "passwd" "passphrase" "secret" "token" "api_key" "apikey"
    "api-key" "access_token" "refresh_token" "authorization" "auth"
    "bearer" "private_key")
  "A word naming a secret: the word after it is the value, and is replaced.")

(defparameter +scheme-words+ '("bearer" "basic" "token" "digest")
  "A scheme word after `authorization': the credential is the word after it.")

(defparameter +word-extras+ "-_./+@$~*!?"
  "What may sit inside a word beside alphanumerics. #\\: and #\\= split one.")

(defun word-char-p (char)
  (or (alphanumericp char) (find char +word-extras+)))

(defun token-punctuation ()
  " \t\n\r\"'`,;()[]{}<>")

(defun canonical-name (word)
  "WORD lowercased with the punctuation a sentence puts around it removed."
  (string-downcase (string-trim (token-punctuation) word)))

(defun secret-name-p (word)
  (and word (member (canonical-name word) +secret-names+ :test #'string=) t))

(defun secret-token-p (word)
  "Whether WORD looks like a pasted credential: a known prefix and a body."
  (and (>= (length word) 8)
       (some (lambda (prefix) (uiop:string-prefix-p prefix word)) +secret-prefixes+)
       t))

(defun assignment-split (word)
  "WORD as (values HEAD TAIL) at its first #\\= or #\\:, or NIL."
  (let ((at (position-if (lambda (char) (member char '(#\= #\:))) word)))
    (when (and at (plusp at) (< (1+ at) (length word)))
      (values (subseq word 0 at) (subseq word (1+ at))))))

(defun redact-string (text)
  "TEXT with token-looking words, and values assigned to secret-named keys,
replaced by <redacted>."
  (if (or (null text) (zerop (length text)))
      text
      (with-output-to-string (out)
        (let ((i 0) (n (length text)) (previous nil))
          (loop while (< i n)
                do (let ((char (char text i)))
                     (if (word-char-p char)
                         (let ((end (or (position-if-not #'word-char-p text :start i) n)))
                           (let ((word (subseq text i end)))
                             (multiple-value-bind (head tail) (assignment-split word)
                               (cond
                                 ;; password=hunter2 in one word: the value alone goes
                                 ((and head (secret-name-p head) tail)
                                  (write-string head out)
                                  (write-string "=<redacted>" out)
                                  (setf previous nil))
                                 ((secret-token-p word)
                                  (write-string "<redacted>" out)
                                  (setf previous nil))
                                 ((and (secret-name-p previous)
                                       (member word +scheme-words+ :test #'string-equal))
                                  ;; `Authorization: Bearer XYZ' — the scheme stays,
                                  ;; the credential after it goes.
                                  (write-string word out)
                                  (setf previous (canonical-name word)))
                                 ((secret-name-p previous)
                                  (write-string "<redacted>" out)
                                  (setf previous nil))
                                 (t
                                  (write-string word out)
                                  (setf previous (canonical-name word))))))
                           (setf i end))
                         (progn (write-char char out)
                                (setf i (1+ i))))))))))

(defun truncate-text (text)
  "TEXT cut at the section's ceiling, saying how much was left out."
  (let ((limit (max-chars)))
    (if (<= (length text) limit)
        text
        (format nil "~a… (~d chars more)" (subseq text 0 limit) (- (length text) limit)))))

(defun capture-string (text)
  "TEXT as the capture mode allows it: a shape stub, a redacted cut, or a bare one."
  (let ((text (or text "")))
    (cond
      ((equal (capture-mode) "metadata")
       (nlk:json-object "omitted" t "type" "text" "chars" (length text)))
      ((equal (capture-mode) "full")
       (truncate-text text))
      (t
       (truncate-text (redact-string text))))))

(defun capture-value (value &optional (depth 0))
  "VALUE — a decoded JSON value — as the capture mode allows it, no deeper
than the section's ceiling. The root is depth 0."
  (cond
    ((> depth (max-depth)) (capture-string "<max-depth>"))
    ((hash-table-p value)
     (let ((object (nlk:json-object)))
       (maphash (lambda (key item)
                  (setf (gethash key object) (capture-value item (1+ depth))))
                value)
       object))
    ((stringp value) (capture-string value))
    ((vectorp value)
     (map 'vector (lambda (item) (capture-value item (1+ depth))) value))
    (t value)))

;;; --- one turn's trace --------------------------------------------------------

(defstruct (held (:copier nil))
  "What one turn sends: its trace, the observations under it, and whether a
batch already carried them."
  (id (fresh-id) :type string)
  (session-id nil)
  (turn-id nil)
  (started (now))
  (events '())
  (usage '())
  (status nil)
  (sent nil)
  (error nil))

(defun held-count () (if *traces* (hash-table-count *traces*) 0))

(defun held-traces ()
  "Every trace this start holds, newest first."
  (let ((all (if *traces* (loop for trace being the hash-values of *traces* collect trace) '())))
    (sort all #'> :key #'held-started)))

(defun prune-traces ()
  "Drop the oldest sent traces past the limit; they have left already."
  (when (and *traces* (> (hash-table-count *traces*) +held-limit+))
    (loop for trace in (loop for trace being the hash-values of *traces*
                             when (held-sent trace) collect trace)
          for i from 1
          while (> (hash-table-count *traces*) +held-limit+)
          do (remhash (or (held-turn-id trace) (held-session-id trace)) *traces*))))

(defun trace-key (session-id turn-id)
  (or turn-id session-id (format nil "unidentified-~a" (now))))

(defun ensure-trace (session-id turn-id)
  "The trace of SESSION-ID's TURN-ID, made when this is its first fact."
  (unless *traces* (setf *traces* (make-hash-table :test #'equal)))
  (let ((key (trace-key session-id turn-id)))
    (or (gethash key *traces*)
        (setf (gethash key *traces*)
              (make-held :session-id session-id :turn-id turn-id)))))

(defun trace-tags ()
  "The tags every trace carries: the organism, the trace's kind, and the
environment and release when the section names them."
  (coerce (remove nil
                  (list "nodecode" "turn"
                        (let ((env (option :environment "")))
                          (and (plusp (length env)) (format nil "env:~a" env)))
                        (let ((release (option :release "")))
                          (and (plusp (length release)) (format nil "release:~a" release)))))
          'vector))

(defun span-count (trace)
  (count "span-create" (held-events trace) :key (lambda (event) (getf event :type)) :test #'equal))

(defun trace-body (trace)
  "The trace-create body: identity, the turn's spend, and the capture mode."
  (let ((turn (ignore-errors (nle:turn)))
        (release (option :release ""))
        (environment (option :environment "")))
    (nlk:json-object
     "id" (held-id trace)
     "name" +trace-name+
     "timestamp" (stamp (held-started trace))
     :opt "sessionId" (held-session-id trace)
     "tags" (trace-tags)
     :opt "release" (and (plusp (length release)) release)
     "metadata"
     (nlk:json-object
      "source" "nodecode-langfuse"
      "capture_mode" (capture-mode)
      :opt "session_id" (held-session-id trace)
      :opt "turn_id" (held-turn-id trace)
      :opt "status" (held-status trace)
      :opt "environment" (and (plusp (length environment)) environment)
      :opt "rounds" (or (and turn (getf turn :rounds)) (length (held-usage trace)))
      :opt "calls" (or (and turn (getf turn :calls)) (span-count trace))
      "tool_calls" (span-count trace)
      :opt "seconds" (and turn (getf turn :seconds))))))

;;; --- the observations --------------------------------------------------------

(defun usage-details (payload)
  "PAYLOAD's token counts as Langfuse's usageDetails. A count the provider
never reported is left out, never written as a zero it did not say."
  (let ((input (nlk:json-value payload :integer "input-tokens"))
        (output (nlk:json-value payload :integer "output-tokens"))
        (cached (nlk:json-value payload :integer "cached-input-tokens"))
        (write (or (nlk:json-value payload :integer "cache-write-tokens") 0))
        (reasoning (nlk:json-value payload :integer "reasoning-tokens"))
        (total (nlk:json-value payload :integer "total-tokens")))
    (nlk:json-object
     :opt "input" input
     :opt "output" output
     :opt "cache_read_input_tokens" cached
     :opt "cache_creation_input_tokens" (and (plusp write) write)
     :opt "output_reasoning_tokens" reasoning
     :opt "total" total)))

(defun generation-event (trace payload started)
  "One provider round as a generation under TRACE."
  (let ((provider (nlk:json-value payload :string "provider"))
        (model (nlk:json-value payload :string "model"))
        (cost (nlk:json-value payload :number "cost-usd"))
        (known (nlk:json-value payload :boolean "cost-known"))
        (finish (nlk:json-value payload :string "finish-reason"))
        (voided (nlk:json-value payload :boolean "voided")))
    (list :type "generation-create"
          :body (nlk:json-object
                 "id" (fresh-id)
                 "traceId" (held-id trace)
                 "name" +generation-name+
                 "startTime" (stamp started)
                 "endTime" (stamp)
                 :opt "model" (and model (plusp (length model)) model)
                 "modelParameters" (nlk:json-object :opt "provider" provider)
                 "usageDetails" (usage-details payload)
                 :opt "costDetails" (and known (nlk:json-object "total" (or cost 0.0)))
                 :opt "level" (and voided "WARNING")
                 :opt "statusMessage" (and voided "the attempt was voided")
                 "metadata" (nlk:json-object
                             "provider" (or provider "")
                             :opt "finish_reason" finish
                             :opt "estimated" (nlk:json-value payload :boolean "estimated")
                             :opt "voided" voided)))))

(defun tool-span-event (trace name arguments result failed seconds call-id)
  "One tool call as a span under TRACE: sanitized arguments in, sanitized
result out, the wall time, and ERROR when the call failed."
  (list :type "span-create"
        :body (nlk:json-object
               "id" (fresh-id)
               "traceId" (held-id trace)
               "name" (if (and name (plusp (length name))) name +span-name-fallback+)
               "startTime" (stamp (- (now) seconds))
               "endTime" (stamp)
               "input" (capture-value arguments)
               "output" (capture-string (princ-to-string (or result "")))
               :opt "level" (and failed "ERROR")
               "metadata" (nlk:json-object
                           :opt "call_id" call-id
                           "duration_ms" (round (* 1000 seconds))))))

;;; --- recording ----------------------------------------------------------------

(defun record-usage (trace payload)
  "File one round's usage on TRACE, newest last, and its generation event."
  (push payload (held-usage trace))
  (push (generation-event trace payload (- (now) 0)) (held-events trace))
  trace)

(defun record-span (trace name arguments result failed started-seconds call-id)
  (push (tool-span-event trace name arguments result failed started-seconds call-id)
        (held-events trace))
  trace)

(defun terminal-kind-p (kind)
  (member kind '("turn.completed" "turn.failed" "turn.cancelled") :test #'equal))

(defun status-of (kind)
  (cond ((equal kind "turn.completed") "completed")
        ((equal kind "turn.failed") "failed")
        ((equal kind "turn.cancelled") "cancelled")
        (t kind)))

(defun close-trace (trace kind)
  (setf (held-status trace) (status-of kind))
  trace)

(defun record-frame (op)
  "File one durable fact OP carries on its turn's trace: a round's usage as a
generation, a turn's end as the moment its batch goes."
  (multiple-value-bind (kind payload metadata turn-id) (nlk:frame-fact op)
    (when kind
      (let* ((session-id (getf op :session-id))
             (turn (or turn-id (nlk:json-value metadata :string "turn_id")))
             (trace (ensure-trace session-id turn)))
        (setf *current* trace)
        (cond
          ((equal kind "turn.usage")
           (record-usage trace payload)
           (prune-traces)
           trace)
          ((terminal-kind-p kind)
           (close-trace trace kind)
           (send-trace trace)
           trace)
          (t trace))))))

(defun observe-frame (op next)
  "The :FRAME watcher: NEXT publishes the frame first, then the fact is
filed. A recording error is the cell's, never the frame's."
  (prog1 (funcall next op)
    (handler-case (record-frame op)
      (error (condition)
        (setf *last-error* (princ-to-string condition))))))

(defun turn-trace ()
  "The trace the running turn belongs to, from the turn itself, else the one
this thread's newest fact named."
  (let ((turn (ignore-errors (nle:turn))))
    (if turn
        (ensure-trace (getf turn :session-id) (getf turn :turn-id))
        *current*)))

(defun observe-tool (op next)
  "The :TOOL around hook: record the call's arguments, its result and its
wall time as a span, then answer exactly what NEXT answered. A tool that
signals still gets its failed span."
  (let ((started (get-internal-real-time))
        (name (getf op :name))
        (arguments (getf op :arguments))
        (call-id (getf op :call-id))
        (trace (turn-trace))
        (result nil)
        (failed nil))
    (unwind-protect
         (handler-case (setf result (funcall next op))
           (error (condition) (setf failed t) (error condition)))
      (when trace
        (handler-case
            (record-span trace name arguments result (or failed
                                                          (and (stringp result)
                                                               (uiop:string-prefix-p "ERROR:" result)))
                         (/ (float (- (get-internal-real-time) started))
                            internal-time-units-per-second)
                         call-id)
          (error (condition) (setf *last-error* (princ-to-string condition))))))))

;;; --- sending ------------------------------------------------------------------

(defun event-json (event)
  "One ingestion event: its own id and stamp around the body."
  (nlk:json-object "id" (fresh-id)
                    "type" (getf event :type)
                    "timestamp" (stamp)
                    "body" (getf event :body)))

(defun batch-json (trace)
  "TRACE as an ingestion batch: the trace first, so every observation below
it lands under a trace the server already knows, then the observations
oldest first."
  (nlk:encode-json-object
   (nlk:json-object
    "batch" (coerce (cons (event-json (list :type "trace-create" :body (trace-body trace)))
                          (mapcar #'event-json (reverse (held-events trace))))
                    'vector))))

(defun post-ingestion (url headers body)
  "POST BODY to URL => T on 2xx, else (values NIL REASON)."
  (handler-case
      (multiple-value-bind (answer status)
          (nlk:http :post url :content body :timeout +post-timeout+
                              :connect-timeout +post-timeout+
                              :headers (cons (cons "content-type" "application/json") headers))
        (declare (ignore answer))
        (if (< status 300) t (values nil (format nil "HTTP ~d from ~a" status url))))
    (error (condition)
      (values nil (format nil "~a: ~a" (type-of condition)
                          (princ-to-string condition))))))

(defvar *post* #'post-ingestion
  "The POST a batch goes through: a function of URL, HEADERS and BODY
answering true on 2xx, else (values NIL REASON); a test binds it.")

(defun send-trace (trace)
  "Send TRACE's batch once: T when it went, (values NIL REASON) when the
server refused it — the batch stays held, for /langfuse send to retry."
  (cond
    ((or (null trace) (held-sent trace)) t)
    ((null (held-events trace)) (setf (held-sent trace) t))
    (t
     (multiple-value-bind (ok reason)
         (funcall *post* (ingestion-url) (auth-headers) (batch-json trace))
       (cond (ok
              (setf (held-sent trace) t
                    (held-error trace) nil
                    *last-error* nil)
              (when *traces*
                (remhash (or (held-turn-id trace) (held-session-id trace)) *traces*)
                (prune-traces))
              (nle:notice nil :key "nodecode-langfuse")
              t)
             (t
              (setf (held-error trace) (or reason "refused")
                    *last-error* (or reason "refused"))
              (nle:notice (format nil "langfuse: the batch was refused: ~a" *last-error*)
                          :level :warning :key "nodecode-langfuse")
              (values nil reason)))))))

(defun unsent-traces ()
  (remove-if (lambda (trace) (or (held-sent trace) (null (held-events trace))))
             (held-traces)))

;;; --- the verbs, every one a string --------------------------------------------

(defun status ()
  "One line: where a batch goes, what the capture mode is, what is held."
  (format nil "langfuse: ~a · capture ~a · ~d trace~:p held, ~d to send~@[ · last refused: ~a~]"
          (ingestion-url) (capture-mode) (held-count) (length (unsent-traces)) *last-error*))

(defun send-all ()
  "Send every held batch now instead of at its turn's end."
  (let ((waiting (unsent-traces)))
    (if (null waiting)
        "langfuse: nothing held to send"
        (let ((sent 0) (refused 0) (reason nil))
          (dolist (trace waiting)
            (multiple-value-bind (ok why) (send-trace trace)
              (if ok (incf sent) (progn (incf refused) (setf reason why)))))
          (if (zerop refused)
              (format nil "langfuse: sent ~d trace~:p to ~a" sent (ingestion-url))
              (format nil "langfuse: sent ~d, refused ~d: ~a" sent refused reason))))))

(defun show-batch ()
  "The next batch exactly as it would go."
  (let ((trace (first (unsent-traces))))
    (if (null trace)
        "langfuse: nothing held; the next turn's batch is built when it ends"
        (format nil "langfuse: the next batch, as it would go to ~a~%~a"
                (ingestion-url) (batch-json trace)))))

(defun test-trace ()
  "Send one test trace, the smallest proof the keys work."
  (let* ((body (nlk:encode-json-object
                (nlk:json-object
                 "batch" (vector
                          (nlk:json-object
                           "id" (fresh-id)
                           "type" "trace-create"
                           "timestamp" (stamp)
                           "body" (nlk:json-object
                                   "id" (fresh-id)
                                   "name" "nodecode-langfuse test"
                                   "timestamp" (stamp)
                                   "tags" (coerce (list "nodecode" "test") 'vector)
                                   "metadata" (nlk:json-object
                                               "source" "nodecode-langfuse"
                                               "capture_mode" (capture-mode))))))))
         (url (ingestion-url)))
    (multiple-value-bind (ok reason) (funcall *post* url (auth-headers) body)
      (if ok
          (format nil "langfuse: a test trace went to ~a" url)
          (progn (setf *last-error* (or reason "refused"))
                 (format nil "langfuse: the test trace was refused: ~a" *last-error*))))))

(defun clear-held ()
  "Forget what is held, sent or not."
  (format nil "langfuse: dropped ~d held trace~:p" (held-count))
  (let ((count (held-count)))
    (setf *traces* (make-hash-table :test #'equal)
          *current* nil)
    (format nil "langfuse: dropped ~d held trace~:p" count)))

(defun panel (summary)
  "The dialog bare /langfuse answers: what it is doing, over a row for each verb."
  (nle:list-dialog
   :title "Langfuse" :summary summary
   :rows (list (nle:run-row "Send what is held" "/langfuse send"
                             :detail "instead of at each turn's end")
               (nle:run-row "Show the next batch" "/langfuse show"
                             :detail "exactly as it would go")
               (nle:run-row "Send a test trace" "/langfuse test"
                             :detail "the smallest proof the keys work")
               (nle:run-row "Forget what is held" "/langfuse clear"
                             :detail "nothing of it is sent"))))

(defun run-slash (args session-id)
  "The answer to one /langfuse invocation."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words args))
         (head (first words))
         (*operator* t))
    (nlk:with-handlers ((error (condition)
                          (format nil "langfuse: ~a" (princ-to-string condition))))
      (cond ((null head) (let ((line (status))) (values line (panel line))))
            ((string-equal head "send") (send-all))
            ((string-equal head "show") (show-batch))
            ((string-equal head "test") (test-trace))
            ((string-equal head "clear") (clear-held))
            (t (values (format nil "langfuse: ~a is not one of its choices" head)
                       (panel (status))))))))

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "langfuse traces a Nodecode turn to your own Langfuse project: one trace
per turn, one generation per provider round (model, provider, input, output,
cache and reasoning token counts, and the cost the core priced), and one span
per tool call with its sanitized arguments and result. A batch goes to
<base_url>/api/public/ingestion when the turn ends; if the server is
unreachable the batch stays held and /langfuse send retries it. Capture is
sanitized by default (secrets redacted, then cut at max_chars); metadata
drops content for shape stubs; full sends it raw.")

;;; --- the entry: the one declaration -------------------------------------------

(defun traceable-frame-p (kind)
  (or (equal kind "turn.usage") (terminal-kind-p kind)))

(nle:define-cell langfuse
  (:section ("langfuse")
    (:guide "paste the project's public and secret key from Langfuse's settings page; a self-hosted instance needs its base_url; /langfuse test proves the keys")
    ("public_key" :string :required t
                  :doc "Langfuse project public key (pk-lf-...)")
    ("secret_key" :secret :required t
                  :doc "Langfuse project secret key (sk-lf-...)")
    ("base_url" :string :default +default-base-url+
                :doc "https://cloud.langfuse.com, or your self-hosted instance")
    ("capture" :choice :options +capture-modes+ :default "sanitized"
               :doc "sanitized redacts and truncates; metadata keeps structure only; full is raw")
    ("max_chars" :integer :default 12000 :min 200
                 :doc "the ceiling on any one content field")
    ("max_depth" :integer :default 4 :min 0
                 :doc "how deep nested arguments are captured")
    ("environment" :string :default ""
                   :doc "environment tag, for example production")
    ("release" :string :default ""
               :doc "release tag, for example v1.2.0"))
  ;; Two things the declaration cannot say. A section with no key pair could
  ;; only be refused by the server, one batch at a time: refused at the start
  ;; instead, naming the key. And a base URL that is not a web address would
  ;; fail every batch the same way.
  (:settings (lambda (values table &aux (url (string-right-trim "/" (or (getf values :base-url) ""))))
               (declare (ignore table))
               (dolist (key '(:public-key :secret-key))
                 (let ((value (getf values key)))
                   (unless (and (stringp value) (plusp (length value)))
                     (nlk:config-error
                      "langfuse.~a is required: paste the project's two keys from its settings page"
                      (substitute #\_ #\- (string-downcase (symbol-name key)))))))
               (unless (some (lambda (scheme)
                               (and (uiop:string-prefix-p scheme url)
                                    (> (length url) (length scheme))))
                             '("https://" "http://"))
                 (nlk:config-error "langfuse.base_url is ~s; it is a web address, http:// or https://"
                                   url))
               (setf (getf values :base-url) url)
               values))
  (:hook :frame #'observe-frame)
  (:hook :tool #'observe-tool)
  (:command "langfuse" 'run-slash
            :description "Langfuse: what a turn sends, and where"
            :argument-hint "send | show | test | clear")
  (:help :langfuse "langfuse: one trace per turn, one generation per call, one span per tool call" +manual+)
  (:start (lambda ()
            (setf *traces* (make-hash-table :test #'equal)
                  *current* nil
                  *last-error* nil)
            (nle:on-stop (lambda ()
                           (setf *traces* nil *current* nil))))))
