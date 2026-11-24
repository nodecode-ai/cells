;;;; cell.lisp --- Tracekit for Nodecode: a signer outside the agent decides
;;;; every tool call, and signs the decision and its outcome into a run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a pair of hooks of the Hermes plugin in
;;;; Cygnux-Labs/Tracekit (integrations/hermes). This is the same two hooks on
;;;; Nodecode's one interception seam, NLE:HOOK on the :TOOL point, plus the
;;;; signer's own wire spoken from Lisp:
;;;;
;;;;   decide    the signer decides the call under the tool call's own id: a
;;;;             deny blocks it and the model reads the rule that blocked it;
;;;;             an ask waits inside the hook until a person answers in
;;;;             Tracekit (`tracekit approvals`) or the cell's limit passes.
;;;;   consume   only a call the signer lets through runs at all; the signer
;;;;             hands back the arguments it approved, and a call whose
;;;;             arguments are not those does not run.
;;;;   complete  the outcome is recorded against the decision: `ok` with the
;;;;             result (the result itself for a tool the policy marks
;;;;             untrusted, else its digest), or `error` with what failed.
;;;;
;;;; FAILS CLOSED, like the original: no call id, a signer that cannot be
;;;; reached, a refusal, or an error of this cell's own all block the call.
;;;; The signer's policy can make an outage fail open (`fail_modes`), and only
;;;; an outage: a refusal is never a reason to run.
;;;;
;;;; THE WIRE. Tracekit's signer takes one JSON frame per request over
;;;; HTTP (`POST /v2/rpc`, a bearer token for an `https://` signer and none
;;;; for a loopback one), and answers one frame. Every frame is canonical
;;;; JSON, and the digests it carries are SHA-256 over exactly that, so
;;;; WRITE-JCS below is RFC 8785 and EVENT-DIGEST is the signer's own
;;;; `sha256:` event hash: the argument digest a `complete` names is computed
;;;; here over the very text `decide` was sent.
;;;;
;;;; ONE RUN PER BOOT. Hermes gives its hooks a session id and the original
;;;; keeps one run per session; Nodecode's :TOOL point carries no session, so
;;;; a Nodecode process is one run, registered at its first decided call and
;;;; closed when the cell stops (and by the signer at its idle timeout, as
;;;; before). `tracekit last`, from the Tracekit CLI, still exports and
;;;; verifies it offline.

(defpackage #:nodecode-tracekit
  (:use #:cl)
  (:nicknames #:tracekit)
  (:export #:start-cell #:stop-cell #:setting))

(in-package #:nodecode-tracekit)

(nlk:define-peripheral tracekit :not-running t)

(defparameter +rpc-form+ "/v2/rpc" "Where a Tracekit signer takes one JSON frame.")
(defparameter +protocol+ 19 "The signer RPC version this cell speaks (a signer serves a range that holds it).")
(defparameter +keep-chars+ 4096 "How much of a failure's text the signer records.")

;;; --- canonical JSON, as Tracekit commits to it -------------------------------

(defun utf16-units (string)
  "STRING's UTF-16 code units: the order RFC 8785 sorts an object's keys in."
  (loop for char across string
        for code = (char-code char)
        if (< code #x10000) collect code
        else append (list (+ #xD800 (floor (- code #x10000) #x400))
                           (+ #xDC00 (mod (- code #x10000) #x400)))))

(defun json-key-lessp (first second)
  "Whether key FIRST sorts before key SECOND, by UTF-16 code units."
  (let ((left (utf16-units first)) (right (utf16-units second)))
    (loop while (and left right)
          do (cond ((< (first left) (first right)) (return t))
                   ((> (first left) (first right)) (return nil))
                   (t (pop left) (pop right)))
          finally (return (< (length left) (length right))))))

(defun write-json-string (string out)
  "STRING as a JSON string literal: only the characters JSON must escape are."
  (write-char #\" out)
  (loop for char across string
        for code = (char-code char)
        do (cond ((char= char #\") (write-string "\\\"" out))
                 ((char= char #\\) (write-string "\\\\" out))
                 ((< code #x20)
                  (write-string (case char
                                  (#\Backspace "\\b") (#\Tab "\\t") (#\Newline "\\n")
                                  (#\Page "\\f") (#\Return "\\r")
                                  (t (format nil "\\u~4,'0x" code)))
                                out))
                 (t (write-char char out))))
  (write-char #\" out))

(defun ecma-number-text (number)
  "NUMBER as RFC 8785 writes a JSON number, ECMA-262's Number::toString."
  (if (zerop number)
      "0"
      (let* ((negative (minusp number))
             (text (princ-to-string (abs (coerce number 'double-float))))
             (mark (or (position #\e text) (position #\d text)))
             (mantissa (if mark (subseq text 0 mark) text))
             (exponent (if mark (parse-integer text :start (1+ mark)) 0))
             (dot (position #\. mantissa))
             (whole (if dot (subseq mantissa 0 dot) mantissa))
             (fraction (if dot (subseq mantissa (1+ dot)) ""))
             (written (concatenate 'string whole fraction))
             (digits (string-left-trim "0" written))
             ;; NUMBER = 0.<digits> * 10**places
             (places (+ (length whole) exponent (- (length written) (length digits))))
             (digits (string-right-trim "0" digits))
             (sign (if negative "-" "")))
        (cond ((zerop (length digits)) "0")
              ((<= (length digits) places 21)
               (concatenate 'string sign digits (make-string (- places (length digits))
                                                             :initial-element #\0)))
              ((< 0 places 22)
               (concatenate 'string sign (subseq digits 0 places) "." (subseq digits places)))
              ((< -6 places 1)
               (concatenate 'string sign "0." (make-string (- places) :initial-element #\0) digits))
              (t (let ((power (1- places)))
                   (concatenate 'string sign (subseq digits 0 1)
                                (if (> (length digits) 1)
                                    (concatenate 'string "." (subseq digits 1))
                                    "")
                                (if (plusp power) "e+" "e-")
                                (princ-to-string (abs power)))))))))

(defun write-jcs (value out)
  "VALUE in canonical JSON. NIL is JSON `false` (what a decoded wire value is),
:NULL is JSON null, T is true."
  (cond ((eq value t) (write-string "true" out))
        ((null value) (write-string "false" out))
        ((eq value :null) (write-string "null" out))
        ((stringp value) (write-json-string value out))
        ((eq value :false) (write-string "false" out))
        ((integerp value) (format out "~d" value))
        ((floatp value) (write-string (ecma-number-text value) out))
        ((hash-table-p value)
         (write-char #\{ out)
         (loop for first = t then nil
               for key in (sort (loop for key being the hash-keys of value collect key)
                                #'json-key-lessp)
               do (unless first (write-char #\, out))
                  (write-json-string key out)
                  (write-char #\: out)
                  (write-jcs (gethash key value) out))
         (write-char #\} out))
        ((vectorp value)
         (write-char #\[ out)
         (loop for index from 0 below (length value)
               do (when (plusp index) (write-char #\, out))
                  (write-jcs (aref value index) out))
         (write-char #\] out))
        ((listp value)
         (write-char #\[ out)
         (loop for first = t then nil
               for item in value
               do (unless first (write-char #\, out))
                  (write-jcs item out))
         (write-char #\] out))
        (t (write-json-string (princ-to-string value) out))))

(defun json-text (value)
  "VALUE as canonical JSON (RFC 8785): the bytes Tracekit commits to."
  (with-output-to-string (out) (write-jcs value out)))

(defun event-digest (value)
  "Tracekit's `sha256:` event hash of VALUE: the digest of its canonical JSON."
  (nlk:sha256-text (json-text value)))

(defun args-digest (tool args)
  "The digest a `complete` names for a call of TOOL with ARGS, or NIL when
ARGS holds a value canonical JSON can't carry (the signer keeps no digest
either)."
  (handler-case (event-digest (nlk:json-object "tool" tool "args" args))
    (error () nil)))

;;; --- talking to the signer ---------------------------------------------------

(defun trimmed (text)
  "TEXT without the whitespace a file or an environment variable ends in, or NIL."
  (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun hex-id (&optional (length 32))
  "A fresh id the signer's schema takes: hex, one character class."
  (let ((text (make-string length)))
    (dotimes (index length text)
      (setf (char text index) (char "0123456789abcdef" (random 16))))))

(defun signer-address ()
  "The signer's address, without a trailing slash."
  (string-right-trim "/" (string (setting :signer))))

(defun rpc-url ()
  (let ((address (signer-address)))
    (if (uiop:string-suffix-p address +rpc-form+)
        address
        (concatenate 'string address +rpc-form+))))

(defun signer-token ()
  "The bearer token the signer asks for: the environment variable the section
names, else the file it names (re-read every call, so a rotated token is
picked up), else none."
  (or (let ((name (trimmed (setting :token-env))))
        (when (and name (plusp (length name)))
          (let ((value (trimmed (ignore-errors (uiop:getenv name)))))
            (and value (plusp (length value)) value))))
      (let ((file (trimmed (setting :token-file))))
        (when (and file (plusp (length file)) (probe-file file))
          (let ((value (trimmed (uiop:read-file-string file))))
            (and value (plusp (length value)) value))))))

(defun frame (method fields)
  "One request frame: METHOD's name and FIELDS, an alist of (NAME . VALUE)."
  (let ((object (nlk:json-object "method" method)))
    (loop for (name . value) in fields do (setf (gethash name object) value))
    object))

(defun read-frame (text)
  "TEXT as a response frame, or a TRACEKIT-ERROR naming what arrived."
  (handler-case (shasht:read-json text)
    (error () (fail "the Tracekit signer answered no JSON: ~a"
                    (subseq text 0 (min 200 (length text)))))))

(defun rpc (method &optional fields &key (timeout nil))
  "One frame to the signer, as Tracekit's HTTP transport carries it: the
answer frame as a hash table, or a TRACEKIT-ERROR."
  (let* ((body (json-text (frame method fields)))
         (token (signer-token))
         (headers (if token
                      (list (cons "content-type" "application/json")
                            (cons "authorization" (concatenate 'string "Bearer " token)))
                      (list (cons "content-type" "application/json"))))
         (seconds (or timeout (setting :timeout-seconds) 15)))
    (multiple-value-bind (text status)
        (handler-case (nlk:http "POST" (rpc-url) :headers headers :content body :timeout seconds)
          (error (e) (fail "the Tracekit signer at ~a did not answer (~a)" (signer-address) e)))
      (unless (and (integerp status) (<= 200 status 299))
        (fail "the Tracekit signer at ~a answered HTTP ~a: ~a"
              (signer-address) status (subseq (or text "") 0 (min 300 (length (or text ""))))))
      (let ((answer (read-frame text)))
        (when (hash-table-p answer)
          (let ((refusal (gethash "error" answer)))
            (when (hash-table-p refusal)
              (fail "the Tracekit signer refused ~a: ~a (~a)"
                    method (gethash "message" refusal) (gethash "code" refusal)))))
        answer))))

(defun signer-answers-p ()
  "Whether the signer at the address speaks this cell's protocol.
=> (values T VERSION) when it does, (values NIL WHY) when it does not."
  (handler-case
      (let* ((answer (rpc "hello" nil :timeout 3))
             (range (gethash "proto" answer)))
        (if (and (vectorp range)
                 (= 2 (length range))
                 (<= (aref range 0) +protocol+ (aref range 1)))
            (values t (or (gethash "version" answer) "?"))
            (values nil (format nil "it speaks protocol ~a, not ~a" range +protocol+))))
    (error (e) (values nil (format nil "~a" e)))))

;;; --- the run ------------------------------------------------------------------

(defstruct (run (:conc-name run-))
  (id "" :type string)
  (token "" :type string)
  (stream "" :type string)
  (seq 0)
  (fail-modes nil)
  (allowed 0)
  (held 0)
  (denied 0)
  (completed 0))

(defvar *run* nil "The Tracekit run of this Nodecode process, registered at its first decided call.")
(defvar *last-refusal* nil "The last words the model read instead of a call, for /tracekit.")

(defun run-fail-open-p (run)
  "Whether RUN's policy lets a call run while the signer cannot be reached."
  (let ((modes (and run (run-fail-modes run))))
    (and (hash-table-p modes) (equal "open" (gethash "default" modes)))))

(defun ensure-run ()
  "The process's run, registered at the first call: one run, agent named as
the section says, closed when the cell stops."
  (or *run*
      (let ((answer (rpc "register_run"
                          (list (cons "request_id" (hex-id))
                                (cons "run_id" (hex-id))
                                (cons "agent" (nlk:json-object "name" (setting :agent)))))))
        (setf *run* (make-run :id (gethash "run_id" answer)
                              :token (gethash "run_token" answer)
                              :stream (hex-id)
                              :fail-modes (gethash "fail_modes" answer))))))

(defun event-call (run method fields)
  "One per-run event call: the run's token, its stream, and the next client_seq."
  (rpc method (append (list (cons "request_id" (hex-id))
                             (cons "run_id" (run-id run))
                             (cons "run_token" (run-token run))
                             (cons "stream" (run-stream run))
                             (cons "client_seq" (incf (run-seq run))))
                       fields)))

(defun run-call (run method fields)
  "One per-run call that is no event: the run's id and token."
  (rpc method (append (list (cons "run_id" (run-id run))
                             (cons "run_token" (run-token run)))
                       fields)))

(defun named-rules (frame)
  "The rules a refusal names, as the model reads them: TK-UNKNOWN-TOOL only
flags a tool the policy doesn't name."
  (let ((rules (and (hash-table-p frame) (gethash "rule_ids" frame))))
    (when (vectorp rules)
      (let ((named (remove "TK-UNKNOWN-TOOL" rules :test #'equal)))
        (when (plusp (length named))
          (format nil "~{~a~^, ~}" (coerce named 'list)))))))

(defun close-run (&optional (reason "the cell stopped"))
  "Close the process's run, best effort: a signer already gone closes it at its
own idle timeout."
  (when *run*
    (let ((run *run*))
      (setf *run* nil)
      (ignore-errors (run-call run "close_run" (list (cons "reason" reason)))))))

;;; --- deciding one call ----------------------------------------------------------

(defstruct (admitted (:conc-name admitted-))
  (decision-id "" :type string)
  (digest "" :type string)
  (send-result nil)
  (recorded t))

(defun await-approval (run tool call-id why)
  "Ask the signer for an approval of CALL-ID and wait for a person's answer in
Tracekit, inside the cell's own limit.
=> the approval id when it was approved, the refusal text when it was not."
  (let* ((request (run-call run "approval_request"
                             (list (cons "request_id" (hex-id))
                                   (cons "tool_call_id" call-id)
                                   (cons "reason" (format nil "~a: ~a" tool why)))))
         (approval (gethash "approval_id" request))
         (state (gethash "state" request))
         (deadline (+ (get-universal-time) (or (setting :ask-seconds) 120))))
    (loop while (and (equal state "requested") (< (get-universal-time) deadline))
          do (let ((answer (run-call run "approval_wait"
                                     (list (cons "approval_id" approval)
                                           (cons "timeout_ms"
                                                 (* 1000 (max 1 (min 30 (- deadline (get-universal-time))))))))))
               (setf state (gethash "state" answer))))
    (cond ((member state '("approved" "consumed") :test #'equal) (values approval nil))
          ((equal state "requested")
           ;; withdrawn, so no one approves a call that never ran
           (ignore-errors (run-call run "approval_abandon"
                                     (list (cons "request_id" (hex-id))
                                           (cons "approval_id" approval)
                                           (cons "reason" "no answer within the cell's limit"))))
           (values nil (format nil "Tool call held by Tracekit policy (~a) and not approved: requested" why)))
          (t (values nil (format nil "Tool call held by Tracekit policy (~a) and not approved: ~a" why state))))))

(defun consume (run decision tool call-id args approval why)
  "Have the signer let the call run now.
=> (values NIL ADMITTED) when it may, (values REFUSAL NIL) when it may not."
  (let* ((fields (list (cons "request_id" (hex-id))
                        (cons "tool_call_id" call-id)
                        (cons "tool" tool)
                        (cons "args_source" "parsed")
                        (cons "args" args)))
         (answer (run-call run "approval_consume"
                            (if approval (cons (cons "approval_id_hint" approval) fields) fields))))
    (cond
      ((not (eq t (gethash "ok" answer)))
       (values (format nil "Tool call blocked by Tracekit policy: ~a"
                       (or (gethash "reason" answer) (named-rules answer) why
                           "the signer refused the call"))
               nil))
      ((let ((approved (gethash "args" answer)))
         (and (hash-table-p approved)
              (not (equal (json-text approved) (json-text args)))))
       (values "Tool call blocked by Tracekit: the arguments the signer approved are not the call's" nil))
      (t (incf (run-allowed run))
         (values nil (make-admitted :decision-id (gethash "decision_id" decision)
                                     :digest (or (args-digest tool args) "")
                                     :send-result (eq t (gethash "send_result" decision))))))))

(defun admit (tool call-id args)
  "Decide CALL-ID in the signer and consume the decision, waiting out a hold.
=> (values NIL ADMITTED) when the call may run, (values REFUSAL NIL) when it may not."
  (let ((decided nil))
    (handler-case
        (let* ((run (ensure-run))
               (decision (event-call run "decide"
                                     (list (cons "tool_call_id" call-id)
                                           (cons "tool" tool)
                                           (cons "args_source" "parsed")
                                           (cons "args" args))))
               (verdict (gethash "decision" decision))
               (why (or (named-rules decision)
                        (gethash "reason" decision)
                        (string verdict))))
          (setf decided t)
          (cond ((equal verdict "deny")
                 (incf (run-denied run))
                 (values (format nil "Tool call blocked by Tracekit policy: ~a" why) nil))
                (t (let ((approval nil))
                     (when (equal verdict "ask")
                       (incf (run-held run))
                       (multiple-value-bind (approved held) (await-approval run tool call-id why)
                         (when held (return-from admit (values held nil)))
                         (setf approval approved)))
                     (consume run decision tool call-id args approval why)))))
      (error (e)
        ;; No decision: the call does not run, unless the policy fails open. A
        ;; call already decided runs or does not by that decision alone.
        (cond ((and (not decided) (or (setting :fail-open) (run-fail-open-p *run*)))
               (values nil (make-admitted :recorded nil)))
              (decided
               (values (format nil "Tool call refused by Tracekit: the signer could not finish deciding it (~a)" e) nil))
              (t (values (format nil "Tool call refused by Tracekit: the signer could not decide it (~a)" e) nil)))))))

;;; --- recording the outcome ------------------------------------------------------

(defun tool-call-failed-p ()
  "Whether the core recorded the call this hook wraps as failed: its own flag,
read by name so a core that keeps none answers NIL."
  (let ((symbol (find-symbol "*TOOL-CALL-FAILED*" '#:nodecode.evolved)))
    (and symbol (boundp symbol) (symbol-value symbol))))

(defun result-field (admitted text)
  "The result a `complete` carries: the text itself for a tool the policy
marks untrusted, and when it fits a frame; otherwise its digest."
  (if (and (admitted-send-result admitted) (< (length text) 400000))
      text
      (event-digest text)))

(defun complete-call (admitted call-id text &key failed)
  "Record the outcome of CALL-ID against its decision. Never signals: the tool
already ran, and its result reaches the model either way."
  (when (and admitted (admitted-recorded admitted) *run*)
    (handler-case
        (let* ((run *run*)
               (records (list (cons "tool_call_id" call-id)
                              (cons "decision_id" (admitted-decision-id admitted))
                              (cons "args_digest" (admitted-digest admitted))
                              (cons "status" (if failed "error" "ok")))))
          (if failed
              (push (cons "error" (subseq text 0 (min +keep-chars+ (length text)))) records)
              (let ((result (result-field admitted text)))
                (when result (push (cons "result" result) records))))
          (event-call run "complete" records)
          (incf (run-completed run)))
      (error () nil))))

;;; --- the :TOOL point -------------------------------------------------------------

(defun gate (op next)
  "Decide the call in the signer, let it run only on a consume, and record its
outcome. A refusal answers the model's words and marks the call failed."
  (let ((tool (or (getf op :name) "unknown"))
        (call-id (let ((id (getf op :call-id))) (if (stringp id) id "")))
        (args (let ((arguments (getf op :arguments)))
                (if (hash-table-p arguments) arguments (nlk:json-object)))))
    (if (zerop (length call-id))
        (refuse "Tool call blocked by Tracekit: the call came with no call id to decide it under")
        (multiple-value-bind (refusal admitted) (admit tool call-id args)
          (if refusal
              (refuse refusal)
              (let ((text nil))
                (handler-case (setf text (funcall next op))
                  (error (e)
                    (complete-call admitted call-id (format nil "ERROR: ~a: ~a" (type-of e) e)
                                   :failed t)
                    (error e)))
                (setf text (if (stringp text) text (format nil "~a" text)))
                (complete-call admitted call-id text :failed (tool-call-failed-p))
                text))))))

(defun refuse (text)
  "TEXT, the model's words for a call that did not run, recorded as failed."
  (setf *last-refusal* text)
  (nle:failure text))

;;; --- /tracekit -------------------------------------------------------------------

(defun status-text ()
  "What /tracekit says: the signer, this process's run, what the run decided,
and whether the signer answers now."
  (let ((signer (handler-case (signer-address) (error () "(no signer set)"))))
    (with-output-to-string (out)
      (format out "Tracekit: signer ~a, agent ~a~%"
              signer (or (ignore-errors (setting :agent)) "?"))
      (if *run*
          (format out "run ~a: ~d allowed, ~d held for a person, ~d denied, ~d outcomes recorded~%"
                  (run-id *run*) (run-allowed *run*) (run-held *run*)
                  (run-denied *run*) (run-completed *run*))
          (format out "no run yet: one starts at this process's first tool call~%"))
      (multiple-value-bind (answers why) (signer-answers-p)
        (format out "signer now: ~a~%"
                (if answers (format nil "answering, protocol ~a" why) (format nil "~a" why))))
      (when *last-refusal*
        (format out "last refusal: ~a~%" (subseq *last-refusal* 0 (min 300 (length *last-refusal*))))))))

(defun run-command (arguments session-id)
  "The `/tracekit` command: this process's Tracekit state."
  (declare (ignore arguments session-id))
  (values (status-text) nil))

;;; --- the declaration ---------------------------------------------------------------

(defparameter +manual+
  "Tracekit (Cygnux-Labs/Tracekit) puts a signer outside the agent: before a
tool call runs, the signer decides it under the call's own id. A deny blocks
the call and the model reads the rule that blocked it; an ask waits for a
person, who approves or rejects it in Tracekit (`tracekit approvals`, or the
signer's own page), until the cell's ask_seconds pass. Only a decision the
signer consumes runs, and when the call is over its outcome is recorded
against the decision: ok with the result, or error with what failed. One
Tracekit run covers this Nodecode process, from its first decided call until
the cell stops; `tracekit last` exports and verifies it offline.

  /tracekit              the signer, the run, and what it decided

The cell fails closed: no call id, a signer that cannot be reached, a refusal
or an error of the cell's own all block the call, and the model is told which.
`fail_open` in the cell's section lets a call run unrecorded while the signer
is unreachable (a refusal still blocks).")

(nle:define-cell tracekit
  (:section ("tracekit")
    (:guide "signer is the Tracekit signer's HTTP address (the dev signer prints it); a token is only needed for an https signer")
    (:any-of "signer")
    ("signer" :string :required t
     :doc "the signer's address: http://127.0.0.1:PORT for a loopback signer, https://host for a deployed one")
    ("token_env" :env :doc "the environment variable holding the signer's bearer token")
    ("token_file" :path :doc "a file holding the bearer token, re-read on every call")
    ("agent" :string :default "nodecode" :doc "the agent name the run is registered under")
    ("timeout_seconds" :integer :default 15 :min 1 :doc "one call to the signer's ceiling, in seconds")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long a held call waits for a person's answer before it is withdrawn")
    ("fail_open" :boolean :default nil
     :doc "let a call run unrecorded while the signer is unreachable; a refusal still blocks"))
  (:hook :tool #'gate)
  (:help :tracekit "tracekit: a signer outside the agent decides every tool call and signs the run" +manual+)
  (:command "tracekit" #'run-command
            :description "The Tracekit signer, this run, and what it decided"
            :session nil)
  (:start (lambda ()
            ;; No signer, nothing to decide against: refuse rather than run
            ;; every call ungated (an absent member alone is the setup
            ;; panel's gate, not the start's).
            (unless (plusp (length (or (trimmed (setting :signer)) "")))
              (error 'nlk:config-refusal
                     :detail (concatenate 'string
                                          "tracekit: the section names no signer -- set \"signer\" to the "
                                          "signer's address, http://127.0.0.1:PORT for the one the Tracekit CLI prints")))
            (setf *run* nil *last-refusal* nil)
            (nle:on-stop (lambda () (close-run "the cell stopped"))))))
