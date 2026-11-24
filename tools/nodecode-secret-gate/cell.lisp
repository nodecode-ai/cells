;;;; cell.lisp --- the three tools, the audit on every tool result, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT THE ORIGINAL DID, KEPT. A model about to hand text to a person, a
;;;; file or a service scans it first (secret_scan), gates the outbound share
;;;; (secret_gate_check), and reads back what was decided
;;;; (secret_gate_receipts); a hook audits EVERY tool result for leaked
;;;; secrets without blocking or altering it; /secret-gate previews any text.
;;;; The rules, the verdicts (CLEAR / REVIEW / LEAKED), the gates (ALLOW /
;;;; WARN / BLOCK) and the masked-preview-only evidence are the original's.
;;;;
;;;; WHAT THE PORT ADDED, and only this: the original shipped its skill as a
;;;; file it never registered, so its guidance arrives here as the manual topic
;;;; (help :secret-gate); and the host's plugin-state mirror became a plain
;;;; in-memory ring, because the log already holds every tool result the audit
;;;; ever read.

(in-package #:nodecode-secret-gate)

;;; --- the words, and the numbers ------------------------------------------------

(defparameter +manual+
  "Scan text before it leaves: the secret-gate cell looks for credential shapes
locally, with no network, no service and no model, and never returns the secret
itself -- only the rule that fired, the line, and a mask of its public prefix
and length.
  secret_scan           CLEAR, REVIEW or LEAKED for text about to be shared
  secret_gate_check     ALLOW, WARN or BLOCK plus a receipt id: the last check
                        before an outbound share. A BLOCKED receipt can never
                        be approved -- remove the secret and re-scan.
  secret_gate_receipts  the recent decisions, masked, as an audit trail
LEAKED is a known provider format (an AWS key id, a GitHub, Slack or OpenAI
token, a private-key block, a JWT, a key or password assignment). REVIEW is a
long high-entropy string or a path to a credential file: a person decides.
Every tool result is audited too, and a suspicious one lands as a receipt and
a warning without blocking anything.
  /secret-gate          how many receipts are held, and what is audited
  /secret-gate TEXT     that text as the gate sees it
A found secret is spent once it was shared: rotate it, then re-scan."
  "What (help :secret-gate) answers while the cell runs.")

(defparameter +gates+
  '(("CLEAR" . "ALLOW") ("REVIEW" . "WARN") ("LEAKED" . "BLOCK"))
  "The gate each verdict maps to, as the original gated them.")

(defparameter +entropies+
  '(("3.0" . 3.0d0) ("4.0" . 4.0d0) ("5.0" . 5.0d0))
  "The entropy thresholds the section offers, by the label it stores.")

(defparameter +default-entropy+ "4.0")

(defparameter +hook-findings-per-call+ 5
  "How many findings one audited tool result may put on its receipt.")

(defparameter +default-max-receipts+ 100)

(defparameter +scan-tool+ "secret_scan")
(defparameter +check-tool+ "secret_gate_check")
(defparameter +receipts-tool+ "secret_gate_receipts")

(defvar *max-receipts* +default-max-receipts+
  "How many receipts the ring holds; the section sets it at start.")

(defvar *receipts* '()
  "The newest first, masked: no secret material is held here.")

(defvar *receipt-counter* 0)

(defvar *audited* 0
  "How many tool results the audit has read since the cell started.")

;;; --- receipts -------------------------------------------------------------------

(defun utc-stamp (&optional (style :text))
  "Now in UTC: a readable instant, or the compact form a receipt id carries."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (if (eq style :id)
        (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)
        (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                year month day hour minute second))))

(defun record-receipt (kind verdict gate findings)
  "Append one receipt for a KIND decision on FINDINGS (already masked) and
answer it. The ring keeps the newest *MAX-RECEIPTS*."
  (let* ((findings (mapcar (lambda (f)
                              (list :rule (getf f :rule) :line (getf f :line)
                                    :masked (getf f :masked) :severity (getf f :severity)))
                            findings))
         (receipt (list :id (format nil "HSG-~a-~4,'0d" (utc-stamp :id) (incf *receipt-counter*))
                        :at (utc-stamp)
                        :kind kind
                        :verdict verdict
                        :gate gate
                        :rules (sort (remove-duplicates
                                      (mapcar (lambda (f) (getf f :rule)) findings)
                                      :test #'string=)
                                     #'string<)
                        :findings findings)))
    (push receipt *receipts*)
    (when (> (length *receipts*) *max-receipts*)
      (setf *receipts* (subseq *receipts* 0 *max-receipts*)))
    receipt))

(defun receipts (&optional (limit 10))
  "The newest LIMIT receipts, newest first."
  (subseq *receipts* 0 (min (max 0 limit) (length *receipts*))))

(defun gate-for (verdict)
  "The gate VERDICT maps to."
  (or (cdr (assoc verdict +gates+ :test #'string=)) "WARN"))

;;; --- how a decision reads out -----------------------------------------------------

(defun finding-line (item)
  (format nil "  line ~d: ~a -- ~a (~a)"
          (getf item :line) (getf item :rule) (getf item :masked) (getf item :hint)))

(defun scan-answer (text)
  "TEXT as the scan sees it: the verdict, then one line a finding, masked."
  (let* ((scanned (scan-text text))
         (findings (getf scanned :findings)))
    (if findings
        (format nil "~a: ~d finding~:p in ~d line~:p~%~{~a~^~%~}"
                (getf scanned :verdict) (length findings) (getf scanned :lines)
                (mapcar #'finding-line findings))
        (format nil "CLEAR: nothing matched in ~d line~:p" (getf scanned :lines)))))

(defun gate-answer (text destination)
  "TEXT gated for DESTINATION: the decision, the receipt, and what to do."
  (let* ((scanned (scan-text text))
         (verdict (getf scanned :verdict))
         (gate (gate-for verdict))
         (receipt (record-receipt "gate_check" verdict gate (getf scanned :findings))))
    (format nil "~a: ~a~@[ for ~a~]~%~{~a~^~%~}~%receipt ~a -- ~a"
            gate verdict
            (and (stringp destination) (plusp (length destination)) destination)
            (mapcar #'finding-line (getf scanned :findings))
            (getf receipt :id)
            (cond ((string= gate "BLOCK")
                   "do not send it: a blocked receipt is never approved, so remove the secret and re-scan")
                  ((string= gate "WARN")
                   "a person has to look before this is sent")
                  (t "safe to send")))))

(defun status-text ()
  "What /secret-gate answers with nothing typed."
  (format nil "secret gate: ~d receipt~:p held, bounded at ~d~@[; the newest is ~a~]; ~d tool result~:p audited~[; auditing is off~;~]; the entropy rule fires at ~,1f bit~:p a character; the scan is local and nothing was ever sent anywhere."
          (length *receipts*) *max-receipts*
          (and *receipts* (getf (first *receipts*) :id))
          *audited*
          (if (ignore-errors (setting :audit-tool-results)) 1 0)
          *min-entropy*))

(defun run-slash (args session-id)
  "/secret-gate with nothing typed answers the state; with text, that text as
the gate sees it. => the answer's text, and no panel."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (values (if (plusp (length text)) (scan-answer text) (status-text)) nil)))

;;; --- the audit on every tool result ------------------------------------------------

(defun audit-result (tool-name text)
  "Scan one tool result and, when it is not CLEAR, record a receipt and say so.
=> the receipt, or NIL. Nothing about TEXT changes."
  (incf *audited*)
  (let* ((scanned (scan-text text))
         (verdict (getf scanned :verdict)))
    (unless (string= verdict +clear+)
      (let* ((findings (getf scanned :findings))
             (shown (subseq findings 0 (min +hook-findings-per-call+ (length findings))))
             (receipt (record-receipt "tool_result" verdict (gate-for verdict) shown)))
        (nle:notice (format nil "secret gate: tool ~a returned ~a content (receipt ~a, rule~:p ~{~a~^, ~}); the result itself is unchanged"
                            tool-name verdict (getf receipt :id) (getf receipt :rules))
                    :level :warning)
        receipt))))

(defun audit-call (op next)
  "The :TOOL advice: run the tool, read what it answered, say so if it looks
like a secret -- and hand the same text back, unaltered."
  (let ((result (funcall next op)))
    (when (and (stringp result)
               (plusp (length result))
               (ignore-errors (setting :audit-tool-results)))
      (handler-case (audit-result (getf op :name) result)
        (error () nil)))
    result))

;;; --- the tools ------------------------------------------------------------------

(nlk:define-contract secret-scan-request
  (:doc "Scan text for leaked credentials here, on this machine, before it is pasted, shared or sent: code, a config, a chat message, a file's contents. Answers CLEAR, REVIEW or LEAKED with the rule that fired and a masked preview -- the secret itself is never returned. Deterministic and offline.")
  ("text" :text :required t :doc "the text to scan for leaked secrets"))

(nlk:define-contract secret-gate-check-request
  (:doc "The gate decision for content about to be shared: ALLOW, a WARN that needs a person to look, or a BLOCK that must not be sent, plus a receipt id. Use it as the last check before any outbound share. A blocked receipt can never be approved: remove the secret and re-scan.")
  ("text" :text :required t :doc "the content about to be shared or sent")
  ("destination" :text :default ""
                 :doc "where it is going, e.g. a chat reply or a public post"))

(nlk:define-contract secret-gate-receipts-request
  (:doc "List the recent secret-gate receipts -- verdict, gate and masked evidence -- to audit what was blocked or warned about, or to show the person the receipt for a blocked share. A receipt never holds secret material.")
  ("limit" :integer :default 10 :doc "how many recent receipts to list, 1 to 50"))

(defun secret-scan-tool (request)
  "The secret_scan tool."
  (scan-answer (secret-scan-request-text request)))

(defun secret-gate-check-tool (request)
  "The secret_gate_check tool."
  (gate-answer (secret-gate-check-request-text request)
               (secret-gate-check-request-destination request)))

(defun receipt-line (receipt)
  (format nil "~a  ~a  ~a (~a)~@[  rules: ~{~a~^, ~}~]~@[~%    ~{~a~^~%    ~}~]"
          (getf receipt :id) (getf receipt :at) (getf receipt :verdict) (getf receipt :gate)
          (getf receipt :rules)
          (mapcar (lambda (f) (format nil "line ~d: ~a -- ~a"
                                      (getf f :line) (getf f :rule) (getf f :masked)))
                  (getf receipt :findings))))

(defun secret-gate-receipts-tool (request)
  "The secret_gate_receipts tool."
  (let* ((limit (max 1 (min 50 (secret-gate-receipts-request-limit request))))
         (held (receipts limit)))
    (if held
        (format nil "~d receipt~:p held~%~{~a~^~%~}" (length *receipts*) (mapcar #'receipt-line held))
        "no receipts yet: nothing has been gated or audited since the cell started")))

;;; --- the section the cell runs on ---------------------------------------------------

(defun entropy-threshold (label)
  "The entropy a token must reach, by the label the section stores."
  (or (cdr (assoc label +entropies+ :test #'string=)) 4.0d0))

(nlk:define-peripheral secret-gate :not-running t)

(nle:define-cell secret-gate
  (:section ("secret-gate")
    (:guide "nothing here needs setting: the rules are the original's, the defaults audit every tool result and hold 100 receipts; raise min_entropy when a token-shaped id keeps coming back as REVIEW")
    ("audit_tool_results" :boolean :default t
                          :doc "scan every tool result and warn on a suspicious one; the result itself is never changed")
    ("max_receipts" :integer :default 100 :min 1
                    :doc "how many receipts the ring holds, newest kept")
    ("min_entropy" :choice :options '("3.0" "4.0" "5.0") :default "4.0"
                   :doc "bits per character a long token needs to be REVIEW: 3.0 is loose, 5.0 strict"))
  (:hook :tool #'audit-call)
  (:tool +scan-tool+ (nlk:contract 'secret-scan-request) 'secret-scan-tool)
  (:tool +check-tool+ (nlk:contract 'secret-gate-check-request) 'secret-gate-check-tool)
  (:tool +receipts-tool+ (nlk:contract 'secret-gate-receipts-request) 'secret-gate-receipts-tool)
  (:command "secret-gate" 'run-slash
            :description "Scan text for leaked secrets before it is shared"
            :argument-hint "[TEXT]")
  (:help :secret-gate "secret-gate: scans text for leaked credentials before it is shared, and gates the outbound share" +manual+)
  (:start (lambda ()
            (setf *receipts* '()
                  *receipt-counter* 0
                  *audited* 0
                  *max-receipts* (or (setting :max-receipts) +default-max-receipts+)
                  *min-entropy* (entropy-threshold (or (setting :min-entropy) +default-entropy+)))
            (nle:on-stop (lambda ()
                           (setf *receipts* '()
                                 *receipt-counter* 0
                                 *audited* 0
                                 *max-receipts* +default-max-receipts+
                                 *min-entropy* 4.0d0))))))
