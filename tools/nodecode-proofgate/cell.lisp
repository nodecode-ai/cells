;;;; cell.lisp --- ProofGate: no proof, no "done".
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Whydiriki700/proofgate for Nodecode. The original was a
;;;; Hermes plugin with three hooks: one recorded every tool call as
;;;; evidence, one held the finished reply and checked its completion claims
;;;; against that evidence, and one briefed the model once per session.
;;;;
;;;; Here the evidence is collected by one :TOOL point hook (the around
;;;; chain wraps every dispatch, so a call is seen whatever it was), the
;;;; check is the proof_check tool, and the answer is a verdict in the
;;;; original's three words:
;;;;
;;;;   PASS      every completion claim is backed by tool evidence
;;;;   DOWNGRADE some are not; they are named as unverified
;;;;   BLOCK     none is, or a claim is contradicted by a call that failed
;;;;
;;;; In `enforce' mode a BLOCK is the tool call's own failure, so the model
;;;; cannot report done past it; `warn' only reports; `off' records nothing.
;;;; Every check is appended to a ledger of JSON lines under the Nodecode
;;;; home: nothing here touches the network.

(defpackage #:nodecode-proofgate
  (:use #:cl)
  (:export #:start-cell))

(in-package #:nodecode-proofgate)

(nlk:define-peripheral proofgate :not-running t)

;;; --- the one setting read on the hook's hot path ---------------------------

(defun mode ()
  "The mode this cell is running in; `off' whenever it is not."
  (let ((value (handler-case (setting :mode) (error () nil))))
    (if (member value '("enforce" "warn" "off") :test #'equal) value "off")))

;;; --- evidence ---------------------------------------------------------------

(defparameter +field-limit+ 800
  "How much of one field of a call is kept: enough to match a claim, not a transcript.")

(defvar *evidence* (make-hash-table :test #'equal)
  "Session id -> its evidence records, newest first. In memory only: what a
check reads, and what no ledger on disk carries.")

(defvar *session-override* nil
  "The session a record is filed under when this thread runs for no turn, as a
test does; NIL in a turn, where (NLE:TURN) answers the session.")

(defun session-id ()
  (or *session-override*
      (getf (nle:turn) :session-id)
      ""))

(defun clip (value)
  (let ((text (cond ((stringp value) value)
                    ((null value) "")
                    (t (format nil "~a" value)))))
    (if (> (length text) +field-limit+)
        (concatenate 'string (subseq text 0 +field-limit+) " ...[truncated]")
        text)))

(defun strings-text (strings)
  "What a call's strings object carried, as text: the payloads a write or a
send was made with."
  (if (hash-table-p strings)
      (with-output-to-string (out)
        (maphash (lambda (name text)
                   (format out "~a ~a " name (if (stringp text) text "")))
                 strings))
      ""))

(defun down (text)
  (string-downcase (or text "")))

(defun contains (text needle)
  (and (stringp needle) (plusp (length needle)) (search needle text) t))

(defun contains-any (text words)
  (and (some (lambda (word) (contains text word)) words) t))

(defun remember (session record)
  (let* ((limit (or (handler-case (setting :max-evidence) (error () nil)) 200))
         (bucket (cons record (gethash session *evidence*))))
    (setf (gethash session *evidence*)
          (if (> (length bucket) limit) (subseq bucket 0 limit) bucket))
    record))

(defun record-evidence (op answer failed)
  "Keep one call: its name, what it was asked with, what it answered, and
whether the dispatch recorded it failed."
  (let* ((arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments)))
         (record (list :name (or (getf op :name) "tool")
                       :form (clip form)
                       :strings (clip (strings-text strings))
                       :result (clip answer)
                       :failed (and failed t)
                       :ts (get-universal-time))))
    (remember (session-id)
              (list* :text (down (format nil "~a ~a ~a ~a"
                                          (getf record :name) (getf record :form)
                                          (getf record :strings) (getf record :result)))
                     record))))

(defun tool-call-failed-p ()
  "Whether the dispatch this hook wraps recorded the call failed, read off
the engine's own flag rather than parsed back out of the result text."
  (let ((symbol (find-symbol "*TOOL-CALL-FAILED*" '#:nodecode.evolved)))
    (and symbol (boundp symbol) (symbol-value symbol) t)))

(defun observe-call (op next)
  "The :TOOL point: record the call, then let the body run. Evidence is a
courtesy, never the call's own failure, so a recording error is swallowed."
  (let ((answer (funcall next op)))
    (unless (equal (mode) "off")
      (handler-case (record-evidence op answer (tool-call-failed-p))
        (error () nil)))
    answer))

(defun session-evidence ()
  (gethash (session-id) *evidence* '()))

;;; --- claims -----------------------------------------------------------------

(defparameter +write-verbs+
  '("created" "wrote" "written" "saved" "generated" "updated" "edited"
    "produced" "dibuat" "disimpan" "tertulis" "diperbarui")
  "A sentence with a path and one of these claims a file was written.")

(defparameter +send-verbs+
  '("sent" "delivered" "posted" "published" "emailed" "forwarded"
    "terkirim" "dikirim" "terbit" "diterbitkan")
  "A sentence with one of these claims something left the machine.")

(defparameter +run-phrases+
  '("tests passed" "test passed" "tests pass" "all tests pass" "build succeeded"
    "build passed" "installed successfully" "command ran" "ran successfully"
    "executed successfully" "berhasil dijalankan" "tes lulus" "semua tes")
  "A sentence with one of these claims work ran.")

(defparameter +completion-words+
  (append +write-verbs+ +send-verbs+
          '("done" "completed" "finished" "selesai" "berhasil" "green" "passing"))
  "The words that turn a bare figure into a completion claim.")

(defparameter +extensions+
  '("md" "txt" "py" "json" "jsonl" "csv" "html" "htm" "pdf" "docx" "xlsx" "pptx"
    "png" "jpg" "jpeg" "webp" "gif" "svg" "yaml" "yml" "toml" "xml" "zip" "lisp"
    "asd" "sh" "js" "ts" "tsx" "css" "log" "db" "mp4" "sql" "rb" "go" "rs")
  "What a file name ends in, in the original's own list plus the languages of this one.")

(defparameter +punctuation+ ".,;:()[]{}<>\"'`!?"
  "What a token is trimmed of before it is judged.")

(defun trim-token (token)
  (string-trim (list #\Space #\Tab #\Return) (string-trim +punctuation+ token)))

(defun split-on (text char)
  "TEXT's segments between each CHAR."
  (let ((parts '()) (start 0))
    (loop for position = (position char text :start start)
          while position
          do (push (subseq text start position) parts)
             (setf start (1+ position)))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun last-segment (path)
  (car (last (remove "" (split-on path #\/) :test #'equal))))

(defun known-extension-p (token)
  (let ((dot (position #\. token :from-end t)))
    (and dot (> dot 0) (< (1+ dot) (length token))
         (member (down (subseq token (1+ dot))) +extensions+ :test #'equal)
         t)))

(defun path-token-p (token)
  "Whether TOKEN names a file: a path, or a name with a file extension."
  (let ((clean (trim-token token)))
    (and (plusp (length clean))
         (or (and (find #\/ clean) (> (length clean) 1))
             (known-extension-p clean))
         t)))

(defun figure-token-p (token)
  "Whether TOKEN is a figure: a number, a money amount or a percentage."
  (let* ((clean (trim-token token))
         (body (string-trim "$%" clean)))
    (and (plusp (length body))
         (every (lambda (c) (or (digit-char-p c) (char= c #\,) (char= c #\.))) body)
         (some #'digit-char-p body)
         t)))

(defun sentences (text)
  "TEXT cut where a claim ends: a line break, or . ! ? before space or end."
  (let ((parts '()) (start 0) (n (length text)))
    (flet ((cut (end) (push (string-trim '(#\Space #\Tab #\Return) (subseq text start end)) parts)))
      (loop for i from 0 below n
            for c = (char text i)
            do (cond ((member c '(#\Newline #\Return)) (cut i) (setf start (1+ i)))
                     ((and (member c '(#\. #\! #\?))
                           (or (= (1+ i) n)
                               (member (char text (1+ i)) '(#\Space #\Tab #\Newline #\Return))))
                      (cut (1+ i))
                      (setf start (1+ i)))))
      (when (< start n) (cut n)))
    (remove-if (lambda (s) (zerop (length s))) (nreverse parts))))

(defun extract-claims (text)
  "The completion claims TEXT makes, one plist each: :FAMILY, :TEXT, :TARGET."
  (let ((claims '()))
    (dolist (sentence (sentences (or text "")))
      (let* ((low (down sentence))
             (tokens (remove-if (lambda (token) (zerop (length token)))
                                (remove "" (split-on sentence #\Space) :test #'equal))))
        (when (contains-any low +write-verbs+)
          (dolist (token tokens)
            (when (path-token-p token)
              (push (list :family "file" :text sentence :target (trim-token token)) claims))))
        (when (contains-any low '("done" "created" "saved" "wrote" "written" "updated"))
          (dolist (token tokens)
            (when (and (path-token-p token) (known-extension-p (trim-token token)))
              (push (list :family "file" :text sentence :target (trim-token token)) claims))))
        (when (contains-any low +send-verbs+)
          (push (list :family "send" :text sentence :target nil) claims))
        (when (contains-any low +run-phrases+)
          (push (list :family "run" :text sentence :target nil) claims))
        (dolist (token tokens)
          (when (figure-token-p token)
            (let ((clean (trim-token token)))
              (when (or (char= (char clean 0) #\$)
                        (char= (char clean (1- (length clean))) #\%)
                        (contains-any low +completion-words+))
                (push (list :family "number" :text sentence :target clean) claims)))))))
    (remove-duplicates (nreverse claims)
                       :test (lambda (a b) (and (equal (getf a :family) (getf b :family))
                                                (equal (getf a :target) (getf b :target))
                                                (equal (getf a :text) (getf b :text)))))))

;;; --- verification -----------------------------------------------------------

(defparameter +write-shown+
  '("write-file" "write_file" "(edit " "with-open-file" "spit" "patch_file"
    "ensure-directories-exist" "(edit")
  "What a call's own text shows when the call wrote a file.")

(defparameter +command-shown+
  '("(sh " "(sh\"" "run-program" "run-shell-command" "launch-program" "shell-command")
  "What a call's own text shows when the call ran a command.")

(defparameter +send-shown+
  '("send" "message" "post" "publish" "mail" "notify" "submit" "webhook")
  "What a call's own text shows when the call sent something.")

(defun path-hit (claimed text)
  "Whether TEXT carries the claimed path, whole or by its last segment."
  (let ((clean (down (trim-token claimed))))
    (or (contains text clean)
        (let ((base (down (last-segment clean))))
          (and base (> (length base) 2) (contains text base) t)))))

(defun any-record (records predicate)
  (and (some predicate records) t))

(defun write-written-p (record)
  (contains-any (getf record :text) +write-shown+))

(defun verify-claim (claim records)
  "CLAIM against RECORDS: its :PROVEN and :CONTRADICTED flags settled."
  (let* ((family (getf claim :family))
         (target (getf claim :target))
         (ok nil)
         (bad nil))
    (flet ((live (record) (not (getf record :failed)))
           (dead (record) (getf record :failed)))
      (ecase family
        ("file"
         ;; A path is proof only from a call that wrote and succeeded: a read
         ;; that mentioned the same name is not a file this session made.
         (setf ok (any-record records
                               (lambda (r)
                                 (and (live r) (write-written-p r) (path-hit target (getf r :text)))))
               bad (and (not ok)
                        (any-record records
                                    (lambda (r)
                                      (and (dead r) (write-written-p r)
                                           (path-hit target (getf r :text))))))))
        ("send"
         (setf ok (any-record records (lambda (r) (and (live r) (contains-any (getf r :text) +send-shown+))))
               bad (and (not ok)
                        (any-record records
                                    (lambda (r) (and (dead r) (contains-any (getf r :text) +send-shown+)))))))
        ("run"
         (setf ok (any-record records (lambda (r) (and (live r) (contains-any (getf r :text) +command-shown+))))
               bad (and (not ok)
                        (any-record records
                                    (lambda (r) (and (dead r) (contains-any (getf r :text) +command-shown+)))))))
        ("number"
         (let ((figure (down target)))
           (setf ok (any-record records (lambda (r) (contains (getf r :text) figure))))))))
    (list :family family :text (getf claim :text) :target target
          :proven (and ok t) :contradicted (and bad t))))

(defun check-reply (text records)
  "TEXT's verdict against RECORDS: (:VERDICT :CLAIMS :UNPROVEN).
Fail closed as the original did: nothing proven is a BLOCK, and so is a
claim a failed call contradicts."
  (let* ((claims (mapcar (lambda (claim) (verify-claim claim records)) (extract-claims text)))
         (unproven (remove-if (lambda (claim) (getf claim :proven)) claims)))
    (list :verdict (cond ((null claims) "PASS")
                         ((null unproven) "PASS")
                         ((some (lambda (claim) (getf claim :contradicted)) unproven) "BLOCK")
                         ((= (length unproven) (length claims)) "BLOCK")
                         (t "DOWNGRADE"))
          :claims claims
          :unproven unproven)))

;;; --- the ledger -------------------------------------------------------------

(defun ledger-path ()
  (nlk:home "proofgate/ledger.jsonl"))

(defun append-ledger (object)
  (let ((path (ledger-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :append
                              :if-does-not-exist :create)
      (write-line (nlk:encode-json-object object) out))
    object))

(defun ledger-rows (&optional (limit 100))
  "The ledger's lines as decoded objects, newest last, at most LIMIT of them."
  (let ((path (ledger-path)))
    (if (not (probe-file path))
        '()
        (with-open-file (in path :direction :input)
          (let ((rows '()))
            (loop for line = (read-line in nil :eof)
                  until (eq line :eof)
                  do (let ((text (string-trim '(#\Space #\Tab #\Return) line)))
                       (when (plusp (length text))
                         (handler-case (push (nlk:decode-json text) rows)
                           (error () nil)))))
            (let ((recent (nreverse rows)))
              (if (> (length recent) limit)
                  (subseq recent (- (length recent) limit))
                  recent)))))))

(defun row (object key)
  (and (hash-table-p object) (gethash key object)))

(defun verdict-counts (rows)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (entry rows)
      (let ((verdict (or (row entry "verdict") "?")))
        (setf (gethash verdict counts) (1+ (or (gethash verdict counts) 0)))))
    counts))

(defun counts-line (rows)
  (let ((counts (verdict-counts rows)))
    (if (zerop (hash-table-count counts))
        "nothing checked yet"
        (format nil "~{~a~^, ~}"
                (sort (loop for verdict being the hash-keys of counts
                            using (hash-value n)
                            collect (format nil "~a=~d" verdict n))
                      #'string<)))))

(defun ledger-text (&optional (limit 100))
  "The /proof answer: counts, then the last few verdicts."
  (let ((rows (ledger-rows limit)))
    (if (null rows)
        "ProofGate ledger is empty: no reply has been checked yet."
        (with-output-to-string (out)
          (format out "ProofGate ledger (~d checked): ~a" (length rows) (counts-line rows))
          (dolist (entry (last rows (min 5 (length rows))))
            (format out "~%- ~a | claims=~a unproven=~a | session=~a"
                    (or (row entry "verdict") "?")
                    (or (row entry "claims") 0)
                    (or (row entry "unproven") 0)
                    (let ((session (row entry "session"))) (if (stringp session) session "-"))))))))

;;; --- the tools and the command -----------------------------------------------

(nlk:define-contract proof-check-request
  (:doc "A draft final reply to hold against this session's tool evidence: the
claim families are a file written, a message sent, a command or test that ran,
and a figure quoted.")
  ("reply" :text :required t
   :doc "the reply as it would be sent, verbatim, so its claims can be read"))

(defun claim-object (claim)
  (nlk:json-object "family" (getf claim :family)
                   "text" (getf claim :text)
                   "target" (or (getf claim :target) "")
                   "proven" (and (getf claim :proven) t)
                   "contradicted" (and (getf claim :contradicted) t)))

(defun report-json (result records)
  (nlk:encode-json-object
   (nlk:json-object
    "success" t
    "verdict" (getf result :verdict)
    "claims" (length (getf result :claims))
    "unproven" (coerce (mapcar #'claim-object (getf result :unproven)) 'vector)
    "checked_tool_calls" (length records)
    "mode" (mode)
    "note" (cond ((equal (getf result :verdict) "PASS")
                   "every completion claim in this reply is backed by a tool call of this session")
                  ((equal (getf result :verdict) "DOWNGRADE")
                   "the unproven claims are listed: say they are unverified, or do the work and check again")
                  (t "no claim is backed by tool evidence of this session: do the work, or drop the claim")))))

(defun append-verdict (verdict result)
  (handler-case
      (append-ledger (nlk:json-object
                      "ts" (get-universal-time)
                      "session" (session-id)
                      "verdict" verdict
                      "claims" (length (getf result :claims))
                      "unproven" (length (getf result :unproven))))
    (error () nil)))

(defun check-tool (request)
  "The proof_check tool: check a draft reply, record the verdict, and in
`enforce' mode answer a BLOCK as the call's own failure."
  (let* ((records (session-evidence))
         (result (check-reply (proof-check-request-reply request) records))
         (verdict (getf result :verdict))
         (report (report-json result records)))
    (append-verdict verdict result)
    (if (and (equal (mode) "enforce") (equal verdict "BLOCK"))
        (nle:failure (format nil "ERROR: ProofGate withheld this report: no completion claim in it is backed by tool evidence of this session, so it is not sent as done - do the work, or say it is undone, then check again.~%~a"
                             report))
        report)))

(defun proof-status-tool (arguments)
  "The proof_status tool: what this machine's ledger holds, for a model that
wants to see its own record mid-work."
  (declare (ignore arguments))
  (let* ((rows (ledger-rows 100))
         (recent (if (> (length rows) 5) (subseq rows (- (length rows) 5)) rows)))
    (nlk:encode-json-object
     (nlk:json-object
      "success" t
      "checked_replies" (length rows)
      "verdicts" (coerce (mapcar (lambda (entry)
                                   (nlk:json-object
                                    "verdict" (or (row entry "verdict") "?")
                                    "claims" (or (row entry "claims") 0)
                                    "unproven" (or (row entry "unproven") 0)))
                                 recent)
                          'vector)
      "mode" (mode)
      "note" "the ledger is a JSON Lines file under the Nodecode home"))))

(defun proof-command (arguments session-id)
  "The /proof command: the verdict ledger, read from the terminal or the TUI."
  (declare (ignore arguments session-id))
  (values (ledger-text) nil))

(defparameter +manual+
  "proofgate: no proof, no 'done'. Before reporting work complete, call
proof_check with the reply you are about to send; it is held against the tool
calls of this session and answers PASS (every claim backed), DOWNGRADE (some
claims unproven, named) or BLOCK (none backed, or a failed call contradicts
one). In enforce mode a BLOCK is the tool call's own failure. /proof shows the
ledger of what was checked, proof_status your own recent record. The claim
families are: a file written, a message sent, a command or test that ran, a
figure quoted."
  "The manual's one topic: what the gate asks of a reply, and how to answer it.")

(nle:define-cell proofgate
  (:section ("proofgate")
    (:guide "nothing here is required; mode is enforce, warn or off - enforce makes an unproven report fail the check, warn only reports it, off records nothing")
    ("mode" :choice :options '("enforce" "warn" "off") :default "warn"
     :doc "enforce answers a BLOCK as the tool call's failure; warn only reports; off records no evidence")
    ("max_evidence" :integer :default 200 :min 1
     :doc "how many tool calls of one session stay in memory for the check"))
  (:tool "proof_check" (nlk:contract 'proof-check-request) 'check-tool)
  (:tool "proof_status"
         (list :description "ProofGate's own record: how many replies were checked on this machine, the last few verdicts, and the mode the gate runs in."
               :parameters (nlk:json-object "type" "object"
                                             "properties" (nlk:json-object)))
         'proof-status-tool)
  (:command "proof" 'proof-command
            :description "ProofGate verdicts for recent replies")
  (:help :proofgate "no proof, no done: a reply's claims checked against tool evidence" +manual+)
  (:hook :tool #'observe-call)
  (:start (lambda ()
            (setf *evidence* (make-hash-table :test #'equal))
            (nle:on-stop (lambda () (setf *evidence* (make-hash-table :test #'equal))))
            (when (equal (mode) "enforce")
              (nle:notice "proofgate: an unproven completion claim fails the check"
                          :key "nodecode-proofgate" :level :info)
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-proofgate")))))))
