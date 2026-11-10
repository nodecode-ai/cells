;;;; cell.lisp --- the contract, the tool, /airlock, the primer, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites swaps Hermes's terminal for one whose
;;;; commands go through an Airlock gate; here the same gate answers one
;;;; model-facing tool, airlock_run, and the verbs in AIRLOCK. Nothing hooks
;;;; this machine's shell: what runs, runs in Airlock's disposable sandbox,
;;;; and what comes back is a decision plus a receipt that this cell checks
;;;; under a key pinned locally.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "airlock": {"base_url": "https://airlock.example.internal",
;;;;               "public_key_hex": "<64 hex characters>",
;;;;               "token_env": "AIRLOCK_API_TOKEN"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-airlock)

;;; --- the boundary -------------------------------------------------------------

(nlk:define-contract airlock-command
  (:doc "Run one command through the operator's Airlock policy gate, and answer what the gate
decided: BLOCKED when the policy denied it (nothing ran), ALLOWED when it ran but the receipt
could not be checked, VERIFIED when the signed receipt checked out under the key pinned on this
machine. Use it for work that should not touch this machine unreviewed -- an unfamiliar script, a
build, a service to poke at -- or when the operator wants a signed record of what ran. The
command's own output and exit status come back with the answer; a nonzero exit is an answer, not
a refusal.")
  ("command" :text :required t
   :doc "the command, as the section's language reads it (bash, or python when the section says so)")
  ("input" :text
   :doc "text Airlock writes to the sandbox as input.txt before the command runs; it is a file, not standard input"))

;;; --- what the model reads -------------------------------------------------------

(defparameter +primer+
  "The Airlock gate runs a command for the operator under their own policy: it decides allow or
deny, runs what it allows in a disposable sandbox (not on this machine), and signs a receipt.
  airlock_run(command, input)  -- the tool: one command through the gate, its verdict and output
  (airlock:run \"ls -la\")      ; => (:status \"VERIFIED\" :exit-code 0 :stdout \"...\")
  (airlock:health)             ; => the gate's own health object
  (airlock:receipt \"<hash>\")   ; => one receipt, fetched and checked
  (airlock:last-receipt)       ; => the receipt of the last command this cell ran
  (airlock:verify-text \"{...}\") ; => a receipt an operator pasted, checked under the pinned key
/airlock answers the gate's health and the key this machine pins, /airlock last the last receipt.
The answer says BLOCKED (the policy denied; nothing ran), ALLOWED (it ran, but verify_receipts is
off or the receipt did not verify -- do not read it as a signature) or VERIFIED (hash and Ed25519
signature checked under the pinned key, and the output matched the signed digests). The signed
receipt does not carry the command text, so VERIFIED proves a decision and an execution, not which
string was evaluated. Each arena call is a fresh sandbox: files do not survive between calls, and
there is no cwd field, so start in the default directory and make the command say where to go.
AIRLOCK-ERROR names any refusal; a command whose answer was lost may already have run."
  "What (help :airlock) answers while the cell runs.")

(defparameter +output-cap+ 12000
  "How much of a command's output one answer carries.")

(defvar *last-receipt* nil
  "The last receipt this run of the cell verified: what /airlock last shows.")

;;; --- one command, decided ------------------------------------------------------

(defun receipt-of-step (step)
  "The verified receipt STEP carries: inline when the gate inlined it, else
fetched by id. => the decoded, verified receipt."
  (let ((inline (gethash "receipt" step))
        (id (gethash "receipt_id" step)))
    (cond ((hash-table-p inline) inline)
          ((and (stringp id) (plusp (length id)))
           (decode-receipt-text (receipt-text id)))
          (t (fail "Airlock's arena answer carries neither a receipt nor a receipt_id")))))

(defun untrusted (step detail)
  "A BLOCKED answer for a step this cell refused to trust: what the step says
in the open, and the reason it was not believed. Nothing here is signed."
  (list :verdict (let ((value (gethash "verdict" step)))
                   (if (and (stringp value) (plusp (length value))) value "unknown"))
        :reason nil :rules '() :action-id nil
        :exit-code +exit-invalid-receipt+ :stdout "" :stderr "" :execution-p nil
        :status (let ((value (gethash "status" step)))
                  (and value (princ-to-string value)))
        :receipt-id (let ((value (gethash "receipt_id" step)))
                      (and value (princ-to-string value)))
        :detail detail))

(defun decided (command &key input)
  "COMMAND, and INPUT, through the gate: the arena's step, its receipt (when
the section verifies), and the bound fields. => a plist.

A gate that cannot be reached refuses; a step whose receipt or whose output
does not bind does not refuse -- it answers BLOCKED with the reason, the way
the plugin answered, so the model reads what happened instead of an error."
  (running-settings)
  (unless (and (stringp command) (plusp (length (trimmed command))))
    (fail "run was given no command"))
  (let* ((step (arena command :input-text (trimmed input)))
         (verify (setting :verify-receipts))
         (receipt nil)
         (bound nil))
    (when verify
      (handler-case
          (setf receipt (verify-receipt (receipt-of-step step) (setting :public-key-hex)))
        (airlock-error (condition)
          (return-from decided
            (append (untrusted step (airlock-error-detail condition))
                    (list :command command :verified nil))))))
    (handler-case
        (setf bound (if verify
                        (bind-step step receipt command)
                        ;; Without a pinned key nothing is signed as far as this
                        ;; cell is concerned: show the step as the gate sent it,
                        ;; and never call it VERIFIED.
                        (bind-step-unverified step)))
      (airlock-error (condition)
        (return-from decided
          (append (untrusted step (airlock-error-detail condition))
                  (list :command command :verified nil)))))
    (when receipt
      (setf *last-receipt* receipt))
    (append bound (list :command command :verified (and receipt t)))))

(defun bind-step-unverified (step)
  "STEP bound to nothing: what the gate said, with no VERIFIED claim."
  (unless (hash-table-p step)
    (fail "Airlock's arena answer is not a JSON object"))
  (let* ((verdict (let ((value (gethash "verdict" step)))
                    (and (stringp value) (plusp (length value)) value)))
         (execution (gethash "execution" step))
         (allow (equal verdict "allow")))
    (when (and allow (not (hash-table-p execution)))
      (fail "Airlock allowed the command but sent no execution to read"))
    (list :verdict (or verdict "deny")
          :reason (let ((value (gethash "reason" step)))
                    (and (stringp value) value))
          :rules (let ((value (gethash "rules_fired" step)))
                   (if (vectorp value) (coerce value 'list) '()))
          :action-id (let ((action (gethash "action" step)))
                       (and (hash-table-p action) (gethash "id" action)))
          :exit-code (and allow (gethash "exit_code" execution))
          :stdout (if allow (or (gethash "stdout" execution) "") "")
          :stderr (if allow (or (gethash "stderr" execution) "") "")
          :execution-p (and allow t)
          :status (let ((value (gethash "status" step)))
                    (and value (princ-to-string value)))
          :receipt-id (let ((value (gethash "receipt_id" step)))
                        (and value (princ-to-string value))))))

;;; --- what the answer says -------------------------------------------------------

(defun status-word (result)
  "The one word the answer opens with: VERIFIED, ALLOWED or BLOCKED. A step
whose receipt or output failed to bind is BLOCKED whatever the gate said,
and an allow without a pinned key is only ALLOWED."
  (let ((verdict (getf result :verdict)))
    (cond ((not (equal verdict "allow")) "BLOCKED")
          ((getf result :verified) "VERIFIED")
          ((getf result :detail) "BLOCKED")
          (t "ALLOWED"))))

(defun answer-exit (result)
  "The exit status the answer reports."
  (or (getf result :exit-code) +exit-blocked+))

(defun result-text (result)
  "RESULT, a decided command, as the text a model or an operator reads."
  (let ((status (status-word result))
        (stdout (or (getf result :stdout) ""))
        (stderr (or (getf result :stderr) "")))
    (with-output-to-string (out)
      (format out "[~a] airlock~%verdict=~a status=~a~%"
              status (getf result :verdict) (or (getf result :status) "blocked"))
      (when (getf result :reason)
        (format out "reason=~a~%" (getf result :reason)))
      (when (getf result :rules)
        (format out "rules=~{~a~^,~}~%" (getf result :rules)))
      (when (getf result :receipt-id)
        (format out "receipt_id=~a~%" (getf result :receipt-id)))
      (format out "receipt_verified=~:[false~;true~]~%" (getf result :verified))
      (when (getf result :detail)
        (format out "~a~%" (getf result :detail)))
      (format out "exit ~a~%" (answer-exit result))
      (when (plusp (length stdout))
        (write-string (if (> (length stdout) +output-cap+)
                          (subseq stdout 0 +output-cap+)
                          stdout)
                      out)
        (when (> (length stdout) +output-cap+)
          (format out "~%[~d more characters]" (- (length stdout) +output-cap+))))
      (when (plusp (length (nlk:trimmed stderr)))
        (unless (or (zerop (length stdout))
                    (char= #\Newline (char stdout (1- (length stdout)))))
          (terpri out))
        (format out "[stderr]~%~a" stderr)))))

(defun run-tool (request)
  "The airlock_run tool: one command through the gate, its verdict and output."
  (result-text (decided (airlock-command-command request)
                         :input (airlock-command-input request))))

;;; --- the operator's command ------------------------------------------------------

(defun command-health ()
  "The gate's health, and what this machine pins."
  (let ((health (healthz)))
    (format nil "airlock: ~a answers~%~@[policy ~a~%~]pinned key ~:[none -- ALLOWED only, not VERIFIED~;~a~]~%language ~a; timeout ~ds~%verify_receipts ~:[false~;true~]"
            (setting :base-url)
            (let ((version (gethash "version" health)))
              (and version (princ-to-string version)))
            (setting :public-key-hex)
            (setting :public-key-hex)
            (setting :language) (setting :timeout-s) (setting :verify-receipts))))

(defun command-last ()
  "The receipt of the last command this cell ran, as words."
  (unless *last-receipt*
    (return-from command-last
      "airlock: no command has run since this cell started, so there is no receipt to show"))
  (let* ((decision (gethash "decision" *last-receipt*))
         (verdict (and decision (gethash "verdict" decision)))
         (execution (gethash "execution" *last-receipt*))
         (previous (gethash "prev_hash" *last-receipt*)))
    (format nil "airlock: last receipt ~a~%verdict ~a~@[ (~a)~]~@[~%exit ~a~]~%signed at ~a~@[~%prev_hash ~a (informational; the chain is not verified here)~]"
            (gethash "hash" *last-receipt*) verdict
            (and decision (gethash "reason" decision))
            (and execution (gethash "exit_code" execution))
            (or (gethash "ts" *last-receipt*) "an unreported time")
            previous)))

(defun command-receipt (id)
  "One receipt by id: fetched, checked under the pinned key, summarised."
  (let ((receipt (verify-receipt (receipt-text id) (setting :public-key-hex))))
    (format nil "airlock: receipt ~a verifies under the pinned key~%verdict ~a~%action ~a"
            (gethash "hash" receipt)
            (gethash "verdict" (gethash "decision" receipt))
            (gethash "action_id" receipt))))

(defun run-command (args session-id)
  "The answer to one /airlock invocation: health with no argument, the last
receipt for LAST, one receipt for an id."
  (declare (ignore session-id))
  (let ((word (string-downcase (or (trimmed args) ""))))
    (handler-case
        (cond ((member word '("" "health" "status") :test #'equal) (command-health))
              ((equal word "last") (command-last))
              (t (command-receipt word)))
      (airlock-error (condition)
        (format nil "airlock: ~a" (airlock-error-detail condition))))))

;;; --- the verbs ---------------------------------------------------------------------

(define-verb run (command &key input)
  "Run COMMAND through the operator's Airlock gate. INPUT is text Airlock
writes to the sandbox as input.txt. => a plist (:status :verdict :exit-code
:stdout :stderr :command :verified ...), with :status one of VERIFIED,
ALLOWED, BLOCKED."
  (decided command :input input))

(define-verb health ()
  "Airlock's own health answer. => its JSON object."
  (healthz))

(define-verb receipt (id)
  "The receipt ID, fetched and verified under the key this section pins. =>
the receipt's JSON object."
  (verify-receipt (receipt-text id) (setting :public-key-hex)))

(define-verb last-receipt ()
  "The last receipt this cell verified, or NIL before the first command."
  *last-receipt*)

(define-verb verify-text (text)
  "TEXT, a receipt an operator pasted, checked against the pinned key. => the
receipt's JSON object."
  (verify-receipt text (setting :public-key-hex)))

(defun receipt-verdict (receipt)
  "RECEIPT's signed verdict, for a form that only wants that."
  (let ((decision (and (hash-table-p receipt) (gethash "decision" receipt))))
    (and (hash-table-p decision) (gethash "verdict" decision))))

(defun receipt-id (receipt)
  "RECEIPT's signed hash, which is its id."
  (and (hash-table-p receipt) (gethash "hash" receipt)))

;;; --- start -----------------------------------------------------------------------------

(defun install ()
  "Refuse without an origin, and without a key when receipts are verified;
then say where commands go."
  (unless (setting :base-url)
    (nlk:config-error "base_url names no Airlock: set airlock.base_url to the https origin of your Airlock deployment (there is no built-in host)"))
  (when (and (setting :verify-receipts) (not (setting :public-key-hex)))
    (nlk:config-error "verify_receipts is on and public_key_hex is empty: pin the Ed25519 key you obtained out-of-band, or set verify_receipts false"))
  (nle:notice (format nil "airlock: commands go through ~a~@[, receipts checked under the pinned key~]"
                      (setting :base-url) (setting :verify-receipts))
              :key "nodecode-airlock")
  (nle:on-stop (lambda ()
                 (setf *last-receipt* nil)
                 (nle:notice nil :key "nodecode-airlock"))))

(nle:define-cell airlock
  (:section ("airlock")
    (:guide "set base_url to the https origin of your own Airlock deployment (there is no built-in host, and a redirect is refused); pin public_key_hex -- the Ed25519 key its operator publishes out-of-band, never one fetched from the server; token_env names an environment variable holding its API token, for a private instance; verify_receipts false shows ALLOWED instead of VERIFIED")
    ("base_url" :string :required t
     :doc "the https origin of your Airlock API, no path (there is no built-in default)")
    ("public_key_hex" :string
     :doc "the Ed25519 public key receipts are checked under, 64 lowercase hex characters, obtained from its operator out-of-band")
    ("verify_receipts" :boolean :default t
     :doc "check each receipt's hash and signature, and fail closed when they do not verify")
    ("language" :choice :options '("bash" "python") :default "bash"
     :doc "the language field sent to the arena for a command")
    ("timeout_s" :integer :default 60 :min 1
     :doc "seconds one arena call may take")
    ("token_env" :env :doc "environment variable holding the API token of a private Airlock")
    ("token" :secret :doc "the API token itself, kept in a 0600 file; token_env names one instead"))
  (:settings 'read-settings)
  (:tool "airlock_run" (nlk:contract 'airlock-command) 'run-tool)
  (:help :airlock
         "airlock: airlock_run sends one command through your Airlock gate and checks its signed receipt"
         +primer+)
  (:command "airlock" 'run-command
            :description "The Airlock gate: its health and pinned key, or the last receipt"
            :argument-hint "HEALTH | LAST | RECEIPT-ID")
  (:start #'install))
