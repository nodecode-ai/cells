;;;; receipts.lisp --- a receipt is proof only when the bytes it names are the
;;;; bytes that were signed, and only what it signed is shown as verified.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites wrote the rules out in receipts.py and
;;;; binding.py; this file keeps them, because they are the point of the tool:
;;;;
;;;;   - the hash is SHA-256 over canonical(receipt without hash and
;;;;     sig_ed25519), and the signature is Ed25519 over the same bytes under
;;;;     a public key pinned locally -- never one fetched from the server at
;;;;     verify time, since a server that could forge a receipt could serve a
;;;;     matching key;
;;;;   - the outer arena step is NOT signed, so every field shown under
;;;;     VERIFIED has to be bound to something the receipt did sign: the
;;;;     verdict, the action id, and the stdout / stderr / exit status by
;;;;     their digests and exact JSON integer;
;;;;   - a signature that checks out over fields that are absent, truncated or
;;;;     not integers is not enough: those refuse rather than display.
;;;;
;;;; The signed receipt does not carry the command text -- VERIFIED proves a
;;;; decision and an execution, not which command string was evaluated. The
;;;; outer action.code is compared when present, as a consistency check only.

(in-package #:nodecode-airlock)

;;; --- the codes one command answers with -------------------------------------

(defparameter +exit-blocked+ 126 "The policy gate denied, or nothing was trusted.")
(defparameter +exit-unavailable+ 75 "Airlock was not reachable; nothing ran.")
(defparameter +exit-invalid-receipt+ 74 "A receipt, or the step carrying it, did not bind.")
(defparameter +exit-misconfigured+ 78 "Trust material was missing before any call.")
(defparameter +exit-unsupported+ 125 "Airlock's arena has no field for what was asked.")

;;; --- hex and digests --------------------------------------------------------

(defun hex-string-p (text size)
  "Whether TEXT is exactly SIZE lowercase hex characters."
  (and (stringp text)
       (= (length text) size)
       (loop for char across text always (digit-char-p char 16))
       (equal text (string-downcase text))))

(defun hex->octets (text)
  "The octets TEXT, lowercase hex, spells."
  (let ((octets (make-array (/ (length text) 2) :element-type '(unsigned-byte 8))))
    (loop for index from 0 below (length octets)
          do (setf (aref octets index)
                   (parse-integer text :start (* 2 index) :end (+ 2 (* 2 index)) :radix 16)))
    octets))

(defun octets->hex (octets)
  "OCTETS as lowercase hex."
  (format nil "~(~{~2,'0x~}~)" (coerce octets 'list)))

(defun sha256-hex (text)
  "The SHA-256 of TEXT's UTF-8 bytes, lowercase hex."
  (octets->hex (ironclad:digest-sequence
                :sha256 (sb-ext:string-to-octets (or text "") :external-format :utf-8))))

(defun ed25519-verify-p (public-key-hex message signature)
  "Whether SIGNATURE, octets, verifies MESSAGE, octets, under the raw
Ed25519 public key PUBLIC-KEY-HEX as a locally pinned trust anchor."
  (handler-case
      (let ((key (ironclad:make-public-key :ed25519 :y (hex->octets public-key-hex))))
        (and (ironclad:verify-signature key message signature) t))
    (error () nil)))

;;; --- the document -----------------------------------------------------------

(defun receipt-member (object name &optional what)
  "(values VALUE PRESENT-P) for member NAME of the decoded OBJECT."
  (declare (ignore what))
  (gethash name object)
  (values (gethash name object) (nth-value 1 (gethash name object))))

(defun require-receipt-member (object name what)
  "NAME's value in OBJECT, refusing when it is absent."
  (multiple-value-bind (value present) (receipt-member object name what)
    (unless present
      (fail "~a is missing its ~a member" what name))
    value))

(defun require-object-member (object name what)
  "NAME's value in OBJECT, refusing unless it is a JSON object."
  (let ((value (require-receipt-member object name what)))
    (unless (hash-table-p value)
      (fail "~a:~a must be a JSON object" what name))
    value))

(defun require-integer-member (object name what)
  "NAME's value in OBJECT, refusing unless it is an exact JSON integer:
not true/false, not a float, not a string that looks like one."
  (let ((value (require-receipt-member object name what)))
    (unless (integerp value)
      (fail "~a:~a must be an exact JSON integer, not ~s" what name value))
    value))

(defun require-string-member (object name what)
  "NAME's value in OBJECT, refusing unless it is a string."
  (let ((value (require-receipt-member object name what)))
    (unless (stringp value)
      (fail "~a:~a must be a string" what name))
    value))

(defun require-digest-member (object name what)
  "NAME's value in OBJECT, refusing unless it is exactly 64 lowercase hex
characters -- the schema marks some digests optional, and an absent one is
refused rather than shown as trusted."
  (let ((value (require-receipt-member object name what)))
    (unless (hex-string-p value 64)
      (fail "~a:~a must be exactly 64 lowercase hex characters, not ~s" what name value))
    value))

(defun decode-receipt-text (text)
  "TEXT, a receipt, as a decoded JSON object; anything else refuses."
  (let ((object (handler-case (nlk:decode-json text :whole t)
                  (error (condition)
                    (fail "the receipt is not JSON (~a)"
                          (nlk:one-line (princ-to-string condition) :cap 160))))))
    (unless (hash-table-p object)
      (fail "the receipt is not a JSON object"))
    object))

(defun require-receipt-shape (raw)
  "The shape every receipt has before its hash means anything."
  (dolist (name '("schema_v" "receipt_v" "task_id" "action_id" "decision" "hash"
                  "sig_ed25519"))
    (require-receipt-member raw name "the receipt"))
  (let ((decision (require-object-member raw "decision" "the receipt")))
    (require-receipt-member decision "verdict" "receipt.decision"))
  (let ((hash (require-receipt-member raw "hash" "the receipt"))
        (signature (require-receipt-member raw "sig_ed25519" "the receipt")))
    (unless (hex-string-p hash 64)
      (fail "the receipt's hash is not 64 lowercase hex characters"))
    (unless (hex-string-p signature 128)
      (fail "the receipt's sig_ed25519 is not 128 lowercase hex characters")))
  (multiple-value-bind (previous present) (receipt-member raw "prev_hash")
    ;; JSON null arrives as :NULL here and is the ordinary case for a first
    ;; receipt; only a present, non-null value has a shape to check.
    (when (and present previous (not (eq previous :null)) (not (hex-string-p previous 64)))
      (fail "the receipt's prev_hash is neither null nor 64 lowercase hex characters")))
  raw)

(defun signed-payload (raw)
  "RAW, the decoded receipt, as the canonical octets its hash and signature cover."
  (let ((body (make-hash-table :test #'equal)))
    (loop for name being the hash-keys of raw
          unless (member name '("hash" "sig_ed25519") :test #'equal)
            do (setf (gethash name body) (gethash name raw)))
    (jcs-canonical body)))

(defun verify-receipt (text-or-object public-key-hex &key (what "the receipt"))
  "TEXT-OR-OBJECT, a receipt, checked: shape, hash, and Ed25519 signature
under PUBLIC-KEY-HEX. => the decoded receipt. Refuses (AIRLOCK-ERROR) on any
mismatch, naming which check failed."
  (let* ((raw (if (stringp text-or-object)
                  (decode-receipt-text text-or-object)
                  text-or-object))
         (raw (require-receipt-shape raw))
         (canonical (signed-payload raw))
         (hash (require-receipt-member raw "hash" what))
         (signature (require-receipt-member raw "sig_ed25519" what))
         (digest (octets->hex (ironclad:digest-sequence :sha256 canonical))))
    (unless (equal digest hash)
      (fail "INVALID RECEIPT: its hash is not the SHA-256 of its canonical bytes (signed ~a, got ~a)"
            hash digest))
    (unless (ed25519-verify-p public-key-hex canonical (hex->octets signature))
      (fail "INVALID RECEIPT: its Ed25519 signature does not verify under the public key this section pins"))
    raw))

;;; --- binding the unsigned step to the signed document -------------------------

(defun step-member (step name)
  "NAME's value in STEP, or (values NIL NIL) when absent."
  (receipt-member step name "the arena step"))

(defun require-step-member (step name)
  "NAME's value in STEP, refusing when it is absent."
  (require-receipt-member step name "the arena step"))

(defun same-value-p (a b)
  "Whether two decoded JSON values are the same wire value."
  (equal a b))

(defun bind-step (step receipt command)
  "STEP, the arena answer, bound to RECEIPT, the verified receipt of COMMAND.
=> a plist (:verdict :reason :rules :action-id :exit-code :stdout :stderr
:status :receipt-id :execution-p). Refuses when a field shown as verified is
not covered by the signature."
  (unless (hash-table-p step)
    (fail "Airlock's arena answer is not a JSON object"))
  (let ((signed-hash (require-receipt-member receipt "hash" "the receipt")))
    (multiple-value-bind (receipt-id present) (step-member step "receipt_id")
      (when (and present receipt-id (not (same-value-p (princ-to-string receipt-id)
                                                       (princ-to-string signed-hash))))
        (fail "BINDING FAILED: the step's receipt_id does not match the signed receipt's hash")))
    (let* ((decision (require-object-member receipt "decision" "the receipt"))
           (verdict (require-string-member decision "verdict" "receipt.decision"))
           (action-id (require-receipt-member receipt "action_id" "the receipt")))
      (unless (member verdict '("allow" "deny" "require_approval") :test #'equal)
        (fail "BINDING FAILED: the signed decision.verdict ~s is not one Airlock writes" verdict))
      (unless (and (stringp action-id) (plusp (length action-id)))
        (fail "BINDING FAILED: the signed action_id is missing"))
      (unless (same-value-p (require-receipt-member decision "action_id" "receipt.decision")
                             action-id)
        (fail "BINDING FAILED: receipt.action_id does not match decision.action_id"))
      (multiple-value-bind (outer-action outer-action-p) (step-member step "action")
        (when (and outer-action-p (hash-table-p outer-action))
          (multiple-value-bind (outer-id id-p) (receipt-member outer-action "id" "the arena step")
            (when (and id-p outer-id (not (same-value-p outer-id action-id)))
              (fail "BINDING FAILED: the step's action.id does not match the signed action_id")))
          (multiple-value-bind (code code-p) (receipt-member outer-action "code" "the arena step")
            (when (and code-p (not (same-value-p code command)))
              (fail "BINDING FAILED: the step's action.code is not the command that was sent (consistency check only: the command text is not signed)")))))
      (multiple-value-bind (outer-verdict outer-p) (step-member step "verdict")
        (when (and outer-p outer-verdict (stringp outer-verdict) (plusp (length outer-verdict))
                   (not (same-value-p outer-verdict verdict)))
          (fail "BINDING FAILED: the step's verdict ~s disagrees with the signed decision.verdict ~s"
                outer-verdict verdict)))
      (let ((reason (let ((value (gethash "reason" decision)))
                      (when (or (null value) (stringp value)) value)))
            (rules (let ((value (gethash "rules_fired" decision)))
                     (cond ((null value) '())
                           ((and (vectorp value) (every #'stringp value)) (coerce value 'list))
                           (t (fail "BINDING FAILED: signed decision.rules_fired is not a list of strings")))))
            (status (let ((value (gethash "status" step)))
                      (and value (princ-to-string value))))
            (execution-p nil)
            (exit-code nil)
            (stdout "")
            (stderr ""))
        (unless (same-value-p verdict "allow")
          ;; A deny's signed document must not carry an execution, and the
          ;; step's own execution body is unsigned: show no output at all.
          (multiple-value-bind (signed-execution present) (receipt-member receipt "execution" "the receipt")
            (when (and present (not (null signed-execution)))
              (fail "BINDING FAILED: a signed ~a receipt carries an execution" verdict)))
          (return-from bind-step
            (list :verdict verdict :reason reason :rules rules :action-id action-id
                  :exit-code nil :stdout "" :stderr "" :execution-p nil
                  :status (or status "blocked") :receipt-id signed-hash)))
        (let* ((signed (require-object-member receipt "execution" "the receipt"))
               (outer (multiple-value-bind (value present) (step-member step "execution")
                        (unless (and present (hash-table-p value))
                          (fail "BINDING FAILED: the arena step is missing execution for an allow decision"))
                        value)))
          (unless (and status (equal status "executed"))
            (fail "BINDING FAILED: the arena status ~s is not \"executed\" for an allow decision" status))
          (when (eq t (gethash "report_invalid" signed))
            (fail "BINDING FAILED: the signed execution reports itself invalid"))
          (when (or (eq t (gethash "stdout_truncated" signed))
                    (eq t (gethash "stderr_truncated" signed)))
            (fail "BINDING FAILED: the signed execution reports truncated output, so the outer previews cannot be bound"))
          (let ((signed-exit (require-integer-member signed "exit_code" "receipt.execution"))
                (outer-exit (require-integer-member outer "exit_code" "execution")))
            (unless (= signed-exit outer-exit)
              (fail "BINDING FAILED: the outer exit_code ~d disagrees with the signed ~d"
                    outer-exit signed-exit))
            (setf exit-code signed-exit))
          (setf stdout (require-string-member outer "stdout" "execution")
                stderr (require-string-member outer "stderr" "execution"))
          (let ((stdout-digest (require-digest-member signed "stdout_sha256" "receipt.execution"))
                (stderr-digest (require-digest-member signed "stderr_sha256" "receipt.execution")))
            (unless (equal (sha256-hex stdout) stdout-digest)
              (fail "BINDING FAILED: the step's stdout does not match the signed stdout_sha256"))
            (unless (equal (sha256-hex stderr) stderr-digest)
              (fail "BINDING FAILED: the step's stderr does not match the signed stderr_sha256")))
          (setf execution-p t)
          (list :verdict verdict :reason reason :rules rules :action-id action-id
                :exit-code exit-code :stdout stdout :stderr stderr
                :execution-p execution-p
                :status (or status "executed")
                :receipt-id signed-hash))))))
