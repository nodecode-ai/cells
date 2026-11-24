;;;; cell.lisp --- tenuo: a warrant checked before every tool call runs.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of hermes-tenuo, which checks each tool call of a
;;;; Hermes Agent against a signed, expiring permission slip before the handler
;;;; runs. Here the slip is a JSON warrant file the operator writes, the check
;;;; is one hook on the core's :TOOL point, and every decision -- allow or
;;;; deny, with the arguments the call carried -- is appended to a local JSONL
;;;; audit log, the way the original appends to $HERMES_HOME/tenuo/audit.jsonl.
;;;;
;;;; WHAT ENFORCES WHAT. The warrant names tools and, per tool, constraints on
;;;; named arguments:
;;;;
;;;;   {"expires_at": "2030-01-01T00:00:00Z",
;;;;    "capabilities": {
;;;;       "eval": "*",
;;;;       "read_file": {"path": {"under": "/data/reports"}},
;;;;       "websearch": {"query": {"glob": "release-*"}},
;;;;       "cron": {"action": {"one_of": ["list", "show"]}}}}
;;;;
;;;; A call passes when its tool is in the warrant and every argument the
;;;; warrant names holds its constraint: a literal is an exact value, "*" is
;;;; anything, {"under": DIR} is DIR or a path beneath it (traversal-safe:
;;;; the path is normalised before the prefix compare, and "/data/reports-x"
;;;; is not under "/data/reports"), {"glob": PATTERN} matches * and ?, and
;;;; {"one_of": [...]} is membership. A named argument the call does not
;;;; carry, an unknown constraint, and a tool the warrant does not name all
;;;; refuse: the check fails closed. A warrant with an expiry refuses every
;;;; call past it.
;;;;
;;;; WITH NO WARRANT the cell is a documented no-op, as the original is: it
;;;; loads, hooks the point, and every call passes. That is the state it is in
;;;; until "warrant" is set, and /tenuo says so.
;;;;
;;;; SIGNING. hermes-tenuo warrants are Ed25519-signed by a control plane and
;;;; verified by the tenuo Rust core. There is no crypto in a cell's core, so
;;;; this rewrite keeps the property that matters -- the file is not silently
;;;; rewritten under the operator -- with an HMAC-SHA256 of the warrant file's
;;;; exact bytes kept beside it as <warrant>.sig: "key_env" names the
;;;; environment variable holding the key, and /tenuo sign writes the .sig
;;;; (it prints the hex when the key is not in a file's reach). With "key_env"
;;;; set, a missing or mismatching .sig refuses every call, so a tampered
;;;; warrant is a refusal rather than a quiet promotion. With no key the
;;;; warrant is read unsigned, and /tenuo says which it is.
;;;;
;;;; NOT A SECURITY BOUNDARY against eval: an agent with eval can rewrite the
;;;; file and the key is in its environment, so this is the speed bump the
;;;; guard cell calls itself -- a prompt injection that cannot run eval, or a
;;;; subprocess, still cannot widen what it was given.

(defpackage #:nodecode-tenuo
  (:use #:cl))

(in-package #:nodecode-tenuo)

(nlk:define-peripheral tenuo :not-running t)

;;; --- small readers ---------------------------------------------------------

(defun text-of (value)
  "VALUE as the string a constraint compares, or NIL."
  (cond ((stringp value) value)
        ((null value) nil)
        ((eq value t) "true")
        ((integerp value) (princ-to-string value))
        (t nil)))

(defun path-parts (path)
  "PATH's components, \"/\" for the root, \".\" dropped and \"..\" resolved."
  (let ((parts (if (and (plusp (length path)) (char= (char path 0) #\/))
                   (list "/")
                   '())))
    (dolist (part (uiop:split-string (substitute #\/ (code-char 92) path) :separator "/")
                  (nreverse parts))
      (cond ((string= part "") nil)
            ((string= part ".") nil)
            ((string= part "..") (when (cdr parts) (pop parts)))
            (t (push part parts))))))

(defun under-p (value root)
  "TEXT is ROOT or a path beneath it, compared after normalisation."
  (let ((v (path-parts value)) (r (path-parts root)))
    (and (<= (length r) (length v))
         (equal r (subseq v 0 (length r))))))

(defun glob-p (pattern text)
  "PATTERN matches TEXT, * for any run of characters and ? for one of them."
  (labels ((walk (p i)
             (cond ((= p (length pattern)) (= i (length text)))
                   ((char= (char pattern p) #\*)
                    (or (walk (1+ p) i)
                        (and (< i (length text)) (walk p (1+ i)))))
                   ((>= i (length text)) nil)
                   ((let ((c (char pattern p)))
                      (or (char= c #\?) (char= c (char text i))))
                    (walk (1+ p) (1+ i)))
                   (t nil))))
    (walk 0 0)))

(defun one-of-p (values value)
  "VALUE is one of the array VALUES, compared as text."
  (let ((text (text-of value)))
    (and text
         (loop for candidate across values
               thereis (equal text (text-of candidate))))))

(defun constraint-holds-p (constraint value)
  "VALUE holds CONSTRAINT: a literal, \"*\", or {\"under\"/\"glob\"/\"one_of\"}."
  (cond ((and (stringp constraint) (string= constraint "*")) t)
        ((stringp constraint) (equal constraint (text-of value)))
        ((hash-table-p constraint)
         (let ((under (gethash "under" constraint))
               (glob (gethash "glob" constraint))
               (one-of (gethash "one_of" constraint))
               (exact (gethash "exact" constraint)))
           (cond ((and (stringp under) (text-of value)) (under-p (text-of value) under))
                 ((and (stringp glob) (stringp value)) (glob-p glob value))
                 ((and (vectorp one-of) (plusp (length one-of))) (one-of-p one-of value))
                 ((stringp exact) (equal exact (text-of value)))
                 (t nil))))
        (t nil)))

(defun parse-when (text)
  "TEXT, an ISO-8601 instant (UTC unless it says otherwise), as a universal
time, or NIL when it is not one."
  (when (and (stringp text) (>= (length text) 10))
    (handler-case
        (let* ((year (parse-integer text :start 0 :end 4))
               (month (parse-integer text :start 5 :end 7))
               (day (parse-integer text :start 8 :end 10))
               (hour 0) (minute 0) (second 0) (offset 0)
               (body (if (= (length text) 10) text (subseq text 0 (min 19 (length text))))))
          (when (> (length body) 10)
            (setf hour (parse-integer body :start 11 :end 13)
                  minute (parse-integer body :start 14 :end 16))
            (when (> (length body) 17)
              (setf second (parse-integer body :start 17 :end 19))))
          (let ((sign (position-if (lambda (c) (and (find c "+-") (> (position c text) 10))) text)))
            (when sign
              (let* ((sign-char (char text sign))
                     (rest (subseq text (1+ sign)))
                     (minutes (+ (* 60 (parse-integer rest :start 0 :end 2))
                                 (if (> (length rest) 2) (parse-integer rest :start 3 :end 5) 0))))
                (setf offset (if (char= sign-char #\-) (- minutes) minutes)))))
          (- (encode-universal-time second minute hour day month year 0) (* 60 offset)))
      (error () nil))))

(defun iso-time (universal)
  "UNIVERSAL as an ISO-8601 instant in UTC."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

;;; --- the warrant -----------------------------------------------------------

(defun read-warrant (path)
  "PATH's warrant as (:tools HASH :expires UNIVERSAL :error TEXT).
An unreadable file is :ERROR, which refuses every call (fail closed)."
  (handler-case
      (let ((object (nlk:decode-json (uiop:read-file-string path) :whole t)))
        (if (hash-table-p object)
            (let ((tools (gethash "capabilities" object)))
              (list :tools (and (hash-table-p tools) tools)
                    :expires (parse-when (gethash "expires_at" object))
                    :error (unless (hash-table-p tools)
                             "the warrant names no capabilities object")))
            (list :tools nil :expires nil
                  :error "the warrant is not a JSON object")))
    (error (condition)
      (list :tools nil :expires nil
            :error (format nil "the warrant could not be read (~a)" condition)))))

;;; --- the signature, when a key names one -----------------------------------

(defun hex (octets)
  (format nil "~(~{~2,'0x~}~)" (coerce octets 'list)))

(defun hmac-hex (key path)
  "The HMAC-SHA256 of PATH's exact bytes under KEY, as hex, or NIL."
  (handler-case
      (progn
        (asdf:load-system "ironclad")
        (let* ((key-bytes (ironclad:ascii-string-to-byte-array key))
               (hmac (ironclad:make-hmac key-bytes :sha256)))
          (with-open-file (in path :element-type '(unsigned-byte 8))
            (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
              (loop for n = (read-sequence buffer in)
                    while (plusp n)
                    do (ironclad:update-hmac hmac buffer :end n))))
          (hex (ironclad:hmac-digest hmac))))
    (error () nil)))

(defun trimmed-text (path)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (uiop:read-file-string path)))

(defun signature-state (path key-env)
  "How PATH's signature stands: (:signed T/NIL :why TEXT)."
  (if (null key-env)
      (list :signed nil :why "unsigned: no key_env is set")
      (let ((key (uiop:getenv key-env))
            (sig-path (concatenate 'string (uiop:native-namestring path) ".sig")))
        (cond ((null key)
               (list :signed nil :why (format nil "~a is not set" key-env)))
              ((not (probe-file sig-path))
               (list :signed nil :why (format nil "~a has no signature; run /tenuo sign" sig-path)))
              (t (let ((expected (hmac-hex key path)))
                   (if (and expected (equal expected (trimmed-text sig-path)))
                       (list :signed t :why "signed")
                       (list :signed nil :why "the signature does not match this file"))))))))

(defun sign-warrant (path key-env)
  "Write PATH's .sig under the key KEY-ENV names. => its text, for the model."
  (let ((key (and key-env (uiop:getenv key-env))))
    (if (null key)
        (format nil "tenuo: ~a is not set, so there is no key to sign with" (or key-env "key_env"))
        (let ((digest (hmac-hex key path)))
          (if (null digest)
              "tenuo: could not sign (no HMAC-SHA256 here)"
              (progn
                (with-open-file (out (concatenate 'string (uiop:native-namestring path) ".sig")
                                      :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
                  (write-string digest out))
                (format nil "tenuo: signed ~a (~a)" (file-namestring path) key-env)))))))

;;; --- the decision ----------------------------------------------------------

(defparameter +own-tools+ '("tenuo_check" "tenuo_audit")
  "The cell's own readers: they answer even under a warrant that names neither,
so a model can look at the policy that refused it.")

(defun decide (tool arguments &optional (policy (getf *tenuo* :warrant)))
  "TOOL against ARGUMENTS under POLICY. => :ALLOW or :DENY, and the reason."
  (labels ((deny (control &rest args) (values :deny (apply #'format nil control args)))
           (allow (control &rest args) (values :allow (apply #'format nil control args))))
    (cond
      ((member tool +own-tools+ :test #'equal) (allow "~a reads the warrant" tool))
      ((null policy) (allow "no warrant is configured"))
      ((getf policy :error) (deny "~a" (getf policy :error)))
      ((and (getf (getf policy :signature) :required)
            (not (getf (getf policy :signature) :signed)))
       (deny "the warrant is not signed as key_env requires: ~a"
             (getf (getf policy :signature) :why)))
      ((and (getf policy :expires) (<= (getf policy :expires) (get-universal-time)))
       (deny "the warrant expired at ~a" (iso-time (getf policy :expires))))
      (t (let ((capability (gethash tool (getf policy :tools))))
           (cond
             ((null capability) (deny "~a is not in the warrant" tool))
             ((or (eq capability t) (and (stringp capability) (string= capability "*")))
              (allow "the warrant allows ~a" tool))
             ((hash-table-p capability)
              (block arguments
                (maphash
                 (lambda (name constraint)
                   (let ((value (and (hash-table-p arguments) (gethash name arguments))))
                     (cond ((null value)
                            (return-from arguments
                              (deny "the call passes no ~a, which the warrant constrains" name)))
                           ((not (constraint-holds-p constraint value))
                            (return-from arguments
                              (deny "~a does not hold for ~a=~s" 
                                    (if (hash-table-p constraint)
                                        (format nil "the ~a constraint" (or (loop for key being the hash-keys
                                                                                   of constraint
                                                                                   return key)
                                                                             "unknown"))
                                        (format nil "the constraint ~s" constraint))
                                    name (text-of value)))))))
                 capability)
                (allow "the warrant allows ~a" tool)))
             (t (deny "the warrant's entry for ~a is not a capability" tool))))))))

;;; --- the audit log ---------------------------------------------------------

(defun home-directory ()
  (let ((named (uiop:getenv "NODECODE_HOME")))
    (if (and named (plusp (length named)))
        (uiop:ensure-directory-pathname (pathname (concatenate 'string named "/")))
        (merge-pathnames ".nodecode/" (user-homedir-pathname)))))

(defun audit-path ()
  "Where decisions are appended: audit_log, else tenuo/audit.jsonl under the
Nodecode home."
  (let ((named (setting :audit-log)))
    (if (and named (plusp (length (uiop:native-namestring named))))
        (pathname named)
        (merge-pathnames "tenuo/audit.jsonl" (home-directory)))))

(defun record (tool arguments decision reason call-id)
  "Append one decision to the audit log. An unwritable log never refuses a call."
  (ignore-errors
    (let ((path (audit-path)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :append
                                 :if-does-not-exist :create)
        (write-line (nlk:encode-json-object
                     (nlk:json-object "at" (iso-time (get-universal-time))
                                      "tool" (or tool "")
                                      "arguments" (if (hash-table-p arguments)
                                                      arguments
                                                      (nlk:json-object))
                                      "decision" (if (eq decision :allow) "ALLOW" "DENY")
                                      "reason" (or reason "")
                                      "call_id" (or call-id "")))
                    out)))
    t))

(defun audit-records (&key (limit 10) denied-only)
  "The audit log's last LIMIT records, oldest first: a list of hash tables."
  (let ((path (audit-path)))
    (when (probe-file path)
      (let ((records '()))
        (with-open-file (in path :if-does-not-exist nil)
          (loop for line = (read-line in nil nil)
                while line
                do (let ((record (handler-case (nlk:decode-json line)
                                   (error () nil))))
                     (when (and (hash-table-p record)
                                (or (not denied-only)
                                    (equal "DENY" (gethash "decision" record))))
                       (push record records)))))
        (let* ((all (nreverse records))
               (tail (if (> (length all) limit) (last all limit) all)))
          tail)))))

(defun audit-text (&key (limit 10) denied-only)
  "The audit log as the lines a person reads."
  (let ((records (audit-records :limit limit :denied-only denied-only)))
    (if (null records)
        (if denied-only "tenuo: no denials in the audit log" "tenuo: the audit log is empty")
        (format nil "~{~a~%~}"
                (loop for record in records
                      collect (format nil "~a  ~5a  ~a  ~@[-- ~a~]"
                                      (subseq (or (gethash "at" record) "") 0
                                              (min 19 (length (or (gethash "at" record) ""))))
                                      (or (gethash "decision" record) "?")
                                      (or (gethash "tool" record) "?")
                                      (let ((reason (gethash "reason" record)))
                                        (and reason (plusp (length reason))
                                             (not (equal "ALLOW" (gethash "decision" record)))
                                             reason))))))))

;;; --- the hook --------------------------------------------------------------

(defun refusal-text (tool reason)
  (format nil "ERROR: refused by nodecode-tenuo: ~a; ~a did not run -- do not retry it or work around it, ask the operator for a warrant that allows it"
          reason tool))

(defun enforce (op next)
  "The :TOOL point: a call the warrant refuses answers the refusal, as the
call's failure, and NEXT never runs."
  (let* ((tool (getf op :name))
         (arguments (getf op :arguments))
         (call-id (getf op :call-id)))
    (multiple-value-bind (decision reason) (decide tool arguments)
      (record tool arguments decision reason call-id)
      (if (and (eq decision :deny) (equal "block" (setting :on-denial)))
          (nle:failure (refusal-text tool reason))
          (funcall next op)))))

;;; --- what the model and the operator can ask -------------------------------

(defparameter +check-schema+
  (list :description
        "Ask whether a tool call would pass the warrant, without making it: the same check the hook runs before a call."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "tool" (nlk:json-object "type" "string"
                                                "description" "the tool name, as a call would carry it")
                       "arguments" (nlk:json-object "type" "string"
                                                     "description" "the call's arguments, as a JSON object"))
         "required" (vector "tool")))
  "The tenuo_check boundary, in the schema the provider reads.")

(defparameter +audit-schema+
  (list :description
        "Read the local audit log: one line per tool call the warrant allowed or refused, newest last."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "denied_only" (nlk:json-object "type" "boolean"
                                                        "description" "only the calls the warrant refused")
                       "limit" (nlk:json-object "type" "integer"
                                                 "description" "how many of the newest records to read"))
         "required" (vector)))
  "The tenuo_audit boundary, in the schema the provider reads.")

(defun member-text (arguments name default)
  "ARGUMENTS' member NAME as text, or DEFAULT when the call did not carry it."
  (let ((value (and (hash-table-p arguments) (gethash name arguments))))
    (if (stringp value) value default)))

(defun check-call (request)
  (let* ((tool (member-text request "tool" ""))
         (text (member-text request "arguments" "{}"))
         (arguments (handler-case (let ((value (nlk:decode-json text :whole t)))
                                    (if (hash-table-p value) value (nlk:json-object)))
                      (error () (nlk:json-object)))))
    (multiple-value-bind (decision reason) (decide tool arguments)
      (if (eq decision :allow)
          (format nil "allowed: ~a" reason)
          (format nil "refused: ~a" reason)))))

(defun read-audit (request)
  (let ((limit (and (hash-table-p request) (gethash "limit" request)))
        (denied-only (and (hash-table-p request) (gethash "denied_only" request))))
    (audit-text :limit (if (and (integerp limit) (plusp limit)) limit 10)
                :denied-only (and denied-only t))))

(defun status-text ()
  "What /tenuo says: the warrant, its expiry, its signature, and the count of
denials in the log."
  (let* ((path (setting :warrant))
         (policy (getf *tenuo* :warrant))
         (key-env (setting :key-env)))
    (with-output-to-string (out)
      (format out "tenuo: ~a~%"
              (cond ((null path) "no warrant set -- every tool call passes (a no-op)")
                    ((getf policy :error) (format nil "the warrant is unreadable: ~a -- every tool call is refused"
                                                  (getf policy :error)))
                    (t (format nil "~a: ~d tool~:p named~@[, expires ~a~]"
                               (file-namestring path)
                               (hash-table-count (getf policy :tools))
                               (and (getf policy :expires)
                                    (iso-time (getf policy :expires)))))))
      (format out "~&signature: ~a~%"
              (if (null path)
                  "n/a"
                  (let ((state (signature-state path key-env)))
                    (if (getf state :signed) "verified" (getf state :why)))))
      (format out "~&on denial: ~a~%" (setting :on-denial))
      (format out "~&audit log: ~a~%" (audit-path))
      (let ((records (audit-records :limit 1000)))
        (format out "~&~d decision~:p recorded, ~d refused~%"
                (length records)
                (count-if (lambda (record) (equal "DENY" (gethash "decision" record)))
                          records))))))

(defun run-slash (args session-id)
  "The /tenuo command: no argument is the status, \"audit\" the newest
decisions, \"sign\" signs the warrant, and a tool name asks whether a call
would pass."
  (declare (ignore session-id))
  (let ((args (string-trim '(#\Space #\Tab) (or args ""))))
    (cond ((string= args "") (status-text))
          ((string-equal args "audit") (audit-text :limit 20))
          ((string-equal args "denied") (audit-text :limit 20 :denied-only t))
          ((string-equal args "sign")
           (let ((path (setting :warrant)))
             (if (null path)
                 "tenuo: no warrant is set"
                 (sign-warrant path (setting :key-env)))))
          (t (multiple-value-bind (decision reason) (decide args (nlk:json-object))
               (format nil "~(~a~): ~a" decision reason))))))

(defparameter +primer+
  "tenuo: a warrant file names the tools a call may use and, per tool, the
arguments it allows; every call is checked against it before it runs, and each
decision goes to a local audit log. A refused call answers ERROR: refused by
nodecode-tenuo and did not run: do not retry it, ask the operator. Read the
policy with /tenuo, tenuo_check (would this call pass?) and tenuo_audit."
  "What (help :tenuo) answers and every request's help line carries.")

(nle:define-cell tenuo
  (:section ("tenuo")
    (:guide "point warrant at a JSON warrant file; the key_env key signs it if you want it signed; on_denial log only watches")
    ("warrant" :path :required t
     :doc "the JSON warrant that names what a tool call may do, and until when")
    ("key_env" :env
     :doc "environment variable holding the HMAC key the warrant is signed with")
    ("audit_log" :path
     :doc "where decisions are appended; default tenuo/audit.jsonl under the Nodecode home")
    ("on_denial" :choice :options '("block" "log") :default "block"
     :doc "block refuses the call; log records the denial and lets it run"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let* ((path (getf values :warrant))
                      (real (and path (probe-file path))))
                 (append (list :warrant (and real (read-warrant real))
                               :signature (and real
                                               (let ((state (signature-state real (getf values :key-env))))
                                                 (list :required (and (getf values :key-env) t)
                                                       :signed (getf state :signed)
                                                       :why (getf state :why)))))
                         values))))
  (:tool "tenuo_check" +check-schema+ 'check-call)
  (:tool "tenuo_audit" +audit-schema+ 'read-audit)
  (:help :tenuo "tenuo: a warrant checked before every tool call runs" +primer+)
  (:hook :tool #'enforce)
  (:command "tenuo" #'run-slash
            :description "The warrant, its audit log, and whether a call would pass")
  (:start (lambda ()
            (let ((path (setting :warrant)))
              (cond ((null path)
                     (nle:notice "tenuo: no warrant set -- every tool call passes"
                                 :key "nodecode-tenuo"))
                    ((getf (getf *tenuo* :warrant) :error)
                     (nle:notice (format nil "tenuo: ~a -- every tool call is refused"
                                         (getf (getf *tenuo* :warrant) :error))
                                 :key "nodecode-tenuo"))
                    (t (let ((state (signature-state path (setting :key-env))))
                         (unless (or (getf state :signed) (null (setting :key-env)))
                           (nle:notice (format nil "tenuo: ~a" (getf state :why))
                                       :level :warning :key "nodecode-tenuo")))))
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-tenuo"))))))
)