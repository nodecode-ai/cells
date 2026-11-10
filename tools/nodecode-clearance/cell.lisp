;;;; cell.lisp --- per-sender tool permissions for Nodecode's chat surfaces.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of yuzuru99's hermes-clearance. Every tool call is checked in
;;;; code against the tier of the person whose message the turn answers, so a
;;;; prompt cannot talk its way to a tool its sender's tier does not name:
;;;; decisions are deny-by-default, deny beats allow, and a policy that cannot
;;;; be read fails closed for everyone but the operator's own session.
;;;;
;;;; Where the original read the gateway's session identity out of the
;;;; environment, this reads the session the running turn belongs to --
;;;; (NLE:TURN), bound by the engine on any thread a turn runs a call on -- and
;;;; asks the channel kit which lane, and so which sender, holds it. A session
;;;; no channel claims is the operator's own, and the operator is the owner; a
;;;; session a channel claims is a room turn, at its sender's tier.
;;;;
;;;; Data over code: the tiers, the groups and the platform prefixes are
;;;; DEFPARAMETERs, so a layer can rebind them without touching a function.

(defpackage #:nodecode-clearance
  (:use #:cl))

(in-package #:nodecode-clearance)

(nlk:define-peripheral clearance :not-running t)

;;; --- the tiers ---------------------------------------------------------------

(defparameter +tier-order+ '("blocked" "guest" "user" "admin" "owner")
  "The built-in tiers, weakest first: the order /clearance tiers lists them in.")

(defparameter +default-tiers+
  '(("owner"   :allow ("*") :deny () :rate 0 :chat t :manage t :audit t)
    ("admin"   :allow ("*") :deny () :rate 0 :chat t :manage nil :audit t)
    ("user"    :allow ("@web" "@media" "@memory" "@skills") :deny () :rate 60
     :chat t :manage nil :audit nil)
    ("guest"   :allow ("@chat") :deny () :rate 20 :chat t :manage nil :audit nil)
    ("blocked" :allow () :deny ("*") :rate 0 :chat nil :manage nil :audit nil))
  "The five tiers and the fields a `tiers` override changes one at a time.
The owner's row is put back to full access at every policy build, so a typo in
the config can never lock the owner out.")

(defparameter +groups+
  '(("@eval" "eval")
    ("@shell" "eval")
    ("@web" "web_*" "search_*" "fetch_*" "perplexity_*")
    ("@media" "image_*" "video_*" "audio_*" "speech_*" "tts_*" "media_*")
    ("@memory" "memory*" "recall*" "session_search")
    ("@skills" "skill*")
    ("@schedule" "cron_*" "goal_*" "prs_*" "qa_*")
    ("@mcp" "mcp__*")
    ("@chat" "ask" "clarify" "todo_*"))
  "Shorthands usable in a tier's allow and deny lists. Anything else is a tool
name or a glob. `eval` is the one tool that reaches the whole machine, which
is why it is a group of its own.")

(defparameter +local-platforms+ '("" "cli" "tui" "desktop" "local" "acp" "web")
  "Platforms whose turns come from the operator's own machine or page.")

(defparameter +known-platforms+ '("telegram" "discord" "slack" "matrix" "whatsapp"
                                  "imessage" "email" "x")
  "Session-id prefixes that are chat platforms, read when the channel cells are
off or a session has no lane. A running channel cell's lane answers the
platform and the sender exactly, and then this list is not consulted.")

(defparameter +audit-max-bytes+ (* 2 1024 1024)
  "The audit log is moved aside to .1 past this size, as the original did.")

;;; --- the policy's shape ------------------------------------------------------

(defstruct (tier (:copier nil))
  "One tier: what it may call, how often, and what it may look at."
  (name "guest" :type string)
  (allow '() :type list)
  (deny '() :type list)
  (rate 0 :type integer)
  (chat t)
  (manage nil)
  (audit nil))

(defstruct (policy (:copier nil))
  "What one decision reads: the tiers, and who is where."
  (tiers (make-hash-table :test #'equal))
  (assigned (make-hash-table :test #'equal))
  (default-tier nil)
  (local-tier nil))

(defvar *assigned* (make-hash-table :test #'equal)
  "Sender key -> tier, seeded from `users` at start, changed by /clearance set.")

(defvar *hits* (make-hash-table :test #'equal)
  "Sender key -> the universal times of its turns in the last hour.")

;;; --- text and tools ----------------------------------------------------------

(defun words (text)
  "TEXT's blank-separated words."
  (let ((words '()) (start nil))
    (loop for i from 0 to (length text)
          for ch = (and (< i (length text)) (char text i))
          do (if (and ch (not (member ch '(#\Space #\Tab #\Newline #\Return))))
                 (unless start (setf start i))
                 (when start (push (subseq text start i) words) (setf start nil))))
    (nreverse words)))

(defun prefix-p (prefix text)
  "Whether TEXT starts with PREFIX and PREFIX ends there or at a dash."
  (and (stringp text)
       (<= (length prefix) (length text))
       (string-equal prefix text :end2 (length prefix))
       (or (= (length text) (length prefix))
           (char= (char text (length prefix)) #\-))))

(defun wildcard-p (text pattern)
  "TEXT against PATTERN, where `*` is any run and `?` any one character."
  (let ((at 0) (px 0) (star nil) (mark 0))
    (loop
      (cond ((>= at (length text))
             (loop while (and (< px (length pattern)) (char= (char pattern px) #\*))
                   do (incf px))
             (return (= px (length pattern))))
            ((and (< px (length pattern)) (char= (char pattern px) #\*))
             (setf star px mark at) (incf px))
            ((and (< px (length pattern))
                  (or (char= (char pattern px) #\?)
                      (char-equal (char pattern px) (char text at))))
             (incf at) (incf px))
            (star (setf px (1+ star) at (1+ mark) mark at))
            (t (return nil))))))

(defun expand (patterns)
  "PATTERNS with every group shorthand written out as the tool names it spans."
  (loop for pattern in patterns
        for name = (string-trim " " (princ-to-string pattern))
        when (plusp (length name))
          append (or (cdr (assoc name +groups+ :test #'string-equal)) (list name))))

(defun matching-pattern (tool patterns)
  "The first of PATTERNS, group or glob, that names TOOL, or NIL."
  (loop for pattern in patterns
        when (some (lambda (form) (wildcard-p tool form)) (expand (list pattern)))
          do (return pattern)))

(defun decide (tier tool)
  "Whether TOOL may run at TIER, and the rule that decided it.
=> (values ALLOWED RULE): deny beats allow, and a tool no rule names is denied."
  (let ((denied (matching-pattern tool (tier-deny tier))))
    (if denied
        (values nil (format nil "deny ~a" denied))
        (let ((allowed (matching-pattern tool (tier-allow tier))))
          (if allowed
              (values t (format nil "allow ~a" allowed))
              (values nil "default-deny"))))))

(defun describe-tier (tier)
  "TIER in one line: what it may call."
  (cond ((and (null (tier-deny tier)) (member "*" (tier-allow tier) :test #'string=))
         "all tools")
        ((not (tier-chat tier)) "no access")
        (t (format nil "~{~a~^, ~}~@[; denied: ~{~a~^, ~}~]"
                   (or (tier-allow tier) '("no tools")) (tier-deny tier)))))

(defun as-list (value)
  "VALUE, a JSON array or a comma-separated string, as a list of names."
  (let ((names (cond ((stringp value)
                      (words (substitute #\Space #\, value)))
                     ((vectorp value)
                      (loop for item across value
                            when (stringp item) collect (string-trim " " item)))
                     ((consp value)
                      (loop for item in value
                            when (stringp item) collect (string-trim " " item)))
                     (t '()))))
    (remove "" names :test #'string=)))

(defun as-integer (value default)
  "VALUE as a whole number no smaller than zero, or DEFAULT."
  (let ((number (typecase value
                  (integer value)
                  (real (round value))
                  (string (ignore-errors (parse-integer value :junk-allowed t)))
                  (t nil))))
    (if (and number (>= number 0)) number default)))

(defun tier-from-spec (name spec)
  "The tier NAME runs at, out of its SPEC."
  (make-tier :name name
             :allow (as-list (getf spec :allow))
             :deny (as-list (getf spec :deny))
             :rate (as-integer (getf spec :rate) 0)
             :chat (getf spec :chat)
             :manage (getf spec :manage)
             :audit (getf spec :audit)))

(defun copy-map (table)
  "A shallow copy of TABLE."
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p table)
      (loop for key being the hash-keys of table using (hash-value value)
            do (setf (gethash key copy) value)))
    copy))

;;; --- the policy --------------------------------------------------------------

(defun build-tiers (overrides warnings)
  "The built-in tiers with OVERRIDES applied field by field, as a hash of name
to spec; a tier the config names that is not built in starts as a guest's."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name . spec) in +default-tiers+
          do (setf (gethash name table) (copy-list spec)))
    (when (hash-table-p overrides)
      (loop for name being the hash-keys of overrides using (hash-value spec)
            for key = (string-downcase (string-trim " " name))
            do (if (not (hash-table-p spec))
                   (push (format nil "tiers.~a is not an object; ignored" key) warnings)
                   (let ((base (or (gethash key table)
                                   (copy-list (rest (assoc "guest" +default-tiers+
                                                           :test #'string=))))))
                     (setf (gethash key table) base)
                     (loop for field being the hash-keys of spec using (hash-value value)
                           do (cond ((string= field "allow")
                                     (setf (getf base :allow) (as-list value)))
                                    ((string= field "deny")
                                     (setf (getf base :deny) (as-list value)))
                                    ((string= field "rate_per_hour")
                                     (setf (getf base :rate) (as-integer value 0)))
                                    ((member field '("chat" "manage" "audit") :test #'string=)
                                     (setf (getf base (intern (string-upcase field) :keyword))
                                           (and value t)))
                                    (t (push (format nil "tiers.~a.~a is not a field; ignored"
                                                     key field)
                                             warnings))))))))
    (setf (gethash "owner" table)
          (list :allow '("*") :deny '() :rate 0 :chat t :manage t :audit t))
    table))

(defun build-policy (warnings)
  "The policy the running cell reads: its tiers, who is assigned to which, and
the tiers a local session and an unnamed sender get."
  (let* ((specs (build-tiers (setting :tier-table) warnings))
         (rows (make-hash-table :test #'equal)))
    (loop for name being the hash-keys of specs using (hash-value spec)
          do (setf (gethash name rows) (tier-from-spec name spec)))
    (flet ((named (key default)
             (let ((name (string-downcase (or (setting key) default))))
               (or (gethash name rows)
                   (progn (push (format nil "~a ~s is not a tier; using ~s"
                                        key name default)
                                warnings)
                          (gethash default rows))))))
      (make-policy :tiers rows
                   :assigned (copy-map *assigned*)
                   :default-tier (named :default-tier "guest")
                   :local-tier (named :local-tier "owner")))))

(defun tier-named (policy name)
  "POLICY's tier NAME, or its guest tier: an unknown name never widens access."
  (or (gethash (string-downcase (or name "")) (policy-tiers policy))
      (gethash "guest" (policy-tiers policy))
      (tier-from-spec "guest" (rest (assoc "guest" +default-tiers+ :test #'string=)))))

(defun tier-for (policy identity)
  "The tier IDENTITY's turns run at."
  (ecase (getf identity :kind)
    (:local (policy-local-tier policy))
    (:unknown (policy-default-tier policy))
    (:gateway (let ((key (getf identity :key))
                    (platform (getf identity :platform)))
                (tier-named policy (or (gethash key (policy-assigned policy))
                                       (gethash (format nil "~a:*" platform)
                                                (policy-assigned policy))
                                       (gethash "*" (policy-assigned policy))
                                       (tier-name (policy-default-tier policy))))))))

(defun known-tier-p (policy name)
  "Whether POLICY holds a tier NAME."
  (and (gethash (string-downcase (or name "")) (policy-tiers policy)) t))

(defun tier-weakness (a b)
  "Whether tier A ranks weaker than tier B."
  (let ((ia (or (position a +tier-order+ :test #'string=) (length +tier-order+)))
        (ib (or (position b +tier-order+ :test #'string=) (length +tier-order+))))
    (if (= ia ib) (string< a b) (< ia ib))))

(defun tier-picker (policy)
  "The tier names POLICY holds, weakest first."
  (sort (loop for name being the hash-keys of (policy-tiers policy) collect name)
        #'tier-weakness))

;;; --- who is talking ----------------------------------------------------------

(defun lane-speaker (session-id)
  "The (values PLATFORM USER-ID) of the channel lane SESSION-ID belongs to, or
NIL: read off the channel kit's own lane table, so a room turn is attributed
the way the room attributes it. Nothing here needs the kit to be running."
  (let ((package (find-package "NODECODE-CHANNEL-KIT")))
    (when (and package (stringp session-id))
      (flet ((sym (name) (find-symbol name package)))
        (let ((hosts (sym "*HOSTS*")) (lanes (sym "HOST-LANES")) (host-id (sym "HOST-ID"))
              (find-lane (sym "FIND-LANE")) (owner (sym "LANE-OWNER-ID")))
          (when (and hosts (boundp hosts) lanes (fboundp lanes) host-id (fboundp host-id)
                     find-lane (fboundp find-lane) owner (fboundp owner))
            (loop for host in (symbol-value hosts)
                  for lane = (ignore-errors (funcall find-lane (funcall lanes host) session-id))
                  when lane
                    do (return (values (funcall host-id host) (funcall owner lane))))))))))

(defun session-platform (session-id)
  "The chat platform SESSION-ID's prefix names, or NIL for a local session."
  (let ((text (or session-id "")))
    (loop for prefix in (or (ignore-errors (setting :platforms)) +known-platforms+)
          for name = (string-downcase (princ-to-string prefix))
          when (prefix-p name text)
            do (return name))))

(defun make-identity (platform user-id)
  "The identity PLATFORM and USER-ID name, as the original keyed one:
`platform:user_id`, `local`, or `platform:?` when the sender is unknown."
  (let ((platform (string-downcase (or platform "")))
        (user-id (string-trim " " (or user-id ""))))
    (cond ((and (member platform +local-platforms+ :test #'string=)
                (zerop (length user-id)))
           (list :kind :local :key "local" :platform platform))
          ((zerop (length user-id))
           (list :kind :unknown :platform platform
                 :key (format nil "~a:?" (if (zerop (length platform)) "gateway" platform))))
          (t (list :kind :gateway :platform platform
                   :key (format nil "~a:~a" platform user-id))))))

(defun identity-of (session-id)
  "Who SESSION-ID's turn answers: a plist of :KIND, :KEY and :PLATFORM.
A local session is the operator's own; a room session nobody assigned a tier
falls to the default tier, and is never guessed into the owner's."
  (multiple-value-bind (platform user-id) (lane-speaker session-id)
    (cond (platform (make-identity platform user-id))
          ((session-platform session-id) (make-identity (session-platform session-id) ""))
          (t (make-identity "" "")))))

(defun identity-label (identity)
  "IDENTITY as a sentence names it."
  (getf identity :key))

(defun current-session-id ()
  "The session the running turn belongs to, or NIL outside a turn."
  (ignore-errors (getf (nle:turn) :session-id)))

(defun session-policy (session-id)
  "The policy and identity SESSION-ID's command reads, and the warnings."
  (let ((warnings '()))
    (values (build-policy warnings) (identity-of session-id) warnings)))

;;; --- rate limits and the audit log -------------------------------------------

(defun rate-check (key limit)
  "Whether KEY's turn may run within LIMIT per hour, and the seconds until the
next slot frees. LIMIT of zero or less is no limit."
  (if (or (null limit) (<= limit 0))
      (values t 0)
      (let* ((now (get-universal-time))
             (hits (remove-if (lambda (at) (>= (- now at) 3600)) (gethash key *hits*))))
        (if (>= (length hits) limit)
            (values nil (max 1 (- 3600 (- now (reduce #'min hits)))))
            (progn (setf (gethash key *hits*) (cons now hits))
                   (values t 0))))))

(defun rate-used (key)
  "How many of KEY's turns the last hour holds."
  (let ((now (get-universal-time)))
    (count-if (lambda (at) (< (- now at) 3600)) (gethash key *hits*))))

(defun audit-path ()
  "The cell's own audit file, in the operator's home rather than the folder."
  (nlk:home "clearance/audit.jsonl"))

(defun stamp-now ()
  "Now as an ISO-8601 stamp, to the second."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun field-name (key)
  "KEY as a JSON member name: lowercase, dashes as underscores."
  (substitute #\_ #\- (string-downcase (princ-to-string key))))

(defun rotate-audit (path)
  "Move PATH aside as .1 once it is past its size, as the original did."
  (when (and (probe-file path)
             (> (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))
                +audit-max-bytes+))
    (ignore-errors (rename-file path (concatenate 'string (namestring path) ".1")))))

(defun audit-event (event identity tier &rest fields)
  "Append one line to the audit log -- names, never argument values -- when
`audit` is on. Auditing that fails never breaks a turn."
  (when (ignore-errors (setting :audit))
    (handler-case
        (let ((path (audit-path)))
          (rotate-audit path)
          (nlk:append-json-line
           path
           (apply #'nlk:make-json-object
                  "ts" (stamp-now) "event" event
                  "who" (getf identity :key) "tier" (tier-name tier)
                  (loop for (key value) on fields by #'cddr
                        append (list (field-name key)
                                     (if (listp value)
                                         (coerce (mapcar (lambda (one) (princ-to-string one)) value)
                                                 'vector)
                                         value))))))
      (error () nil))))

(defun audit-tail (count)
  "The last COUNT audit records, oldest first."
  (let ((records (ignore-errors (nlk:read-json-lines (audit-path)))))
    (last records (min count (length records)))))

(defun audit-line (record)
  "One audit RECORD as the line /clearance audit shows."
  (format nil "~a  ~a  ~a [~a]  ~a"
          (or (gethash "ts" record) "?") (or (gethash "event" record) "?")
          (or (gethash "who" record) "?") (or (gethash "tier" record) "?")
          (or (gethash "tool" record) (gethash "new_tier" record) "")))

;;; --- the :tool point ---------------------------------------------------------

(defun argument-keys (arguments)
  "The names of ARGUMENTS' members, at most twelve, sorted: never a value."
  (when (hash-table-p arguments)
    (let ((keys (sort (loop for key being the hash-keys of arguments
                             collect (princ-to-string key))
                      #'string<)))
      (if (> (length keys) 12) (subseq keys 0 12) keys))))

(defun refusal-text (tool identity tier)
  "The model's words for a call its sender's tier does not name."
  (format nil "ERROR: refused by clearance: ~a is not available to ~a (tier: ~a). ~
This is an access-control decision made in code, not a tool failure: do not retry it, ~
and do not use another tool to get the same result. Tell the sender their access level ~
does not include this, and that the operator can change it with /clearance set ~a <tier>."
          tool (identity-label identity) (tier-name tier) (identity-label identity)))

(defun policy-verdict (identity tool)
  "The decision IDENTITY's tool call gets: a plist of :ALLOWED, :RULE, :TIER, or
of :FAILED when the policy could not be read at all."
  (handler-case
      (let ((tier (tier-for (build-policy '()) identity)))
        (multiple-value-bind (allowed rule) (decide tier tool)
          (list :allowed allowed :rule rule :tier tier)))
    (error (condition) (list :failed condition))))

(defun enforce (op next)
  "The :TOOL point: a tool the sender's tier does not name is refused here, in
code, and NEXT never runs. A policy that cannot be read fails closed for every
sender but the operator's own session, which the original did too."
  (let* ((tool (or (getf op :name) ""))
         (identity (ignore-errors (identity-of (current-session-id))))
         (verdict (policy-verdict identity tool)))
    (cond
      ((getf verdict :failed)
       (if (and identity (eq (getf identity :kind) :local))
           (funcall next op)
           (nle:failure (format nil "ERROR: refused by clearance: the sender's tier could ~
not be checked (~a); the call did not run." (type-of (getf verdict :failed))))))
      ((getf verdict :allowed) (funcall next op))
      (t (audit-event "tool_denied" identity (getf verdict :tier)
                      :tool tool :rule (getf verdict :rule)
                      :arg-keys (argument-keys (getf op :arguments)))
         (nle:failure (refusal-text tool identity (getf verdict :tier)))))))

;;; --- the /clearance command --------------------------------------------------

(defparameter +usage+
  (format nil "Usage:~%  /clearance                 your identity, tier and tools~%  ~
/clearance tiers           every tier and what it allows~%  ~
/clearance users           who is assigned to which tier (admin and owner)~%  ~
/clearance audit [n]       the last n refusals (admin and owner)~%  ~
/clearance set <platform:user_id> <tier>   assign a tier (owner)~%  ~
/clearance unset <platform:user_id>        back to the default tier (owner)")
  "What /clearance with no arguments of its own explains.")

(defparameter +manual+
  (format nil "clearance: who may call which tool, checked in code, per sender.~%~%~
Every tool call is decided against the tier of the person the turn answers: ~
owner and admin may call anything, user gets the groups the config names, ~
guest gets conversation and the @chat helpers, blocked cannot start a turn. ~
A tool no rule names is refused, deny beats allow, and a tool call outside a ~
turn is the operator's own, at the owner's tier.~%~%~
  /clearance                 your key, tier, tools and rate~%  ~
/clearance tiers           every tier, its tools and its hourly limit~%  ~
/clearance users           who is assigned where (admin, owner)~%  ~
/clearance audit [n]       the last refusals, names only (admin, owner)~%  ~
/clearance set <key> <tier>   assign a sender, e.g. telegram:123456789 admin~%  ~
/clearance unset <key>     back to the default tier~%~%~
Keys are `platform:user_id`, `platform:*` for a whole platform, or `*` for ~
everyone; the config's `users` list holds them as key=tier entries, and ~
/clearance set writes one. Tiers and their tools live in the config's ~
`tiers` object, field by field: allow, deny, rate_per_hour, chat, manage, ~
audit. Groups: @eval, @shell, @web, @media, @memory, @skills, @schedule, ~
@mcp, @chat. This is not a sandbox: an allowed tool keeps its whole reach, ~
so keep restricted tiers small.")
  "The manual topic /clearance answers with, and the summary every request
carries beside it.")

(defun me-text (identity tier)
  "What /clearance says about the person who typed it."
  (format nil "You are ~a.~%Tier: ~a~%Tools: ~a~%Rate: ~a"
          (identity-label identity) (tier-name tier) (describe-tier tier)
          (if (plusp (tier-rate tier))
              (format nil "~d/~d turns this hour"
                      (rate-used (getf identity :key)) (tier-rate tier))
              "no rate limit")))

(defun tiers-text (policy)
  "What /clearance tiers lists."
  (format nil "Tiers:~%~{~a~}~%Defaults: unknown senders = ~a, local sessions = ~a."
          (loop for name in (tier-picker policy)
                for tier = (gethash name (policy-tiers policy))
                collect (format nil "  ~a: ~a~@[, ~d/h~]"
                                name (describe-tier tier)
                                (and (plusp (tier-rate tier)) (tier-rate tier))))
          (tier-name (policy-default-tier policy))
          (tier-name (policy-local-tier policy))))

(defun users-text (policy)
  "What /clearance users lists."
  (let ((rows (sort (loop for key being the hash-keys of (policy-assigned policy)
                           collect key)
                     #'string<)))
    (if (null rows)
        (format nil "No senders assigned: everyone in a room is ~a."
                (tier-name (policy-default-tier policy)))
        (format nil "Senders:~%~{~a~}"
                (loop for key in rows
                      collect (format nil "  ~a -> ~a" key
                                      (gethash key (policy-assigned policy))))))))

(defun audit-text (count)
  "What /clearance audit shows."
  (let ((records (audit-tail count)))
    (if (null records)
        "The audit log is empty."
        (format nil "The last ~d audit events:~%~{~a~}" (length records)
                (mapcar #'audit-line records)))))

(defun user-entries ()
  "The `users` list to write back: every assignment, as key=tier, sorted."
  (sort (loop for key being the hash-keys of *assigned* using (hash-value tier)
              collect (format nil "~a=~a" key tier))
        #'string<))

(defun save-users ()
  "Write the assignments into the shared config; NIL when the write failed,
and never an error: the change this session made stands either way."
  (handler-case
      (progn (nodecode.evolved:config-set '("clearance" "users")
                                          (coerce (user-entries) 'vector))
             t)
    (error () nil)))

(defun normalize-key (text)
  "TEXT as a sender key: `platform:user_id`, `platform:*`, or `*`; NIL for
anything else."
  (let ((text (string-downcase (string-trim " " (or text "")))))
    (cond ((string= text "*") "*")
          ((not (find #\: text)) nil)
          (t (let ((colon (position #\: text)))
               (when (and (plusp colon) (< (1+ colon) (length text)))
                 (format nil "~a:~a" (subseq text 0 colon)
                         (string-trim " " (subseq text (1+ colon))))))))))

(defun read-user-entry (entry warnings)
  "The (key . tier) a `key=tier` ENTRY names, or NIL, with WARNINGS."
  (let* ((entry (string-trim " " (princ-to-string entry)))
         (equals (position #\= entry :from-end t)))
    (cond
      ((null equals)
       (push (format nil "users: ~s is not key=tier; ignored" entry) warnings) nil)
      (t (let ((key (normalize-key (subseq entry 0 equals)))
               (tier (string-downcase (string-trim " " (subseq entry (1+ equals))))))
           (cond ((null key)
                  (push (format nil "users: ~s is not platform:user_id; ignored" entry)
                        warnings)
                  nil)
                 ((string= tier "")
                  (push (format nil "users: ~s names no tier; ignored" entry) warnings)
                  nil)
                 (t (cons key tier))))))))

(defun seed-assignments (entries warnings)
  "Fill *ASSIGNED* from the section's ENTRIES, warning about what does not read."
  (clrhash *assigned*)
  (dolist (entry (if (vectorp entries) (coerce entries 'list) entries))
    (let ((pair (read-user-entry entry warnings)))
      (when pair (setf (gethash (car pair) *assigned*) (cdr pair))))))

(defun assign-user (key tier-name)
  "Record KEY at TIER-NAME for this session, or forget it when the tier is NIL."
  (if tier-name
      (setf (gethash key *assigned*) tier-name)
      (remhash key *assigned*))
  key)

(defun assign-text (identity verb arguments)
  "What /clearance set|unset answers: the assignment, and whether it reached
the shared config or only this session."
  (unless arguments
    (return-from assign-text +usage+))
  (let ((key (normalize-key (first arguments)))
        (tier-name (and (string= verb "set") (second arguments)))
        (warnings '()))
    (unless key
      (return-from assign-text
        (format nil "~s is not platform:user_id (for example telegram:123456789)."
                (first arguments))))
    (when (and (string= verb "set") (null tier-name))
      (return-from assign-text +usage+))
    (when (and (string= verb "set")
               (not (known-tier-p (build-policy warnings) tier-name)))
      (return-from assign-text
        (format nil "Unknown tier ~s. Tiers: ~{~a~^, ~}." tier-name
                (tier-picker (build-policy warnings)))))
    (when (and (string= verb "unset") (not (gethash key *assigned*)))
      (return-from assign-text (format nil "~a has no tier assigned." key)))
    (assign-user key (and (string= verb "set") (string-downcase tier-name)))
    (let ((policy (build-policy warnings)))
      (audit-event "tier_set" identity (tier-for policy identity)
                   :target key
                   :new-tier (if (string= verb "set")
                                 (string-downcase tier-name)
                                 (tier-name (policy-default-tier policy))))
      (format nil "~a is now ~a.~@[~%~a~]"
              key (if (string= verb "set")
                      (string-downcase tier-name)
                      (tier-name (policy-default-tier policy)))
              (unless (save-users)
                "Saved for this session: the shared config was not writable.")))))

(defun run-clearance (args session-id)
  "The /clearance command: what you are, what each tier may call, who is
assigned, the audit log, and the two assignments only the owner may make."
  (multiple-value-bind (policy identity warnings) (session-policy session-id)
    (declare (ignore warnings))
    (let* ((words (words (or args "")))
           (verb (string-downcase (or (first words) "me")))
           (tier (tier-for policy identity))
           (count (or (and (second words) (ignore-errors (parse-integer (second words))))
                      10)))
      (handler-case
          (cond ((member verb '("me" "whoami") :test #'string=) (me-text identity tier))
                ((string= verb "tiers") (tiers-text policy))
                ((member verb '("help" "-h" "--help") :test #'string=) +usage+)
                ((string= verb "users")
                 (if (tier-audit tier)
                     (users-text policy)
                     (format nil "Only admin or owner can list senders (you are ~a)."
                             (tier-name tier))))
                ((string= verb "audit")
                 (if (tier-audit tier)
                     (audit-text (min (max count 1) 100))
                     (format nil "Only admin or owner can read the audit log (you are ~a)."
                             (tier-name tier))))
                ((member verb '("set" "unset") :test #'string=)
                 (if (tier-manage tier)
                     (assign-text identity verb (rest words))
                     (progn (audit-event "manage_denied" identity tier :command verb)
                            (format nil "Only the owner can change tiers (you are ~a)."
                                    (tier-name tier)))))
                (t +usage+))
        (error (condition)
          (format nil "clearance: ~a" condition))))))

;;; --- the cell ---------------------------------------------------------------

(nle:define-cell clearance
  (:section ("clearance")
    (:guide "leave it empty and every room sender is a guest while your own sessions are the owner; users assigns people as platform:user_id=tier; tiers changes a tier's allow, deny, rate_per_hour, chat, manage or audit")
    ("users" :list
     :doc "Assignments, one `platform:user_id=tier` each (`telegram:*` a whole platform, `*` everyone); /clearance set writes one")
    ("default_tier" :string :default "guest"
     :doc "The tier of a room sender nobody assigned, and of a turn whose sender is unknown")
    ("local_tier" :string :default "owner"
     :doc "The tier of the operator's own sessions: the TUI, the CLI, the desktop app and its page")
    ("platforms" :list
     :doc "Session-id prefixes that are chat platforms, read when a channel cell is off (telegram, discord, slack, ...)")
    ("audit" :boolean :default t
     :doc "Record refusals and tier changes, tool and argument names only, in the operator's home"))
  (:settings (lambda (values table)
               (let ((tiers (gethash "tiers" table)))
                 (list* :tier-table (and (hash-table-p tiers) tiers) values))))
  (:hook :tool #'enforce)
  (:command "clearance" #'run-clearance
            :description "Who may call which tool, per sender"
            :argument-hint "[me|tiers|users|audit [n]|set <platform:user_id> <tier>|unset <platform:user_id>]")
  (:help :clearance
   "clearance: per-sender tool tiers - (/clearance), (/clearance set telegram:123456789 admin)"
   +manual+)
  (:start (lambda ()
            (let ((warnings '()))
              (seed-assignments (or (ignore-errors (setting :users)) '()) warnings)
              (clrhash *hits*)
              ;; One policy built now, so a bad entry is said once, out loud,
              ;; rather than on the first refusal.
              (build-policy warnings)
              (when (and warnings (ignore-errors (setting :audit)))
                (nle:notice (format nil "clearance: ~{~a~^; ~}"
                                    (remove-duplicates warnings :test #'string=))
                            :level :warning :key "nodecode-clearance"))
              (nle:on-stop
               (lambda ()
                 (clrhash *assigned*)
                 (clrhash *hits*)
                 (nle:notice nil :key "nodecode-clearance")))))))
