;;;; cell.lisp --- the policy, the :tool veto, /household-tiers, the tool, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One shared bot serves a household: the owner, other people, guests, and
;;;; in a family a child with hours. The plugin this is a rewrite of kept that
;;;; policy in a JSON document beside the bot and answered every tool call from
;;;; it; this cell reads the same document, in the same shape, and vetoes at
;;;; Nodecode's own :tool point.
;;;;
;;;; The person is the CONVERSATION here, because a turn carries its session
;;;; and not a platform sender (NLE:TURN): the document's "sessions" map names
;;;; a session id, or a prefix of one ending in `*', to a tier. An id the
;;;; document does not name gets `default', and a turn with no session at all
;;;; is the local operator, the owner.
;;;;
;;;; A tier is the list of tools it may call, `["*"]' for every one; a tier
;;;; with "hours" may only call them inside its window. A call outside the
;;;; tier, or outside the window, is vetoed with (NLE:FAILURE ...) and
;;;; written to the audit ledger, and the model reads the veto as the failed
;;;; call it is. The policy document's own "audit" member turns the ledger
;;;; off and bounds what it keeps.
;;;;
;;;; Config, a sibling top-level key next to `index' and `cron':
;;;;   "household-tiers": {"policy_file": "household-tiers.json",
;;;;                       "default_tier": "guest", "enforce": true,
;;;;                       "audit": true}
;;;; The policy document itself is docs of its own; see config.example.jsonc
;;;; and the manual below. Nothing here leaves the machine.

(in-package #:nodecode-household-tiers)

;;; --- the manual -------------------------------------------------------------------------

(defparameter +manual+
  "Household Tiers gives each conversation with this bot a tier, and a tier decides what its
turns may do. A Nodecode rewrite of the Hermes plugin household-tiers.
  (household-tiers:summary &key days)   the household's last DAYS (default 7): who was blocked
      on what, and who wrote outside their hours; the owner's to read
/household-tiers            the same summary, for the owner
/household-tiers 2          the summary over the last two days
/household-tiers tier <session> <tier>   give one session a tier; not your own
/household-tiers forget <session>        delete that session's audit lines
A session is what a turn reports: a chat's room, a lane, a background job. The policy document
names a session id, or a prefix of one ending in `*', to a tier, and a tier lists the tools it
may call -- `*' for every one -- beside the hours it may call them in. A call the policy does not
allow is refused before it runs, and the model reads the refusal as the failed call it is.
ERROR: HOUSEHOLD-TIERS-ERROR when a verb runs with the cell stopped."
  "What (help :household-tiers) answers while the cell runs.")

;;; --- the policy document ------------------------------------------------------------------

(defparameter +default-tiers+
  '(("owner" . ("*"))
    ("user" . ("web_search" "web_extract"))
    ("kid" . ("web_search" "web_extract"))
    ("guest"))
  "Tier -> the tools it may call, when the document declares no tiers of its own.
The original plugin's defaults, named for the tools a Nodecode session offers.")

(defparameter +own-tool+ "household_tiers"
  "The one tool this cell registers, always callable: its body gates on the caller's tier.")

(defun policy-path ()
  "The policy document the section names, under the Nodecode home when relative."
  (merge-pathnames (pathname (setting :policy-file)) (nlk:home)))

(defun policy-document ()
  "The policy document as decoded JSON, or an empty object when there is none."
  (let ((path (policy-path)))
    (if (probe-file path)
        (let ((doc (nlk:decode-json (nlk:read-text path) :whole t)))
          (if (hash-table-p doc) doc (nlk:json-object)))
        (nlk:json-object))))

(defun tier-table (doc)
  "The document's tiers over the defaults: tier name -> the tool names it may call."
  (let ((table (copy-tree +default-tiers+))
        (declared (nlk:json-value doc :any "tiers")))
    (when (hash-table-p declared)
      (maphash (lambda (name tools)
                 (let ((names (cond ((vectorp tools) (coerce tools 'list))
                                    ((stringp tools) (list tools))
                                    (t '())))
                       (entry (assoc name table :test #'equal)))
                   (if entry
                       (setf (cdr entry) (remove-if-not #'stringp names))
                       (push (cons name (remove-if-not #'stringp names)) table))))
               declared))
    table))

(defun tier-tools (tier doc)
  "The tools TIER may call, as the document names them."
  (cdr (assoc tier (tier-table doc) :test #'equal)))

(defun tier-names (doc)
  "Every tier the document holds, sorted."
  (sort (mapcar #'car (tier-table doc)) #'string<))

(defun allowed-p (tier tool doc)
  "Whether TIER may call TOOL."
  (let ((tools (tier-tools tier doc)))
    (or (member "*" tools :test #'equal) (member tool tools :test #'equal) nil)))

(defun session-tier (who doc)
  "The tier WHO (a session id) has: its own entry, the longest prefix entry
ending in `*' that matches it, else the document's default."
  (let* ((map (or (let ((s (nlk:json-value doc :any "sessions")))
                    (and (hash-table-p s) s))
                  (let ((s (nlk:json-value doc :any "users")))
                    (and (hash-table-p s) s))))
         (exact (and map (gethash who map)))
         (prefix (and (null exact) map (prefix-tier who map)))
         (default (or (let ((d (nlk:json-value doc :any "default")))
                        (and (stringp d) (plusp (length d)) d))
                      (setting :default-tier))))
    (or exact prefix default)))

(defun prefix-tier (who map)
  "The tier of the longest `prefix*' entry in MAP that WHO starts with, or NIL."
  (let ((best nil) (best-length -1))
    (maphash (lambda (key tier)
               (when (and (stringp key) (stringp tier)
                          (> (length key) 1)
                          (char= #\* (char key (1- (length key))))
                          (uiop:string-prefix-p (subseq key 0 (1- (length key))) who)
                          (> (length key) best-length))
                 (setf best tier best-length (length key))))
             map)
    best))

(defun tier-of (who doc)
  "The tier a session WHO runs under. No session at all is the local operator."
  (if (member who '("local") :test #'equal) "owner" (session-tier who doc)))

;;; --- hours -----------------------------------------------------------------------------

(defun parse-clock (text)
  "HH:MM as minutes since midnight, or NIL."
  (let* ((colon (and (stringp text) (position #\: text)))
         (hours (and colon (parse-integer text :end colon :junk-allowed t)))
         (minutes (and colon (parse-integer text :start (1+ colon) :junk-allowed t))))
    (and (integerp hours) (integerp minutes)
         (<= 0 hours 23) (<= 0 minutes 59)
         (+ (* hours 60) minutes))))

(defun parse-window (text)
  "HH:MM-HH:MM as (START . END) minutes since midnight, or NIL."
  (let* ((dash (and (stringp text) (position #\- text)))
         (start (and dash (parse-clock (subseq text 0 dash))))
         (end (and dash (parse-clock (subseq text (1+ dash))))))
    (and start end (cons start end))))

(defun tier-window (tier doc)
  "TIER's allowed window as (START . END) minutes, or NIL when it has none."
  (let* ((hours (nlk:json-value doc :any "hours"))
         (rule (and (hash-table-p hours) (nlk:json-value hours :any tier)))
         (allow (and (hash-table-p rule) (nlk:json-value rule :any "allow"))))
    (parse-window allow)))

(defun now-minutes ()
  "Minutes since midnight, local time."
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (+ (* hour 60) minute)))

(defun inside-window-p (window moment)
  "Whether MOMENT, minutes since midnight, is inside WINDOW; a window past
midnight is the two spans it covers."
  (destructuring-bind (start . end) window
    (if (<= start end)
        (and (<= start moment) (< moment end))
        (or (>= moment start) (< moment end)))))

;;; --- the audit ledger ---------------------------------------------------------------------

(defun audit-path ()
  "The decision ledger: one JSON line per veto, under the Nodecode home."
  (nlk:home "household-tiers-audit.jsonl"))

(defun retention-days (doc)
  "How many days the document keeps, or 30."
  (let* ((audit (nlk:json-value doc :any "audit"))
         (days (and (hash-table-p audit) (nlk:json-value audit :any "retention_days"))))
    (if (and (numberp days) (not (minusp days))) days 30)))

(defun audit-on-p (doc)
  "Whether a decision is written: the section's switch, and the document's,
which is on unless it says otherwise."
  (and (setting :audit)
       (let ((audit (nlk:json-value doc :any "audit")))
         (if (not (hash-table-p audit))
             t
             (multiple-value-bind (enabled present)
                 (nlk:json-value audit :any "enabled")
             (if present (and enabled t) t))))))

(defun record (doc &rest fields)
  "One decision line, when the audit is on and the ledger can be written."
  (ignore-errors
    (when (audit-on-p doc)
      (nlk:append-json-line
       (audit-path)
       (apply #'nlk:make-json-object (append (list "t" (get-universal-time)) fields))))))

(defun days-label (days)
  "DAYS as the summary spells it: a whole number, else one decimal."
  (if (= days (round days))
      (format nil "~d" (round days))
      (format nil "~,1f" days)))

(defun digest-text (doc days)
  "The household summary over the last DAYS, as /household-tiers prints it."
  (declare (ignore doc))
  (let ((since (- (get-universal-time) (round (* days 86400))))
        (order '())
        (blocks (make-hash-table :test #'equal))
        (curfews (make-hash-table :test #'equal)))
    (dolist (line (nlk:read-json-lines (audit-path)))
      (let ((stamp (nlk:json-value line :any "t")))
        (when (and (integerp stamp) (>= stamp since))
          (let ((who (or (nlk:json-value line :any "who") "?"))
                (decision (nlk:json-value line :any "decision")))
            (unless (member who order :test #'equal) (push who order))
            (cond
              ((equal decision "curfew") (incf (gethash who curfews 0)))
              ((equal decision "block")
               (let ((tool (or (let ((name (nlk:json-value line :any "tool")))
                                 (and (stringp name) name))
                               "?"))
                     (counts (or (gethash who blocks)
                                 (setf (gethash who blocks) (make-hash-table :test #'equal)))))
                 (incf (gethash tool counts 0)))))))))
    (if (null order)
        (format nil "Household summary (~a days): no events." (days-label days))
        (with-output-to-string (out)
          (format out "Household summary (~a days):" (days-label days))
          (dolist (who (nreverse order))
            (format out "~%- ~a: blocked ~a; outside hours ~a"
                    who (blocked-text (gethash who blocks)) (gethash who curfews 0)))))))

(defun blocked-text (counts)
  "One person's blocked tools, most blocked first, or `nothing'."
  (if (or (null counts) (zerop (hash-table-count counts)))
      "nothing"
      (format nil "~{~a~^, ~}"
              (loop for tool being the hash-keys of counts using (hash-value count)
                    collect (format nil "~a x~d" tool count)
                    into lines
                    finally (return (sort lines #'string<))))))

(defun purge (doc id)
  "Drop expired ledger lines, and ID's; -> how many of ID's went."
  (let ((cutoff (- (get-universal-time) (round (* (retention-days doc) 86400))))
        (forgotten 0)
        (kept '()))
    (dolist (line (nlk:read-json-lines (audit-path)))
      (let ((who (nlk:json-value line :any "who"))
            (target (nlk:json-value line :any "target"))
            (stamp (nlk:json-value line :any "t")))
        (cond
          ((and id (or (equal who id) (equal target id))) (incf forgotten))
          ((and (integerp stamp) (< stamp cutoff)) nil)
          (t (push line kept)))))
    (when (probe-file (audit-path))
      (ignore-errors
        (nlk:write-file-atomically
         (audit-path)
         (with-output-to-string (out)
           (dolist (line (nreverse kept)) (format out "~a~%" (nlk:encode-json-object line)))))))
    forgotten))

;;; --- the session a turn runs for --------------------------------------------------------

(defun current-session ()
  "The session this turn runs for, or NIL outside a turn (the local operator)."
  (getf (nle:turn) :session-id))

(defun who-now ()
  "The session id a call or a command runs for."
  (or (current-session) "local"))

;;; --- the veto ----------------------------------------------------------------------------

(defun veto-text (and-what)
  (format nil "ERROR: household-tiers: ~a" and-what))

(defun check-call (op next)
  "The :TOOL point: a call the caller's tier or its hours do not allow is the
call's failure, and NEXT never runs. A policy that cannot be read refuses
every call, the owner's included."
  (let* ((tool (getf op :name))
         (who (who-now)))
    (when (equal tool +own-tool+) (return-from check-call (funcall next op)))
    (when (not (setting :enforce)) (return-from check-call (funcall next op)))
    (let ((doc (handler-case (policy-document)
                 (error (e) (return-from check-call
                              (nle:failure
                               (veto-text (format nil "the policy could not be read (~a), so this call was refused." e))))))))
      (handler-case
          (let* ((tier (tier-of who doc))
                 (window (tier-window tier doc))
                 (moment (now-minutes)))
            (cond
              ((and window (not (inside-window-p window moment)))
               (record doc "decision" "curfew" "who" who "tier" tier "tool" tool "why" "curfew")
               (nle:failure (veto-text (format nil "~a (~a) is outside their hours (~a), so this call waits."
                                               who tier (window-text window)))))
              ((not (allowed-p tier tool doc))
               (record doc "decision" "block" "who" who "tier" tier "tool" tool "why" "tool_denied")
               (nle:failure (veto-text (format nil "~a (~a) may not use ~a. Ask the owner to change the tier."
                                               who tier tool))))
              (t (funcall next op))))
        (error (e)
          (nle:failure (veto-text (format nil "the policy could not be read (~a), so the call was refused." e))))))))

(defun window-text (window)
  "WINDOW as the HH:MM-HH:MM the document spells."
  (format nil "~2,'0d:~2,'0d-~2,'0d:~2,'0d"
          (floor (car window) 60) (mod (car window) 60)
          (floor (cdr window) 60) (mod (cdr window) 60)))

;;; --- the summary, for the model and for the operator -------------------------------------

(defun summary (&key (days 7))
  "The household's last DAYS: who was blocked on what, and who wrote outside
their hours. The account a household's owner asks for; => one block of text."
  (let* ((doc (policy-document))
         (who (who-now))
         (tier (tier-of who doc)))
    (if (equal tier "owner")
        (digest-text doc days)
        (format nil "Only the owner reads the household summary; this conversation is ~a." tier))))

(defparameter +tool-schema+
  (list :description
        "The household's last days: who was blocked on which tool, and who wrote outside their hours. The owner's to read; a conversation of another tier is told whose it is."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "days" (nlk:json-object "type" "number"
                                                "description" "how many days back to count, default 7"))
         "required" (vector)))
  "The household_tiers tool's advertised shape.")

(defun household-summary (args)
  "The household_tiers tool: SUMMARY over DAYS from its arguments."
  (let ((days (nlk:json-value args :any "days")))
    (summary :days (if (and (numberp days) (plusp days)) days 7))))

;;; --- /household-tiers ---------------------------------------------------------------------

(defparameter +owner-only+ "Only the owner can use /household-tiers."
  "What a conversation of another tier hears from the command.")

(defun argument-days (text)
  "TEXT as a number of days, or NIL."
  (let ((days (ignore-errors (read-from-string text))))
    (and (numberp days) (not (minusp days)) days)))

(defun set-session-tier (doc target tier)
  "Write TARGET's tier into DOC, which is then saved."
  (let* ((sessions (let ((s (nlk:json-value doc :any "sessions")))
                      (and (hash-table-p s) s)))
         (table (or sessions
                    (let ((fresh (nlk:json-object)))
                      (setf (gethash "sessions" doc) fresh)
                      fresh))))
    (setf (gethash target table) tier)
    doc))

(defun save-document (doc)
  "The document back to its file, atomically."
  (let ((path (policy-path)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path (nlk:encode-json-object doc))))

(defun run-command (args session-id)
  "One /household-tiers invocation: the summary, a tier changed, or one
session's audit lines forgotten. RUN takes the words typed after the command
and the session that typed them."
  (let* ((words (nlk:split-words (or args "")))
         (who (or session-id "local"))
         (first-word (string-downcase (or (first words) ""))))
    (handler-case
        (let* ((doc (policy-document))
               (tier (tier-of who doc))
               (owner (equal tier "owner")))
          (cond
            ((equal first-word "tier")
             (if (not owner)
                 +owner-only+
                 (command-tier doc who words)))
            ((equal first-word "forget")
             (if (not owner)
                 +owner-only+
                 (command-forget doc who words)))
            ((null words)
             (if owner (digest-text doc 7) +owner-only+))
            ((argument-days first-word)
             (if owner (digest-text doc (argument-days first-word)) +owner-only+))
            (t "Usage: /household-tiers [days] | tier <session> <tier> | forget <session>")))
      (error (e)
        (format nil "household-tiers: the policy could not be read (~a)." e)))))

(defun command-tier (doc who words)
  "`tier <session> <tier>': the tier DOC names, saved, and the line back."
  (let ((target (second words)) (new-tier (third words)))
    (cond
      ((or (null target) (null new-tier))
       "Usage: /household-tiers tier <session> <tier>")
      ((member target (list who) :test #'equal)
       "You cannot change your own tier from a conversation.")
      ((not (member new-tier (tier-names doc) :test #'equal))
       (format nil "Unknown tier. Available: ~{~a~^, ~}" (tier-names doc)))
      (t
       (save-document (set-session-tier doc target new-tier))
       (record doc "decision" "tier_change" "who" who "target" target "tier" new-tier "why" "tier_change")
       (format nil "Done: ~a is now ~a." target new-tier)))))

(defun command-forget (doc who words)
  "`forget <session>': that session's ledger lines gone, and the count back."
  (let ((target (second words)))
    (if (null target)
        "Usage: /household-tiers forget <session>"
        (let ((count (purge doc target)))
          (record doc "decision" "forget" "who" who "why" "forget" "count" count)
          (format nil "Forgot ~d audit entries for ~a." count target)))))

;;; --- the cell ----------------------------------------------------------------------------

(nle:define-cell household-tiers
  (:section ("household-tiers")
    (:guide "policy_file is the JSON document the tiers live in, under the Nodecode home when relative; default_tier is what an unlisted conversation gets; enforce off only watches")
    ("policy_file" :path :default "household-tiers.json" :doc "the policy document")
    ("default_tier" :choice :options '("owner" "user" "kid" "guest") :default "guest"
                    :doc "the tier of a conversation the policy does not name")
    ("enforce" :boolean :default t :doc "off logs nothing and refuses nothing")
    ("audit" :boolean :default t :doc "keep one line per decision in household-tiers-audit.jsonl"))
  (:tool "household_tiers" +tool-schema+ 'household-summary)
  (:command "household-tiers" 'run-command
            :description "Who the household blocked, and their hours"
            :argument-hint "[days] | tier <session> <tier> | forget <session>")
  (:help :household-tiers "household-tiers: per-conversation tool tiers and quiet hours - (household-tiers:summary)" +manual+)
  (:hook :tool #'check-call))
