;;;; cell.lisp --- what needs the operator, at which level, and when it is said.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An attention item is one thing the operator should know about: a question
;;;; or an approval that blocks the work (C3), a failure or an abnormal state
;;;; that needs a look (C2), an informational update (C1), or nothing at all
;;;; (C0). The cell keeps them in one local inbox, decides their level from the
;;;; reason and the authority of the evidence, refuses to shout during quiet
;;;; hours or past an hourly interrupt budget, and tells the operator once
;;;; through a standing notice. Nothing here dials anything: the inbox is one
;;;; JSON file under the home, mode 0600.

(in-package #:nodecode-deepcanary)

;;; --- the words ---------------------------------------------------------------

(defparameter +levels+ '("C1" "C2" "C3")
  "The levels a signal may carry: C1 informational, C2 a failure to look at,
C3 the work waits for the operator. C0 is a verdict, not an item: healthy work
needs no attention, so it is never written down.")

(defparameter +authorities+ '("host" "runtime" "derived" "heuristic")
  "How a signal was observed: host or runtime is evidence the harness itself
produced; derived is a conclusion from that evidence; heuristic is a guess. A
guess never makes C3.")

(defparameter +policy+
  ;; (REASON LEVEL ACTION WHY) -- the deterministic table, one place.
  '(("HUMAN_APPROVAL_REQUIRED" "C3" "ESCALATE" "an approval blocks the work")
    ("HUMAN_QUESTION_PENDING" "C3" "ESCALATE" "the work waits for an answer")
    ("HOST_UNREACHABLE" "C2" "INTERRUPT" "the host stopped answering")
    ("HOST_SUSPECTED_STALL" "C2" "INTERRUPT" "a task stopped making progress")
    ("TOOL_FAILURE_LOOP" "C2" "INTERRUPT" "a tool keeps failing")
    ("NO_MEANINGFUL_PROGRESS" "C2" "INTERRUPT" "a long run produced nothing new")
    ("TASK_FAILED" "C2" "INTERRUPT" "a task failed")
    ("COMPLETION_SUSPICIOUS" "C2" "INTERRUPT" "a task says it is done, the evidence disagrees")
    ("CONTEXT_PRESSURE" "C1" "INBOX" "the context window is filling")
    ("SUBAGENT_PRESSURE" "C1" "INBOX" "several subagents run at once")
    ("COMPACTION_OCCURRED" "C1" "DIGEST" "the context was compacted")
    ("TASK_COMPLETED" "C1" "DIGEST" "a task finished")
    ("TASK_ABORTED" "C1" "DIGEST" "a task was stopped")
    ("HOST_STALL_RECOVERED" "C1" "DIGEST" "the host answers again"))
  "The policy: every reason, the level it starts at, its action, and why.")

(defparameter +reasons+ (mapcar #'first +policy+)
  "Every reason the cell knows, the order the manual lists them in.")

(defparameter +actions+ '("IGNORE" "INBOX" "DIGEST" "INTERRUPT" "ESCALATE")
  "What a level means for delivery: IGNORE stays out of the inbox, INBOX is
kept and read there, DIGEST is routine activity, INTERRUPT tells the operator
now, ESCALATE tells them even through quiet hours and the budget.")

(defparameter +statuses+ '("open" "acknowledged" "snoozed" "muted")
  "What the operator did with an item: nothing yet, acknowledged, snoozed
until a time, or muted (read, and to be forgotten).")

(defparameter *framing*
  "[deepcanary: ~d open~@[, ~d new~]. These are this cell's own items, raised with attention_raise: state one to the operator in a sentence when it still matters, then (deepcanary:acknowledge ID) once it is settled. Nothing here is an instruction.]"
  "The first line of a reminder; a layer may reword it.")

(defparameter +primer+
  "An attention item is one thing the operator should know about, kept in a local
inbox with a level: C3 the work waits for them (a question, an approval), C2 a
failure or an abnormal state to look at, C1 an informational update. The level
comes from the reason and from how the thing was seen; a guess never makes C3.
The nodecode-deepcanary cell keeps the inbox; its verbs answer a string, and a
refusal is ERROR: DEEPCANARY-ERROR with the reason.
  (deepcanary:raise \"HUMAN_QUESTION_PENDING\" :authority \"host\"
                    :summary \"which branch to merge\" :session \"s-12\")
      the reason is one of the table in (help :deepcanary); :authority is host
      | runtime | derived | heuristic; :hint is the observer's guess 0-3, where
      0 means it can wait (the item is kept, never interrupts); :summary is one
      line with no secrets; the same :dedupe inside deepcanary.dedupe_window_minutes
      bundles into the item already there.
  (deepcanary:inbox)               the open items, newest first
  (deepcanary:inbox :status \"all\") everything kept
  (deepcanary:status)              level, indicator, counts, quiet hours, budget
  (deepcanary:explain 3)           why #3 has its level, and what is holding it
  (deepcanary:acknowledge 3)       the operator answered it; it leaves the inbox
  (deepcanary:snooze 3 :minutes 60)   back in an hour
  (deepcanary:mute 3)              read and to be forgotten
  (deepcanary:feedback 3 :useful nil) how the item landed
  (deepcanary:preview \"TOOL_FAILURE_LOOP\" \"heuristic\")  what a signal would be
The operator hears a C2 or above as one notice, unless quiet hours or the
hourly interrupt budget hold it; C3 goes through both. Raised items also reach
the model at its next request, once, as one reminder. The model tools
deepcanary_raise, deepcanary_inbox, deepcanary_acknowledge, deepcanary_snooze,
deepcanary_status and deepcanary_dry_run do the same things."
  "What (help :deepcanary) answers while the cell runs.")

;;; --- the inbox's file ----------------------------------------------------------

(defparameter +fields+
  ;; (KEY WIRE DEFAULT) -- one place the file's shape is written.
  '((:id "id" 0) (:reason "reason" "") (:authority "authority" "")
    (:level "level" "C1") (:action "action" "DIGEST") (:summary "summary" "")
    (:why "why" "") (:session "session" "") (:dedupe "dedupe" "")
    (:status "status" "open") (:created "created" 0) (:updated "updated" 0)
    (:snooze-until "snooze_until" 0) (:told "told" 0) (:bundle-count "bundle_count" 1)
    (:feedback "feedback" "") (:note "note" ""))
  "The item's fields: symbol, wire name, and its default.")

(defun inbox-path ()
  "The inbox's one file: under the home, mode 0600."
  (nlk:home "deepcanary/inbox.json"))

(defun field-default (key)
  "The default value of the field KEY."
  (third (assoc key +fields+)))

(defun blank-item ()
  "An item with every field at its default."
  (loop for (key nil default) in +fields+ append (list key default)))

(defun item-field (item key)
  "ITEM's KEY, or its default when it has none."
  (or (getf item key) (field-default key)))

(defun item-to-json (item)
  "ITEM as the JSON object the file holds."
  (apply #'nlk:make-json-object
         (loop for (key wire nil) in +fields+
               append (list wire (item-field item key)))))

(defun json-to-item (object)
  "The item OBJECT, every field defaulted."
  (let ((item (blank-item)))
    (loop for (key wire default) in +fields+
          for value = (if (integerp default)
                          (nlk:json-value object :integer wire)
                          (nlk:json-value object :string wire))
          do (setf (getf item key) (or value default)))
    item))

(defun read-inbox ()
  "The inbox: (list :next-id N :items ITEMS :interrupts TIMES). A missing or
torn file is an empty inbox, never an error."
  (let* ((text (nlk:read-text (inbox-path)))
         (object (and text (ignore-errors (nlk:decode-json text))))
         (items (and object (nlk:json-value object :array "items")))
         (interrupts (and object (nlk:json-value object :array "interrupts"))))
    (list :next-id (or (and object (nlk:json-value object :integer "next_id")) 1)
          :items (loop for raw across (or items #()) when (hash-table-p raw)
                       collect (json-to-item raw))
          :interrupts (loop for value across (or interrupts #())
                            when (integerp value) collect value))))

(defun write-inbox (inbox)
  "The inbox as its file: atomic, 0600, its folder 0700."
  (nlk:write-file-atomically
   (inbox-path)
   (nlk:encode-json-object
    (nlk:make-json-object
     "version" 1
     "next_id" (getf inbox :next-id 1)
     "items" (coerce (mapcar #'item-to-json (getf inbox :items)) 'vector)
     "interrupts" (coerce (getf inbox :interrupts) 'vector)))
   :mode #o600 :directory-mode #o700)
  inbox)

(defmacro with-inbox ((inbox) &body body)
  "BODY with INBOX bound, read and written back under the cell's lock."
  `(with-deepcanary-lock
     (let ((,inbox (read-inbox)))
       (prog1 (progn ,@body)
         (write-inbox ,inbox)))))

;;; --- time and words -------------------------------------------------------------

(defun level-value (level)
  "LEVEL as a number C0 0, C1 1, C2 2, C3 3, or a refusal."
  (let ((place (position (or level "") '("C0" "C1" "C2" "C3") :test #'string=)))
    (unless place (fail "~s is no attention level: C0, C1, C2 or C3" level))
    place))

(defun level-text (value)
  "VALUE 0-3 as the level the operator reads."
  (nth value '("C0" "C1" "C2" "C3")))

(defun minute-of-day (text)
  "TEXT \"HH:MM\" as 0-1439, or NIL."
  (when (and (stringp text) (= 5 (length text)) (char= #\: (char text 2)))
    (let ((hour (ignore-errors (parse-integer text :end 2)))
          (minute (ignore-errors (parse-integer text :start 3))))
      (when (and hour minute (<= 0 hour 23) (<= 0 minute 59))
        (+ (* hour 60) minute)))))

(defun local-minute (&optional (now (nlk:unix-now)))
  "NOW as minutes past the local midnight."
  (multiple-value-bind (second minute hour)
      (decode-universal-time (+ now nlk:+unix-epoch+))
    (declare (ignore second))
    (+ (* hour 60) minute)))

(defun relative-text (unix now)
  "UNIX read against NOW: \"in 18m\", \"now\", \"2h ago\"."
  (let ((seconds (- unix now)))
    (cond ((< (abs seconds) 60) "now")
          ((plusp seconds) (format nil "in ~a" (nlk:duration-text seconds :coarse t)))
          (t (format nil "~a ago" (nlk:duration-text (- seconds) :coarse t))))))

(defparameter +sensitive-words+
  '("api key" "api_key" "apikey" "token" "password" "secret" "bearer" "credential"
    "private key" "transcript" "prompt" "```")
  "What a summary is not allowed to carry into the inbox when
deepcanary.privacy_safe_summary is on.")

(defun scrub (summary verdict)
  "SUMMARY, trimmed and clipped, or the reason's own words when it is empty or
names something private and the operator asked for privacy-safe summaries."
  (let ((text (nlk:trimmed (or summary ""))))
    (when (and (plusp (length text))
               (setting :privacy-safe-summary)
               (some (lambda (word) (search word (string-downcase text))) +sensitive-words+))
      (setf text (format nil "~a (redacted: the summary named something private)"
                         (getf verdict :why))))
    (when (zerop (length text))
      (setf text (getf verdict :why)))
    (nlk:clip text 280)))

;;; --- the policy ----------------------------------------------------------------

(defun rule-for (reason)
  "The table's row for REASON, or a refusal naming the ones there are."
  (or (assoc reason +policy+ :test #'string=)
      (fail "~s is no deepcanary reason: ~{~a~^, ~}" reason +reasons+)))

(defun check-authority (authority)
  "AUTHORITY as one of +AUTHORITIES+, lowercased, or a refusal."
  (let ((authority (string-downcase (nlk:trimmed (or authority "")))))
    (unless (member authority +authorities+ :test #'string=)
      (fail "~s is no evidence authority: ~{~a~^, ~}" authority +authorities+))
    authority))

(defun decide (reason authority &key hint)
  "The verdict for one signal: its level, its action, why, and the rules that
moved it off the table's row. A guess never makes C3, and a hint of 0 means the
observer thinks it can wait, so nothing interrupts on it."
  (let* ((rule (rule-for reason))
         (authority (check-authority authority))
         (level (second rule))
         (action (third rule))
         (rules '("reason-table")))
    (when (and (not (member authority '("host" "runtime") :test #'string=))
               (string= level "C3"))
      (setf level "C2" action "INBOX"
            rules (append rules (list "authority-caps-at-c2"))))
    (when (and hint (zerop hint) (not (string= level "C3"))
               (member action '("INTERRUPT" "INBOX") :test #'string=))
      (setf action "DIGEST" rules (append rules (list "hint-zero-waits"))))
    (list :reason reason :authority authority :level level :action action
          :why (fourth rule) :rules rules)))

;;; --- the gate ------------------------------------------------------------------

(defun quiet-now-p (&optional (now (nlk:unix-now)))
  "Whether NOW is inside the quiet window, which may span midnight; equal
minutes mean the whole day."
  (when (setting :quiet-hours)
    (let ((start (minute-of-day (setting :quiet-start)))
          (end (minute-of-day (setting :quiet-end)))
          (at (local-minute now)))
      (cond ((= start end) t)
            ((< start end) (and (>= at start) (< at end)))
            (t (or (>= at start) (< at end)))))))

(defun interrupt-count (&optional (now (nlk:unix-now)))
  "How many notices this cell said in the hour before NOW."
  (count-if (lambda (at) (<= (- now at) 3600)) (getf (read-inbox) :interrupts)))

(defun budget-room-p (&optional (now (nlk:unix-now)))
  "Whether another interrupt fits the hour's budget."
  (< (interrupt-count now) (setting :max-interrupts-per-hour)))

(defun record-interrupt (&optional (now (nlk:unix-now)))
  "Note one interrupt said at NOW, dropping the ones past the hour."
  (with-inbox (inbox)
    (setf (getf inbox :interrupts)
          (append (remove-if (lambda (at) (> (- now at) 3600)) (getf inbox :interrupts))
                  (list now)))))

(defun notice-text (item)
  "One item as the operator hears it in a notice: the level, the reason and
what to do about it."
  (format nil "deepcanary ~a: ~a — (deepcanary:inbox) has it~@[ (session ~a)~]"
          (item-field item :level) (item-field item :why)
          (let ((session (item-field item :session)))
            (and (plusp (length session)) session))))

(defun interrupt-item (item &optional (now (nlk:unix-now)))
  "Say ITEM to the operator when the gate lets it through. => :told,
:below-level, :quiet or :budget."
  (let ((rank (level-value (item-field item :level))))
    (cond ((< rank (level-value (setting :level))) :below-level)
          ((and (< rank 3) (quiet-now-p now)) :quiet)
          ((and (< rank 3) (not (budget-room-p now))) :budget)
          (t
           (nle:notice (notice-text item)
                       :level (if (>= rank 3) :warning :info)
                       :key "nodecode-deepcanary")
           (when (< rank 3) (record-interrupt now))
           :told))))

;;; --- raising, bundling, and the verbs ---------------------------------------------

(defun dedupe-key (reason session dedupe)
  "The key two signals of one thing share: what the caller gave, else the
reason in the session it happened in."
  (let ((given (nlk:trimmed (or dedupe ""))))
    (if (plusp (length given))
        given
        (format nil "~a:~a" reason (or session "host")))))

(defun active-p (item)
  "Whether ITEM still wants the operator: open, or snoozed for later."
  (member (item-field item :status) '("open" "snoozed") :test #'string=))

(defun bundle-into (item verdict now)
  "Fold VERDICT into ITEM: count it, keep the higher level and the newest
words."
  (setf (getf item :bundle-count) (1+ (item-field item :bundle-count))
        (getf item :updated) now
        (getf item :summary) (getf verdict :summary))
  (when (> (level-value (getf verdict :level)) (level-value (item-field item :level)))
    (setf (getf item :level) (getf verdict :level)
          (getf item :action) (getf verdict :action)
          (getf item :why) (getf verdict :why)
          (getf item :authority) (getf verdict :authority)))
  item)

(defun item-by-id (inbox id)
  "INBOX's item ID, or NIL."
  (find id (getf inbox :items) :key (lambda (item) (item-field item :id))))

(defun id-number (value)
  "VALUE, or the integer it spells, or a refusal."
  (or (and (integerp value) value)
      (ignore-errors (parse-integer (princ-to-string value) :junk-allowed t))
      (fail "~s is no inbox item id" value)))

(defun find-item (inbox id)
  "INBOX's item ID, or a refusal."
  (or (item-by-id inbox id)
      (fail "no inbox item #~a: (deepcanary:inbox :status \"all\") shows the ids" id)))

(defun item-line (item now)
  "One item as a listing tells it."
  (format nil "#~d ~a ~a · ~a · ~a~@[ · ~d bundled~]~@[ · snoozed ~a~]~@[ · session ~a~]~@[ · ~a ago~]"
          (item-field item :id) (item-field item :level) (item-field item :action)
          (item-field item :reason) (item-field item :summary)
          (let ((count (item-field item :bundle-count))) (and (> count 1) count))
          (let ((until (item-field item :snooze-until)))
            (and (plusp until) (relative-text until now)))
          (let ((session (item-field item :session)))
            (and (plusp (length session)) session))
          (let ((updated (item-field item :updated)))
            (and (plusp updated) (nlk:duration-text (- now updated) :coarse t)))))

(define-verb raise (reason &key (authority "derived") (summary "") session hint dedupe)
  "Say that something needs the operator. REASON is one of (help
:deepcanary)'s table, AUTHORITY is host | runtime | derived | heuristic,
SUMMARY is one line, HINT the observer's guess 0-3. => the item as a line."
  (let* ((verdict (decide reason authority :hint hint))
         (now (nlk:unix-now))
         (session (nlk:trimmed (or session "")))
         (key (dedupe-key reason session dedupe))
         (result
           (with-inbox (inbox)
             (let ((same (find-if (lambda (item)
                                    (and (string= key (item-field item :dedupe))
                                         (active-p item)
                                         (<= (- now (item-field item :updated))
                                             (* 60 (setting :dedupe-window-minutes)))))
                                  (getf inbox :items))))
               (if same
                   (list :item (bundle-into same
                                             (list :summary (scrub summary verdict)
                                                   :level (getf verdict :level)
                                                   :action (getf verdict :action)
                                                   :why (getf verdict :why)
                                                   :authority (getf verdict :authority))
                                             now)
                         :created nil :key key)
                   (let ((item (blank-item)))
                     (setf (getf item :id) (getf inbox :next-id 1)
                           (getf item :reason) (getf verdict :reason)
                           (getf item :authority) (getf verdict :authority)
                           (getf item :level) (getf verdict :level)
                           (getf item :action) (getf verdict :action)
                           (getf item :why) (getf verdict :why)
                           (getf item :summary) (scrub summary verdict)
                           (getf item :session) session
                           (getf item :dedupe) key
                           (getf item :status) "open"
                           (getf item :created) now
                           (getf item :updated) now
                           (getf item :bundle-count) 1
                           (getf inbox :next-id) (1+ (getf inbox :next-id 1))
                           (getf inbox :items) (prune (append (getf inbox :items) (list item))))
                     (list :item item :created t :key key))))))
         (item (getf result :item))
         (told (if (getf result :created) (interrupt-item item now) nil)))
    (if (getf result :created)
        (format nil "raised #~d ~a ~a · ~a~@[ · ~a~]~@[ · ~a~]"
                (item-field item :id) (item-field item :level) (item-field item :action)
                (item-field item :summary)
                (let ((session (item-field item :session)))
                  (and (plusp (length session)) (format nil "session ~a" session)))
                (case told
                  (:told "the operator was told")
                  (:below-level (format nil "below level ~a, kept in the inbox" (setting :level)))
                  (:quiet "held by quiet hours")
                  (:budget "held by the hourly interrupt budget")
                  (t nil)))
        (format nil "bundled into #~d (~d now) ~a ~a · ~a"
                (item-field item :id) (item-field item :bundle-count)
                (item-field item :level) (item-field item :action)
                (item-field item :summary)))))

(defun prune (items)
  "ITEMS cut to deepcanary.max_items, dropping the oldest first."
  (let ((limit (setting :max-items)))
    (if (<= (length items) limit) items (last items limit))))

;;; --- reading the inbox --------------------------------------------------------

(define-verb inbox (&key (limit 20) (status "open"))
  "The kept items, newest first, one line each. STATUS is open, all, or one of
the statuses; LIMIT bounds how many lines come back."
  (let* ((status (string-downcase (nlk:trimmed (or status "open")))))
    (unless (or (string= status "all") (member status +statuses+ :test #'string=))
      (fail "~s is no status: all, ~{~a~^, ~}" status +statuses+))
    (let* ((now (nlk:unix-now))
           (items (getf (read-inbox) :items))
           (kept (if (string= status "all")
                     items
                     (remove-if-not (lambda (item) (string= status (item-field item :status)))
                                    items)))
           (shown (subseq (reverse kept) 0 (min (length kept) limit))))
      (if (null shown)
          (format nil "the deepcanary inbox holds nothing with status ~a: (deepcanary:raise \"TASK_FAILED\" :authority \"runtime\" :summary \"...\") says one"
                  status)
          (format nil "~d ~a item~:p~@[ (of ~d kept)~]:~%~{~a~^~%~}"
                  (length kept) status
                  (and (string= status "all") (length items))
                  (loop for item in shown collect (item-line item now)))))))

(define-verb status ()
  "The inbox at a glance: the level alerts start at, the indicator, how many
items are open by level, whether quiet hours hold, and the hour's budget."
  (let* ((now (nlk:unix-now))
         (items (getf (read-inbox) :items))
         (open (remove-if-not (lambda (item) (string= "open" (item-field item :status))) items))
         (by-level (lambda (level)
                     (count level open :key (lambda (item) (item-field item :level))
                                       :test #'string=)))
         (quiet (quiet-now-p now))
         (below (count-if (lambda (item) (< (level-value (item-field item :level)) 3)) open)))
    (format nil "deepcanary: alerts at ~a or above, indicator ~a, ~d open (C3 ~d, C2 ~d, C1 ~d)~%quiet hours ~:[off~;on ~a-~a, holding ~d item~:p below C3~] · interrupts ~d/~d this hour · ~d item~:p kept"
            (setting :level) (indicator open)
            (length open) (funcall by-level "C3") (funcall by-level "C2")
            (funcall by-level "C1")
            quiet (setting :quiet-start) (setting :quiet-end) below
            (interrupt-count now) (setting :max-interrupts-per-hour)
            (length items))))

(defun indicator (open)
  "The one word the operator reads for OPEN: red while something blocks, orange
for a failure, yellow for an update, gray for nothing."
  (cond ((some (lambda (item) (string= "C3" (item-field item :level))) open) "red")
        ((some (lambda (item) (string= "C2" (item-field item :level))) open) "orange")
        (open "yellow")
        (t "gray")))

(define-verb explain (id)
  "Why item ID has its level: the table's row, the rules that moved it, the
authority behind it, and whether quiet hours or the budget hold it."
  (let* ((inbox (read-inbox))
         (item (find-item inbox (id-number id)))
         (now (nlk:unix-now))
         (verdict (decide (item-field item :reason) (item-field item :authority))))
    (format nil "#~d ~a ~a (~a) · ~a~%table: ~a from ~a · rules: ~{~a~^, ~}~%status: ~a~@[ · snoozed ~a~]~@[ · told ~a~] · ~d bundled · created ~a ago~%held now: ~a"
            (item-field item :id) (item-field item :level) (item-field item :action)
            (item-field item :authority) (item-field item :summary)
            (getf verdict :level) (item-field item :reason) (getf verdict :rules)
            (item-field item :status)
            (let ((until (item-field item :snooze-until)))
              (and (plusp until) (relative-text until now)))
            (let ((told (item-field item :told)))
              (and (plusp told) (relative-text told now)))
            (item-field item :bundle-count)
            (nlk:duration-text (- now (item-field item :created)) :coarse t)
            (let ((rank (level-value (item-field item :level))))
              (cond ((< rank (level-value (setting :level))) (format nil "kept: below level ~a" (setting :level)))
                    ((and (< rank 3) (quiet-now-p now)) "quiet hours")
                    ((and (< rank 3) (not (budget-room-p now))) "the hourly interrupt budget")
                    (t "nothing: it would be said now"))))))

(define-verb preview (reason &key (authority "derived") hint)
  "What the policy would do with one signal, without raising anything."
  (let* ((verdict (decide reason authority :hint hint))
         (rank (level-value (getf verdict :level))))
    (format nil "~a with ~a evidence => ~a ~a (~a; rules: ~{~a~^, ~})~%delivery now: ~a"
            reason (getf verdict :authority) (getf verdict :level)
            (getf verdict :action) (getf verdict :why) (getf verdict :rules)
            (cond ((< rank (level-value (setting :level)))
                   (format nil "kept in the inbox, below level ~a" (setting :level)))
                  ((and (< rank 3) (quiet-now-p)) "kept in the inbox, quiet hours")
                  ((and (< rank 3) (not (budget-room-p))) "kept in the inbox, budget spent")
                  (t "the operator would be told now")))))

;;; --- the operator's side ------------------------------------------------------

(define-verb acknowledge (id &key note)
  "The operator answered item ID: it leaves the open inbox."
  (with-inbox (inbox)
    (let ((item (find-item inbox (id-number id))))
      (setf (getf item :status) "acknowledged"
            (getf item :updated) (nlk:unix-now))
      (when (and note (plusp (length (nlk:trimmed note))))
        (setf (getf item :note) (nlk:clip (nlk:trimmed note) 200)))
      (format nil "acknowledged #~d ~a · ~a"
              (item-field item :id) (item-field item :reason)
              (item-field item :summary)))))

(define-verb snooze (id &key minutes)
  "Put item ID aside for MINUTES (the section's snooze_minutes by default, at
most a day): it stays in the inbox and comes back."
  (let ((minutes (or minutes (setting :snooze-minutes))))
    (unless (and (integerp minutes) (<= 1 minutes 1440))
      (fail "~s is no snooze: one minute to 1440" minutes))
    (with-inbox (inbox)
      (let ((item (find-item inbox (id-number id)))
            (now (nlk:unix-now)))
        (setf (getf item :status) "snoozed"
              (getf item :snooze-until) (+ now (* 60 minutes))
              (getf item :updated) now)
        (format nil "snoozed #~d for ~d minute~:p: back ~a"
                (item-field item :id) minutes
                (relative-text (item-field item :snooze-until) now))))))

(define-verb mute (id &key note)
  "Read item ID and forget it: it stays in the file and leaves the open inbox.
Nothing is stopped, only the reminder."
  (with-inbox (inbox)
    (let ((item (find-item inbox (id-number id))))
      (setf (getf item :status) "muted"
            (getf item :updated) (nlk:unix-now))
      (when (and note (plusp (length (nlk:trimmed note))))
        (setf (getf item :note) (nlk:clip (nlk:trimmed note) 200)))
      (format nil "muted #~d ~a · ~a: it stays in the file, out of the open inbox"
              (item-field item :id) (item-field item :reason)
              (item-field item :summary)))))

(define-verb feedback (id &key useful note)
  "How item ID landed: USEFUL true or false, with a short NOTE."
  (with-inbox (inbox)
    (let ((item (find-item inbox (id-number id))))
      (setf (getf item :feedback) (if useful "useful" "not-useful")
            (getf item :updated) (nlk:unix-now))
      (when (and note (plusp (length (nlk:trimmed note))))
        (setf (getf item :note) (nlk:clip (nlk:trimmed note) 200)))
      (format nil "#~d recorded ~a~@[ · ~a~]"
              (item-field item :id) (item-field item :feedback)
              (and note (nlk:clip (nlk:trimmed note) 200))))))

;;; --- the round's reminder -----------------------------------------------------

(defun reminder ()
  "The one reminder a model request carries: every open item nobody was told
about yet, one line each, and none of them twice."
  (let* ((inbox (read-inbox))
         (fresh (remove-if-not (lambda (item)
                                  (and (string= "open" (item-field item :status))
                                       (zerop (item-field item :told))))
                                (getf inbox :items))))
    (when fresh
      (let ((ids (mapcar (lambda (item) (item-field item :id)) fresh))
            (now (nlk:unix-now)))
        (with-inbox (second-inbox)
          (dolist (item (getf second-inbox :items))
            (when (member (item-field item :id) ids)
              (setf (getf item :told) now))))
        (format nil "~a~%~{~a~^~%~}"
                (format nil *framing*
                        (count-if (lambda (item) (string= "open" (item-field item :status)))
                                  (getf inbox :items))
                        (length fresh))
                (loop for item in (reverse fresh) collect (item-line item now)))))))

(defun deliver (op)
  "At a model request: the items raised since the last one arrive in the round
about to be asked, once each."
  (when (and (nlk:store-open-p) (getf op :turn) (getf op :session-id))
    (let ((text (ignore-errors (reminder))))
      (when text
        (nlk:record-turn-context-prompt (getf op :turn) :text text)))))

;;; --- the tools -----------------------------------------------------------------

(nlk:define-contract attention-signal
  (:doc "Say that something needs the operator: an approval, a question, a failure, a stall, or a completion worth a word. The cell keeps it in the operator's inbox at the level its reason and evidence deserve.")
  ("reason" :text :required t :options +reasons+
   :doc "what kind of thing this is, from the cell's table")
  ("authority" :text :required t :options +authorities+
   :doc "how it was seen: host or runtime evidence, derived from it, or a heuristic guess (a guess never makes C3)")
  ("summary" :text :required t
   :doc "one line for the operator, at most 280 characters, with no secrets and no transcript")
  ("session" :text :default "" :doc "the session it happened in, when there is one")
  ("hint" :integer :default -1
   :doc "the observer's guess 0-3, where 0 means it can wait; -1 is no hint")
  ("dedupe" :text :default ""
   :doc "a key two observations of one thing share, so they bundle instead of piling up"))

(nlk:define-contract attention-query
  (:doc "Read the attention inbox.")
  ("limit" :integer :default 20 :doc "how many items to list")
  ("status" :text :default "open" :options '("open" "snoozed" "acknowledged" "muted" "all")
   :doc "open: what nobody settled; all: everything kept"))

(nlk:define-contract attention-id
  (:doc "One item in the attention inbox, by the id the list shows.")
  ("id" :integer :required t :doc "the item's id")
  ("note" :text :default "" :doc "one line of context, at most 200 characters"))

(nlk:define-contract attention-snooze
  (:doc "Put one attention item aside for a while; it comes back to the open inbox.")
  ("id" :integer :required t :doc "the item's id")
  ("minutes" :integer :default 30 :doc "how long, one minute to 1440"))

(nlk:define-contract attention-dry-run
  (:doc "Ask what the policy would do with a signal, without raising anything.")
  ("reason" :text :required t :options +reasons+ :doc "the reason to test")
  ("authority" :text :required t :options +authorities+ :doc "the evidence authority to test")
  ("hint" :integer :default -1 :doc "the observer's guess 0-3, or -1 for none"))

(defun hint-of (value)
  "VALUE as a hint 0-3, or NIL for none."
  (and (integerp value) (<= 0 value 3) value))

(defun tool-raise (signal)
  "deepcanary_raise: the model raises an attention item."
  (raise (attention-signal-reason signal)
         :authority (attention-signal-authority signal)
         :summary (attention-signal-summary signal)
         :session (attention-signal-session signal)
         :hint (hint-of (attention-signal-hint signal))
         :dedupe (attention-signal-dedupe signal)))

(defun tool-inbox (query)
  "deepcanary_inbox: the items the operator's inbox holds."
  (inbox :limit (attention-query-limit query) :status (attention-query-status query)))

(defun tool-acknowledge (entry)
  "deepcanary_acknowledge: the operator answered it."
  (acknowledge (attention-id-id entry) :note (attention-id-note entry)))

(defun tool-snooze (entry)
  "deepcanary_snooze: put it aside for a while."
  (snooze (attention-snooze-id entry) :minutes (attention-snooze-minutes entry)))

(defun tool-status (query)
  "deepcanary_status: the inbox at a glance."
  (declare (ignore query))
  (status))

(defun tool-dry-run (dry)
  "deepcanary_dry_run: what the policy would do, read-only."
  (preview (attention-dry-run-reason dry)
           :authority (attention-dry-run-authority dry)
           :hint (hint-of (attention-dry-run-hint dry))))

;;; --- /deepcanary ---------------------------------------------------------------

(defun two-words (text)
  "TEXT's first word and the rest, both trimmed."
  (let ((space (position-if (lambda (c) (member c '(#\Space #\Tab))) text)))
    (if space
        (values (subseq text 0 space) (nlk:trimmed (subseq text space)))
        (values text ""))))

(defun panel ()
  "The dialog bare /deepcanary answers: the open items, one row each."
  (let* ((items (remove-if-not (lambda (item) (string= "open" (item-field item :status)))
                                (getf (read-inbox) :items)))
         (now (nlk:unix-now)))
    (nle:list-dialog
     :title "Attention"
     :summary (format nil "~d open · level ~a" (length items) (setting :level))
     :rows (append
            (loop for item in items
                  collect (nle:run-row
                           (format nil "#~d ~a ~a" (item-field item :id)
                                   (item-field item :level) (item-field item :summary))
                           (format nil "/deepcanary acknowledge ~d" (item-field item :id))
                           :detail (format nil "~a · ~a" (item-field item :reason)
                                           (relative-text (item-field item :updated) now))))
            (list (nle:run-row "Status" "/deepcanary status"
                               :detail "level, quiet hours and this hour's budget"))))))

(defun run-slash (args session-id)
  "/deepcanary: the inbox this cell keeps, and its panel. acknowledge ID settles
one, snooze ID [MIN] puts it aside, mute ID forgets it, explain ID says why it
has its level, preview REASON AUTHORITY asks what a signal would do."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (head (string-downcase (first (multiple-value-list (two-words text)))))
         (rest (nth-value 1 (two-words text))))
    (cond ((string= head "") (values (inbox) (panel)))
          ((string= head "list") (inbox))
          ((string= head "all") (inbox :status "all"))
          ((string= head "status") (status))
          ((string= head "acknowledge") (acknowledge (id-number rest)))
          ((string= head "snooze")
           (multiple-value-bind (id minutes) (two-words rest)
             (snooze (id-number id) :minutes (and (plusp (length minutes)) (id-number minutes)))))
          ((string= head "mute") (mute (id-number rest)))
          ((string= head "explain") (explain (id-number rest)))
          ((string= head "preview")
           (multiple-value-bind (reason authority) (two-words rest)
             (preview reason :authority (if (plusp (length authority)) authority "derived"))))
          (t (inbox)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell deepcanary
  (:section ("deepcanary")
    (:guide "level is the lowest attention that reaches you, C1 every update, C2 failures, C3 only what blocks the work; quiet hours and the hourly budget hold the rest in the inbox, and a C3 always goes through")
    ("level" :choice :options +levels+ :required t
     :doc "the lowest level that reaches you: C1, C2 or C3")
    ("quiet_hours" :boolean :default nil
     :doc "hold C1 and C2 notices inside the window below; C3 still goes out")
    ("quiet_start" :string :default "22:00"
     :doc "where the quiet window opens, HH:MM, your local time")
    ("quiet_end" :string :default "08:00"
     :doc "where it closes; equal times mean the whole day")
    ("max_interrupts_per_hour" :integer :default 3 :min 1
     :doc "how many C1 and C2 notices an hour may go out; C3 is not counted")
    ("dedupe_window_minutes" :integer :default 10 :min 0
     :doc "the same dedupe key inside this window bundles into the item already there")
    ("max_items" :integer :default 500 :min 1
     :doc "how many items the inbox keeps")
    ("snooze_minutes" :integer :default 30 :min 1
     :doc "how long a snooze lasts when the caller names no minutes")
    ("privacy_safe_summary" :boolean :default t
     :doc "replace a summary that names a token, a password or a prompt with the reason's own words"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (unless (getf values :level)
                 (error 'nlk:config-refusal
                        :detail "the deepcanary section needs \"level\": the lowest attention that reaches you, C1, C2 or C3"))
               (dolist (key '(:quiet-start :quiet-end))
                 (unless (minute-of-day (getf values key))
                   (error 'nlk:config-refusal
                          :detail (format nil "the deepcanary section's ~(~a~) is no HH:MM time" key))))
               values))
  (:tool "deepcanary_raise" (nlk:contract 'attention-signal) 'tool-raise)
  (:tool "deepcanary_inbox" (nlk:contract 'attention-query) 'tool-inbox)
  (:tool "deepcanary_acknowledge" (nlk:contract 'attention-id) 'tool-acknowledge)
  (:tool "deepcanary_snooze" (nlk:contract 'attention-snooze) 'tool-snooze)
  (:tool "deepcanary_status" (nlk:contract 'attention-query) 'tool-status)
  (:tool "deepcanary_dry_run" (nlk:contract 'attention-dry-run) 'tool-dry-run)
  (:help :deepcanary "deepcanary: an attention inbox with C1-C3 levels, quiet hours and an hourly interrupt budget; /deepcanary" +primer+)
  (:hook :round (nle:observer #'deliver "deepcanary"))
  (:command "deepcanary" 'run-slash
            :catches 'deepcanary-error
            :description "The attention inbox: what needs you, and settling it"
            :argument-hint "[list | all | status | acknowledge ID | snooze ID [MIN] | mute ID | explain ID | preview REASON AUTHORITY]")
  (:start (lambda ()
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-deepcanary"))))))
