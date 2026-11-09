;;;; cell.lisp --- the store, the actions, the tool, /muse, the due reminder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-muse for Nodecode. The original is a Hermes assembly:
;;;; a single muse_manage tool over forty actions, six Cron jobs, a Dashboard
;;;; and a Skill, holding goals, interests, ideas, a Feed of authored articles,
;;;; reminder candidates and research runs in one profile store. This rewrite
;;;; keeps the part a user asks for by name -- a companion that remembers what
;;;; was promised, keeps a quiet Feed to read, and says one line at your next
;;;; request when a reminder came due -- and drops the rest: no Cron, no
;;;; subagents, no dashboard, no research runs.
;;;;
;;;; State is one JSON file under <home>/muse/, written whole and renamed in,
;;;; so the store is readable with any tool and never half-written. Goals,
;;;; interests, ideas, Feed articles, reminders and the Feed brief are the
;;;; records; every one carries a revision an update may name back.
;;;;
;;;; Three ways in: the model's muse_manage tool (one action, arguments in
;;;; one data object, as the original's schema has them); /muse for the
;;;; operator, with the panel; the verbs below for eval. A reminder that
;;;; comes due is handed to the next model request as one reminder line, and
;;;; is never repeated.

(in-package #:nodecode-muse)

(nlk:define-peripheral muse :not-running t :lock "muse")

;;; --- the words -------------------------------------------------------------------

(defparameter +collections+
  '(("goal" . "goals") ("topic" . "topics") ("idea" . "ideas")
    ("feed" . "feed") ("reminder" . "reminders"))
  "Kind -> the collection it is kept in, in the store and in (context).")

(defparameter +kinds+ (mapcar #'car +collections+)
  "Every kind of record this store holds.")

(defparameter +actions+
  '("context" "status" "goal_create" "goal_update" "interest_record" "idea_add"
    "feed_add" "feed_list" "feed_update" "content_read" "notification_add"
    "notification_resolve" "preferences" "forget")
  "The actions muse_manage answers: the original's names, for the part it keeps.")

(defparameter +reminder-chars+ 240
  "How much of one reminder's text the round's line carries.")

;;; --- the store -------------------------------------------------------------------

(defvar *root* nil
  "The folder this companion's store lives in, when something other than the
home binds it (a test); NIL means <home>/muse/.")

(defvar *companion* nil
  "The loaded state while the cell runs, NIL while it is not: a hash table keyed
by collection name, plus NEXT (ids) and PREFERENCES.")

(defun root ()
  "The store's folder; it is this profile's, never another's."
  (uiop:ensure-directory-pathname (or *root* (nlk:home "muse/"))))

(defun state-file ()
  (merge-pathnames "state.json" (root)))

(defun collection (kind)
  "The collection KIND is kept in, or a refusal naming the kinds there are."
  (or (cdr (assoc kind +collections+ :test #'equal))
      (fail "~s is no kind of record: ~{~a~^, ~}" kind +kinds+)))

(defun fresh-state ()
  (let ((state (make-hash-table :test #'equal))
        (next (make-hash-table :test #'equal)))
    (dolist (kind +kinds+)
      (setf (gethash kind next) 1
            (gethash (collection kind) state) '()))
    (setf (gethash "next" state) next
          (gethash "preferences" state) (make-hash-table :test #'equal))
    state))

(defun load-state ()
  "This profile's store, or an empty one. A file that is not the object this
program writes is a refusal, never a silently empty companion."
  (let ((file (state-file)))
    (if (probe-file file)
        (let ((raw (nlk:decode-json (uiop:read-file-string file)))
              (state (fresh-state)))
          (unless (hash-table-p raw)
            (fail "~a is not a companion store: it holds no object" (namestring file)))
          (dolist (kind +kinds+)
            (setf (gethash (collection kind) state)
                  (coerce (or (nlk:json-value raw :array (collection kind)) #()) 'list)))
          (dolist (pen (list (cons "next" "next") (cons "preferences" "preferences")))
            (let ((object (nlk:json-value raw :object (car pen))))
              (when object (setf (gethash (cdr pen) state) object))))
          state)
        (fresh-state))))

(defun state->json ()
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash "version" object) 1
          (gethash "next" object) (gethash "next" *companion*)
          (gethash "preferences" object) (gethash "preferences" *companion*))
    (dolist (kind +kinds+)
      (setf (gethash (collection kind) object) (coerce (entries kind) 'vector)))
    object))

(defun save-state ()
  "Write the whole store to a sibling file and rename it in, so a reader never
sees half of one."
  (let* ((file (state-file))
         (temp (merge-pathnames "state.json.tmp" (root))))
    (ensure-directories-exist file)
    (with-open-file (out temp :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object (state->json)) out))
    (uiop:rename-file-overwriting-target temp file)))

(defmacro changing (&body body)
  "BODY over the store, under the cell's lock, saved once after it."
  `(with-muse-lock (prog1 (progn ,@body) (save-state))))

;;; --- one record ------------------------------------------------------------------

(defun entries (kind)
  (gethash (collection kind) *companion*))

(defun field (object key &optional (type :string))
  (nlk:json-value object type key))

(defun set-field (object key value)
  (setf (gethash key object) value)
  object)

(defun next-id (kind)
  (let ((next (gethash "next" *companion*)))
    (prog1 (format nil "~a-~d" kind (gethash kind next))
      (incf (gethash kind next)))))

(defun make-record (kind fields)
  "A new record of KIND carrying FIELDS ((KEY . VALUE) ...), after the four
every record has: its id, when it was made and last written, a revision."
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash "id" object) (next-id kind)
          (gethash "created" object) (nlk:iso-now)
          (gethash "updated" object) (gethash "created" object)
          (gethash "revision" object) 1)
    (dolist (pair fields object)
      (let ((value (cdr pair)))
        (when (and value (not (and (stringp value) (zerop (length value)))))
          (setf (gethash (car pair) object) value))))))

(defun keep (record)
  "Append RECORD to its collection."
  (let* ((kind (record-kind (field record "id")))
         (key (collection kind)))
    (setf (gethash key *companion*) (append (gethash key *companion*) (list record)))
    record))

(defun record-kind (id)
  (let ((dash (position #\- id)))
    (or (and dash (find (subseq id 0 dash) +kinds+ :test #'equal))
        (fail "~s is no record of this store: ~{~a~^, ~} then a dash and a number" id +kinds+))))

(defun find-record (kind id)
  (or (find id (entries kind) :test #'equal :key (lambda (object) (field object "id")))
      (fail "no ~a is called ~s: (context section=~a) lists them"
            kind id (collection kind))))

(defun touch (record)
  (incf (gethash "revision" record))
  (setf (gethash "updated" record) (nlk:iso-now))
  record)

(defun check-revision (record data)
  (let ((expected (arg-integer data "expected_revision")))
    (when (and expected (/= expected (field record "revision" :integer)))
      (fail "~a is at revision ~d, not ~d: read it again and merge"
            (field record "id") (field record "revision" :integer) expected))))

;;; --- the arguments ---------------------------------------------------------------

(defun data-object (data)
  "DATA as the object the actions read: a hash table as the tool decodes it, a
plist for eval, or nothing."
  (cond ((null data) (make-hash-table :test #'equal))
        ((hash-table-p data) data)
        ((listp data)
         (let ((table (make-hash-table :test #'equal)))
           (loop for (key value) on data by #'cddr
                 do (setf (gethash (string-downcase (string key)) table)
                          (cond ((and (listp value) (not (null value))
                                      (every #'stringp value))
                                 (coerce value 'vector))
                                ((or (keywordp value) (and (symbolp value) value))
                                 (string-downcase (string value)))
                                (t value))))
           table))
        (t (fail "the arguments must be one object, not ~s" data))))

(defun arg (data key &optional (type :string))
  (nlk:json-value data type key))

(defun arg-text (data key)
  (let ((value (arg data key :text)))
    (and value (nlk:trimmed value))))

(defun required (data key)
  (or (arg-text data key) (fail "~a is required" key)))

(defun arg-integer (data key)
  (arg data key :integer))

(defun arg-list (data key)
  "KEY as a list of strings: an array as the store keeps it, a list, or one
string as a one-element list."
  (let ((value (and data (gethash key data))))
    (cond ((vectorp value) (coerce value 'list))
          ((listp value) value)
          ((and (stringp value) (plusp (length value))) (list value))
          (t '()))))

;;; --- time ------------------------------------------------------------------------

(defun stamp (value)
  "VALUE as an ISO-8601 stamp: absent or empty is now, a day (2026-11-10) is
that day's midnight UTC, anything else is read as it stands."
  (let ((value (and value (nlk:trimmed value))))
    (cond ((or (null value) (zerop (length value))) (nlk:iso-now))
          ((= (length value) 10) (concatenate 'string value "T00:00:00Z"))
          (t value))))

(defun seconds-of (stamp)
  (or (nlk:iso-universal stamp)
      (fail "~s is not an ISO-8601 time: 2026-11-10 or 2026-11-10T09:00" stamp)))

(defun days-from-now (days)
  (nlk:iso-time (+ (get-universal-time) (* days 86400)) :millis nil))

(defun due-p (record &optional (now (get-universal-time)))
  (let ((at (nlk:iso-universal (field record "due"))))
    (and at (<= at now))))

(defun pending-due (&optional (now (get-universal-time)))
  "The reminders that came due and were not delivered yet, oldest first."
  (remove-if-not (lambda (record)
                   (and (equal "pending" (field record "status")) (due-p record now)))
                 (reverse (entries "reminder"))))

;;; --- the lines -------------------------------------------------------------------

(defun goal-line (goal)
  (format nil "~a  ~(~a~)  ~a~@[  (by ~a)~]~@[  ~d%%~]~@[  ~a~]"
          (field goal "id") (or (field goal "status") "active") (field goal "title")
          (field goal "deadline") (field goal "progress" :integer)
          (field goal "note")))

(defun topic-line (topic)
  (format nil "~a  ~a~@[  (until ~a)~]~@[  ~a~]"
          (field topic "id") (field topic "title") (field topic "expires")
          (field topic "rationale")))

(defun idea-line (idea)
  (format nil "~a  ~a~@[  (for ~a)~]~@[  ~a~]"
          (field idea "id") (field idea "title") (field idea "goal_id")
          (field idea "rationale")))

(defun feed-line (article)
  (format nil "~a  ~a  ~a~@[  ~a~]"
          (field article "id") (subseq (field article "created") 0 10)
          (field article "title") (field article "why")))

(defun reminder-line (reminder)
  (format nil "~a  ~(~a~)~@[  (due ~a)~]  ~a"
          (field reminder "id") (or (field reminder "status") "pending")
          (field reminder "due") (field reminder "text")))

(defun record-line (kind record)
  (cond ((equal kind "goal") (goal-line record))
        ((equal kind "topic") (topic-line record))
        ((equal kind "idea") (idea-line record))
        ((equal kind "feed") (feed-line record))
        (t (reminder-line record))))

(defun active-goals ()
  (remove-if-not (lambda (goal) (equal "active" (or (field goal "status") "active")))
                 (reverse (entries "goal"))))

(defun live-articles ()
  (remove-if (lambda (article) (field article "deleted" :boolean))
             (reverse (entries "feed"))))

(defun clipped (text limit)
  (if (and (stringp text) (> (length text) limit))
      (concatenate 'string (subseq text 0 (- limit 3)) "...")
      text))

;;; --- the actions -----------------------------------------------------------------

(defun overview-text ()
  (format nil "Muse, this profile's companion. State: ~a~%~{  ~a~^~%~}"
          (namestring (state-file))
          (append
           (loop for kind in +kinds+
                 collect (format nil "~a: ~d  (context section=~a)"
                                 (collection kind) (length (entries kind))
                                 (collection kind)))
           (list (format nil "reminders due: ~d~@[  next: ~a~]"
                         (length (pending-due))
                         (field (first (pending-due)) "text"))))))

(defun store-text (section &optional (limit 25))
  "The store as the model or the operator reads it: SECTION a collection name
(goals, topics, ideas, feed, reminders), preferences, status, or nothing for
the directory of sections."
  (let* ((section (or (and section (nlk:trimmed (string-downcase section))) "overview"))
         (kind (car (rassoc section +collections+ :test #'equal))))
    (cond ((equal section "overview") (overview-text))
          ((equal section "status") (status-text))
          ((equal section "preferences")
           (format nil "feed brief: ~a" (or (field (gethash "preferences" *companion*) "feed_brief")
                                             "(none: name the interests that should steer the Feed)")))
          (kind
           (let ((records (if (equal kind "reminder") (reverse (entries kind)) (entries kind))))
             (if (null records)
                 (format nil "nothing under ~a yet" (collection kind))
                 (format nil "~{~a~^~%~}" (loop for record in records
                                                 for index from 1 to limit
                                                 collect (record-line kind record))))))
          (t (overview-text)))))

(defun status-text ()
  (let* ((goals (entries "goal"))
         (count (lambda (status)
                  (count-if (lambda (goal) (equal status (or (field goal "status") "active")))
                            goals))))
    (format nil "muse: ~d active, ~d done, ~d cancelled goal~:p; ~d Feed article~:p; ~d reminder~:p (~d due); ~d interest~:p; store ~a"
            (funcall count "active") (funcall count "done") (funcall count "cancelled")
            (length (live-articles)) (length (entries "reminder")) (length (pending-due))
            (length (entries "topic")) (namestring (state-file)))))

(defun create-goal (data)
  (let ((goal (make-record "goal"
                            (list (cons "title" (required data "title"))
                                  (cons "description" (arg-text data "description"))
                                  (cons "criteria" (arg-text data "criteria"))
                                  (cons "deadline" (arg-text data "deadline"))
                                  (cons "status" "active")))))
    (changing (keep goal))
    (format nil "goal ~a kept: ~a~@[~%done when: ~a~]~%(goal_update status=done with a note once it is)"
            (field goal "id") (field goal "title") (field goal "criteria"))))

(defun update-goal (data)
  (let* ((goal (find-record "goal" (required data "id")))
         (status (arg-text data "status"))
         (progress (arg-integer data "progress")))
    (when (and status (not (member status '("active" "done" "cancelled") :test #'equal)))
      (fail "status is active, done or cancelled, not ~s" status))
    (changing
      (check-revision goal data)
      (touch goal)
      (when status (set-field goal "status" status))
      (when progress (set-field goal "progress" (max 0 (min 100 progress))))
      (dolist (pair '(("note" . "note") ("deadline" . "deadline")
                       ("description" . "description") ("criteria" . "criteria")))
        (let ((value (arg-text data (car pair))))
          (when value (set-field goal (cdr pair) value))))
      (when (equal status "done") (set-field goal "closed" (nlk:iso-now))))
    (format nil "goal ~a is now ~a: ~a~@[  (~a)~]"
            (field goal "id") (field goal "status") (field goal "title")
            (field goal "note"))))

(defun record-interest (data)
  (let* ((title (required data "title"))
         (days (or (arg-integer data "expires_days") (setting :topic-days)))
         (topic (make-record "topic"
                             (list (cons "title" title)
                                   (cons "rationale" (arg-text data "rationale"))
                                   (cons "expires" (days-from-now days))))))
    (changing (keep topic))
    (format nil "interest ~a kept: ~a until ~a (a newer mention renews it)"
            (field topic "id") title (field topic "expires"))))

(defun add-idea (data)
  (let ((idea (make-record "idea"
                           (list (cons "title" (required data "title"))
                                 (cons "rationale" (arg-text data "rationale"))
                                 (cons "goal_id" (arg-text data "goal_id"))
                                 (cons "sources" (arg-list data "sources"))))))
    (changing (keep idea))
    (format nil "idea ~a kept: ~a (a suggestion, not work started)"
            (field idea "id") (field idea "title"))))

(defun add-article (data)
  (let* ((title (required data "title"))
         (body (required data "content"))
         (key (or (arg-text data "key") title))
         (article (make-record "feed"
                               (list (cons "key" key)
                                     (cons "title" title)
                                     (cons "content" body)
                                     (cons "sources" (arg-list data "sources"))
                                     (cons "why" (arg-text data "why"))
                                     (cons "goal_id" (arg-text data "goal_id"))
                                     (cons "topic_id" (arg-text data "topic_id"))
                                     (cons "position" (or (arg-integer data "position") 0))))))
    (changing
      (when (find key (live-articles) :test #'equal :key (lambda (a) (field a "key")))
        (fail "the Feed already carries ~s: a stable key is one article, not two" key))
      (keep article))
    (format nil "Feed ~a: ~a (~d chars, ~d source~:p)"
            (field article "id") title (length body) (length (arg-list data "sources")))))

(defun list-feed (data)
  (let* ((limit (or (arg-integer data "limit") 10))
         (query (arg-text data "query"))
         (articles (remove-if-not
                    (lambda (article)
                      (or (null query)
                          (search (string-downcase query)
                                  (string-downcase (format nil "~a ~a"
                                                           (field article "title")
                                                           (field article "content")))
                                  :test #'char-equal)))
                    (live-articles))))
    (if (null articles)
        (format nil "the Feed is empty~@[ for ~s~]" query)
        (format nil "~{~a~^~%~}" (loop for article in articles
                                        for index from 1 to limit
                                        collect (feed-line article))))))

(defun read-record (data)
  (let* ((id (required data "id"))
         (kind (or (arg-text data "kind") (record-kind id)))
         (record (find-record kind id)))
    (if (equal kind "feed")
        (format nil "~a~%~%~a~%~%Sources: ~{~a~^, ~}~@[~%Why: ~a~]~%(read ~a)"
                (field record "title")
                (clipped (field record "content") (setting :feed-chars))
                (let ((sources (arg-list record "sources")))
                  (if sources sources (list "(none recorded)")))
                (field record "why") (field record "id"))
        (record-line kind record))))

(defun update-article (data)
  (let ((article (find-record "feed" (required data "id"))))
    (changing
      (check-revision article data)
      (touch article)
      (dolist (pair '(("title" . "title") ("content" . "content") ("feedback" . "feedback")))
        (let ((value (arg-text data (car pair))))
          (when value (set-field article (cdr pair) value))))
      (let ((sources (arg-list data "sources"))
            (position (arg-integer data "position")))
        (when sources (set-field article "sources" sources))
        (when position (set-field article "position" position)))
      (when (arg data "delete" :boolean) (set-field article "deleted" t)))
    (format nil "Feed ~a ~a" (field article "id")
            (if (field article "deleted" :boolean)
                "removed: its key will not come back"
                "updated"))))

(defun add-reminder (data)
  (let ((reminder (make-record "reminder"
                               (list (cons "text" (required data "text"))
                                     (cons "due" (stamp (arg-text data "due")))
                                     (cons "status" "pending")
                                     (cons "goal_id" (arg-text data "goal_id"))))))
    (seconds-of (field reminder "due"))
    (changing (keep reminder))
    (format nil "reminder ~a kept for ~a: ~a (one line at the next request after it is due)"
            (field reminder "id") (field reminder "due") (field reminder "text"))))

(defun resolve-reminder (data)
  (let* ((reminder (find-record "reminder" (required data "id")))
         (action (or (arg-text data "action") "done")))
    (when (equal (field reminder "status") "resolved")
      (format nil "reminder ~a was already resolved" (field reminder "id")))
    (changing
      (check-revision reminder data)
      (touch reminder)
      (cond ((equal action "snoozed")
             (set-field reminder "status" "pending")
             (set-field reminder "due" (days-from-now 1))
             (set-field reminder "snoozed" (nlk:iso-now)))
            ((member action '("done" "dismissed" "stop") :test #'equal)
             (set-field reminder "status" "resolved")
             (set-field reminder "resolution" action)
             (set-field reminder "resolved" (nlk:iso-now)))
            (t (fail "action is done, dismissed, stop or snoozed, not ~s" action))))
    (format nil "reminder ~a: ~(~a~)~@[  (due ~a)~]"
            (field reminder "id") (or (field reminder "resolution") "snoozed")
            (and (equal (field reminder "status") "pending") (field reminder "due")))))

(defun preferences (data)
  (let ((brief (arg-text data "feed_brief")))
    (if (null brief)
        (store-text "preferences")
        (progn
          (changing
            (set-field (gethash "preferences" *companion*) "feed_brief" brief)
            (set-field (gethash "preferences" *companion*) "updated" (nlk:iso-now)))
          (format nil "feed brief kept (~d chars); old articles stay as they are" (length brief))))))

(defun forget-record (data)
  (let* ((id (required data "id"))
         (kind (or (arg-text data "kind") (record-kind id)))
         (record (find-record kind id))
         (key (collection kind)))
    (changing
      (setf (gethash key *companion*) (remove record (entries kind))))
    (format nil "forgot ~a (~a); the rest of the store is untouched" id kind)))

(define-verb manage (action &optional data)
  "One Muse action, as muse_manage takes it: ACTION one of +ACTIONS+, DATA its
arguments as an object (or a plist through eval). => the answer as text."
  (let ((data (data-object data))
        (action (nlk:trimmed (string-downcase (princ-to-string action)))))
    (cond ((equal action "context") (store-text (arg-text data "section") (or (arg-integer data "limit") 25)))
          ((equal action "status") (status-text))
          ((equal action "goal_create") (create-goal data))
          ((equal action "goal_update") (update-goal data))
          ((equal action "interest_record") (record-interest data))
          ((equal action "idea_add") (add-idea data))
          ((equal action "feed_add") (add-article data))
          ((equal action "feed_list") (list-feed data))
          ((equal action "feed_update") (update-article data))
          ((equal action "content_read") (read-record data))
          ((equal action "notification_add") (add-reminder data))
          ((equal action "notification_resolve") (resolve-reminder data))
          ((equal action "preferences") (preferences data))
          ((equal action "forget") (forget-record data))
          (t (fail "~s is no Muse action: ~{~a~^, ~}" action +actions+)))))

;;; --- the verbs, for eval ---------------------------------------------------------

(define-verb context (&optional section)
  "The companion's store: a collection (goals, topics, ideas, feed, reminders),
preferences, status, or nothing for the directory of sections."
  (nodecode-muse::store-text section))

(define-verb goals ()
  "Every goal, newest first, done and cancelled included."
  (nodecode-muse::context "goals"))

(define-verb goal (title &key criteria deadline description)
  "Keep one goal, with what done means and by when."
  (nodecode-muse::create-goal
   (data-object (list "title" title "criteria" criteria "deadline" deadline
                      "description" description))))

(define-verb goal-done (id &optional note)
  "Mark goal ID done with NOTE, after telling the operator what was checked."
  (nodecode-muse::update-goal (data-object (list "id" id "status" "done" "note" note))))

(define-verb feed (&optional query)
  "The Feed's articles, newest first, optionally those matching QUERY."
  (nodecode-muse::list-feed (data-object (list "query" query "limit" 20))))

(define-verb feed-add (title content &key sources why)
  "Add one authored article to the Feed, with its sources and why it matters."
  (nodecode-muse::add-article
   (data-object (list "title" title "content" content "sources" sources "why" why))))

(define-verb remind (text &key due)
  "Keep a reminder; at the next model request after DUE it is said once."
  (nodecode-muse::add-reminder (data-object (list "text" text "due" due))))

(define-verb resolve (id &optional action)
  "Resolve reminder ID as done (default), dismissed, stop or snoozed."
  (nodecode-muse::resolve-reminder (data-object (list "id" id "action" action))))

;;; --- the tool --------------------------------------------------------------------

(nlk:define-contract muse-call
  (:doc "The companion in this profile: goals with completion criteria, expiring interests, ideas, a Feed of authored articles, and reminders. Read (help :muse) before the first write: each action's fields are named there. Every argument goes inside data, never beside action, e.g. {\"action\":\"context\",\"data\":{\"section\":\"feed\",\"limit\":10}}.")
  ("action" :text :required t :options +actions+
            :doc "what to do; every action's fields are in (help :muse)")
  ("data" :object
          :doc "the action's arguments as one object: title, id, status, note, criteria, deadline, due, query, limit, expected_revision, and so on"))

(defun answer-tool (call)
  "The tool body: the action's answer, or the refusal as the ERROR line the
model repairs from."
  (handler-case (manage (muse-call-action call) (muse-call-data call))
    (muse-error (condition) (nle:failure (format nil "ERROR: muse: ~a" (muse-error-detail condition))))
    (error (condition) (nle:failure (format nil "ERROR: muse: ~a" condition)))))

;;; --- /muse -----------------------------------------------------------------------

(defun panel ()
  "Bare /muse's rows: what the companion is holding, and the next step of each."
  (nle:list-dialog
   :title "Muse" :summary (format nil "~d reminder~:p due, ~d goal~:p active"
                                  (length (pending-due)) (length (active-goals)))
   :rows (append
          (loop for goal in (active-goals)
                for index from 1 to 5
                collect (nle:run-row (format nil "Done: ~a" (field goal "title"))
                                      (format nil "/muse done ~a" (field goal "id"))
                                      :detail (format nil "~@[done when ~a~]~@[ by ~a~]"
                                                      (field goal "criteria") (field goal "deadline"))))
          (loop for article in (live-articles)
                for index from 1 to 3
                collect (nle:run-row (format nil "Read: ~a" (field article "title"))
                                      (format nil "/muse read ~a" (field article "id"))
                                      :detail (or (field article "why") "in the Feed")))
          (list (nle:ask-row "Keep a goal" "Goal" "/muse goal"
                             :placeholder "what to finish, and by when" :verb "keep"
                             :detail "the companion holds it and asks at your next requests")
                (nle:ask-row "Remind me" "Remind" "/muse remind"
                             :placeholder "one line" :verb "keep"
                             :detail "said once, at the next request after it is due")))))

(defun run-slash (args session-id)
  "/muse typed alone: the store and the panel. done ID, cancel ID, resolve ID,
read ID, feed [QUERY], goal TITLE, remind TEXT, forget ID."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond ((zerop (length text)) (values (overview-text) (panel)))
          ((equal head "done") (update-goal (data-object (list "id" rest "status" "done"))))
          ((equal head "cancel") (update-goal (data-object (list "id" rest "status" "cancelled"))))
          ((equal head "resolve") (resolve-reminder (data-object (list "id" rest "action" "done"))))
          ((member head '("read" "show") :test #'equal)
           (read-record (data-object (list "id" rest))))
          ((equal head "feed") (list-feed (data-object (list "query" rest "limit" 20))))
          ((equal head "goal") (create-goal (data-object (list "title" rest))))
          ((equal head "remind") (add-reminder (data-object (list "text" rest))))
          ((equal head "forget") (forget-record (data-object (list "id" rest))))
          (t (fail "~s is not one of /muse's: done, cancel, resolve, read, feed, goal, remind, forget"
                   head)))))

;;; --- the due reminder -------------------------------------------------------------

(defun delivery-text (reminders)
  "One reminder line for the round about to be asked: what came due, said as
the operator's own errands, never as new instructions."
  (format nil "[muse: ~d reminder~:p came due; the operator wrote them for themselves. Say one, if it still matters, and resolve it with notification_resolve.]~{~%~a~}"
          (length reminders)
          (mapcar (lambda (reminder)
                    (format nil "  ~a: ~a" (field reminder "id")
                            (clipped (field reminder "text")
                                     (if *muse* (setting :remind-chars) +reminder-chars+))))
                  reminders)))

(defun mark-delivered (reminders)
  "REMINDERS are said: delivered, so no later request says them again."
  (changing
    (dolist (reminder reminders)
      (set-field reminder "status" "delivered")
      (set-field reminder "delivered" (nlk:iso-now)))))

(defun deliver (op)
  "At a model request: the reminders that came due ride it as one reminder, and
each is marked delivered so no later request says it again."
  (let ((turn (getf op :turn)))
    (when (and turn *companion* (pending-due))
      (let ((due (pending-due)))
        (nlk:record-turn-context-prompt turn :text (delivery-text due))
        (mark-delivered due)))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "Muse is this profile's companion: goals with completion criteria, expiring
interests, ideas, a quiet Feed of authored articles, and reminders that say
themselves once. It runs no scheduled job: a reminder comes due, and the next
model request carries it; nothing runs while the session idles.
The model has one tool, muse_manage: action, and every argument inside data.
  context   {section,limit}  the store; section one of goals, topics, ideas,
            feed, reminders, preferences, status; no data lists the sections
  status    {}                counts, what is due, where the store is
  goal_create {title,description,criteria,deadline}
  goal_update {id,status,note,progress,deadline,expected_revision}
            status is active, done or cancelled: tell the operator what was
            checked, then record done with a note
  interest_record {title,rationale,expires_days}  a passing topic; it expires
  idea_add  {title,rationale,goal_id,sources}  a suggestion, never work started
  feed_add  {key,title,content,sources,why,goal_id,topic_id}  one article; a
            stable key keeps the same article from arriving twice
  feed_list {query,limit} / content_read {id,kind}  the index and one body
  feed_update {id,title,content,sources,feedback,position,delete,expected_revision}
  notification_add {text,due,goal_id}  due is ISO-8601 or absent for now
  notification_resolve {id,action}  done, dismissed, stop or snoozed
  preferences {feed_brief}    what the Feed should be about
  forget {id,kind}            remove one record
Through eval, or from /muse: (muse:goals), (muse:goal \"title\" :criteria \"...\"),
(muse:goal-done \"goal-1\" \"...\"), (muse:feed), (muse:feed-add \"title\" \"body\"
:sources '(\"https://...\") :why \"...\"), (muse:remind \"text\" :due \"2026-11-10\"),
(muse:resolve \"reminder-1\"), (muse:context \"feed\"). /muse alone opens the
panel; /muse done ID, cancel ID, resolve ID, read ID, feed [QUERY], goal TITLE,
remind TEXT, forget ID are the lines it runs.
The store is one JSON file, <home>/muse/state.json, this profile's alone. Keep
what the operator actually asked for: a goal is accepted work, an interest
expires, an idea is not authority, and the Feed is for reading, not for a quota."
  "What (help :muse) answers while the cell runs.")

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell muse
  (:section ("muse")
    (:guide "nothing is required: the store is this profile's own, under muse/; topic_days is how long an interest lasts without a newer mention, feed_chars how much of one article content_read answers, remind_chars how much of a reminder the round carries")
    ("topic_days" :integer :default 14 :min 1
                  :doc "how long an interest lasts without a newer mention")
    ("feed_chars" :integer :default 4000 :min 200
                  :doc "how much of one article's body content_read answers")
    ("remind_chars" :integer :default 240 :min 40
                    :doc "how much of one reminder the round's line carries"))
  (:tool "muse_manage" (nlk:contract 'muse-call) 'answer-tool)
  (:help :muse "muse: this profile's companion - goals, expiring interests, ideas, a quiet Feed and one-line reminders at your next request; muse_manage, /muse, (help :muse)" +primer+)
  (:hook :round (nle:observer #'deliver "muse"))
  (:command "muse" 'run-slash
            :catches 'muse-error
            :description "The companion: goals, Feed, reminders")
  (:start (lambda ()
            (setf *companion* (load-state))
            (nle:on-stop (lambda () (setf *companion* nil))))))
