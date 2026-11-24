;;;; cell.lisp --- hyperactive: the goal tree, the promises, the tap cards, the pulse.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of justin06lee/hyperactive for Nodecode, MIT, credited in README and NOTICE.
;;;;
;;;; The original kept its rows in SQLite and woke itself through two Hermes cron
;;;; jobs that printed what was due. Nodecode has no cron of its own outside the
;;;; cron cell, so this rewrite keeps one JSON file under the home and runs one
;;;; worker: every pulse_minutes it looks for what is due and makes the result
;;;; the cell's standing notice -- the operator sees it at once, and the model
;;;; reads it at the tail of its next request (its notices block).
;;;;
;;;; What the original guarantees is kept: every message names the goal it
;;;; serves; a card is asked, re-asked once after card_minutes, and only a
;;;; LOW-stakes card is settled for the user after as long again, through a
;;;; typed action (reschedule a follow-up, schedule one, set a goal's status),
;;;; never an arbitrary tool call; money, public and irreversible cards wait
;;;; for the user forever; quiet hours defer, never drop.

(defpackage #:nodecode-hyperactive
  (:use #:cl)
  (:nicknames #:hyperactive)
  (:export #:hyperactive-error #:hyperactive-error-detail
           ;; the verbs a model or the operator calls; every one answers a string
           #:add-goal #:followup #:card #:commit #:decide #:status
           #:answer-card #:close-commitment #:review-decision
           #:pulse #:pulse-text #:brief #:why #:run-slash
           ;; data, so a layer can reword it
           #:+primer+ #:*notice-key*))

(in-package #:nodecode-hyperactive)

(nlk:define-peripheral hyperactive :not-running t :lock "hyperactive")

;;; --- the words ---------------------------------------------------------------

(defparameter +horizons+ '("life" "year" "quarter" "week")
  "The tree's horizons, outermost first.")

(defparameter +stakes+ '("low" "money" "public" "irreversible")
  "What a card can put at stake. Only LOW is ever settled without an answer.")

(defparameter +goal-statuses+ '("active" "done" "paused" "dropped")
  "What a goal's status can be.")

(defparameter *notice-key* "nodecode-hyperactive"
  "The standing notice the pulse keeps: one key, so a later pulse replaces the last and an empty one clears it.")

(defparameter *state* nil
  "The rows this home holds -- goals, commitments, decisions, follow-ups, cards, events -- or NIL
while the cell is not running. One JSON file under the home is its rendering.")

;;; --- when --------------------------------------------------------------------

(defun now ()
  "Seconds since the epoch, UTC: every time this cell keeps is one of these."
  (get-universal-time))

(defun clock-minutes (seconds)
  "Minutes after local midnight for SECONDS."
  (multiple-value-bind (s m h) (decode-universal-time seconds)
    (declare (ignore s))
    (+ (* 60 h) m)))

(defun when-text (seconds)
  "SECONDS as a person reads it locally: '18:00' today, 'Sat 07:00' this week,
'Nov 14 07:00' beyond -- the original's own three forms."
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time seconds)
    (declare (ignore s))
    (multiple-value-bind (s2 m2 h2 d2 mo2 y2) (decode-universal-time (now))
      (declare (ignore s2 m2 h2))
      (cond ((and (= d d2) (= mo mo2) (= y y2)) (format nil "~2,'0d:~2,'0d" h m))
            ((and (= y y2) (<= (abs (- seconds (now))) (* 7 86400)))
             (format nil "~a ~2,'0d:~2,'0d" (subseq "MonTueWedThuFriSatSun" (* 3 (1- dow)) (* 3 dow)) h m))
            (t (format nil "~a ~d ~2,'0d:~2,'0d"
                       (subseq "JanFebMarAprMayJunJulAugSepOctNovDec" (* 3 (1- mo)) (* 3 mo)) d h m))))))

(defun span-text (seconds)
  "SECONDS as a duration a person reads: '90 min', '2h', '3 days'."
  (cond ((>= seconds (* 2 86400)) (format nil "~d days" (floor seconds 86400)))
        ((>= seconds 3600) (if (zerop (mod seconds 3600)) (format nil "~dh" (floor seconds 3600))
                               (format nil "~,1fh" (/ seconds 3600.0))))
        (t (format nil "~d min" (max 1 (floor seconds 60))))))

(defun day-name (text)
  "The day index of TEXT ('mon', 'tuesday') or NIL."
  (let ((text (subseq text 0 (min 3 (length text)))))
    (position text '("mon" "tue" "wed" "thu" "fri" "sat" "sun") :test #'string=)))

(defun clock-when (text base)
  "TEXT as a clock time today, tomorrow or on the next such weekday, or a refusal."
  (let ((tomorrow (uiop:string-prefix-p "tomorrow " text))
        (day nil))
    (when tomorrow (setf text (nlk:trimmed (subseq text 9))))
    (unless tomorrow
      (let* ((name (and (>= (length text) 3) (day-name text)))
             (rest (and name (string-left-trim " ." (subseq text 3)))))
        (when (and name (plusp (length rest)) (digit-char-p (char rest 0)))
          (setf day name text rest))))
    (multiple-value-bind (h m)
        (ppcre:register-groups-bind (hour minute ampm) ("^(\\d{1,2})(?::(\\d{2}))?\\s*(am|pm)?$" text)
          (let ((hour (parse-integer hour))
                (minute (if minute (parse-integer minute) 0)))
            (when ampm (setf hour (+ (mod hour 12) (if (string= ampm "pm") 12 0))))
            (and (< hour 24) (< minute 60) (values hour minute))))
      (unless h
        (fail "can't read time ~s (try 'in 2h', '1d', 'tomorrow 07:00' or 2026-11-08T09:00)"
              (if tomorrow (format nil "tomorrow ~a" text) text)))
      (multiple-value-bind (s m0 h0 d mo y dow) (decode-universal-time base)
        (declare (ignore s m0 h0))
        (let ((today (encode-universal-time 0 m h d mo y)))
          (cond (tomorrow (+ today 86400))
                (day (+ today (* 86400 (mod (- day (1- dow)) 7))))
                ((> today base) today)
                (t (+ today 86400))))))))

(defun parse-when (&optional (text "now") &aux (base (now)))
  "TEXT as an epoch: 'now', 'in 2h', '30m', '3d', '2w', '07:00', '7am', 'tomorrow 07:00',
'Sun 07:00', or a local ISO '2026-11-08T09:00'. A refusal when it is none of these."
  (let ((text (string-downcase (nlk:trimmed (or text "")))))
    (cond
      ((or (zerop (length text)) (string= text "now")) base)
      ((let ((duration (nlk:parse-duration (if (uiop:string-prefix-p "in " text) (subseq text 3) text))))
         (and duration (+ base duration))))
      ((ppcre:register-groups-bind (y mo d h mi) ("^(\\d{4})-(\\d{2})-(\\d{2})[t ](\\d{2}):(\\d{2})" text)
         (encode-universal-time 0 (parse-integer mi) (parse-integer h) (parse-integer d)
                                (parse-integer mo) (parse-integer y))))
      (t (clock-when text base)))))

;;; --- the store: one JSON file under the home ----------------------------------

(defun state-file ()
  "Where this home keeps hyperactive's rows: one JSON file of its own."
  (nlk:home "hyperactive/state.json"))

(defun empty-state ()
  (list :next 1 :goals nil :commitments nil :decisions nil :followups nil :cards nil :events nil))

(defun jsonify (value)
  "VALUE as the encoder writes it: a keyword plist an object, a list an array, a scalar itself."
  (cond ((and (consp value) (keywordp (car value)))
         (apply #'nlk:make-json-object
                (loop for (key inner) on value by #'cddr
                      append (list (string-downcase (symbol-name key)) (jsonify inner)))))
        ((listp value) (when value (map 'vector #'jsonify value)))
        (t value)))

(defun unjson (value)
  "Decoded JSON as the rows this cell keeps: objects keyword plists, arrays lists."
  (cond ((hash-table-p value) (unjson (nlk:json-plist value)))
        ((and (vectorp value) (not (stringp value))) (map 'list #'unjson value))
        ((and (consp value) (keywordp (car value)))
         (loop for (key inner) on value by #'cddr collect key collect (unjson inner)))
        (t value)))

(defun load-state ()

  "The rows on disk, or none: a file that does not read as one is started over, never fatal."
  (let ((file (state-file)))
    (if (probe-file file)
        (let ((rows (handler-case (unjson (nlk:decode-json (nlk:read-text file)))
                      (error () nil))))
          (if (and (consp rows) (getf rows :next))
              (let ((state (empty-state)))
                (loop for (key value) on rows by #'cddr
                      when (member key (loop for (k nil) on (empty-state) by #'cddr collect k))
                        do (setf (getf state key) value))
                state)
              (empty-state)))
        (empty-state))))

(defun save-state ()
  "Write the rows: one object, atomically, under the home."
  (let ((file (state-file)))
    (ensure-directories-exist file)
    (nlk:write-file-atomically file (nlk:encode-json-object (jsonify *state*)))
    file))

(defun next-id ()
  (let ((id (or (getf *state* :next) 1)))
    (setf (getf *state* :next) (1+ id))
    id))

(defun rows (place)
  "The rows in PLACE, newest first, as they stand in the state: a change to one is a change to it."
  (getf *state* place))

(defun add-row (place row)
  (push row (getf *state* place))
  row)

(defun find-row (place id)
  (and (integerp id)
       (find-if (lambda (row) (eql id (getf row :id))) (rows place))))

(defun log-event (kind goal detail &optional ref)
  "One line of the trail every delivery leaves, with the goal it served and what caused it."
  (push (list :at (now) :kind kind :goal (or goal 0) :detail detail :ref (or ref ""))
        (getf *state* :events))
  (when (> (length (getf *state* :events)) 200)
    (setf (getf *state* :events) (subseq (getf *state* :events) 0 200))))

(defun touch-goal (goal &optional (at (now)))
  "A goal S saw movement: it is not stalled, and its one nudge may come again."
  (let ((row (find-row :goals goal)))
    (when row
      (setf (getf row :moved) at
            (getf row :nudged) 0))))

;;; --- goals -------------------------------------------------------------------

(defun horizon-rank (goal)
  (or (position (getf goal :horizon) +horizons+ :test #'string=) (length +horizons+)))

(defun goal-chain (goal)
  "GOAL and its ancestors, the goal first."
  (let ((chain '()))
    (loop for row = goal then (find-row :goals (getf row :parent))
          repeat 8
          while row
          do (push row chain))
    (nreverse chain)))

(defun goal-label (goal)
  (format nil "~a (~a)" (getf goal :title) (getf goal :horizon)))

(defun goal-trace (goal)
  "The goal with the tree above it: 'ship it (week) < Q4 (quarter)'."
  (format nil "~{~a~^ < ~}" (mapcar #'goal-label (reverse (goal-chain goal)))))

(defun goal-line (goal &optional extra)
  "The one line every message ends with: the goal it serves."
  (format nil "goal: ~a~@[ · ~a~]" (let ((row (find-row :goals goal))) (if row (goal-trace row) "Inbox")) extra))

(defun inbox-goal ()
  "The goal everything unfiled goes to, made when it is first needed."
  (or (find-if (lambda (row) (and (string= (getf row :title) "Inbox") (string= (getf row :status) "active")))
               (rows :goals))
      (add-goal "Inbox" "week" :why "everything not filed under a goal")))

(defun resolve-goal (goal)
  "GOAL as an id: the one given (a refusal when there is none), or the Inbox."
  (if goal
      (or (find-row :goals goal) (fail "no goal #~a; ha_status lists them" goal))
      (inbox-goal)))

(defun add-goal (title horizon &key parent why)
  "A goal in the tree. => its row."
  (setf title (nlk:trimmed (or title "")))
  (unless (plusp (length title)) (fail "a goal needs a title"))
  (setf horizon (string-downcase (or horizon "week")))
  (unless (member horizon +horizons+ :test #'string=)
    (fail "horizon must be one of ~{~a~^, ~}, not ~s" +horizons+ horizon))
  (when parent (unless (find-row :goals parent) (fail "no goal #~a" parent)))
  (let ((row (list :id (next-id) :parent (or parent 0) :horizon horizon :title title
                   :why (or (nlk:trimmed (or why "")) "") :status "active"
                   :created (now) :moved (now) :nudged 0)))
    (add-row :goals row)
    (log-event "goal.add" (getf row :id) title (format nil "goal ~d" (getf row :id)))
    (save-state)
    row))

(defun set-goal-status (goal status)
  (let ((row (or (find-row :goals goal) (fail "no goal #~a" goal))))
    (setf status (string-downcase (or status "")))
    (unless (member status +goal-statuses+ :test #'string=)
      (fail "status must be one of ~{~a~^, ~}" +goal-statuses+))
    (setf (getf row :status) status)
    (log-event (format nil "goal.~a" status) goal (getf row :title))
    (save-state)
    row))

(defun render-tree (&key (max-lines 25))
  "The active goals, one a line, indented by how deep in the tree they sit."
  (let ((lines '()))
    (dolist (goal (stable-sort (remove-if-not (lambda (row) (string= (getf row :status) "active"))
                                              (copy-list (rows :goals)))
                                #'< :key (lambda (row) (or (getf row :id) 0))))
      (when (< (length lines) max-lines)
        (push (format nil "~a- ~a [goal ~d]~@[ · why: ~a~]"
                      (make-string (* 2 (min 3 (1- (length (goal-chain goal))))) :initial-element #\Space)
                      (goal-label goal) (getf goal :id)
                      (let ((why (getf goal :why))) (and (plusp (length why)) why)))
              lines)))
    (if lines (format nil "~{~a~^~%~}" (nreverse lines)) "(no goals yet)")))

;;; --- follow-ups: what the cell will say later ---------------------------------

(defun add-followup (goal text when &key (kind "followup"))
  "Something to say later, delivered by the pulse even if nobody writes again."
  (setf text (nlk:trimmed (or text "")))
  (unless (plusp (length text)) (fail "a follow-up needs text: the message, self-contained"))
  (let ((row (list :id (next-id) :due (parse-when when) :goal (getf (resolve-goal goal) :id)
                   :kind (let ((given (nlk:trimmed (or kind "")))) (if (plusp (length given)) given "followup"))
                   :text text :status "pending" :sent 0 :created (now))))
    (add-row :followups row)
    (log-event "followup.add" (getf row :goal) text (format nil "follow-up ~d" (getf row :id)))
    (save-state)
    row))

(defun pending-followups ()
  (remove-if-not (lambda (row) (string= (getf row :status) "pending")) (rows :followups)))

(defun followup-lines ()
  (let ((rows (pending-followups)))
    (if rows
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (row)
                          (format nil "#~d ~a~@[ (goal #~d)~]: ~a"
                                  (getf row :id) (when-text (getf row :due))
                                  (let ((goal (getf row :goal))) (and (plusp goal) goal))
                                  (getf row :text)))
                        (stable-sort (copy-list rows) #'< :key (lambda (row) (getf row :due)))))
        "(nothing)")))

;;; --- tap cards: a question with a recommendation and a typed action ------------

(defun option-list (options)
  "OPTIONS as a list of words: a string splits on / , or |, an array is itself; yes and no by default."
  (let ((items (typecase options
                 (null nil)
                 (string (list options))
                 (vector (coerce options 'list))
                 (list options)
                 (t nil))))
    (let ((words (remove "" (remove-duplicates
                             (loop for item in items
                                   for text = (nlk:trimmed (princ-to-string item))
                                   append (mapcar #'nlk:trimmed
                                                   (uiop:split-string text :separator '(#\/ #\, #\|))))
                             :test #'string-equal)
                         :test #'string=)))
      (or words '("yes" "no")))))

(defun join-options (options)
  (format nil "~{~a~^ / ~}" options))

(defun match-option (options text)
  "The option TEXT names ('Yes', 'yes.', '1'), or NIL."
  (let ((text (string-downcase (nlk:trimmed (or text "")))))
    (setf text (string-right-trim " .!" text))
    (or (find text options :test #'string-equal)
        (let ((number (ignore-errors (parse-integer text :junk-allowed t))))
          (and number (<= 1 number (length options)) (nth (1- number) options))))))

(defun either-options (options)
  (cond ((null (rest options)) (first options))
        ((null (cddr options)) (format nil "~a or ~a" (first options) (second options)))
        (t (format nil "~{~a~^, ~} or ~a" (butlast options) (car (last options))))))

(defun stakes-reason (stakes)
  (cond ((string= stakes "money") "It involves money")
        ((string= stakes "public") "It's public")
        ((string= stakes "irreversible") "It can't be undone")
        (t "")))

(defun action-plist (value)
  "An action from a tool call as a plist: the JSON object's members, keys keywords."
  (when (hash-table-p value)
    (loop for key being the hash-keys of value using (hash-value inner)
          append (list (intern (string-upcase key) :keyword) inner))))

(defun action-text (action key)
  (let ((value (and (consp action) (getf action key))))
    (cond ((null value) "") ((stringp value) (nlk:trimmed value)) (t (princ-to-string value)))))

(defun action-int (action key)
  (let ((value (and (consp action) (getf action key))))
    (cond ((integerp value) value)
          ((stringp value) (ignore-errors (parse-integer value :junk-allowed t)))
          (t nil))))

(defun normalize-action (action)
  "ACTION as the typed action the cell may perform itself, or NIL; a refusal when it is not one.
A duration ('in 1m') is kept as written, so it counts from when it runs; a clock time is fixed now."
  (let* ((action (or (action-plist action) (and (consp action) action)))
         (type (string-downcase (action-text action :type))))
    (when (and (consp action) (null (third action)) (null (rest action))) (setf action nil))
    (when action
      (cond
        ((string= type "reschedule")
         (let ((id (action-int action :followup_id))
               (when (action-text action :when)))
           (let ((row (find-row :followups id)))
             (unless (and row (string= (getf row :status) "pending"))
               (fail "reschedule needs the id of a pending follow-up (ha_status lists them)"))
             (let ((at (if (and (plusp (length when)) (nlk:parse-duration
                                                       (if (uiop:string-prefix-p "in " when) (subseq when 3) when)))
                           when
                           (parse-when when))))
               (list :type type :followup_id id :when at
                     :text (let ((text (action-text action :text)))
                             (if (plusp (length text)) text (getf row :text))))))))
        ((string= type "schedule")
         (let ((text (action-text action :text)))
           (unless (plusp (length text)) (fail "a schedule action needs text"))
           (list :type type :text text :when (action-text action :when))))
        ((string= type "goal_status")
         (let ((id (action-int action :goal_id))
               (status (string-downcase (action-text action :status))))
           (unless (find-row :goals id) (fail "a goal_status action needs a goal_id that exists"))
           (unless (member status +goal-statuses+ :test #'string=)
             (fail "a goal_status action needs status: ~{~a~^, ~}" +goal-statuses+))
           (list :type type :goal_id id :status status)))
        (t (fail "action type must be reschedule, schedule or goal_status (or no action)"))))))

(defun add-card (question recommended &key options stakes goal action when)
  "A card: a question, its options, the recommended one, and what to do if it wins."
  (setf question (nlk:trimmed (or question "")))
  (unless (plusp (length question)) (fail "a card needs a question"))
  (let* ((opts (option-list options))
         (choice (match-option opts (or recommended (first opts))))
         (stakes (let ((given (string-downcase (nlk:trimmed (or stakes "")))))
                   (if (plusp (length given)) given "low"))))
    (unless choice (fail "recommended must be one of the options: ~a" (join-options opts)))
    (unless (member stakes +stakes+ :test #'string=)
      (fail "stakes must be one of ~{~a~^, ~}" +stakes+))
    (let ((row (list :id (next-id) :due (parse-when (or when "now"))
                     :action (normalize-action action) :goal (getf (resolve-goal goal) :id) :question question
                     :options (join-options opts) :recommended choice :stakes stakes :status "open"
                     :asked 0 :reasked 0 :held 0 :answer "" :by "" :result "" :created (now))))
      (add-row :cards row)
      (log-event "card.add" (getf row :goal) question (format nil "card ~d" (getf row :id)))
      (save-state)
      row)))

(defun open-cards (&key asked-only)
  (remove-if-not (lambda (row)
                   (and (string= (getf row :status) "open")
                        (or (not asked-only) (plusp (getf row :asked)))))
                 (rows :cards)))

(defun card-step (card at timeout)
  "What to do with CARD now: ask, wait, reask, apply or hold -- the original's one rule."
  (cond ((not (string= (getf card :status) "open")) "wait")
        ((zerop (getf card :asked)) (if (<= (getf card :due) at) "ask" "wait"))
        ((zerop (getf card :reasked)) (if (>= (- at (getf card :asked)) timeout) "reask" "wait"))
        ((< (- at (getf card :reasked)) timeout) "wait")
        ((string= (getf card :stakes) "low") "apply")
        ((zerop (getf card :held)) "hold")
        (t "wait")))

(defun card-question-line (card)
  (format nil "~a  ~a" (getf card :question) (getf card :options)))

(defun card-ask-text (card timeout)
  (let ((low (string= (getf card :stakes) "low")))
    (format nil "~a~%~a~%Reply ~a."
            (card-question-line card)
            (if low
                (format nil "I'd go with ~a. If I don't hear back, I'll do that in ~a."
                        (getf card :recommended) (span-text (* 2 timeout)))
                (format nil "I'd go with ~a, but it's yours to decide. ~a."
                        (getf card :recommended) (stakes-reason (getf card :stakes))))
            (either-options (option-list (getf card :options))))))

(defun card-reask-text (card timeout)
  (if (string= (getf card :stakes) "low")
      (format nil "Still open: ~a~%Still no answer. I'll go with ~a in ~a."
              (card-question-line card) (getf card :recommended) (span-text timeout))
      (format nil "Still open: ~a~%~a, so it waits for you."
              (card-question-line card) (stakes-reason (getf card :stakes)))))

(defun apply-card-action (card)
  "Do what CARD says if its recommendation wins. => a short result, '' when nothing moved."
  (let* ((action (getf card :action))
         (type (action-text action :type)))
    (cond
      ((string= type "reschedule")
       (let ((row (find-row :followups (action-int action :followup_id))))
         (if (and row (string= (getf row :status) "pending"))
             (let* ((when (let ((at (getf action :when)))
                             (if (integerp at) at (parse-when at))))
                    (text (action-text action :text)))
               (setf (getf row :due) when)
               (when (plusp (length text)) (setf (getf row :text) text))
               (format nil "moved \"~a\" to ~a" (getf row :text) (when-text when)))
             (format nil "follow-up #~a was no longer pending, so nothing moved"
                     (action-int action :followup_id)))))
      ((string= type "schedule")
       (let ((row (add-followup (getf card :goal) (action-text action :text) (action-text action :when))))
         (format nil "I'll remind you at ~a" (when-text (getf row :due)))))
      ((string= type "goal_status")
       (let ((row (set-goal-status (action-int action :goal_id) (action-text action :status))))
         (format nil "marked \"~a\" ~a" (getf row :title) (getf row :status))))
      (t ""))))

(defun answer-card (card-id answer &key (by "user") (at (now)))
  "Record the answer to a card; the typed action runs only when the recommendation won."
  (let* ((card (or (find-row :cards card-id) (fail "no card #~a" card-id))))
    (unless (string= (getf card :status) "open")
      (fail "card #~a is already ~a (~a)" card-id (getf card :status)
            (let ((given (getf card :answer))) (if (plusp (length given)) given "unanswered"))))
    (let ((choice (match-option (option-list (getf card :options)) answer)))
      (unless choice (fail "card #~a options are: ~a" card-id (getf card :options)))
      (let ((result (if (string= choice (getf card :recommended)) (apply-card-action card) "")))
        (setf (getf card :status) (if (string= by "timeout") "applied" "answered")
              (getf card :answer) choice
              (getf card :by) by
              (getf card :result) result)
        (log-event (format nil "card.~a" (getf card :status)) (getf card :goal)
                   (format nil "~a by ~a~@[: ~a~]" choice by (and (plusp (length result)) result))
                   (format nil "card ~d" card-id))
        (touch-goal (getf card :goal) at)
        (save-state)
        card))))

(defun card-lines ()
  (let ((rows (open-cards)))
    (if rows
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (row)
                          (format nil "card ~d: ~a (rec. ~a, ~a stakes, goal #~d)~@[ · applies: ~a~]"
                                  (getf row :id) (card-question-line row) (getf row :recommended)
                                  (getf row :stakes) (getf row :goal)
                                  (let ((type (action-text (getf row :action) :type)))
                                    (and (plusp (length type)) type))))
                        rows))
        "(nothing open)")))

;;; --- commitments and decisions -------------------------------------------------

(defun add-commitment (direction person what &key due goal)
  "A promise either way: i_owe (the user promised) or they_owe (someone promised them)."
  (setf direction (string-downcase (nlk:trimmed (or direction ""))))
  (unless (member direction '("i_owe" "they_owe") :test #'string=)
    (fail "direction must be i_owe or they_owe"))
  (setf person (nlk:trimmed (or person "")) what (nlk:trimmed (or what "")))
  (unless (plusp (length person)) (fail "a commitment needs the person it is with"))
  (unless (plusp (length what)) (fail "a commitment needs what was promised"))
  (let ((row (list :id (next-id) :due (if (plusp (length (or due ""))) (parse-when due) 0)
                   :goal (getf (resolve-goal goal) :id) :direction direction
                   :person person :what what
                   :status "open" :notified 0 :created (now))))
    (add-row :commitments row)
    (log-event "commit.add" (getf row :goal) (commitment-text row) (format nil "commitment ~d" (getf row :id)))
    (save-state)
    row))

(defun commitment-text (row)
  (format nil "~a ~a ~a"
          (if (string= (getf row :direction) "i_owe") "I owe" "owes me")
          (getf row :person) (getf row :what)))

(defun close-commitment (commitment-id &key (status "done"))
  (let* ((row (or (find-row :commitments commitment-id) (fail "no commitment #~a" commitment-id))))
    (setf status (string-downcase (or status "done")))
    (unless (member status '("done" "dropped") :test #'string=)
      (fail "a commitment closes as done or dropped"))
    (setf (getf row :status) status (getf row :closed) (now))
    (log-event (format nil "commit.~a" status) (getf row :goal) (commitment-text row))
    (touch-goal (getf row :goal))
    (save-state)
    row))

(defun commitment-lines ()
  (let ((rows (remove-if-not (lambda (row) (string= (getf row :status) "open")) (rows :commitments))))
    (if rows
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (row)
                          (format nil "#~d ~a~@[ · due ~a~] (goal #~d)"
                                  (getf row :id) (commitment-text row)
                                  (let ((due (getf row :due))) (and (plusp due) (when-text due)))
                                  (getf row :goal)))
                        (stable-sort (copy-list rows) #'< :key (lambda (row) (or (getf row :due) 0)))))
        "(nothing)")))

(defun add-decision (title choice &key rationale review goal)
  "A decision, its reason, and when to ask 'still right?'."
  (setf title (nlk:trimmed (or title "")) choice (nlk:trimmed (or choice "")))
  (unless (plusp (length title)) (fail "a decision needs a title"))
  (let ((row (list :id (next-id) :review (if (plusp (length (or review ""))) (parse-when review) 0)
                   :goal (getf (resolve-goal goal) :id) :title title :choice choice
                   :rationale (nlk:trimmed (or rationale ""))
                   :status "open" :notified 0 :created (now))))
    (add-row :decisions row)
    (log-event "decision.add" (getf row :goal) title (format nil "decision ~d" (getf row :id)))
    (save-state)
    row))

(defun review-decision (decision-id verdict &key choice review)
  "Keep or revise a decision, optionally on a new review date."
  (let* ((row (or (find-row :decisions decision-id) (fail "no decision #~a" decision-id)))
         (verdict (string-downcase (nlk:trimmed (or verdict "")))))
    (unless (member verdict '("keep" "revise") :test #'string=)
      (fail "verdict must be keep or revise"))
    (let ((next (if (plusp (length (or review ""))) (parse-when review) 0))
          (new-choice (nlk:trimmed (or choice ""))))
      (when (plusp (length new-choice)) (setf (getf row :choice) new-choice))
      (setf (getf row :review) next
            (getf row :notified) 0
            (getf row :status) (if (plusp next) "open" (if (string= verdict "keep") "kept" "revised")))
      (log-event (format nil "decision.~a" verdict) (getf row :goal) (getf row :choice)
                 (format nil "decision ~d" decision-id))
      (touch-goal (getf row :goal))
      (save-state)
      row)))

(defun decision-lines ()
  (let ((rows (remove-if-not (lambda (row) (string= (getf row :status) "open")) (rows :decisions))))
    (if rows
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (row)
                          (format nil "#~d ~a: ~a~@[ · review ~a~] (goal #~d)"
                                  (getf row :id) (getf row :title) (getf row :choice)
                                  (let ((review (getf row :review))) (and (plusp review) (when-text review)))
                                  (getf row :goal)))
                        rows))
        "(nothing)")))

(defun status-text ()
  "The tree and everything the cell is tracking or has scheduled: what /hyperactive answers."
  (format nil "Goals:~%~a~%~%Open cards:~%~a~%~%Commitments:~%~a~%~%Decisions:~%~a~%~%Scheduled:~%~a"
          (render-tree) (card-lines) (commitment-lines) (decision-lines) (followup-lines)))

(define-verb status ()
  "The goal tree and everything hyperactive is tracking or has scheduled."
  (status-text))

(define-verb brief ()
  "The data a daily brief would be written from: the tree, what is open, what is due."
  (status-text))

;;; --- the pulse: what is due, as one message ------------------------------------

(defun in-quiet-hours-p (spec at)
  "SPEC as '22:00-07:00', local, wrapping midnight; empty means never quiet."
  (let* ((spec (nlk:trimmed (or spec "")))
         (dash (position #\- spec)))
    (when (and dash (plusp dash))
      (let ((start (minute-of (subseq spec 0 dash)))
            (end (minute-of (subseq spec (1+ dash))))
            (now (clock-minutes at)))
        (cond ((or (null start) (null end)) nil)
              ((< start end) (<= start now (1- end)))
              (t (or (>= now start) (< now end))))))))

(defun minute-of (text)
  "TEXT as minutes after midnight ('22:00'), or NIL."
  (let ((colon (position #\: text)))
    (when colon
      (let ((h (ignore-errors (parse-integer text :end colon)))
            (m (ignore-errors (parse-integer text :start (1+ colon) :junk-allowed t))))
        (and h m (<= 0 h 23) (<= 0 m 59) (+ (* 60 h) m))))))


(defun pulse-text (&key (at (now)))
  "What is due now as one message, marking what it says: NIL when nothing is due, or in quiet hours."
  (when (in-quiet-hours-p (setting :quiet-hours) at)
    (return-from pulse-text nil))
  (let* ((timeout (* 60 (setting :card-minutes)))
         (stale (* 86400 (setting :stale-days)))
         (cap (max 1 (setting :max-items)))
         (lines '()))
    (labels ((space-left () (< (length lines) cap))
             (emit (kind goal text &optional ref)
               (push (format nil "~a~%   ~a" text (goal-line goal ref)) lines)
               (log-event kind goal text ref)))
      ;; 1. tap cards: what waits on the person comes first
      (dolist (card (open-cards))
        (when (space-left)
          (let ((step (card-step card at timeout))
                (ref (format nil "card ~d" (getf card :id))))
            (cond
              ((string= step "ask")
               (setf (getf card :asked) at)
               (emit "pulse.card.ask" (getf card :goal) (card-ask-text card timeout) ref))
              ((string= step "reask")
               (setf (getf card :reasked) at)
               (emit "pulse.card.reask" (getf card :goal) (card-reask-text card timeout) ref))
              ((string= step "apply")
               (let ((answer (getf card :recommended)))
                 (answer-card (getf card :id) answer :by "timeout" :at at)
                 (let ((result (getf card :result)))
                   (emit "pulse.card.apply" (getf card :goal)
                         (format nil "No reply on \"~a\", so I went with ~a~@[: ~a~]."
                                 (getf card :question) answer
                                 (and (plusp (length result)) result))
                         ref))))
              ((string= step "hold")
               (setf (getf card :held) at)
               (emit "pulse.card.hold" (getf card :goal)
                     (format nil "\"~a\" is still open. ~a, so I won't decide it for you.~%Reply ~a when you're ready."
                             (getf card :question) (stakes-reason (getf card :stakes))
                             (either-options (option-list (getf card :options))))
                     ref))))))
      ;; 2. follow-ups whose time has come
      (dolist (row (stable-sort (copy-list (pending-followups)) #'< :key (lambda (row) (getf row :due))))
        (when (space-left)
          (when (<= (getf row :due) at)
            (setf (getf row :status) "sent" (getf row :sent) at)
            (emit "pulse.followup" (getf row :goal) (getf row :text)
                  (format nil "follow-up ~d, ~a" (getf row :id) (getf row :kind))))))
      ;; 3. commitments due, one notice each
      (dolist (row (rows :commitments))
        (when (space-left)
          (let ((due (getf row :due)))
            (when (and (plusp due) (<= due at) (string= (getf row :status) "open")
                       (zerop (getf row :notified)))
              (setf (getf row :notified) at)
              (emit "pulse.commit.due" (getf row :goal)
                    (format nil "~a~@[, due ~a~]" (commitment-text row) (when-text due))
                    (format nil "commitment ~d" (getf row :id)))))))
      ;; 4. decisions whose review date is here
      (dolist (row (rows :decisions))
        (when (space-left)
          (let ((review (getf row :review)))
            (when (and (plusp review) (<= review at) (string= (getf row :status) "open")
                       (zerop (getf row :notified)))
              (setf (getf row :notified) at)
              (emit "pulse.decision.review" (getf row :goal)
                    (format nil "Still right? \"~a\": ~a~@[ · ~a~]" (getf row :title) (getf row :choice)
                            (let ((why (getf row :rationale))) (and (plusp (length why)) why)))
                    (format nil "decision ~d" (getf row :id)))))))
      ;; 5. a goal standing still, nudged once until it moves again
      (dolist (row (rows :goals))
        (when (space-left)
          (let ((moved (getf row :moved)))
            (when (and (string= (getf row :status) "active")
                       (zerop (getf row :nudged))
                       (member (getf row :horizon) '("week" "quarter") :test #'string=)
                       (plusp moved)
                       (<= (+ moved (* stale (if (string= (getf row :horizon) "quarter") 4 1))) at))
              (setf (getf row :nudged) at)
              (emit "pulse.goal.stalled" (getf row :id)
                    (format nil "Nothing has moved on \"~a\" for ~a. What's the next step?"
                            (getf row :title) (span-text (- at moved)))
                    (format nil "goal ~d" (getf row :id)))))))
      (save-state)
      (when lines (format nil "~{~a~^~%~}" (nreverse lines))))))

(defun pulse ()
  "One pass: what is due becomes the cell's standing notice -- a toast now, and a line the model
reads at the tail of its next request. Nothing due clears it."
  (let ((text (pulse-text)))
    (nle:notice text :key *notice-key* :level :info)
    text))

(define-verb why ()
  "Why hyperactive spoke: the newest delivery and the goals it served."
  (let ((spoken (remove-if-not (lambda (event)
                                 (let ((kind (getf event :kind)))
                                   (and (stringp kind) (uiop:string-prefix-p "pulse." kind))))
                               (rows :events))))
    (if (null spoken)
        "hyperactive: it has not spoken yet"
        (let ((at (getf (first spoken) :at))
              (lines '()))
          (dolist (event spoken)
            (when (and (eql at (getf event :at)) (< (length lines) 8))
              (push (format nil "- ~a: ~a~%   ~a" (subseq (getf event :kind) 6) (getf event :detail)
                            (goal-line (getf event :goal) (getf event :ref)))
                    lines)))
          (format nil "The last message (~a):~%~{~a~^~%~}" (when-text at) (nreverse lines))))))

;;; --- the tools the model calls ---------------------------------------------------

(defun arg (arguments key)
  (let ((value (and (hash-table-p arguments) (gethash key arguments))))
    (cond ((null value) "")
          ((stringp value) (nlk:trimmed value))
          (t (princ-to-string value)))))

(defun arg-value (arguments key)
  (and (hash-table-p arguments) (gethash key arguments)))

(defun arg-int (arguments key)
  (let ((value (arg-value arguments key)))
    (cond ((integerp value) value)
          ((stringp value) (ignore-errors (parse-integer value :junk-allowed t)))
          (t nil))))

(defun tool-goal-add (arguments)
  (let* ((goal (add-goal (arg arguments "title") (or (arg arguments "horizon") "week")
                           :parent (arg-int arguments "parent_id") :why (arg arguments "why")))
         (when-text-in (arg arguments "follow_up_in"))
         (follow (and (plusp (length when-text-in))
                      (add-followup (getf goal :id)
                                    (let ((text (arg arguments "follow_up_text")))
                                      (if (plusp (length text)) text
                                          (format nil "How is \"~a\" going? What moved, and what's next?"
                                                  (getf goal :title))))
                                    when-text-in))))
    (format nil "goal ~d added: ~a (~a)~@[ · check-in #~d at ~a~]"
            (getf goal :id) (getf goal :title) (getf goal :horizon)
            (and follow (getf follow :id)) (and follow (when-text (getf follow :due))))))

(defun tool-followup (arguments)
  (let ((row (add-followup (arg-int arguments "goal_id") (arg arguments "text")
                            (arg arguments "when")
                            :kind (let ((kind (arg arguments "kind")))
                                    (if (plusp (length kind)) kind "followup")))))
    (format nil "follow-up #~d at ~a~@[ (goal #~d)~]: ~a"
            (getf row :id) (when-text (getf row :due))
            (let ((goal (getf row :goal))) (and (plusp goal) goal))
            (getf row :text))))

(defun tool-card (arguments)
  (let ((card-id (arg-int arguments "card_id")))
    (if card-id
        (let ((row (answer-card card-id (arg arguments "answer"))))
          (let ((result (getf row :result)))
            (format nil "card #~d: ~a~@[ · done: ~a~]" (getf row :id) (getf row :answer)
                    (and (plusp (length result)) result))))
        (let ((row (add-card (arg arguments "question") (arg arguments "recommended")
                             :options (arg-value arguments "options")
                             :stakes (arg arguments "stakes")
                             :goal (arg-int arguments "goal_id")
                             :action (arg-value arguments "action")
                             :when (let ((when (arg arguments "when")))
                                     (if (plusp (length when)) when "now")))))
          (let ((note (when (and (string= (getf row :stakes) "low") (null (getf row :action)))
                        " No action attached: applying the recommendation changes only the record.")))
            (format nil "card #~d, asked at ~a: ~a (~a, rec. ~a, ~a stakes)~a"
                    (getf row :id) (when-text (getf row :due)) (getf row :question)
                    (getf row :options) (getf row :recommended) (getf row :stakes) note))))))

(defun tool-commit (arguments)
  (let ((commitment-id (arg-int arguments "commitment_id")))
    (if commitment-id
        (let ((row (close-commitment commitment-id :status (let ((status (arg arguments "status")))
                                                             (if (plusp (length status)) status "done")))))
          (format nil "commitment #~d: ~a" (getf row :id) (getf row :status)))
        (let ((row (add-commitment (arg arguments "direction") (arg arguments "person")
                                    (arg arguments "what")
                                    :due (arg arguments "due") :goal (arg-int arguments "goal_id"))))
          (format nil "commitment #~d: ~a~@[ · due ~a~]"
                  (getf row :id) (commitment-text row)
                  (let ((due (getf row :due))) (and (plusp due) (when-text due))))))))

(defun tool-decide (arguments)
  (let ((decision-id (arg-int arguments "decision_id")))
    (if decision-id
        (let ((row (review-decision decision-id (arg arguments "verdict")
                                     :choice (arg arguments "choice")
                                     :review (arg arguments "review_in"))))
          (format nil "decision #~d: ~a (~a)~@[ · review ~a~]"
                  (getf row :id) (getf row :status) (getf row :choice)
                  (let ((review (getf row :review))) (and (plusp review) (when-text review)))))
        (let ((row (add-decision (arg arguments "title") (arg arguments "choice")
                                  :rationale (arg arguments "rationale")
                                  :review (arg arguments "review_in")
                                  :goal (arg-int arguments "goal_id"))))
          (format nil "decision #~d: ~a -> ~a~@[ · review ~a~]"
                  (getf row :id) (getf row :title) (getf row :choice)
                  (let ((review (getf row :review))) (and (plusp review) (when-text review))))))))

(defun tool-status (arguments)
  (declare (ignore arguments))
  (status))

;;; --- the schemas the provider shows --------------------------------------------

(defun prop (type &optional description enum)
  (let ((object (nlk:make-json-object "type" type)))
    (when description (setf (gethash "description" object) description))
    (when enum (setf (gethash "enum" object) enum))
    object))

(defparameter *goal-subject*
  "Goal this serves (see the goal tree). Omit only if none fits: it goes to the Inbox."
  "The one description every tool gives its goal member.")

(defparameter *goal-add-schema*
  (list :description
        (concatenate 'string
                     "Record one of the user's goals in their objective tree (life > year > quarter > week). "
                     "Use when the user states something they want to achieve, including 'X by <day>' (horizon week). "
                     "Link it to a parent goal when one fits (see the goal tree). Pass follow_up_in to schedule one "
                     "check-in; that is usually all a new goal needs.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "title" (prop "string" "The goal, short and concrete.")
                       "horizon" (prop "string" "Time horizon." (coerce +horizons+ 'vector))
                       "parent_id" (prop "integer" "Parent goal id, if any.")
                       "why" (prop "string" "Why it matters to the user, in their words.")
                       "follow_up_in" (prop "string" "Optional: when to check in, e.g. 'in 2h', '3d', or a time.")
                       "follow_up_text" (prop "string" "What to ask at the check-in, self-contained."))
         "required" (vector "title" "horizon"))))

(defparameter *followup-schema*
  (list :description
        (concatenate 'string
                     "Schedule something to say to the user on your own later: a follow-up, reminder or nudge. "
                     "Delivered by the cell's pulse even if the user never writes again. Quiet hours defer it, "
                     "never drop it. The pulse makes what is due its standing notice, which the user sees at once "
                     "and you read at the tail of your next request.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "goal_id" (prop "integer" *goal-subject*)
                       "text" (prop "string" "The message, self-contained: it may be read hours later.")
                       "when" (prop "string" "'in 30m', '2h', '1d', 'tomorrow 07:00', or an ISO time.")
                       "kind" (prop "string" "What sort of thing this is." (vector "followup" "reminder" "nudge")))
         "required" (vector "text" "when"))))

(defparameter *card-schema*
  (list :description
        (concatenate 'string
                     "Ask the user a quick question with a recommended answer (a tap card), or record their answer. "
                     "Ask: question + options + recommended + stakes. If unanswered it is re-asked once after the "
                     "card timeout; as long again after that the recommendation is applied ONLY when stakes is "
                     "'low'. Use stakes money, public or irreversible for anything that spends money, is visible to "
                     "others, or can't be undone: those wait for the user forever. action is what the cell itself "
                     "does if the recommendation wins: {type: reschedule, followup_id, when, text?} | "
                     "{type: schedule, text, when} | {type: goal_status, goal_id, status}; scheduled follow-up ids "
                     "are in ha_status. Answer: card_id + answer, when the user replies to a card, even loosely.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "question" (prop "string" "Short, phone-sized question.")
                       "options" (prop "array" "2-4 one-word options. Default yes/no.")
                       "recommended" (prop "string" "The option you recommend.")
                       "stakes" (prop "string" "What the answer puts at stake." (coerce +stakes+ 'vector))
                       "goal_id" (prop "integer" *goal-subject*)
                       "action" (prop "object" "Optional typed action, see the description.")
                       "when" (prop "string" "When to ask. Default now (next pulse).")
                       "card_id" (prop "integer" "To answer: the card's id.")
                       "answer" (prop "string" "To answer: the option the user chose.")))))

(defparameter *commit-schema*
  (list :description
        (concatenate 'string
                     "Track a promise between the user and another named person, in either direction: i_owe (the "
                     "user promised someone something) or they_owe (someone promised the user). A deadline the user "
                     "sets for themselves is a goal (ha_goal_add), not a commitment. It is brought up when due. To "
                     "close one: commitment_id + status done|dropped.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "direction" (prop "string" "Which way the promise runs." (vector "i_owe" "they_owe"))
                       "person" (prop "string" "Who it's with, by first name.")
                       "what" (prop "string" "The thing promised, e.g. 'the deck'.")
                       "due" (prop "string" "When it's due: 'in 2d', or a time. Optional.")
                       "goal_id" (prop "integer" *goal-subject*)
                       "commitment_id" (prop "integer" "To close one: its id.")
                       "status" (prop "string" "How it closed." (vector "done" "dropped"))))))

(defparameter *decide-schema*
  (list :description
        (concatenate 'string
                     "Record a decision the user made, with its rationale and a date to review it; the cell asks "
                     "'still right?' on or after that date. To review one: decision_id + verdict keep|revise "
                     "(+ new choice, + review_in to look again later).")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "title" (prop "string" "What was decided about, e.g. 'Database'.")
                       "choice" (prop "string" "What they chose, e.g. 'SQLite'.")
                       "rationale" (prop "string" "Why, in their words.")
                       "review_in" (prop "string" "When to revisit: '2w', '30d', or a time.")
                       "goal_id" (prop "integer" *goal-subject*)
                       "decision_id" (prop "integer" "To review one: its id.")
                       "verdict" (prop "string" "How the review went." (vector "keep" "revise"))))))

(defparameter *status-schema*
  (list :description "Show the user's goal tree and everything hyperactive is tracking or has scheduled."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object))))

;;; --- /hyperactive --------------------------------------------------------------

(defparameter +usage+
  (concatenate 'string
               "/hyperactive                         goals, cards, commitments, the schedule" (string #\Newline)
               "/hyperactive due                     run the pulse now" (string #\Newline)
               "/hyperactive answer CARD OPTION      answer a tap card" (string #\Newline)
               "/hyperactive why                     why it last spoke" (string #\Newline)
               "/hyperactive decision ID keep|revise [new choice]" (string #\Newline)
               "/hyperactive done COMMITMENT         close a commitment" (string #\Newline)
               "/hyperactive goal HORIZON TITLE [--parent N] [--follow-up WHEN] [--why TEXT]" (string #\Newline)
               "/hyperactive followup GOAL WHEN TEXT")
  "What /hyperactive answers when it does not know the word.")

(defun words-after (words n)
  (format nil "~{~a~^ ~}" (subseq words (min n (length words)))))

(defun slash-goal (words)
  "The one /hyperactive goal HORIZON TITLE [--parent N] [--follow-up WHEN] [--why TEXT]."
  (let ((horizon (first words))
        (parent nil)
        (follow nil)
        (why "")
        (title '()))
    (unless horizon (fail "usage: /hyperactive goal HORIZON TITLE [--parent N] [--follow-up WHEN]"))
    (loop with rest = (rest words)
          while rest
          for word = (pop rest)
          do (cond ((string= word "--parent")
                    (setf parent (ignore-errors (parse-integer (or (pop rest) "")))))
                   ((string= word "--follow-up") (setf follow (pop rest)))
                   ((string= word "--why") (setf why (or (pop rest) "")))
                   (t (push word title))))
    (let* ((goal (add-goal (format nil "~{~a~^ ~}" (nreverse title)) horizon :parent parent :why why))
           (check-in (and follow
                           (add-followup (getf goal :id)
                                          (format nil "How is \"~a\" going? What moved, and what's next?"
                                                  (getf goal :title))
                                          follow))))
      (format nil "goal ~d: ~a~@[ · check-in at ~a~]"
              (getf goal :id) (getf goal :title)
              (and check-in (when-text (getf check-in :due)))))))

(defun run-slash (args session-id)
  "The answer to one /hyperactive invocation: the text, and the same as a bare reply."
  (declare (ignore session-id))
  (let ((words (nlk:split-words (nlk:trimmed (or args "")))))
    (handler-case
        (cond
          ((null words) (status))
          ((string= (first words) "status") (status))
          ((string= (first words) "due") (or (pulse) "hyperactive: nothing is due"))
          ((string= (first words) "brief") (brief))
          ((string= (first words) "why") (why))
          ((string= (first words) "answer")
           (let ((id (ignore-errors (parse-integer (or (second words) "")))))
             (unless (and id (>= (length words) 3))
               (fail "usage: /hyperactive answer CARD OPTION"))
             (let ((row (answer-card id (words-after words 2))))
               (format nil "card #~d: ~a~@[ · done: ~a~]" (getf row :id) (getf row :answer)
                       (let ((result (getf row :result))) (and (plusp (length result)) result))))))
          ((string= (first words) "done")
           (let ((row (close-commitment (ignore-errors (parse-integer (or (second words) "")))
                                        :status (if (string= (or (third words) "") "dropped") "dropped" "done"))))
             (format nil "commitment #~d: ~a" (getf row :id) (getf row :status))))
          ((string= (first words) "goal") (slash-goal (rest words)))
          ((string= (first words) "followup")
           (let ((goal (ignore-errors (parse-integer (or (second words) ""))))
                 (when (or (third words) ""))
                 (text (words-after words 3)))
             (unless (plusp (length when)) (fail "usage: /hyperactive followup GOAL WHEN TEXT"))
             (let ((row (add-followup goal text when)))
               (format nil "follow-up #~d at ~a: ~a" (getf row :id) (when-text (getf row :due)) (getf row :text)))))
          ((string= (first words) "decision")
           (let* ((id (ignore-errors (parse-integer (or (second words) ""))))
                  (verdict (or (third words) ""))
                  (row (review-decision id verdict :choice (words-after words 3))))
             (format nil "decision #~d: ~a (~a)" (getf row :id) (getf row :status) (getf row :choice))))
          (t (format nil "hyperactive: unknown word ~a~%~a" (first words) +usage+)))
      (hyperactive-error (condition)
        (format nil "hyperactive: ~a" (hyperactive-error-detail condition))))))

;;; --- what the model is told ------------------------------------------------------

(defparameter +primer+
  (concatenate 'string
               "The nodecode-hyperactive cell keeps the operator's goal tree (life > year > quarter > week), the "
               "promises they made and are owed, the decisions to revisit, and tap cards: a question with a "
               "recommended answer and the typed action that follows it. It speaks first: every pulse_minutes it "
               "looks for what is due and makes the result its standing notice, which the operator sees at once and "
               "you read at the tail of your next request; (hyperactive:why) names the goal each line served. "
               "Record what the operator says while it is fresh, through the ha_* tools: ha_goal_add (with "
               "follow_up_in for one check-in), ha_commit for a promise either way, ha_decide with review_in, "
               "ha_followup for anything you want to say later, ha_card for a quick call -- answer it with "
               "ha_card card_id + answer when they reply, even loosely -- and ha_status for the whole picture. A "
               "low-stakes card whose recommendation is unanswered twice is settled for the operator through its "
               "typed action alone; money, public and irreversible ones wait for them however long it takes. Every "
               "message names the goal it serves, so tie what you do to one; the Inbox takes the rest. Quiet "
               "hours defer, never drop.")
  "What (help :hyperactive) answers while the cell runs.")

(defun install ()
  "The rows on disk, the pulse worker, and the notice they leave."
  (setf *state* (load-state))
  (nle:on-stop (lambda () (setf *state* nil)))
  (let ((worker (nlk:worker-start "nodecode-hyperactive" #'pulse
                                  :wake (lambda () (max 30 (* 60 (setting :pulse-minutes))))
                                  :prime t)))
    (nle:on-stop (lambda () (nlk:worker-stop worker)))
    (nle:on-stop (lambda () (nle:notice nil :key *notice-key*)))))

(nle:define-cell hyperactive
  (:section ("hyperactive")
    (:guide "nothing is required; pulse_minutes sets how often it looks for what is due, quiet_hours defers without dropping, max_items caps one message")
    ("pulse_minutes" :integer :default 5 :min 1 :doc "minutes between pulses")
    ("max_items" :integer :default 5 :min 1 :doc "most items one message carries; the rest wait for the next pulse")
    ("card_minutes" :integer :default 60 :min 1
                    :doc "minutes before an unanswered card is asked again, and as long again before a low-stakes one is applied")
    ("stale_days" :integer :default 3 :min 1
                  :doc "days a week goal may stand still before one nudge (quarter goals: four times)")
    ("quiet_hours" :string :default "" :doc "local window with no messages, e.g. 22:00-07:00; empty is never quiet"))
  (:start #'install)
  (:help :hyperactive
         "hyperactive: the goal tree, promises and tap cards, and the pulse that speaks first"
         +primer+)
  (:command "hyperactive" 'run-slash
            :description "The goal tree, the cards, and what is due"
            :argument-hint "[due|answer CARD OPTION|why|done ID|goal HORIZON TITLE]")
  (:tool "ha_goal_add" *goal-add-schema* 'tool-goal-add)
  (:tool "ha_followup" *followup-schema* 'tool-followup)
  (:tool "ha_card" *card-schema* 'tool-card)
  (:tool "ha_commit" *commit-schema* 'tool-commit)
  (:tool "ha_decide" *decide-schema* 'tool-decide)
  (:tool "ha_status" *status-schema* 'tool-status))
