;;;; cell.lisp --- a follow-up board, and the moment it comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A follow-up is not an unfinished task. It is a moment worth returning to:
;;;; "did your throat get better?", "the trip is tomorrow, check the weather".
;;;; The agent writes one down with followup_add and the cell keeps it until
;;;; its time arrives.
;;;;
;;;; When the time has arrived and the gate allows it, the follow-ups come
;;;; back by themselves: at the next model request they arrive as one reminder
;;;; -- the moment to say something, in a conversation already open -- and the
;;;; agent decides whether it is worth a word. Nobody polls.
;;;;
;;;; The gate is code, not judgement: a quiet window, a cooldown between two
;;;; reminders, the categories the operator pushed back on, at most N items and
;;;; at most M lines in one reminder. The board is one JSON file under the
;;;; home, mode 0600; nothing here dials anything.

(in-package #:nodecode-followups)

;;; --- the words ---------------------------------------------------------------

(defparameter +categories+ '("care" "utility" "risk" "curiosity")
  "The four origins a follow-up comes from: care for the person, utility for
the practical errand, risk for the thing about to be decided, curiosity for
the thing worth a look later. A follow-up outside these is refused.")

(defparameter +queries+ '("all" "due")
  "What followup_list may ask for: the whole board, or only what has come due.")

(defparameter *framing*
  "[followups: ~d due~@[, muted: ~@{~a~^, ~}~]. Not a task list: come back to one only if it is still worth a word, then say it plainly; (followups:checked ID) marks one looked at, (followups:retire ID) retires it.]"
  "The first line of a reminder: how many are due and which categories are
muted. A layer may reword it.")

(defparameter +primer+
  "A follow-up is a moment worth returning to, not an unfinished task: something
the operator said that will be worth a word later -- a sore throat, a trip
tomorrow, something bought, a decision coming up. The nodecode-followups cell
keeps the board; its verbs answer a string, and a refusal is
ERROR: FOLLOWUPS-ERROR with the reason.
  (followups:add :subject \"the sore throat\" :why \"care\" :at \"+18h\"
                 :quote \"my throat is killing me\" :hint \"ask if it is better\")
      :why is care | utility | risk | curiosity; :at is \"+18h\", \"+2d\",
      \"now\" or an ISO time; the same :key again updates the item.
  (followups:due)            what has come due, and whether the gate holds it
  (followups:board)          the whole board, one line an item
  (followups:checked 3)      looked at it; it comes back later, with backoff
  (followups:retire 3)       retire it for good (a :note says why)
  (followups:outcome 3 \"annoyed\")  how it landed: accepted | ignored |
      annoyed | useless -- three ignored or one annoyed mutes that category
      for a while.
Due follow-ups reach you by themselves, as one reminder at your next model
request, when the quiet window and the cooldown allow it: never poll. At most
followups.max_items go in one reminder and it is never more than
followups.max_lines lines. The model tools followup_add, followup_list,
followup_checked and followup_close do the same four things."
  "What (help :followups) answers while the cell runs.")

;;; --- the board's file ---------------------------------------------------------

(defparameter +fields+
  ;; (KEY WIRE DEFAULT) -- one place the file's shape is written.
  '((:id "id" 0) (:key "key" "") (:why "why" "") (:subject "subject" "")
    (:quote "quote" "") (:hint "hint" "") (:at "at" 0) (:status "status" "active")
    (:checks "checks" 0) (:last-checked "last_checked" 0) (:sent-at "sent_at" 0)
    (:sends "sends" 0) (:sent-text "sent_text" "") (:outcome "outcome" "")
    (:note "note" "") (:created "created" 0))
  "The item's fields: symbol, wire name, and its default.")

(defun board-path ()
  "The board's one file: under the home, mode 0600."
  (nlk:home "followups/board.json"))

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

(defun read-board ()
  "The board: (list :next-id N :items ITEMS :kv ALIST). A missing or torn file
is an empty board, never an error."
  (let* ((text (nlk:read-text (board-path)))
         (object (and text (ignore-errors (nlk:decode-json text))))
         (items (and object (nlk:json-value object :array "items")))
         (kv (and object (nlk:json-value object :object "kv"))))
    (list :next-id (or (and object (nlk:json-value object :integer "next_id")) 1)
          :items (loop for raw across (or items #()) when (hash-table-p raw)
                       collect (json-to-item raw))
          :kv (let ((pairs '()))
                (when kv
                  (maphash (lambda (key value) (push (cons key value) pairs)) kv))
                (nreverse pairs)))))

(defun write-board (board)
  "The board as its file: atomic, 0600, its folder 0700."
  (nlk:write-file-atomically
   (board-path)
   (nlk:encode-json-object
    (nlk:make-json-object
     "version" 1
     "next_id" (getf board :next-id 1)
     "items" (coerce (mapcar #'item-to-json (getf board :items)) 'vector)
     "kv" (apply #'nlk:make-json-object
                  (loop for (key . value) in (getf board :kv)
                        append (list key value)))))
   :mode #o600 :directory-mode #o700)
  board)

(defun board-get (board name)
  "BOARD's KV entry NAME, or 0."
  (let ((pair (assoc name (getf board :kv) :test #'equal)))
    (if pair (cdr pair) 0)))

(defun (setf board-get) (value board name)
  "Set BOARD's KV entry NAME to VALUE in memory; WRITE-BOARD persists it."
  (let ((pair (assoc name (getf board :kv) :test #'equal)))
    (if pair
        (setf (cdr pair) value)
        (setf (getf board :kv) (append (getf board :kv) (list (cons name value)))))
    value))

(defmacro with-board ((board) &body body)
  "BODY with BOARD bound, read and written back under the cell's lock."
  `(with-followups-lock
     (let ((,board (read-board)))
       (prog1 (progn ,@body)
         (write-board ,board)))))

;;; --- time and words -----------------------------------------------------------

(defun hour-now ()
  "The hour of the local clock, 0-23."
  (nth-value 2 (decode-universal-time (get-universal-time))))

(defun at-text (unix)
  "UNIX as a short stamp an operator reads: 2026-11-10T03:00."
  (and unix (plusp unix) (subseq (nlk:iso-time unix :millis nil) 0 16)))

(defun relative-text (unix now)
  "UNIX read against NOW: \"in 18h\", \"now\", \"2h ago\"."
  (let ((seconds (- unix now)))
    (cond ((< (abs seconds) 60) "now")
          ((plusp seconds) (format nil "in ~a" (nlk:duration-text seconds :coarse t)))
          (t (format nil "~a ago" (nlk:duration-text (- seconds) :coarse t))))))

(defun parse-at (text &optional (now (nlk:unix-now)))
  "The instant TEXT names: a delay (\"+18h\", \"+2d\", \"+90m\"), \"now\", or an
ISO time. => unix seconds."
  (let ((text (nlk:trimmed (or text ""))))
    (cond ((string= text "") (fail "a follow-up needs a time: :at \"+18h\", \"+2d\" or an ISO time"))
          ((string-equal text "now") now)
          ((char= #\+ (char text 0))
           (let ((seconds (nlk:parse-duration (subseq text 1))))
             (unless seconds
               (fail "~s is not a delay: write \"+18h\", \"+90m\" or \"+2d\"" text))
             (+ now seconds)))
          (t
           (let* ((padded (if (= 16 (length text)) (concatenate 'string text ":00") text))
                  (universal (nlk:iso-universal padded)))
             (unless universal
               (fail "~s is not a time: a delay (\"+18h\", \"+2d\") or an ISO time (\"2026-11-10T09:00\")"
                     text))
             (- universal nlk:+unix-epoch+))))))

(defun check-why (why)
  "WHY as one of +CATEGORIES+, lowercased, or a refusal."
  (let ((why (string-downcase (nlk:trimmed (or why "")))))
    (unless (member why +categories+ :test #'string=)
      (fail "~s is not a category: ~{~a~^, ~}" why +categories+))
    why))

(defun key-for (subject now)
  "The key a follow-up on SUBJECT written at NOW gets: its first words, and
the day, so the same subject on the same day updates one item."
  (let* ((words (remove-if (lambda (word) (zerop (length word)))
                            (uiop:split-string (string-downcase (or subject ""))
                                               :separator " -,.;:!?")))
         (slug (format nil "~{~a~^-~}" (subseq (or words (list "followup")) 0
                                                (min 6 (length (or words (list "followup"))))))))
    (format nil "~a-~a" (subseq slug 0 (min 48 (length slug)))
            (subseq (nlk:iso-time now :millis nil) 0 10))))

;;; --- the gate -----------------------------------------------------------------

(defun quiet-hour-p (hour)
  "Whether HOUR is inside the quiet window, which may span midnight; equal
hours mean no window."
  (let ((start (setting :quiet-start)) (end (setting :quiet-end)))
    (cond ((= start end) nil)
          ((< start end) (and (>= hour start) (< hour end)))
          (t (or (>= hour start) (< hour end))))))

(defun outcome-counts (items outcome)
  "How many of ITEMS carry OUTCOME, by category: ((WHY . COUNT) ...)."
  (let ((counts '()))
    (dolist (item items counts)
      (when (string= (item-field item :outcome) outcome)
        (let* ((why (item-field item :why))
               (pair (assoc why counts :test #'string=)))
          (if pair (incf (cdr pair)) (push (cons why 1) counts)))))))

(defun muted-whys (board &optional (now (nlk:unix-now)))
  "The categories the operator pushed back on lately -- one annoyed, or three
ignored, inside followups.mute_days. => ((WHY . WHY-IT-IS-MUTED) ...)"
  (let* ((window (* 86400 (setting :mute-days)))
         (recent (remove-if-not (lambda (item)
                                   (let ((sent (item-field item :sent-at)))
                                     (and (plusp sent) (<= (- now sent) window))))
                                 (getf board :items))))
    (append (loop for (why . nil) in (outcome-counts recent "annoyed")
                  collect (cons why "annoyed"))
            (loop for (why . count) in (outcome-counts recent "ignored")
                  when (>= count 3) collect (cons why (format nil "ignored ~d times" count))))))

(defun due-items (board &optional (now (nlk:unix-now)))
  "BOARD's active items whose time has come and which were not sent since it
did. => their ids, ascending."
  (sort (loop for item in (getf board :items)
              when (and (string= (item-field item :status) "active")
                        (<= (item-field item :at) now)
                        (let ((sent (item-field item :sent-at)))
                          (or (zerop sent) (< sent (item-field item :at)))))
                collect (item-field item :id))
        #'<))

(defun hold-reason (held)
  "The one reason nothing is sendable out of HELD, a ((ID . REASON) ...) list:
night, then cooling, then muted, as the wording that explains it."
  (loop for reason in '("night" "cooling" "muted")
        when (find reason held :key #'cdr :test #'string=) return reason))

(defun gate (&optional (now (nlk:unix-now)))
  "Whether a reminder may go out now, and what holds it back. => a plist:
:NIGHT, :COOLING, :MUTED ((WHY . REASON)...), :DUE IDS, :SENDABLE IDS,
:HELD ((ID . REASON)...), :HOLD why nothing is sendable, :MAY-SEND."
  (let* ((board (read-board))
         (night (quiet-hour-p (hour-now)))
         (cooling (< (- now (board-get board "last_notice"))
                     (* 60 (setting :cooldown-minutes))))
         (muted (muted-whys board now))
         (due (due-items board now))
         (held '())
         (sendable '()))
    (dolist (id due)
      (let* ((item (find id (getf board :items) :key (lambda (i) (item-field i :id))))
             (why (and item (item-field item :why)))
             (reason (cond ((and why (assoc why muted :test #'string=)) "muted")
                           (night "night")
                           (cooling "cooling"))))
        (if reason (push (cons id reason) held) (push id sendable))))
    (setf sendable (nreverse sendable) held (nreverse held))
    (list :now now :night night :cooling cooling :muted muted :due due
          :sendable sendable :held held
          :hold (and held (not sendable) (hold-reason held))
          :may-send (and sendable t))))

;;; --- the reminder ---------------------------------------------------------------

(defun item-line (item now)
  "One follow-up as a reminder tells it."
  (let ((quote (item-field item :quote))
        (hint (item-field item :hint))
        (sends (item-field item :sends)))
    (format nil "#~d ~a · ~a~@[ — ~s~]~@[ (~a)~] (due ~a)~@[, asked ~d time~:p~]"
            (item-field item :id) (item-field item :why) (item-field item :subject)
            (and (plusp (length quote)) (nlk:clip quote 120))
            (and (plusp (length hint)) hint)
            (relative-text (item-field item :at) now)
            (and (plusp sends) sends))))

(defun item-by-id (board id)
  "BOARD's item ID, or NIL."
  (find id (getf board :items) :key (lambda (item) (item-field item :id))))

(defun reminder (&optional (now (nlk:unix-now)))
  "The one reminder this model request carries: the due follow-ups the gate
lets through, one line each, at most followups.max_items of them and at most
followups.max_lines lines. => the text, or NIL when nothing is due or the gate
holds it all."
  (let* ((board (read-board))
         (verdict (gate now))
         (sendable (getf verdict :sendable)))
    (when sendable
      (let* ((ids (subseq sendable 0 (min (length sendable) (setting :max-items))))
             (lines (loop for id in ids
                          for item = (item-by-id board id)
                          when item collect (item-line item now)))
             (muted (loop for (why . nil) in (getf verdict :muted) collect why))
             (more (- (length sendable) (length ids))))
        (nlk:clip
         (format nil "~a~%~{~a~^~%~}~@[~%(~d more due; (followups:due) shows the board.)~]"
                 (format nil *framing* (length sendable) muted)
                 lines (and (plusp more) more))
         4000)))))

;;; --- writing on the board ---------------------------------------------------------

(defun add-item (board &key subject why at quote hint key)
  "Write SUBJECT into BOARD at AT with WHY, QUOTE and HINT; the same KEY again
updates the words and the time. => (values ITEM UPDATED-P)."
  (let* ((now (nlk:unix-now))
         (at (parse-at at now))
         (why (check-why why))
         (subject (nlk:trimmed (or subject "")))
         (given (nlk:trimmed (or key "")))
         (key (if (plusp (length given)) given (key-for subject now))))
    (when (zerop (length subject))
      (fail "a follow-up needs a subject: the thing to come back to, in a few words"))
    (when (< at now)
      (fail "~a is in the past: a follow-up comes back at a time ahead of now" (at-text at)))
    (let ((item (find key (getf board :items)
                      :key (lambda (i) (item-field i :key)) :test #'string=)))
      (if item
          (progn
            (setf (getf item :why) why
                  (getf item :subject) subject
                  (getf item :at) at
                  (getf item :quote) (or quote "")
                  (getf item :hint) (or hint "")
                  (getf item :status) "active")
            (write-board board)
            (values item t))
          (let ((item (blank-item)))
            (setf (getf item :id) (getf board :next-id 1)
                  (getf item :key) key
                  (getf item :why) why
                  (getf item :subject) subject
                  (getf item :at) at
                  (getf item :quote) (or quote "")
                  (getf item :hint) (or hint "")
                  (getf item :created) now
                  (getf item :status) "active"
                  (getf board :next-id) (1+ (getf board :next-id 1))
                  (getf board :items) (append (getf board :items) (list item)))
            (write-board board)
            (values item nil))))))

(defun find-item (board id)
  "BOARD's item ID, or a refusal."
  (or (item-by-id board id)
      (fail "no follow-up #~a on the board: (followups:board) shows the ids" id)))

(defun id-number (value)
  "VALUE, or the integer it spells, or a refusal."
  (or (and (integerp value) value)
      (ignore-errors (parse-integer (princ-to-string value) :junk-allowed t))
      (fail "~s is no follow-up id" value)))

(defun add-text (item updated)
  "One line for the operator about ITEM."
  (format nil "~a #~d ~a · ~a, due ~a (~a)"
          (if updated "updated" "added")
          (item-field item :id) (item-field item :why) (item-field item :subject)
          (at-text (item-field item :at))
          (relative-text (item-field item :at) (nlk:unix-now))))

;;; --- the verbs -------------------------------------------------------------------

(define-verb add (&key subject why (at "+1d") quote hint key)
  "Write a follow-up down. WHY is care | utility | risk | curiosity, AT is
\"+18h\", \"+2d\", \"now\" or an ISO time, QUOTE is what the operator said and
HINT what to look at when it comes back. The same KEY again updates it."
  (with-board (board)
    (multiple-value-bind (item updated)
        (add-item board :subject subject :why why :at at :quote quote :hint hint :key key)
      (add-text item updated))))

(define-verb board ()
  "The whole board as it stands, one line an item, closed items included."
  (let ((items (getf (read-board) :items)))
    (if (null items)
        "the follow-up board is empty: (followups:add :subject \"...\" :why \"care\" :at \"+18h\")"
        (format nil "~d follow-up~:p (~d active):~%~{~a~^~%~}"
                (length items)
                (count "active" items :key (lambda (item) (item-field item :status))
                       :test #'string=)
                (mapcar (lambda (item)
                          (format nil "#~d [~a] ~a · ~a (at ~a~@[, ~d check~:p~])"
                                  (item-field item :id) (item-field item :status)
                                  (item-field item :why) (item-field item :subject)
                                  (at-text (item-field item :at))
                                  (item-field item :checks)))
                        items)))))

(define-verb due (&optional (now (nlk:unix-now)))
  "What has come due now, and whether the gate lets a reminder out."
  (let* ((board (read-board))
         (verdict (gate now)))
    (format nil "due now: ~d~@[; held: ~{~a~^, ~}~]~@[; muted: ~{~a~^, ~}~]~%~
                 ~:[the gate holds it all~;may go out now~]~%~{~a~^~%~}"
            (length (getf verdict :due))
            (loop for (id . reason) in (getf verdict :held)
                  collect (format nil "#~d ~a" id reason))
            (loop for (why . nil) in (getf verdict :muted) collect why)
            (getf verdict :may-send)
            (loop for id in (getf verdict :due)
                  for item = (item-by-id board id)
                  when item collect (item-line item now)))))

(define-verb checked (id &key at note)
  "Mark follow-up ID looked at: it comes back later, each time after a longer
gap (a day, two, four, then weekly), or at AT when given."
  (with-board (board)
    (let* ((item (find-item board (id-number id)))
           (now (nlk:unix-now))
           (backoff (* 86400 (min (expt 2 (min (item-field item :checks) 3)) 7)))
           (next (if at (parse-at at now) (+ now backoff))))
      (setf (getf item :checks) (1+ (item-field item :checks))
            (getf item :last-checked) now
            (getf item :at) next)
      (when (and note (plusp (length (nlk:trimmed note))))
        (setf (getf item :note) (nlk:trimmed note)))
      (format nil "checked #~d ~a: back ~a (~a)"
              (item-field item :id) (item-field item :subject)
              (relative-text next now) (at-text next)))))

(define-verb retire (id &key note)
  "Retire follow-up ID for good, with NOTE when given."
  (with-board (board)
    (let ((item (find-item board (id-number id))))
      (setf (getf item :status) "closed")
      (when (and note (plusp (length (nlk:trimmed note))))
        (setf (getf item :note) (nlk:trimmed note)))
      (format nil "closed #~d ~a~@[ (~a)~]" (item-field item :id)
              (item-field item :subject) (and note (nlk:trimmed note))))))

(define-verb outcome (id value)
  "How follow-up ID landed: accepted | ignored | annoyed | useless. Three
ignored, or one annoyed, mutes that category for a while."
  (let ((value (string-downcase (nlk:trimmed (princ-to-string (or value ""))))))
    (unless (member value '("accepted" "ignored" "annoyed" "useless") :test #'string=)
      (fail "~s is not an outcome: accepted, ignored, annoyed or useless" value))
    (with-board (board)
      (let ((item (find-item board (id-number id))))
        (setf (getf item :outcome) value)
        (when (member value '("annoyed" "useless") :test #'string=)
          (setf (getf item :status) "closed"))
        (format nil "#~d ~a: recorded ~a~@[; ~a is muted for a while~]"
                (item-field item :id) (item-field item :subject) value
                (and (string= value "annoyed") (item-field item :why)))))))

(define-verb digest (&optional (now (nlk:unix-now)))
  "Everything one review needs, as text: the gate, the board and the last
sends."
  (let* ((board (read-board))
         (verdict (gate now))
         (active (remove-if-not (lambda (item) (string= (item-field item :status) "active"))
                                (getf board :items))))
    (format nil "now ~a~%gate: ~:[may go out~:;held: ~a~]~@[; night~]~@[; cooling~]~
                 ~@[; muted ~{~a~^, ~}~]~%board: ~d active, ~d due~%~{~a~^~%~}"
            (at-text now) (getf verdict :hold) (getf verdict :hold)
            (getf verdict :night) (getf verdict :cooling)
            (loop for (why . nil) in (getf verdict :muted) collect why)
            (length active) (length (getf verdict :due))
            (mapcar (lambda (item) (item-line item now)) active))))

;;; --- the round's reminder ----------------------------------------------------------

(defun deliver (op)
  "At a model request: when a follow-up has come due and the gate allows it,
the reminder arrives in the round about to be asked and the cooldown starts."
  (when (and (nlk:store-open-p) (getf op :turn) (getf op :session-id))
    (let ((text (ignore-errors (reminder))))
      (when text
        (with-board (board)
          (setf (board-get board "last_notice") (nlk:unix-now)))
        (nlk:record-turn-context-prompt (getf op :turn) :text text)))))

;;; --- the tools -----------------------------------------------------------------

(nlk:define-contract followup-entry
  (:doc "Write one follow-up down: a moment worth coming back to later -- a sore throat, a trip tomorrow, something bought, a decision coming up.")
  ("subject" :text :required t :doc "the thing to come back to, in a few words")
  ("why" :text :required t :options +categories+
   :doc "care for the person, utility for the practical errand, risk for a decision ahead, curiosity for a thing worth a look")
  ("at" :text :default "+1d" :doc "when it comes back: \"+18h\", \"+2d\", \"now\" or an ISO time")
  ("quote" :text :default "" :slot quote-text :doc "what the operator actually said, verbatim")
  ("hint" :text :default "" :doc "what to look at or ask when it comes back")
  ("key" :text :default "" :doc "a stable key: the same key again updates the item instead of adding a second"))

(nlk:define-contract followup-query
  (:doc "Read the follow-up board.")
  ("which" :text :options +queries+ :default "all"
   :doc "all: every item on the board; due: only what has come due, and whether the gate holds it"))

(nlk:define-contract followup-id
  (:doc "One follow-up on the board, by the id followup_list shows.")
  ("id" :integer :required t :doc "the follow-up's id")
  ("note" :text :default "" :doc "one line: why it is retired, or where it comes back"))

(defun tool-add (entry)
  "followup_add: the model writes a follow-up down."
  (add :subject (followup-entry-subject entry)
       :why (followup-entry-why entry)
       :at (followup-entry-at entry)
       :quote (followup-entry-quote-text entry)
       :hint (followup-entry-hint entry)
       :key (followup-entry-key entry)))

(defun tool-list (query)
  "followup_list: the board, or only what has come due."
  (if (string= "due" (followup-query-which query)) (due) (board)))

(defun tool-checked (entry)
  "followup_checked: it was looked at, so it comes back later."
  (checked (followup-id-id entry) :note (followup-id-note entry)))

(defun tool-retire (entry)
  "followup_close: retire it for good."
  (retire (followup-id-id entry) :note (followup-id-note entry)))

;;; --- /followups -----------------------------------------------------------------

(defun board-summary ()
  "The board as /followups tells it."
  (let* ((board (read-board))
         (active (remove-if-not (lambda (item) (string= (item-field item :status) "active"))
                                (getf board :items)))
         (verdict (gate)))
    (format nil "follow-ups: ~d active, ~d due~@[ (held: ~a)~]~%~{~a~^~%~}"
            (length active) (length (getf verdict :due)) (getf verdict :hold)
            (if active
                (mapcar (lambda (item) (item-line item (nlk:unix-now))) active)
                (list "none: the agent writes one down when something is worth coming back to")))))

(defun panel ()
  "The dialog bare /followups answers: retire one, or look at what is due."
  (let* ((board (read-board))
         (active (remove-if-not (lambda (item) (string= (item-field item :status) "active"))
                                (getf board :items))))
    (nle:list-dialog
     :title "Follow-ups" :summary (format nil "~d active" (length active))
     :rows (append
            (loop for item in active
                  collect (nle:run-row
                           (format nil "Retire #~d ~a" (item-field item :id)
                                   (item-field item :subject))
                           (format nil "/followups close ~d" (item-field item :id))
                           :detail (format nil "~a · due ~a"
                                           (item-field item :why)
                                           (relative-text (item-field item :at)
                                                          (nlk:unix-now)))))
            (list (nle:run-row "What is due" "/followups due"
                               :detail "and whether the gate lets a reminder out"))))))

(defun run-slash (args session-id)
  "/followups: the board this cell keeps, and its panel. close ID retires one,
checked ID marks one looked at."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((string= head "") (values (board-summary) (panel)))
          ((string= head "due") (due))
          ((string= head "digest") (digest))
          ((string= head "close") (retire (id-number rest)))
          ((string= head "checked") (checked (id-number rest)))
          (t (board-summary)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell followups
  (:section ("followups")
    (:guide "the agent writes a follow-up down and it comes back by itself when its time has arrived; the quiet window, the cooldown and the caps keep it from being noise")
    ("quiet_start" :integer :default 23 :min 0
     :doc "the hour the quiet window opens: no reminder goes out from here")
    ("quiet_end" :integer :default 8 :min 0
     :doc "the hour it closes; a window may span midnight, and equal hours mean no window")
    ("cooldown_minutes" :integer :default 90 :min 0
     :doc "the least time between two reminders")
    ("max_items" :integer :default 3 :min 1
     :doc "how many follow-ups one reminder may carry")
    ("max_lines" :integer :default 6 :min 1
     :doc "how many lines one reminder may take")
    ("mute_days" :integer :default 30 :min 0
     :doc "how long a category the operator pushed back on stays muted"))
  (:tool "followup_add" (nlk:contract 'followup-entry) 'tool-add)
  (:tool "followup_list" (nlk:contract 'followup-query) 'tool-list)
  (:tool "followup_checked" (nlk:contract 'followup-id) 'tool-checked)
  (:tool "followup_close" (nlk:contract 'followup-id) 'tool-retire)
  (:help :followups "followups: a board of moments worth coming back to, delivered at the next request when their time has come; /followups" +primer+)
  (:hook :round (nle:observer #'deliver "followups"))
  (:command "followups" 'run-slash
            :catches 'followups-error
            :description "The follow-up board: what has come due, and retiring one"
            :argument-hint "[due | digest | close ID | checked ID]"))
