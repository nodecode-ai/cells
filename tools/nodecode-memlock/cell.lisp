;;;; cell.lisp --- pinned standing instructions, put back in front of the model every turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-memlock. An operator's standing instruction ("always
;;;; answer in bullet points", "never touch main") is worth keeping past the
;;;; moment it was said, and here the conversation it was said in gets folded,
;;;; thinned and edited away: so a pin is a record of its own, in a JSON file
;;;; under the home, and every turn's first request opens with the pins.
;;;;
;;;; The original detected compaction and re-injected only the anchors that had
;;;; drifted out of the live region. This image re-asserts the pins at the
;;;; start of every turn instead: the harness folds history where it lists, so
;;;; a reminder riding the head of a turn cannot be aged out of it, and no
;;;; drift audit, keyword probes or embedding model are needed.

(defpackage #:nodecode-memlock
  (:use #:cl))

(in-package #:nodecode-memlock)

(nlk:define-peripheral memlock :not-running t)

;;; --- the words -------------------------------------------------------------------
;;; Data, so a layer can reword them, and so the marker the model reads is in
;;; one place.

(defparameter *marker*
  "[Standing instructions — still active]"
  "The line the reminder block opens on: the model reads it as the operator's
pinned rules, not as new conversation.")

(defparameter *more-line*
  "  - (~d more pinned, not shown here; /pins lists them all)"
  "The last line when the reminder budget runs out, over how many were left out.")

(defparameter *empty-line*
  "No pins. A standing instruction is pinned with the pin tool, or /pins pin <text>."
  "What /pins says when there is nothing pinned.")

(defparameter +actions+ '("pin" "update" "unpin" "list")
  "The pin tool's actions, and the enum its schema advertises.")

(defparameter +primer+
  "A pin is one standing instruction the operator wants kept, through the
nodecode-memlock cell: it lives in a JSON file under the home and opens every
turn's first request, so it survives the conversation being folded or edited.
The pin tool (action pin|update|unpin|list) is the model's way to it:
  pin     text=..., priority 1-100 (default 50), reminder the one line each turn
          repeats (default: the instruction's first sentence).
  update  pin_id=... and the new text/reminder/priority.
  unpin   pin_id=... removes it from the set.
  list    every pin, with its id, priority and text.
Pin when the operator states a rule that must keep applying -- 'always',
'never', 'from now on' -- and say so; do not pin an ordinary request. The
operator types /pins to see them, /pins pin <text> to pin one, /pins unpin
<id> to drop one. A pin is the organism's: every session reads the same set."
  "What (help :memlock) answers while the cell runs.")

;;; --- where the pins live ------------------------------------------------------------

(defun pins-pathname ()
  "The pins file the section names, or a refusal saying which setting to fill."
  (let ((name (and (pins-configured-p) (nlk:trimmed (setting :pins-file)))))
    (unless name
      (fail "memlock.pins_file is not set: name the file the pins are kept in, e.g. memlock/pins.json under the home, in /settings (or the memlock section of config.jsonc)"))
    (if (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
        (nlk:expand-home name)
        (nlk:home name))))

(defun pins-configured-p ()
  "T when the section says where the pins live."
  (let ((name (ignore-errors (setting :pins-file))))
    (and (stringp name) (plusp (length (nlk:trimmed name))))))

(defun read-pins ()
  "Every pin in the file, oldest first, as plists (:id :text :reminder :priority).
A missing or unreadable file is no pins -- never a refusal on the turn path."
  (let ((path (ignore-errors (pins-pathname))))
    (cond ((null path) '())
          ((null (probe-file path)) '())
          (t (let ((object (ignore-errors (nlk:decode-json (nlk:read-text path)))))
               (loop for entry across (nlk:json-array object "pins")
                     for text = (nlk:json-value entry :string "text")
                     when (and text (plusp (length text)))
                       collect (list :id (or (nlk:json-value entry :string "id") "pin_unknown")
                                     :text text
                                     :reminder (or (nlk:json-value entry :string "reminder")
                                                   (first-sentence text))
                                     :priority (or (nlk:json-value entry :integer "priority") 50))))))))

(defun write-pins (pins)
  "PINS to the file, atomically: tmp, then rename. => the pathname."
  (let* ((path (pins-pathname))
         (tmp (uiop:parse-native-namestring
               (concatenate 'string (namestring path) ".tmp"))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :external-format :utf-8)
      (write-string (nlk:encode-json-object
                     (nlk:json-object "format" 1
                                      "pins" (apply #'vector (mapcar #'pin-json pins))))
                    out))
    (rename-file tmp path)
    path))

(defun pin-json (pin)
  "PIN as the JSON object the file keeps."
  (nlk:make-json-object "id" (getf pin :id)
                         "text" (getf pin :text)
                         "reminder" (getf pin :reminder)
                         "priority" (getf pin :priority)))

(defun ordered-pins (pins)
  "PINS highest priority first, then by the order they were pinned."
  (stable-sort (copy-list pins) #'> :key (lambda (pin) (getf pin :priority))))

(defun load-pins ()
  "Every pin, in delivery order."
  (ordered-pins (read-pins)))

;;; --- text --------------------------------------------------------------------------

(defun flatten (text)
  "TEXT with every run of white space as one space, trimmed: a pin is one line,
so it can neither open a second reminder item nor a marker of its own."
  (let ((out (make-string-output-stream))
        (space nil)
        (written nil))
    (loop for character across (princ-to-string (or text ""))
          do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                 (setf space written)
                 (progn (when space (write-char #\Space out))
                        (setf space nil written t)
                        (write-char character out))))
    (nlk:trimmed (get-output-stream-string out))))

(defun first-sentence (text)
  "TEXT up to its first sentence end, at most 120 characters: the line a turn
repeats when no reminder was given."
  (let* ((text (flatten text))
         (cut (or (position-if (lambda (character) (find character ".!?")) text)
                  (length text))))
    (subseq text 0 (min 120 (length text) (1+ cut)))))

(defun reminder-line (pin)
  "The one line PIN is delivered as."
  (format nil "  - ~a" (first-sentence (or (getf pin :reminder) (getf pin :text)))))

(defun reminder-block (pins)
  "PINS as one reminder block under the marker, packed by priority into the
section's reminder_chars, or NIL when nothing is pinned."
  (let ((lines '())
        (room (setting :reminder-chars))
        (shown 0))
    (dolist (pin (ordered-pins pins))
      (let ((line (reminder-line pin)))
        (when (and lines (> (+ 1 (length line)) room))
          (return))
        (push line lines)
        (incf shown)
        (decf room (1+ (length line)))))
    (when (null lines)
      (return-from reminder-block nil))
    (when (< shown (length pins))
      (push (format nil *more-line* (- (length pins) shown)) lines))
    (format nil "~a~%~{~a~^~%}~}" *marker* (nreverse lines))))

(defun status-text ()
  "Every pin as the operator reads it, or the one line saying there are none."
  (running-settings)
  (pins-pathname)
  (let ((pins (load-pins)))
    (if (null pins)
        *empty-line*
        (with-output-to-string (out)
          (format out "~d pinned standing instruction~:p, re-asserted at every turn:~%"
                  (length pins))
          (dolist (pin pins)
            (format out "  ~a  priority ~d  ~a~%"
                    (getf pin :id) (getf pin :priority) (getf pin :text)))))))

;;; --- pinning -------------------------------------------------------------------------

(defun clamp-priority (priority)
  "PRIORITY inside 1-100, 50 when nothing was said."
  (max 1 (min 100 (or priority 50))))

(defun new-pin-id (text)
  "A pin id no other pin carries: pin_ and eight hex digits of TEXT and the
clock."
  (format nil "pin_~a" (subseq (nlk:sha256-text
                                  (format nil "~a~a~a" text (get-universal-time) (random 1000000)))
                                 7 15)))

(defun add-pin (text reminder priority)
  "Pin TEXT. => the answer the model or the operator reads."
  (let ((text (flatten text)))
    (when (zerop (length text))
      (fail "pinning needs text: the standing instruction, in one sentence"))
    (let* ((pins (read-pins))
           (cap (setting :max-pins)))
      (when (>= (length pins) cap)
        (fail "already ~d pin~:p, the most memlock.max_pins allows: /pins unpin <id> drops one" cap))
      (let ((pin (list :id (new-pin-id text)
                       :text text
                       :reminder (let ((line (flatten reminder)))
                                   (if (plusp (length line)) (first-sentence line) (first-sentence text)))
                       :priority (clamp-priority priority))))
        (write-pins (append pins (list pin)))
        (format nil "Pinned (id=~a, priority=~d): ~a~%Every turn from now on opens with it, so it survives the conversation being compacted."
                (getf pin :id) (getf pin :priority) (getf pin :text))))))

(defun update-pin (pin-id text reminder priority)
  "Rewrite the pin PIN-ID in place, keeping its id and place in the file."
  (let* ((pins (read-pins))
         (pin (find (flatten pin-id) pins :key (lambda (p) (getf p :id)) :test #'string-equal)))
    (unless pin
      (fail "no pin ~s: /pins lists every id" (flatten pin-id)))
    (let ((text (flatten text)))
      (when (plusp (length text))
        (setf (getf pin :text) text)
        (setf (getf pin :reminder) (let ((line (flatten reminder)))
                                      (if (plusp (length line)) (first-sentence line) (first-sentence text))))))
    (when priority (setf (getf pin :priority) (clamp-priority priority)))
    (write-pins pins)
    (format nil "Updated (id=~a, priority=~d): ~a"
            (getf pin :id) (getf pin :priority) (getf pin :text))))

(defun drop-pin (pin-id)
  "Remove the pin PIN-ID. => the answer the caller reads."
  (let* ((pins (read-pins))
         (pin (find (flatten pin-id) pins :key (lambda (p) (getf p :id)) :test #'string-equal)))
    (unless pin
      (fail "no pin ~s: /pins lists every id" (flatten pin-id)))
    (write-pins (remove pin pins))
    (format nil "Unpinned ~a: ~a" (getf pin :id) (getf pin :text))))

;;; --- the model's way in ---------------------------------------------------------------

(nlk:define-contract pin-call
  (:doc "Pin a standing instruction the operator wants kept: it is put in front of you at the start of every turn, so it keeps applying after the conversation is compacted. Use it for a rule that must keep holding ('always ...', 'never ...', 'from now on ...'), not for an ordinary request. action=list shows every pin with its id; update and unpin need the id a list named.")
  ("action" :text :default "pin" :options +actions+
            :doc "pin adds one (the default); update rewrites one by pin_id; unpin removes one; list shows them all")
  ("text" :text
          :doc "the instruction, one sentence, in the operator's words; needed by pin and update")
  ("pin_id" :text
            :doc "the id list answered for the pin to update or unpin")
  ("reminder" :text
              :doc "the one line each turn repeats, at most 120 characters; default: the instruction's first sentence")
  ("priority" :integer
              :doc "1-100, highest delivered first when the reminder budget runs out; default 50"))

(defun pin-tool (call)
  "The pin tool: the action CALL names, against the pins file."
  (let ((action (string-downcase (or (pin-call-action call) "pin"))))
    (cond ((string= action "pin") (add-pin (pin-call-text call) (pin-call-reminder call) (pin-call-priority call)))
          ((string= action "update") (update-pin (pin-call-pin-id call) (pin-call-text call)
                                                  (pin-call-reminder call) (pin-call-priority call)))
          ((string= action "unpin") (drop-pin (pin-call-pin-id call)))
          ((string= action "list") (status-text))
          (t (fail "~s is no pin action: pin, update, unpin or list" action)))))

;;; --- the operator's way in -------------------------------------------------------------

(defun run-slash (args session-id)
  "One /pins invocation: no argument lists, 'pin TEXT' pins, 'unpin ID' drops."
  (declare (ignore session-id))
  (let* ((args (nlk:trimmed (or args "")))
         (lower (string-downcase args)))
    (cond ((zerop (length args)) (status-text))
          ((uiop:string-prefix-p "unpin " lower) (drop-pin (subseq args 6)))
          ((uiop:string-prefix-p "drop " lower) (drop-pin (subseq args 5)))
          ((uiop:string-prefix-p "pin " lower) (add-pin (subseq args 4) nil nil))
          (t (add-pin args nil nil)))))

;;; --- every turn ---------------------------------------------------------------------

(defun deliver (op next)
  "The :round point: at the first round of a turn, the pins go into the request
about to be built, as a reminder the model reads before anything else."
  (prog1 (funcall next op)
    (handler-case
        (let ((turn (getf op :turn))
              (round (getf op :round)))
          (when (and turn (nlk:store-open-p) (eql 0 (or round 0))
                     (pins-configured-p))
            (let ((block (reminder-block (read-pins))))
              (when block
                (nlk:record-turn-context-prompt turn :text block)))))
      (error (condition) (warn "memlock: ~a" condition)))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell memlock
  (:section ("memlock")
    (:guide "pins_file is the JSON file the pins are kept in (memlock/pins.json under the home is the usual answer); reminder_chars caps how much one turn repeats; max_pins caps how many exist; - /cells shows whether the folder started")
    ("pins_file" :path :required t
     :doc "the file the pins are kept in, e.g. memlock/pins.json under the home; the pin tool and /pins refuse until it is set")
    ("max_pins" :integer :default 16 :min 1
                :doc "how many standing instructions may be pinned at once")
    ("reminder_chars" :integer :default 600 :min 40
                       :doc "the most the reminder block may carry, in characters; the highest priorities fit first"))
  (:tool "pin" (nlk:contract 'pin-call) 'pin-tool)
  (:hook :round #'deliver)
  (:command "pins" 'run-slash
            :catches 'memlock-error
            :description "The pinned standing instructions: list, pin or unpin one")
  (:help :memlock "memlock: pins standing instructions and puts them in front of the model every turn; /pins" +primer+))
