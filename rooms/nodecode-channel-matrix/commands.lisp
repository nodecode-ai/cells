;;;; commands.lisp --- the command menu, as MSC4391 state in each room.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Matrix spec keeps no bot command menu yet. MSC4391
;;;; (https://github.com/matrix-org/matrix-spec-proposals/pull/4391) keeps one
;;;; in each room as state: one org.matrix.msc4391.command_description event
;;;; per command, its state key the command and the bot's id hashed, so two
;;;; bots offering one command never overwrite each other. A client that reads
;;;; it (gomuks, Sable) completes "/" from it and sends the command with the
;;;; bot mentioned, which the lane already reads as an ask addressed to it.
;;;;
;;;; The kit plans a menu once per catalog shape; Matrix keeps it per room,
;;;; and a room joined later, or one where the bot has only now been let set
;;;; state, needs it as well. So the book keeps what each room's state holds of
;;;; the bot's commands and who may set them, a plan sends only what differs,
;;;; and the lap asks the kit to plan again whenever the book has learned
;;;; something since the last plan (MENU-REARM, adapter.lisp).

(in-package #:nodecode-channel-matrix)

;;; --- what one command is -----------------------------------------------------------

(defun json-shape (value)
  "VALUE, decoded JSON or JSON about to be sent, in a form EQUAL compares: an
object an alist sorted by key, an array a list, an empty object NIL."
  (typecase value
    (hash-table (sort (loop for key being the hash-keys of value using (hash-value item)
                            collect (cons key (json-shape item)))
                      #'string< :key #'car))
    (string value)
    (vector (map 'list #'json-shape value))
    (t value)))

(defun shape-get (shape &rest keys)
  "The value under KEYS in SHAPE, a JSON-SHAPE of an object."
  (loop for key in keys
        do (setf shape (and (listp shape) (cdr (assoc key shape :test #'equal)))))
  shape)

(defun command-state-key (command bot-user-id)
  "The state key of COMMAND's event for BOT-USER-ID: the padded base64 of the
SHA-256 of the command followed by the user id, MSC4391's own."
  (cl-base64:usb8-array-to-base64-string
   (ironclad:digest-sequence
    :sha256 (sb-ext:string-to-octets (concatenate 'string command bot-user-id) :external-format :utf-8))))

(defun text-container (text)
  "TEXT as the extensible text container MSC4391 describes things in."
  (nlk:json-object "m.text" (vector (nlk:json-object "body" text))))

(defun command-tail (entry)
  "What ENTRY's usage says follows its command, or NIL when nothing does."
  (let ((usage (nlk:trimmed (or (getf entry :usage) "")))
        (head (format nil "/~a" (getf entry :name))))
    (when (uiop:string-prefix-p head usage)
      (setf usage (nlk:trimmed (subseq usage (length head)))))
    (cond ((plusp (length usage)) usage)
          ((getf entry :autocomplete) "arguments")
          (t nil))))

(defun command-content (entry)
  "ENTRY, one line of the kit's menu, as its command_description content: the
command, what it does, and -- when it takes any -- one optional free-text
tail its usage describes, so a client asks for nothing it would not pass."
  (let ((tail (command-tail entry)))
    (nlk:json-object
     "command" (getf entry :name)
     "description" (text-container (or (getf entry :description) (getf entry :name)))
     :when tail "parameters" (vector (nlk:json-object
                                      "key" "args"
                                      "schema" (nlk:json-object "schema_type" "primitive" "type" "string")
                                      "optional" t
                                      "description" (text-container tail)))
     :when tail "fi.mau.tail_parameter" "args")))

(defun menu-commands (entries bot-user-id)
  "ENTRIES, the kit's menu, as (STATE-KEY . CONTENT) per command."
  ;; An alias is a command of its own here, as in every platform's menu: the
  ;; kit spelled it so, and NLE:SLASH resolves what a client sends back.
  (loop for entry in entries
        collect (cons (command-state-key (getf entry :name) bot-user-id) (command-content entry))))

;;; --- who may set it -----------------------------------------------------------------

(defun power-level (value default)
  "A power level as the spec reads one: an integer, or a string an old room
spelled one as."
  (cond ((integerp value) value)
        ((stringp value) (or (ignore-errors (parse-integer value)) default))
        (t default)))

(defun menu-writable-p (book room bot-user-id)
  "Whether BOT-USER-ID may set ROOM's command state, as far as BOOK knows. A
room's creator may in a version 12 room whatever the levels say; a room
whose levels are not known is tried."
  (let ((levels (gethash room book.levels :unknown)))
    (cond ((member bot-user-id (gethash room book.creators) :test #'equal) t)
          ((eq levels :unknown) t)
          ;; No m.room.power_levels at all: state_default is 0.
          ((null levels) t)
          (t (>= (power-level (shape-get levels "users" bot-user-id)
                              (power-level (shape-get levels "users_default") 0))
                 (power-level (shape-get levels "events" +command-event-type+)
                              (power-level (shape-get levels "state_default") 50)))))))

(defun menu-levels-text (book room bot-user-id)
  "Why BOT-USER-ID may not set ROOM's command state, in words."
  (let ((levels (gethash room book.levels)))
    (format nil "setting its state needs power level ~d, and ~a has ~d"
            (power-level (shape-get levels "events" +command-event-type+)
                         (power-level (shape-get levels "state_default") 50))
            bot-user-id
            (power-level (shape-get levels "users" bot-user-id)
                         (power-level (shape-get levels "users_default") 0)))))

(defun room-creators (content sender)
  "Who created a room whose m.room.create carries CONTENT and SENDER, when
its version makes creators outrank every level (12 and later), else NIL."
  (let ((version (ignore-errors (parse-integer (or (nlk:json-value content :string "room_version") "1")))))
    (when (and version (>= version 12))
      (remove nil (cons sender (coerce (or (nlk:json-value content :array "additional_creators") #())
                                       'list))))))

;;; --- what the book learns -----------------------------------------------------------

(defun note-menu-event (book room-id event bot-user-id)
  "Learn from one state EVENT of the menu's types in ROOM-ID: a command the
room holds (the bot's own, or :FOREIGN for another's at one of its keys),
the room's levels, its creators. => T when BOOK changed."
  (let* ((type (nlk:json-value event :string "type"))
         (content (or (nlk:json-value event :object "content") (nlk:json-object)))
         (key (nlk:json-value event :string "state_key"))
         (sender (nlk:json-value event :string "sender")))
    (flet ((note (table at value)
             (multiple-value-bind (held known) (gethash at table)
               (unless (and known (equal held value))
                 (setf (gethash at table) value)
                 (incf book.menu-generation)
                 t))))
      (cond
        ((equal type +command-event-type+)
         (nlk:when-let (held (and key (gethash room-id book.menus)))
           (note held key (if (equal sender bot-user-id) (json-shape content) :foreign))))
        ((equal type "m.room.power_levels")
         (when (note book.levels room-id (json-shape content))
           (when (and (gethash room-id book.menus) (not (menu-writable-p book room-id bot-user-id)))
             (warn "matrix: room ~a keeps no command menu from this bot: ~a"
                   room-id (menu-levels-text book room-id bot-user-id)))
           t))
        ((equal type "m.room.create")
         (note book.creators room-id (room-creators content sender)))))))

(defun note-room-state (book room-id events bot-user-id)
  "Learn ROOM-ID's menu from EVENTS, its whole state as /rooms/{room}/state
answers it: the room is known from here on, a command it does not hold is
one it lacks."
  (book-note book.menus room-id (make-hash-table :test #'equal :synchronized t))
  (incf book.menu-generation)
  ;; Levels before commands do not matter; the create event before the levels
  ;; does, for the warning, so creators are learned first.
  (flet ((pass (types)
           (loop for event across events
                 when (member (nlk:json-value event :string "type") types :test #'equal)
                   do (note-menu-event book room-id event bot-user-id))))
    (pass '("m.room.create"))
    (pass (list "m.room.power_levels" +command-event-type+))))

;;; --- the plans ----------------------------------------------------------------------

(defun state-plan (room type state-key content &key (timeout-seconds 30))
  "The PUT of one state event of TYPE and STATE-KEY carrying CONTENT into ROOM."
  ;; Setting state is idempotent: 5xx and 429 retried.
  (rest-plan "PUT" (room-path room "state" type state-key) "set_state" t timeout-seconds content))

(defun menu-plans (book bot-user-id entries &key (timeout-seconds 30))
  "The calls that make every room BOOK knows, and the bot may set state in,
hold ENTRIES as its command menu: a command it lacks or holds otherwise is
set, one of the bot's it holds that ENTRIES no longer name is emptied. A
menu every room holds already is the one call that does nothing, so the kit
records its shape."
  ;; Tick thread: no I/O. Records the book generation planned from, which
  ;; MENU-REARM compares.
  (let ((generation book.menu-generation)
        (wanted (menu-commands entries bot-user-id))
        (plans '()))
    (sb-ext:with-locked-hash-table (book.menus)
      (maphash
       (lambda (room held)
         (when (menu-writable-p book room bot-user-id)
           (sb-ext:with-locked-hash-table (held)
             (loop for (key . content) in wanted
                   unless (equal (json-shape content) (gethash key held))
                     do (push (state-plan room +command-event-type+ key content
                                          :timeout-seconds timeout-seconds)
                              plans))
             (maphash (lambda (key shape)
                        (when (and shape (not (eq shape :foreign)) (not (assoc key wanted :test #'equal)))
                          (push (state-plan room +command-event-type+ key (nlk:json-object)
                                            :timeout-seconds timeout-seconds)
                                plans)))
                      held))))
       book.menus))
    (setf book.menu-planned generation)
    (or (nreverse plans) (list nil))))
