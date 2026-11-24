;;;; state.lisp --- the one file the rule keeps, and the values it runs on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin kept this in its host's plugin state; a cell keeps it
;;;; in one readable file under the home, written atomically, so a restart of
;;;; the image resumes the back-off instead of starting it over:
;;;;
;;;;   (:last-activity <utc> :last-user <utc> :last-nudge <utc>
;;;;    :nudges <n> :last-brief <local midnight> :bound-at <utc> :last-result "...")
;;;;
;;;; It holds timestamps and a count, never a word of a conversation.
;;;;
;;;; *LIVE* is the section as the cell runs on it: the typed settings the
;;;; declaration materialized, a copy the operator's own changes move. A change
;;;; is written back to the section with CONFIG-SET, so the config and the
;;;; running cell never disagree after the next start.

(in-package #:nodecode-checkin)

;;; --- the file ----------------------------------------------------------------

(defun state-path ()
  "Where the state lives: the file a test named, else checkins/state.sexp."
  (or *state-file* (nlk:home "checkins/state.sexp")))

(defun load-state ()
  "The state the file holds, as a plist; NIL when it is absent or unreadable."
  (let ((path (state-path)))
    (when (uiop:file-exists-p path)
      (handler-case
          (with-open-file (in path :direction :input)
            (let ((*read-eval* nil)
                  (object (read in nil nil)))
              (when (and (listp object) (evenp (length object))) object)))
        (error () nil)))))

(defun save-state ()
  "Write *STATE* where a restart reads it, atomically."
  (let ((path (state-path)))
    (ignore-errors
      (ensure-directories-exist path)
      (nlk:write-file-atomically
       path (with-standard-io-syntax (let ((*print-pretty* t)) (prin1-to-string *state*)))
       :mode #o600 :directory-mode #o700))
    (values)))

(defmacro with-state (&body body)
  "BODY holding the state lock, on the state as the file has it."
  `(bt2:with-lock-held (*state-lock*) ,@body))

(defun state (key &optional default)
  "KEY's value in the state."
  (getf *state* key default))

(defun (setf state) (value key)
  "Set KEY in the state and write the file."
  (setf (getf *state* key) value)
  (save-state)
  value)

(defun note-activity (utc &key user)
  "Record that the session was spoken in at UTC: USER is the operator's own
turn, which resets the back-off."
  (setf (state :last-activity) utc)
  (when user
    (setf (state :last-user) utc
          (state :nudges) 0))
  (save-state))

;;; --- the section as it runs ---------------------------------------------------

(defvar *live* '()
  "The section's values as the cell runs on them, a plist: :session, :base,
:start, :end, :offset, :paused, :brief, :voice, :tick.")

(defun live (key &optional default)
  "KEY's running value."
  (getf *live* key default))

(defparameter *section-keys*
  '((:session . "session") (:base . "base_minutes") (:start . "active_start")
    (:end . "active_end") (:offset . "utc_offset_minutes") (:paused . "paused")
    (:brief . "morning_brief") (:voice . "voice") (:tick . "tick_seconds"))
  "The running value each member of the section is kept under.")

(defun live-from-settings ()
  "The plist the cell runs on, read off the section's typed settings."
  (setf *live*
        (list :session (or (setting :session) "")
              :base (or (setting :base-minutes) 120)
              :start (or (setting :active-start) 8)
              :end (or (setting :active-end) 22)
              :offset (or (setting :utc-offset-minutes) 0)
              :paused (setting :paused)
              :brief (setting :morning-brief)
              :voice (or (setting :voice) "texting")
              :tick (or (setting :tick-seconds) 60)))
  *live*)

(defun set-live (key value)
  "Move one running value, and write it into the section so a restart keeps it.
=> the CONFIG-SET diff line, or NIL when nothing could be written (a test
home with no shared config)."
  (setf (getf *live* key) value)
  (let ((name (cdr (assoc key *section-keys*))))
    (when name
      (ignore-errors (nle:config-set (list "checkin" name) value)))))

(defun bound-p ()
  "Whether a session has been named to check in on."
  (let ((session (live :session)))
    (and (stringp session) (plusp (length session)))))

(defun bind (session-id &key (now (funcall *clock*)))
  "Check in on SESSION-ID from now on: the chat the command was typed in."
  (set-live :session session-id)
  (setf (state :bound-at) now (state :nudges) 0)
  (setf (state :last-nudge) nil)
  (save-state)
  session-id)

(defun unbind ()
  "Stop checking in on any session."
  (set-live :session "")
  (save-state)
  (values))
