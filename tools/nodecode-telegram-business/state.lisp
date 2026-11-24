;;;; state.lisp --- one JSON file under the home: connections, drafts, mutes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept a SQLite database; a cell's state is one private
;;;; JSON file, read once at start and written after every change, so a
;;;; restart keeps the drafts the owner was still looking at.

(in-package #:nodecode-telegram-business)

(defvar *state* nil
  "The loaded state while the cell runs, NIL before START and after STOP.")

(defun new-table () (make-hash-table :test #'equal))

(defun state-path ()
  (nlk:home "telegram-business/state.json"))

(defun empty-state ()
  (let ((state (new-table)))
    (setf (gethash "offset" state) 0
          (gethash "next_draft" state) 0
          (gethash "connections" state) (new-table)
          (gethash "drafts" state) (new-table)
          (gethash "pending_edits" state) (new-table)
          (gethash "paused" state) (new-table)
          (gethash "names" state) (new-table))
    state))

(defun read-state ()
  "The state on disk, or an empty one: a torn or foreign file is not fatal,
it is a fresh start."
  (let ((object (ignore-errors
                 (nlk:decode-json (nlk:read-text (state-path))))))
    (if (hash-table-p object)
        (let ((state (empty-state)))
          (loop for key being the hash-keys of object using (hash-value value)
                do (setf (gethash key state) value))
          state)
        (empty-state))))

(defun save-state ()
  (when *state*
    (nlk:write-file-atomically (state-path) (nlk:encode-json-object *state*)
                               :mode :private)))

(defun state () (or *state* (setf *state* (read-state))))

(defun part (name) (gethash name (state)))
(defun (setf part) (value name) (setf (gethash name (state)) value))
(defun connections () (part "connections"))
(defun drafts () (part "drafts"))
(defun paused () (part "paused"))
(defun pending-edits () (part "pending_edits"))

(defun field (object key) (and (hash-table-p object) (gethash key object)))
(defun (setf field) (value object key) (setf (gethash key object) value))

(defun chat-key (chat-id)
  (and chat-id (princ-to-string chat-id)))

(defun connection (id) (and (stringp id) (gethash id (connections))))
(defun draft (id) (and (stringp id) (gethash id (drafts))))
(defun names () (part "names"))
