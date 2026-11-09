;;;; inbox.lisp --- what Ando holds for this agent, and the claim that clears it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ando's inbox is the durable record of everything sent to the agent
;;;; while it was away: a mention, a direct message, a reply in a thread it
;;;; is in. The sweep reads both scopes it keeps (direct and updates), pages
;;;; through each, and keeps the rows that are a message; what it answers it
;;;; then marks read with the claim Ando asks for, so the next sweep does not
;;;; hand the same person's words over twice.

(in-package #:nodecode-ando)

(defparameter +message-types+ '("mention" "direct_message" "reply")
  "The inbox rows that are a message to answer; the rest are activity about one.")

(defparameter +page-limit+ 100
  "Rows one page of the inbox asks for.")

(defun inbox-page (connection scope &optional cursor)
  "SCOPE's next page of CONNECTION's inbox."
  (call-tool connection "get_agent_inbox"
             (if cursor
                 (nlk:make-json-object "scope" scope "limit" +page-limit+ "cursor" cursor)
                 (nlk:make-json-object "scope" scope "limit" +page-limit+))))

(defun scope-items (connection scope)
  "Every message row SCOPE holds, first page first."
  (let ((items '())
        (cursor nil)
        (seen (make-hash-table :test #'equal)))
    (loop
      (let ((page (inbox-page connection scope cursor)))
        (loop for item across (nlk:json-array page "items")
              when (member (nlk:json-value item :string "type") +message-types+
                           :test #'string=)
                do (push item items))
        (let ((page-meta (nlk:json-value page :object "page")))
          (unless (nlk:json-value page-meta :boolean "has_more")
            (return))
          (let ((next (nlk:json-value page-meta :string "next_cursor")))
            (when (or (null next) (gethash next seen))
              (return))
            (setf (gethash next seen) t
                  cursor next)))))
    (nreverse items)))

(defun sweep (connection)
  "Every message row Ando holds for CONNECTION, direct before updates, each
row once: the two scopes are one inbox seen from two sides, and a scroll
that ends on a boundary hands the same activity to both."
  (let ((seen (make-hash-table :test #'equal))
        (rows '()))
    (dolist (item (append (scope-items connection "direct")
                           (scope-items connection "updates"))
              (nreverse rows))
      (let ((event (nlk:json-value item :string "event_id")))
        (unless (and event (gethash event seen))
          (when event (setf (gethash event seen) t))
          (push item rows))))))

(defun message-of (connection message-id)
  "MESSAGE-ID's message as Ando answers it."
  (let ((answer (call-tool connection "get_message"
                            (nlk:make-json-object "message_id" message-id))))
    (or (nlk:json-value answer :object "data") answer)))

(defun acknowledge (connection item state)
  "Tell Ando ITEM is in STATE, best effort: a lost claim is not a lost answer,
and the cell's own table already keeps it from answering twice."
  (ignore-errors
    (call-tool connection "acknowledge_agent_inbox_item"
               (nlk:make-json-object
                "event_id" (nlk:json-value item :string "event_id")
                "expected_revision" (or (nlk:json-value item :integer "revision") 1)
                "state" state
                "execution_id" (ando-connection-receiver-id connection))))
  nil)
