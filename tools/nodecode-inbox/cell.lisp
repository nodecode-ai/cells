;;;; cell.lisp --- the tool, /inbox and the manual entry.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A read-only view of everything in flight in this home: the cells that
;;;; have something standing to say, sessions whose answer waits on the
;;;; person, cron jobs that failed or were paused, the evaluations still
;;;; running, cron answers not yet delivered, what is scheduled and the
;;;; sessions seen most recently. Nothing is changed, nothing is written, and
;;;; every source may be absent -- an empty home reads as all clear.
;;;;
;;;; Two ways in: the `inbox' tool (the model reads it and tells the
;;;; operator) and /inbox SECTION (the operator reads it in the shell). The
;;;; plist behind both is (NODECODE-INBOX:SNAPSHOT), for a script.

(in-package #:nodecode-inbox)

(nlk:define-peripheral inbox :not-running t)

(defparameter +manual+
  "(inbox) reads everything in flight in this home, read-only: what needs attention (what a cell last
said under a standing-notice key, sessions whose answer waits on the person, cron jobs that failed
or were paused), what is running (background evaluations, each with its id for (eval-status N)),
cron answers not yet delivered to the session that scheduled them, what is scheduled next, and the
sessions seen most recently with how long ago. Nothing is changed; a source that cannot be read is
named in the Not read footer instead of failing the read. The tool's section argument narrows it to
attention, progress, finished, scheduled or sessions; /inbox takes the same word, and
/inbox schedule prints the cron jobs anyway if a shell reads it as /inbox. (NODECODE-INBOX:SNAPSHOT)
answers the same data as a plist for a script, and (NODECODE-INBOX:INBOX-TEXT :ONLY \"attention\")
is the text."
  "What (help :inbox) answers while the cell runs.")

(defparameter +section-options+ '("" "attention" "progress" "finished" "scheduled" "sessions")
  "What the tool's section member may be, empty for the whole inbox.")

(nlk:define-contract inbox-query
  (:doc "Read the inbox: one read-only view of everything in flight in this Nodecode home -- what needs attention (a cell's standing notice, a session whose answer waits on the person, a cron job that failed or was paused), what is running (background evaluations), cron answers not yet delivered, what is scheduled, and the sessions seen most recently. Nothing is changed and nothing is written. Read it when the operator asks what is going on, what is pending or what ran, and before deciding there is nothing to do.")
  ("section" :text :default "" :options +section-options+
             :doc "one section of the inbox, or empty for all of it"))

(defun read-inbox (query)
  "The inbox tool: the whole view, or the one section QUERY names."
  (let ((section (inbox-query-section query)))
    (or (inbox-text :only section)
        "the inbox is empty")))

(defun run-slash (args session-id)
  "/inbox [SECTION]: the view, or one section of it."
  (declare (ignore session-id))
  (let ((section (string-downcase (string-trim " " (or args "")))))
    (cond ((member section (cons "" +sections+) :test #'string=)
           (let ((text (inbox-text :only section)))
             (if (plusp (length text)) text "the inbox is empty")))
          (t (format nil "~a is not one of the sections: ~{~a~^, ~}"
                     section (remove "" +sections+))))))

(nle:define-cell inbox
  (:section ("inbox")
    (:guide "nothing is required here; limit is how many rows a section prints")
    ("limit" :integer :default 20 :min 1
             :doc "how many rows each section of the inbox prints"))
  (:tool "inbox" (nlk:contract 'inbox-query) 'read-inbox)
  (:command "inbox" 'run-slash
            :description "Everything in flight: pending, running, scheduled, open"
            :argument-hint "ATTENTION|PROGRESS|FINISHED|SCHEDULED|SESSIONS")
  (:help :inbox
         "inbox: what is in flight, read-only - pending, running, scheduled, open"
         +manual+))
