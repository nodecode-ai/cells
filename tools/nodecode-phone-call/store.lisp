;;;; store.lisp --- the calls, their states, their approvals, and the limits.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept this in one SQLite file shared with its bridge, because
;;;; two processes had to agree on a call. A cell is one process, so the call
;;;; is a plist in a table and an approval is a time stamp on it: the same
;;;; state machine, the same one-use fifteen-minute approval, the same "one
;;;; live call, ten a day, sixty seconds between two calls to one number".
;;;; *CLOCK* is a function so a test can hold it still.

(in-package #:nodecode-phone-call)

(defvar *calls* nil "Call id -> call plist, for the calls this process made.")
(defvar *order* nil "Call ids, newest first: what /phone-call log reads.")
(defvar *counter* 0 "How many calls this process has prepared.")
(defvar *clock* (lambda () (get-universal-time))
  "Seconds since the epoch; a test binds a counter here.")

(defparameter +terminal-states+ '("completed" "no_answer" "busy" "failed" "canceled" "expired")
  "A call in one of these is over.")
(defparameter +live-states+ '("dialing" "ringing" "in_progress")
  "A call in one of these is on the line: at most one at a time.")

(defparameter +transitions+
  '(("awaiting_approval" . ("approved" "canceled" "expired"))
    ("approved" . ("dialing" "canceled" "expired"))
    ("dialing" . ("ringing" "in_progress" "no_answer" "busy" "failed" "completed" "canceled"))
    ("ringing" . ("in_progress" "no_answer" "busy" "failed" "completed" "canceled"))
    ("in_progress" . ("completed" "failed" "canceled")))
  "What the machine allows; anything not listed is refused, as in the original.")

(defun now () (funcall *clock*))

(defun reset-store ()
  "Forget every call: what START-CELL does, so a restart begins clean."
  (setf *calls* (make-hash-table :test #'equal) *order* nil *counter* 0))

(defun get-call (id)
  "The call ID names, or NIL."
  (and (stringp id) *calls* (gethash id *calls*)))

(defun (setf get-call) (call id)
  (setf (gethash id *calls*) call))

(defun remember (call)
  "Register CALL under its id, newest first."
  (push (getf call :id) *order*)
  (setf (gethash (getf call :id) *calls*) call)
  call)

(defun update-call (id &rest fields)
  "CALL ID with FIELDS set; answers the call."
  (let ((call (get-call id)))
    (loop for (key value) on fields by #'cddr
          do (setf (getf call key) value))
    (setf (get-call id) call)
    call))

(defun call-state (call) (getf call :state))
(defun terminal-p (state) (member state +terminal-states+ :test #'equal))
(defun live-p (state) (member state +live-states+ :test #'equal))

(defun add-line (id role text)
  "One line of what was said, kept for the transcript."
  (let ((call (get-call id)))
    (when call
      (update-call id :lines (append (getf call :lines) (list (list role text)))))))

(defun transition-refusal (state new)
  "Why STATE may not become NEW, or NIL."
  (unless (member new (cdr (assoc state +transitions+ :test #'equal)) :test #'equal)
    (format nil "a call that is ~a cannot become ~a" state new)))

(defun transition (id new &rest fields)
  "Move CALL ID to NEW when the machine allows it."
  (let ((call (get-call id)))
    (unless call (fail "there is no call ~a" id))
    (let ((why (transition-refusal (call-state call) new)))
      (when why (fail "~a" why)))
    (apply #'update-call id :state new
           (if (and (terminal-p new) (not (getf call :ended-at)))
               (append (list :ended-at (now)) fields)
               fields))))

;;; --- approvals ----------------------------------------------------------------

(defun pending-approval-p (call)
  "Is this call waiting for the operator to approve it?"
  (equal (call-state call) "awaiting_approval"))

(defun approve-call (id)
  "Record the operator's approval: one use, fifteen minutes, this call only."
  (let ((call (get-call id)))
    (unless call (fail "there is no call ~a" id))
    (expire-if-stale id)
    (setf call (get-call id))
    (unless (equal (call-state call) "awaiting_approval")
      (fail "call ~a is ~a, not waiting for approval" id (call-state call)))
    (transition id "approved" :approved-at (now)
                :approval-expires (+ (now) +approval-ttl-s+)
                :approval-used-p nil)
    call))

(defun approval-refusal (id)
  "Why ID may not be dialed now, or NIL. Checked before every dial, and again
when the approval is spent."
  (let ((call (expire-if-stale id)))
    (unless call (return-from approval-refusal (format nil "there is no call ~a" id)))
    (cond ((not (equal (call-state call) "approved"))
           (format nil "call ~a is ~a, not approved (approve it with /phone-call approve ~a)"
                   id (call-state call) id))
          ((getf call :approval-used-p)
           "this approval was already used; prepare a new call")
          ((>= (now) (or (getf call :approval-expires) 0))
           (format nil "the approval for ~a expired; prepare the call again" id))
          (t nil))))

(defun consume-approval (id)
  "Spend the approval: answers T when this caller won it."
  (let ((call (get-call id)))
    (when (and call (not (getf call :approval-used-p)))
      (update-call id :approval-used-p t)
      t)))

(defun expire-if-stale (id)
  "EXPIRED for a card or an approval older than fifteen minutes; answers the call."
  (let ((call (get-call id)))
    (when call
    (cond ((and (equal (call-state call) "awaiting_approval")
                (> (- (now) (or (getf call :created-at) 0)) +approval-ttl-s+))
           (transition id "expired" :error "approval_ttl"))
          ((and (equal (call-state call) "approved")
                (or (getf call :approval-used-p)
                    (>= (now) (or (getf call :approval-expires) 0))))
           (transition id "expired" :error "approval_ttl"))))
    (get-call id)))

;;; --- the limits ---------------------------------------------------------------

(defun recent-calls (&optional (limit 10))
  "The last LIMIT calls, newest first."
  (loop for id in *order* for call = (get-call id) when call collect call into out
        finally (return (subseq out 0 (min limit (length out))))))

(defun live-call (&optional exclude)
  "The call on the line, if any."
  (find-if (lambda (call) (and (live-p (call-state call))
                                (not (equal (getf call :id) exclude))))
           (recent-calls 50)))

(defun calls-dialed-since (since)
  "Calls placed at or after SINCE."
  (count-if (lambda (call) (and (getf call :dialed-at) (>= (getf call :dialed-at) since)))
            (recent-calls 200)))

(defun last-dialed-at (number)
  "When NUMBER was last dialed, or NIL."
  (let ((times (loop for call in (recent-calls 200)
                     when (and (equal (getf call :number) number) (getf call :dialed-at))
                       collect (getf call :dialed-at))))
    (and times (apply #'max times))))

(defun rate-limit-refusal (number &optional exclude)
  "Why one more call may not be placed now, or NIL: one live call, the daily
limit, and sixty seconds between two calls to one number."
  (let ((live (live-call exclude)))
    (when live
      (return-from rate-limit-refusal
        (format nil "another call is already active (~a); wait for it to end" (getf live :id)))))
  (let ((limit (or (ignore-errors (setting :daily-call-limit)) +daily-limit-default+)))
    (when (>= (calls-dialed-since (- (now) 86400)) limit)
      (return-from rate-limit-refusal
        (format nil "the daily call limit of ~d in 24 hours is spent" limit))))
  (let ((last (last-dialed-at number)))
    (when (and last (< (- (now) last) +min-gap-same-number-s+))
      (return-from rate-limit-refusal
        (format nil "this number was dialed moments ago; wait ~d s before trying again"
                (1+ (- +min-gap-same-number-s+ (- (now) last)))))))
  nil)
