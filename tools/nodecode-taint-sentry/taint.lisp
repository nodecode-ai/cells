;;;; taint.lisp --- per-session taint state, ported from the plugin's taint.py.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A mark says: this session has ingested content from a source that is not
;;;; allowed to give instructions. It is set by PROVENANCE (which tool produced
;;;; the content), never by a model's opinion, and it decays on a TTL so one
;;;; fetched page does not gate the session forever.
;;;;
;;;; The state is process memory, keyed by session id, and deliberately not
;;;; persisted: a resumed session re-earns its taint the moment it reads
;;;; untrusted content again, and stale taint would gate work for no reason.

(in-package #:nodecode-taint-sentry)

(defparameter +max-sessions+ 512
  "Above this, the sessions nobody has touched in a whole TTL are dropped.")

(defstruct (session (:constructor make-a-session))
  (marks (make-hash-table :test #'equal) :type hash-table)
  (artifacts (make-hash-table :test #'equal) :type hash-table)
  (sensitive '())
  (total 0)
  (created (get-universal-time) :type integer))

(defvar *sessions* (make-hash-table :test #'equal)
  "session id -> SESSION. Live only.")

(defvar *lock* (and (find-package :sb-thread)
                    (sb-thread:make-mutex :name "taint-sentry"))
  "One writer at a time: the hook runs on a turn thread, a command may not.")

(defmacro with-store-lock (&body body)
  "BODY with the store held, where the image has threads."
  (if (find-package :sb-thread)
      `(sb-thread:with-mutex (*lock*) ,@body)
      `(progn ,@body)))

(defun reset-store ()
  "Forget every session's taint. The cell's own, for a test or a purge."
  (with-store-lock (clrhash *sessions*)))

(defun session-of (id)
  "The SESSION record for ID, made when it is the first mark."
  (or (gethash id *sessions*)
      (setf (gethash id *sessions*) (make-a-session))))

(defun ttl-seconds ()
  "How long a mark stays live, from the section."
  (max 1 (or (ignore-errors (setting :taint-ttl-seconds)) 1800)))

(defun prune (session)
  "Drop SESSION's marks older than the taint TTL, and its artifacts with them:
a file written from untrusted content a TTL ago is ordinary again."
  (let ((cutoff (- (get-universal-time) (ttl-seconds))))
    (maphash (lambda (label mark)
               (when (< (getf mark :last) cutoff)
                 (remhash label (session-marks session))))
             (session-marks session))
    session))

(defun mark-untrusted (session-id &key label tool host reason signatures severity turn)
  "Record one untrusted source in SESSION-ID, and answer its label. The same
label twice counts, and keeps the newest time: it is one source, not two."
  (with-store-lock
    (let ((session (prune (session-of session-id))))
      (incf (session-total session))
      (let ((existing (gethash label (session-marks session))))
        (setf (gethash label (session-marks session))
              (list :label label
                    :tool (or tool "")
                    :host (or host "")
                    :reason (or reason "")
                    :signatures (or signatures '())
                    :severity (or severity "")
                    :count (1+ (or (getf existing :count) 0))
                    :first (or (getf existing :first) (get-universal-time))
                    :last (get-universal-time)
                    :turn (or turn ""))))
      label)))

(defun tainted-p (session-id)
  "Whether SESSION-ID carries a live mark."
  (with-store-lock
    (let ((session (gethash (or session-id "") *sessions*)))
      (and session (plusp (hash-table-count (session-marks (prune session))))))))

(defun snapshot (session-id)
  "What SESSION-ID carries, as data.
=> a plist :SOURCES (label -> mark plist), :ARTIFACTS (path -> label),
   :TOTAL, :AGE."
  (with-store-lock
    (let* ((session (and session-id (gethash session-id *sessions*)))
           (session (and session (prune session)))
           (sources (make-hash-table :test #'equal)))
      (when session
        (maphash (lambda (label mark)
                   (setf (gethash label sources)
                         (list :label label :tool (getf mark :tool) :host (getf mark :host)
                               :reason (getf mark :reason) :signatures (getf mark :signatures)
                               :severity (getf mark :severity) :count (getf mark :count)
                               :age (- (get-universal-time) (getf mark :last)))))
                 (session-marks session)))
      (list :sources sources
            :artifacts (if session (session-artifacts session) (make-hash-table :test #'equal))
            :total (if session (session-total session) 0)
            :age (if session (- (get-universal-time) (session-created session)) 0)))))

(defun session-labels (session-id)
  "Every live label SESSION-ID carries, sorted."
  (let ((table (getf (snapshot session-id) :sources)))
    (sort (loop for label being the hash-keys of table collect label) #'string<)))

(defun describe-sources (session-or-snapshot)
  "The sources in words, for a refusal or a receipt: LABEL (reason), ..."
  (let* ((snapshot (if (stringp session-or-snapshot)
                       (snapshot session-or-snapshot)
                       session-or-snapshot))
         (table (getf snapshot :sources))
         (parts '()))
    (maphash (lambda (label mark)
               (push (format nil "~a (~a)" label (getf mark :reason)) parts))
             table)
    (format nil "~{~a~^, ~}" (sort parts #'string<))))

(defun nearest-tainted-session ()
  "The session carrying the newest live mark, or NIL. A status asked from a
place that has no session of its own (a front end, a fresh call) still answers
about the taint that is live in this process."
  (with-store-lock
    (let ((best nil) (newest 0))
      (maphash (lambda (id session)
                 (prune session)
                 (let ((mark (first (sort (loop for m being the hash-values of (session-marks session)
                                                collect m)
                                          #'> :key (lambda (m) (getf m :last))))))
                   (when (and mark (> (getf mark :last) newest))
                     (setf newest (getf mark :last) best id))))
               *sessions*)
      best)))

;;; --- files written from untrusted content -----------------------------------

(defun note-artifact (session-id path label)
  "Remember that PATH was written while LABEL's content was live, so reading it
back later is untrusted too."
  (with-store-lock
    (when (and path (plusp (length path)))
      (setf (gethash path (session-artifacts (session-of session-id))) label))))

(defun artifact-label (session-id path)
  "The label PATH was written under, or NIL."
  (with-store-lock
    (let ((session (and session-id (gethash session-id *sessions*))))
      (and session (gethash path (session-artifacts session))))))

(defun note-sensitive (session-id path)
  "Remember that PATH was read: something that sends data out afterwards is the
two-step half of an exfiltration."
  (with-store-lock
    (let ((session (session-of session-id)))
      (pushnew path (session-sensitive session) :test #'equal)
      (setf (session-sensitive session) (subseq (session-sensitive session) 0
                                                (min 16 (length (session-sensitive session))))))))

(defun recent-sensitive (session-id)
  "The newest sensitive path SESSION-ID read, or an empty string."
  (with-store-lock
    (let ((session (and session-id (gethash session-id *sessions*))))
      (or (and session (first (session-sensitive session))) ""))))
