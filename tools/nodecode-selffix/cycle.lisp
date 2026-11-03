;;;; cycle.lisp --- one observe cycle, and the wake source that runs it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cycle: the stop, the breakers and the idle gate, all in Lisp and before
;;;; any work, so a cycle that holds costs nothing; then the lock, the first
;;;; captured case the backlog ranks that the ledger still allows, and its
;;;; repro: one Lisp form, asked sealed in a fresh process laid at the newest
;;;; generation (runner.lisp). A repro that answers 1 there is a finding: one
;;;; readable .lisp file under selffix/findings/, committed to that folder's
;;;; own git, with the generation step where it turned from gone to standing.
;;;; The commit is the lock: a later fix is checked against the repro as that
;;;; commit holds it, never against one the fixing session rewrote. Observe
;;;; changes no code, and makes no model call of its own.
;;;;
;;;; It runs as a source on the cron wake: when the wake is due, the source
;;;; runs one cycle and reports a new finding or a pause as news, which the
;;;; wake's turn reads. A wake that is off runs no cycle.

(in-package #:nodecode-selffix)

(defparameter *backlog-limit* 20 "Items read from the backlog for one cycle.")

(defvar *backlog* 'backlog "The backlog reader; a test binds a stub.")
(defun item-repro (item) (repro-form (case-of item)))
(defun item-deftest (item) (repro-deftest (case-of item)))

(defvar *repro-form* 'item-repro
  "(funcall *repro-form* ITEM) => the repro's form as text, answering 1 while
the failure stands, 0 once gone and 2 when it cannot judge, or NIL when the
case is no one form (repro.lisp); a test binds a stub.")
(defvar *repro-deftest* 'item-deftest
  "(funcall *repro-deftest* ITEM) => the deftest the owning cell gets
upstream, as text, or NIL (repro.lisp); a test binds a stub.")
(defvar *generations* 'generation-numbers "The sealed generations, oldest first; a test binds a stub.")
(defvar *busy* 'busy-reason "The idle gate's reader; a test binds a stub.")

(defun findings-dir () (selffix-path "findings/"))
(defun finding-file (fp) (merge-pathnames (format nil "~a.lisp" fp) (findings-dir)))

(defun item-text (item)
  "What ITEM is called: its title, else its key."
  (princ-to-string (or (getf item :title) (getf item :key))))

(defun finding-line (fp item)
  (format nil "~a (~(~a~), seen ~d time~:p): ~a" fp (getf item :source) (or (getf item :count) 1)
          (nlk:one-line (item-text item) :cap 120)))

(defun git (&rest arguments)
  "Run git in the findings folder => its output; a nonzero exit signals."
  (multiple-value-bind (out err status)
      (nlk:run-bounded (list* "git" "-c" "user.name=selffix" "-c" "user.email=selffix@localhost"
                              "-C" (native (findings-dir)) arguments)
                       :seconds 30)
    (unless (eql status 0)
      (fail "git ~{~a~^ ~} exited ~a: ~a" arguments status (nlk:one-line (princ-to-string (or err "")) :cap 200)))
    (string-trim '(#\Space #\Newline) (princ-to-string (or out "")))))

(defun write-finding (fp item text located now)
  "FP's finding as one readable file, committed => (values PATH COMMIT)."
  (let ((path (finding-file fp))
        (deftest (ignore-errors (funcall *repro-deftest* item))))
    (ensure-directories-exist path)
    (unless (probe-file (merge-pathnames ".git/" (findings-dir)))
      (git "init" "-q"))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out ";;;; selffix finding ~a: ~a~%" fp (nlk:one-line (item-text item) :cap 120))
      (format out ";;;; The repro answers 1 while the failure stands. It is locked by this file's~%~
                   ;;;; commit: a fix counts only when the repro as committed answers 0.~%~%")
      (write-line (readable (list :finding fp
                                  :source (getf item :source) :key (getf item :key)
                                  :count (or (getf item :count) 1) :last (getf item :last)
                                  :found (iso now) :located located :repro text))
                  out)
      (when deftest
        (format out "~%;;; The test the cell that owns the function gets upstream:~%~a~%" deftest)))
    (git "add" "--" (file-namestring path))
    (git "commit" "-q" "-m" (format nil "finding ~a: ~a" fp (nlk:one-line (item-text item) :cap 72)) "--" (file-namestring path))
    (values path (git "rev-parse" "HEAD"))))

(defun locked-repro (fp commit)
  "The repro text of FP's finding as COMMIT holds it, whatever the file says now."
  (getf (with-standard-io-syntax
          (let ((*read-eval* nil) (*package* (find-package '#:keyword)))
            (read-from-string (git "show" (format nil "~a:~a.lisp" commit fp)))))
        :repro))

(defun reproduce (path)
  "The finding at PATH, its repro as its last commit holds it, asked sealed at
the newest generation => 1 while the failure stands, 0 once gone, -1 when it
could not run."
  (let* ((fp (pathname-name path))
         (commit (git "log" "-1" "--format=%H" "--" (format nil "~a.lisp" fp)))
         (numbers (funcall *generations*)))
    (if (zerop (length commit))
        -1
        (ecase (ask-at (locked-repro fp commit) (car (last numbers)))
          (:stands 1)
          (:gone 0)
          ((:undecided :unknown) -1)))))

(defun run-one (now)
  "With the lock held: one item through its repro => (STATUS DETAIL)."
  (let ((items (handler-case (funcall *backlog* :limit *backlog-limit* :fixable t)
                 (error (condition) (record "failed" :detail (format nil "backlog: ~a" condition))
                   (return-from run-one (list :failed (format nil "the backlog could not be read: ~a" condition)))))))
    (multiple-value-bind (kept dropped) (eligible items (ledger-rows))
      (dolist (item dropped)
        (record "dropped" :fp (fingerprint item) :item item
                          :detail (format nil "~d cycles without a finding" *attempt-cap*)))
      (if (null kept)
          (list :nothing "nothing in the backlog to reproduce")
          (let* ((item (first kept))
                 (fp (fingerprint item)))
            (handler-case
                (let ((text (funcall *repro-form* item))
                      (numbers (funcall *generations*)))
                  (cond ((null text)
                         (record "not-reproduced" :fp fp :item item :detail "the case is no one form")
                         (list :not-reproduced (finding-line fp item)))
                        (t
                         (multiple-value-bind (verdict detail) (ask-at text (car (last numbers)))
                           (case verdict
                             (:stands
                              (multiple-value-bind (path commit) (write-finding fp item text (and numbers (locate text numbers)) now)
                                (record "finding" :fp fp :item item :detail (namestring path) :commit commit)
                                (list :finding (finding-line fp item))))
                             ((:gone :undecided)
                              (record "not-reproduced" :fp fp :item item
                                                       :detail (if (eq verdict :gone) "the repro answered 0" "the repro could not judge"))
                              (list :not-reproduced (finding-line fp item)))
                             (t
                              (record "failed" :fp fp :item item :detail detail)
                              (list :failed (format nil "~a: ~a" fp detail))))))))
              (error (condition)
                (record "failed" :fp fp :item item :detail (princ-to-string condition))
                (list :failed (format nil "~a: ~a" fp condition)))))))))

(defun cycle (&key (now (get-universal-time)) (idle-seconds (* 60 (setting :idle-minutes))))
  "One observe cycle => (values STATUS DETAIL). STATUS is :STOPPED, :PAUSED,
:BUSY or :LOCKED when a gate held, else :NOTHING, :FINDING, :NOT-REPRODUCED
or :FAILED."
  (let ((rows (ledger-rows)))
    (nlk:when-let (stop (stopped rows))
      (return-from cycle (values :stopped (format nil "stopped at ~a~@[ by ~a~]" (row stop "at") (row stop "by")))))
    (nlk:when-let (reason (breaker rows))
      (return-from cycle (values :paused reason)))
    (nlk:when-let (busy (funcall *busy* now idle-seconds))
      (return-from cycle (values :busy busy)))
    (unless (take-lock)
      (return-from cycle (values :locked "another cycle holds the lock")))
    (unwind-protect (values-list (run-one now))
      (drop-lock))))

(defun post-findings ()
  "Stand the findings count on the board under the cell's key, or clear it."
  (let ((count (length (findings (ledger-rows)))))
    (nle:notice (and (plusp count)
                     (format nil "selffix: ~d finding~:p, each with a failing test (/selffix findings)" count))
                :key "nodecode-selffix")))

;;; --- the wake source -------------------------------------------------------------
;;;
;;; Cron asks its wake sources on the ticker thread, and a cycle is slow: one
;;; sealed run is a generation laid and a fresh process (each bounded by
;;; *REPRO-SECONDS*), and a finding's locate asks up to *LOCATE-PROBES* more.
;;; Run there, it would hold every other cron job back for minutes. So the
;;; source starts the cycle on a thread of its own and answers at once with
;;; what the ledger gained since the wake's cursor: a cycle's news is said at
;;; the next due wake, and a wake that is skipped (a money gate) leaves the
;;; cursor, so it loses nothing. The findings notice stands at once.

(defvar *async* t
  "Whether a wake starts its cycle on a thread of its own; a test binds NIL.")

(defvar *cycle-thread* nil "The thread the last wake started its cycle on, or NIL.")

(defvar *cycle-mutex* (sb-thread:make-mutex :name "selffix cycle thread"))

(defparameter +carried+
  '(*root* *present* *backlog* *backlog-limit* *backlog-days* *backlog-sources* *excluded-session-p*
    *repro-form* *repro-deftest* *generations* *busy* *ask* *lay* *repro-seconds* *locate-probes*
    *attempt-cap* *failure-breaker*)
  "The specials a cycle thread takes from the wake that starts it, since a
new thread sees only their global values.")

(defun cycle-and-notice (idle-seconds)
  (when (eq :finding (cycle :idle-seconds idle-seconds))
    (post-findings)))

(defun start-cycle (idle-seconds)
  "One cycle on a thread of its own, unless the last one still runs => the thread, or NIL."
  (let ((values (mapcar #'symbol-value +carried+)))
    (sb-thread:with-mutex (*cycle-mutex*)
      (unless (and *cycle-thread* (bt2:thread-alive-p *cycle-thread*))
        (setf *cycle-thread*
              (nlk:spawn "nodecode-selffix cycle"
                (progv +carried+ values
                  (handler-case (cycle-and-notice idle-seconds)
                    (error (condition) (warn "selffix cycle: ~a" condition))))))))))

(defvar *said* (make-hash-table :test 'equal)
  "Rows this process said as news, each with its time: a row in the very second
a wake fired may have been said by that wake, whose cursor is that second.")

(defun news-line (row pause)
  (if (equal (row row "outcome") "finding")
      (format nil "selffix found ~a (~(~a~)): ~a" (row row "fp") (row row "source") (row row "key"))
      (format nil "selffix paused: ~a, the last ~a: ~a; (selffix:selffix :on) resumes it"
              pause (row row "fp") (row row "detail"))))

(defun news-since (since)
  "What the ledger gained at or after SINCE (a universal time, NIL for ever)
that is news: each finding, and the failure that tripped the breaker while
the pause holds. A row in SINCE's own second that was said already is not
said again."
  (let* ((rows (ledger-rows))
         (from (if since (iso since) ""))
         (pause (breaker rows))
         (trip (and pause (find "failed" rows :key (lambda (r) (row r "outcome")) :test #'equal :from-end t))))
    (maphash (lambda (key at) (when (string< at from) (remhash key *said*))) *said*)
    (loop for r in rows
          for at = (row r "at")
          for key = (readable r)
          when (and (stringp at) (string<= from at)
                    (or (equal (row r "outcome") "finding") (eq r trip))
                    (not (and (string= at from) (gethash key *said*))))
            collect (progn (setf (gethash key *said*) at)
                           (news-line r pause)))))

(defun wake-source (since)
  "The cron wake's source, on the ticker thread at $0: the ledger's news since
the wake's cursor, and the next cycle started beside it. News buys the wake a
model turn, so only a finding and the failure that trips the breaker are news;
a single failure, a held pause and every gate say nothing, and the wake keeps
its quiet backoff. /selffix status shows the rest."
  (let ((idle-seconds (* 60 (setting :idle-minutes))))
    (cond (*async* (prog1 (news-since since) (start-cycle idle-seconds)))
          (t (cycle-and-notice idle-seconds)
             (news-since since)))))
