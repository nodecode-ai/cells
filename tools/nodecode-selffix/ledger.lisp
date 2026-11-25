;;;; ledger.lisp --- what the loop remembers: fingerprints, outcomes, the breakers, the stop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One append-only file of Lisp forms, selffix/ledger.sexp: every row is one
;;;; readable plist, read back with *READ-EVAL* off. A row is one outcome: a
;;;; cycle's (finding, not-reproduced, failed), an item dropped, or the
;;;; operator's stop and start. Nothing is rewritten, so the record of what was
;;;; tried, and why it ended, is the same file the gates read.
;;;;
;;;; Taken from Ouroboros's evolution loop (supervisor/evolution_lifecycle.py,
;;;; razzant/ouroboros add0ce32): it pauses after three failed cycles in a row
;;;; and an operator start resumes it; an objective that keeps coming back
;;;; without landing is capped (OBJECTIVE_REPEAT_CAP = 3); an owner stop is
;;;; stored, and nothing the loop runs can lift it.
;;;;
;;;; Two sightings of one failure share a fingerprint: a captured case's exact
;;;; key, its (condition-type . function) printed with every symbol's package;
;;;; a note's key, which is text, through the backlog's NORMALIZE-TEXT.
;;;;
;;;; The backlog ranks and counts; this file only filters: what was dropped
;;;; stays dropped, what already has a finding is not found again, and an item
;;;; tried *ATTEMPT-CAP* times without a finding is dropped.

(in-package #:nodecode-selffix)

(defparameter *failure-breaker* 3
  "Failed cycles in a row that pause the loop until the operator starts it again.")

(defparameter *attempt-cap* 3
  "Cycles one item gets without a finding before it is dropped for good.")

(defparameter +cycle-outcomes+ '("finding" "not-reproduced" "failed")
  "The outcomes a cycle that picked an item ends in.")

(defun ledger-path () (selffix-path "ledger.sexp"))

(defun readable (object)
  "OBJECT printed so it reads back the same in any package."
  (with-standard-io-syntax
    (let ((*package* (find-package '#:keyword)))
      (prin1-to-string object))))

;;; --- fingerprints -----------------------------------------------------------------

(defun fingerprint (item)
  "ITEM's stable id: 16 hex digits over its source and its key."
  (let* ((key (getf item :key))
         (digest (nlk:sha256-text (format nil "~a|~a" (getf item :source)
                                          (if (stringp key) (normalize-text key :cap 400) (readable key))))))
    (subseq digest (1+ (position #\: digest)) (+ 17 (position #\: digest)))))

;;; --- rows -------------------------------------------------------------------------

(defun record (outcome &key fp item detail by commit (at (get-universal-time)))
  "Append one OUTCOME row; keys with no value are left out. Returns OUTCOME."
  (let ((path (ledger-path))
        (row (loop for (key value) on (list :at (iso at) :outcome outcome :fp fp
                                              :source (and item (princ-to-string (getf item :source)))
                                              :key (and item (nlk:one-line (princ-to-string (getf item :key)) :cap 300))
                                              :detail (and detail (nlk:one-line (princ-to-string detail) :cap 300))
                                              :commit commit :by by)
                   by #'cddr when value append (list key value))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create)
      (write-line (readable row) out))
    outcome))

(defun ledger-rows ()
  "Every row, oldest first; a torn or unreadable line is skipped."
  (let ((path (ledger-path)))
    (and (probe-file path)
         (with-open-file (in path)
           (loop for line = (read-line in nil) while line
                 for row = (ignore-errors
                            (with-standard-io-syntax
                              (let ((*read-eval* nil) (*package* (find-package '#:keyword)))
                                (read-from-string line))))
                 when (and (consp row) (evenp (length row))) collect row)))))

(defun row (row key)
  "ROW's value under KEY, a string naming the keyword."
  (getf row (intern (string-upcase key) '#:keyword)))

;;; --- the stop ---------------------------------------------------------------------

(defun stopped (rows)
  "The operator's standing stop row, or NIL: the newest of stopped and started."
  (let ((last (find-if (lambda (r) (member (row r "outcome") '("stopped" "started") :test #'equal))
                       rows :from-end t)))
    (and last (equal (row last "outcome") "stopped") last)))

;;; --- the breakers -----------------------------------------------------------------

(defun failure-streak (rows)
  "Failed cycles in a row, newest back, up to the last operator start."
  (loop for r in (reverse rows)
        for outcome = (row r "outcome")
        until (or (equal outcome "started")
                  (member outcome '("finding" "not-reproduced") :test #'equal))
        count (equal outcome "failed")))

(defun breaker (rows)
  "Why the loop is paused, or NIL."
  (let ((streak (failure-streak rows)))
    (and (>= streak *failure-breaker*)
         (format nil "~d failed cycles in a row" streak))))

;;; --- which items may run ----------------------------------------------------------

(defun history (rows)
  "A table from fingerprint to (ATTEMPTS FOUND DROPPED)."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (r rows table)
      (let ((fp (row r "fp")))
        (when fp
          (let ((entry (or (gethash fp table) (setf (gethash fp table) (list 0 nil nil)))))
            (cond ((member (row r "outcome") '("not-reproduced" "failed") :test #'equal)
                   (incf (first entry)))
                  ((equal (row r "outcome") "finding") (setf (second entry) t))
                  ((equal (row r "outcome") "dropped") (setf (third entry) t)))))))))

(defun eligible (items rows)
  "(values KEPT NEWLY-DROPPED): ITEMS in their own order without those
dropped or already found, and those that just used up *ATTEMPT-CAP*."
  (let ((history (history rows))
        (kept '())
        (dropped '()))
    (dolist (item items)
      (destructuring-bind (&optional (attempts 0) found gone) (gethash (fingerprint item) history)
        (cond ((or found gone))
              ((>= attempts *attempt-cap*) (push item dropped))
              (t (push item kept)))))
    (values (nreverse kept) (nreverse dropped))))

(defun findings (rows)
  "The finding rows, one a fingerprint, newest last."
  (let ((seen (make-hash-table :test #'equal)))
    (remove-if-not (lambda (r)
                     (and (equal (row r "outcome") "finding")
                          (not (gethash (row r "fp") seen))
                          (setf (gethash (row r "fp") seen) t)))
                   rows)))
