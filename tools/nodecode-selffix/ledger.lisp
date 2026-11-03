;;;; ledger.lisp --- what the loop remembers: fingerprints, outcomes, the breakers, the stop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One append-only JSON-lines file, selffix/ledger.jsonl. Every row is one
;;;; outcome: a cycle's (finding, not-reproduced, failed), an item dropped, or
;;;; the operator's stop and start. Nothing is rewritten, so the record of
;;;; what was tried, and why it ended, is the same file the gates read.
;;;;
;;;; Taken from Ouroboros's evolution loop (supervisor/evolution_lifecycle.py,
;;;; razzant/ouroboros add0ce32): it pauses after three failed cycles in a row
;;;; and an operator start resumes it; an objective that keeps coming back
;;;; without landing is capped (OBJECTIVE_REPEAT_CAP = 3); an owner stop is
;;;; stored, and nothing the loop runs can lift it.
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

(defparameter +key-rewrites+
  '(("(?:^|(?<=[\\s'\"(=]))(?:~|\\.{1,2})?/[^\\s'\"():,]+" . "<path>")
    ("\\b(?=[a-z]*[0-9])(?=[0-9]*[a-z])[a-z0-9]{6,}\\b" . "<id>")
    ("[0-9]+" . "#")
    ("\\s+" . " "))
  "Regex and replacement, in order, that make two sightings of one failure
read alike: paths, ids that mix letters and digits, then any number.")

(defun ledger-path () (selffix-path "ledger.jsonl"))

;;; --- fingerprints -----------------------------------------------------------------

(defun normalize-key (text)
  "TEXT lowercased, with paths, mixed ids and numbers replaced, on one line."
  (let ((key (string-downcase (princ-to-string (or text "")))))
    (loop for (regex . replacement) in +key-rewrites+
          do (setf key (ppcre:regex-replace-all regex key replacement)))
    (let ((key (string-trim " " key)))
      (subseq key 0 (min 400 (length key))))))

(defun fingerprint (item)
  "ITEM's stable id: 16 hex digits over its source and its normalized key."
  (let ((digest (nlk:sha256-text (format nil "~a|~a" (getf item :source) (normalize-key (getf item :key))))))
    (subseq digest (1+ (position #\: digest)) (+ 17 (position #\: digest)))))

;;; --- rows -------------------------------------------------------------------------

(defun record (outcome &key fp item detail by sha256 (at (get-universal-time)))
  "Append one OUTCOME row; keys with no value are left out. Returns OUTCOME."
  (let ((path (ledger-path)))
    (ensure-directories-exist path)
    (nlk:append-json-line
     path
     (apply #'nlk:json-object
            (loop for (key value) on (list "at" (iso at) "outcome" outcome "fp" fp
                                           "source" (and item (princ-to-string (getf item :source)))
                                           "key" (and item (nlk:one-line (princ-to-string (getf item :key)) :cap 300))
                                           "detail" (and detail (nlk:one-line (princ-to-string detail) :cap 300))
                                           "sha256" sha256 "by" by)
                  by #'cddr
                  when value append (list key value))))
    outcome))

(defun ledger-rows ()
  "Every row, oldest first; a torn last line is skipped."
  (let ((path (ledger-path)))
    (and (probe-file path) (nlk:read-json-lines path))))

(defun row (row key) (gethash key row))

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
