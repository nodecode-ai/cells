;;;; watchlist.lisp --- the markets the operator asked to be told about.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One Lisp-readable file under the home (polymarket/watchlist.sexp by
;;;; default), rewritten whole under the cell's lock after every change, so a
;;;; restart loses nothing and the file can be read -- or edited -- by anyone.
;;;; A rule alerts once when it becomes true and re-arms when it stops being
;;;; true, so checking every few minutes does not repeat itself.

(in-package #:nodecode-polymarket)

(defparameter *max-items* 40
  "How many markets a watchlist holds.")

(defun watch-path ()
  "Where the watchlist is, under the home."
  (nlk:home (setting :watchlist)))

(defun read-watchlist ()
  "The items as they stand; a file nobody can read is empty, not a refusal."
  (let ((path (watch-path)))
    (if (probe-file path)
        (let ((text (nlk:read-text path)))
          (if (zerop (length (nlk:trimmed text)))
              '()
              (let ((*read-eval* nil)
                    (*package* (find-package :keyword))
                    (*readtable* (copy-readtable nil)))
                (let ((read (ignore-errors (read-from-string text nil nil))))
                  (if (listp read)
                      (remove-if-not (lambda (item) (and (listp item) (getf item :slug))) read)
                      '())))))
        '())))

(defun write-watchlist (items)
  "Write ITEMS whole, atomically, readable only by its owner. => ITEMS."
  (let ((path (watch-path))
        (*package* (find-package :keyword))
        (*print-pretty* t)
        (*print-right-margin* 100))
    (ensure-directories-exist (pathname path))
    (nlk:write-file-atomically
     path (with-output-to-string (out) (prin1 items out) (terpri out))
     :mode #o600 :directory-mode #o700)
    items))

;;; --- the rules -------------------------------------------------------------------

(defun probability (value)
  "VALUE as a 0-1 probability: 70 and 0.7 are both seventy percent, 0 is no rule."
  (let ((parsed (and value (as-number value))))
    (cond ((null parsed) nil)
          ((zerop parsed) nil)
          (t (let ((v (if (> parsed 1) (/ parsed 100.0) parsed)))
               (unless (< 0 v 1)
                 (fail "price lines are between 1 and 99 percent, not ~a" value))
               v)))))

(defun add-rule (items market &key above below move)
  "ITEMS with MARKET watched under the given rules (a 5 pt move when none)."
  (let* ((above (probability above))
         (below (probability below))
         (move (and move (as-number move)))
         (items (remove (getf market :slug) items
                        :key (lambda (item) (getf item :slug)) :test #'equal)))
    (when (and move (not (plusp move)))
      (fail "a move trigger is a positive number of points, not ~a" move))
    (when (and (null above) (null below) (null move))
      (setf move 5.0d0))
    (when (>= (length items) *max-items*)
      (fail "the watchlist is full (~d markets); remove one first" *max-items*))
    (append items
            (list (list :slug (getf market :slug)
                        :question (getf market :question)
                        :above above :below below :move move
                        :triggered '())))))

(defun remove-rule (items ref)
  "ITEMS without the market REF names: by slug, or by words in its question. => (values ITEMS REMOVED-P)."
  (let* ((ref (string-downcase (nlk:trimmed (or ref ""))))
         (kept (remove-if (lambda (item)
                            (or (equal (string-downcase (getf item :slug)) ref)
                                (and (plusp (length ref))
                                     (cl:search ref (string-downcase (or (getf item :question) ""))))))
                          items)))
    (values kept (/= (length kept) (length items)))))

(defun describe-rule (item)
  "ITEM's rules in words."
  (let ((rules '()))
    (when (getf item :above)
      (push (format nil "alert at or above ~a" (pct (getf item :above))) rules))
    (when (getf item :below)
      (push (format nil "alert at or below ~a" (pct (getf item :below))) rules))
    (when (getf item :move)
      (push (format nil "alert on a ~d+ pt move in 24h" (round (getf item :move))) rules))
    (if rules (format nil "~{~a~^, ~}" (nreverse rules)) "no rules")))

(defun check-watchlist (fetch)
  "Evaluate every rule through FETCH (a slug => market function).
=> (values NEW-ALERTS ACTIVE ERRORS UPDATED-ITEMS); a rule alerts once and re-arms."
  (let ((items (read-watchlist))
        (new '()) (active '()) (errors '()) (updated '()))
    (dolist (item items)
      (let ((market (handler-case (funcall fetch (getf item :slug))
                      (error (condition)
                        (push (format nil "~a: ~a" (or (getf item :question) (getf item :slug)) condition)
                              errors)
                        nil))))
        (cond ((null market)
               (push (format nil "~a: market not found (it may have resolved)"
                             (or (getf item :question) (getf item :slug)))
               errors)
               (push item updated))
              (t (let* ((hits (evaluate-watch item market))
                        (before (getf item :triggered)))
                   (dolist (hit hits)
                     (let ((row (list :market market :rule (car hit) :message (cdr hit))))
                       (push row active)
                       (unless (member (car hit) before)
                         (push row new))))
                   (setf (getf item :triggered) (sort (mapcar #'car hits) #'string< :key #'symbol-name))
                   (push item updated))))))
    (let ((updated (nreverse updated)))
      (when items (write-watchlist updated))
      (values (nreverse new) (nreverse active) (nreverse errors) updated))))
