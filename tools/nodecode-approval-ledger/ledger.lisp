;;;; ledger.lisp --- the file, the row, and the report the command prints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One JSON line per decision in the organism's home, and the summaries
;;;; /approval-log prints from it. Nothing here knows what a cell is: the file
;;;; arrives as a path, so the tests write it under a scratch home.

(in-package #:nodecode-approval-ledger)

(defparameter +file-name+ "approval-ledger.jsonl"
  "The ledger's name in the home, unless the section names another file.")

(defparameter +approved-words+ '("allow" "approve" "accept" "yes" "ok" "once" "always" "session")
  "An answer carrying one of these, and no denied word, is an approval.")

(defparameter +denied-words+ '("deny" "denied" "reject" "refuse" "no" "block" "cancel")
  "An answer carrying one of these is a denial: the call did not run.")

(defparameter +unanswered-whys+ '(:timeout :nobody :off)
  "What the question seam answers for a prompt nobody answered.")

;;; --- one row -----------------------------------------------------------------

(defun clip (value limit)
  "VALUE as a string of at most LIMIT characters."
  (let ((text (and value (string value))))
    (if (and text (> (length text) limit)) (subseq text 0 limit) (or text ""))))

(defun classify (answer why)
  "Approved, denied, unanswered or other, from the ANSWER and WHY the question
seam answered with. A prompt nobody answered is unanswered: the call did not run."
  (cond ((or (null answer) (member why +unanswered-whys+ :test #'eq)) "unanswered")
        (t (let ((text (string-downcase (string answer))))
             (cond ((some (lambda (word) (search word text)) +denied-words+) "denied")
                   ((some (lambda (word) (search word text)) +approved-words+) "approved")
                   (t "other"))))))

(defun build-row (rule answer why wait &key session-id surface (ts (get-universal-time)))
  "One ledger row: the rule that asked, where, the answer, how it ended and how
long the answer took. The question's detail is left out -- it carries a command."
  (let* ((outcome (classify answer why))
         (answered (and (string= outcome "approved") (numberp wait)))
         (answered (or answered (and (string= outcome "denied") (numberp wait)))))
    (nlk:make-json-object
     "ts" ts
     "rule" (clip (or rule "question") 80)
     "surface" (clip (or surface "operator") 40)
     "session_id" (clip session-id 200)
     "answer" (if (and answer (string/= "" (string answer)))
                  (clip answer 40)
                  (string-downcase (string (or why :unanswered))))
     "outcome" outcome
     ;; to a thousandth of a second: ROUND answers the quotient, so scale back
     "wait_s" (and answered (* 1d-3 (round (float wait 1d0) 1d-3))))))

;;; --- the file ----------------------------------------------------------------

(defun home-file ()
  "The ledger's path: the section's `file', else the name kept in this home."
  (let ((named (setting :file)))
    (nlk:home (if (and (stringp named) (plusp (length named))) named +file-name+))))

(defun row-ts (row)
  (or (nlk:json-value row :number "ts") 0))

(defun read-rows ()
  "Every parseable row, oldest first. A torn or foreign line is skipped."
  (stable-sort (nlk:read-json-lines (home-file)) #'< :key #'row-ts))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (if in (file-length in) 0)))

(defun write-rows (path rows)
  "ROWS as the whole of PATH, through a neighbour renamed over it."
  (let ((tmp (concatenate 'string (namestring path) ".tmp")))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (dolist (row rows) (write-line (nlk:encode-json-object row) out)))
    (rename-file tmp path)))

(defun trim (path keep)
  "Keep the newest KEEP rows of PATH, and answer how many were dropped."
  (let* ((all (stable-sort (nlk:read-json-lines path) #'< :key #'row-ts))
         (kept (last all (min keep (length all)))))
    (write-rows path kept)
    (- (length all) (length kept))))

(defun trim-when-large ()
  "Past `max_bytes' the ledger keeps its newest `keep_rows' rows, so a busy
install never grows without bound."
  (let ((path (home-file)))
    (when (and (plusp (file-size path)) (> (file-size path) (setting :max-bytes)))
      (trim path (setting :keep-rows)))))

(defun append-row (row)
  (nlk:append-json-line (home-file) row)
  (trim-when-large)
  row)

;;; --- windows -----------------------------------------------------------------

(defun parse-span (word)
  "Seconds for a window word such as 12h or 7d; NIL when WORD is not one."
  (let ((number (and (stringp word) (> (length word) 1) (subseq word 0 (1- (length word)))))
        (unit (and (stringp word) (plusp (length word)) (char word (1- (length word))))))
    (when (and number (plusp (length number)) (<= (length number) 4)
               (every #'digit-char-p number) (member unit '(#\h #\d)))
      (let ((count (parse-integer number)))
        (and (plusp count)
             (* count (if (char= unit #\h) 3600 86400)))))))

(defun parse-count (word)
  "A row count for `recent', 1 to 100; NIL when WORD is not one."
  (when (and (stringp word) (plusp (length word)) (<= (length word) 3)
             (every #'digit-char-p word))
    (let ((count (parse-integer word)))
      (and (<= 1 count 100) count))))

(defun span-title (word)
  (let ((count (parse-integer word :end (1- (length word))))
        (unit (if (char= (char word (1- (length word))) #\h) "hour" "day")))
    (format nil "last ~d ~a~p" count unit count)))

(defun last-session-id (rows)
  "The session of the newest row that carries one."
  (let ((found ""))
    (dolist (row rows found)
      (let ((id (nlk:json-value row :string "session_id")))
        (when (and id (plusp (length id))) (setf found id))))))

(defun select (rows window span now)
  "ROWS inside WINDOW: everything, one session's, or the SPAN seconds before NOW."
  (cond ((string= window "all") (copy-list rows))
        ((string= window "session")
         (let ((id (last-session-id rows)))
           (if (zerop (length id))
               '()
               (remove-if-not (lambda (row)
                                (equal id (nlk:json-value row :string "session_id")))
                              rows))))
        (t (remove-if (lambda (row) (< (row-ts row) (- now span))) rows))))

;;; --- the report --------------------------------------------------------------

(defun median (values)
  (when values
    (let* ((sorted (sort (copy-list values) #'<))
           (length (length sorted))
           (middle (floor length 2)))
      (if (oddp length)
          (nth middle sorted)
          (/ (+ (nth (1- middle) sorted) (nth middle sorted)) 2)))))

(defun human-wait (row)
  "How long a person took to answer ROW, or NIL: an unanswered prompt is nobody's."
  (let ((wait (nlk:json-value row :number "wait_s"))
        (outcome (nlk:json-value row :string "outcome")))
    (and wait (member outcome '("approved" "denied") :test #'string=) wait)))

(defun duration (seconds)
  (cond ((null seconds) "-")
        ((< seconds 10) (format nil "~,1fs" seconds))
        ((< seconds 60) (format nil "~ds" (round seconds)))
        ((< seconds 3600) (format nil "~dm~2,'0ds" (floor seconds 60) (mod (round seconds) 60)))
        (t (format nil "~dh~2,'0dm" (floor seconds 3600) (floor (mod seconds 3600) 60)))))

(defun age (seconds)
  (let ((seconds (max 0 seconds)))
    (cond ((>= seconds 86400) (format nil "~dd ago" (floor seconds 86400)))
          ((>= seconds 3600) (format nil "~dh ago" (floor seconds 3600)))
          ((>= seconds 60) (format nil "~dm ago" (floor seconds 60)))
          (t (format nil "~ds ago" (floor seconds))))))

(defun local-day (ts)
  "The local calendar day of a row's timestamp, as YYYY-MM-DD."
  (handler-case
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (round ts))
        (declare (ignore second minute hour))
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month day))
    (error () "unknown")))

(defun table (header body)
  "HEADER and BODY as aligned lines: the first column left, the rest right."
  (let* ((widths (loop for index from 0 below (length header)
                        collect (loop for row in (cons header body)
                                      maximize (length (nth index row)))))
         (lines '()))
    (dolist (row (cons header body) (nreverse lines))
      (let ((cells '()))
        (loop for cell in row for index from 0
              for width in widths
              do (push (if (zerop index)
                           (format nil "~va" width cell)
                           (format nil "~v@a" width cell))
                       cells))
        (push (string-right-trim " " (format nil "~{~a~^  ~}" (nreverse cells))) lines)))))

(defun group-key (row by)
  (if (string= by "day")
      (local-day (row-ts row))
      (let ((value (nlk:json-value row :string by)))
        (if (and value (plusp (length value))) value "unknown"))))

(defun cells-for (name items)
  (let ((outcomes (mapcar (lambda (row) (or (nlk:json-value row :string "outcome") "")) items))
        (waits (remove nil (mapcar #'human-wait items))))
    (list name (princ-to-string (length items))
          (princ-to-string (count "approved" outcomes :test #'string=))
          (princ-to-string (count "denied" outcomes :test #'string=))
          (princ-to-string (count "unanswered" outcomes :test #'string=))
          (duration (median waits)))))

(defun summarize (rows &key title (by "rule"))
  "Per-BY approval questions: how many, how each ended, and how long a person took.
With BY \"day\" the newest day leads; otherwise the largest group does."
  (if (null rows)
      (format nil "Approvals, ~a: none recorded." title)
      (let ((groups (make-hash-table :test 'equal))
            (order '()))
        (dolist (row rows)
          (let ((key (group-key row by)))
            (unless (gethash key groups) (push key order))
            (push row (gethash key groups))))
        (let* ((entries (loop for key in order collect (cons key (reverse (gethash key groups)))))
               (ordered (if (string= by "day")
                            (let ((known (remove-if (lambda (entry) (string= "unknown" (car entry))) entries))
                                  (unknown (remove-if-not (lambda (entry) (string= "unknown" (car entry))) entries)))
                              (append (sort known #'string> :key #'car) unknown))
                            (sort entries (lambda (a b)
                                            (if (= (length (cdr a)) (length (cdr b)))
                                                (string< (car a) (car b))
                                                (> (length (cdr a)) (length (cdr b))))))))
               (body (mapcar (lambda (entry) (cells-for (car entry) (cdr entry))) ordered)))
          (when (> (length ordered) 1)
            (setf body (append body (list (cells-for "total" rows)))))
          (format nil "Approvals, ~a:~%~%~{~a~%~}~%~a"
                  title
                  (table (list by "asked" "approved" "denied" "unanswered" "median wait") body)
                  "Unanswered: timed out, withdrawn or nobody to ask; the call did not run.")))))

(defun recent-lines (rows &key title (limit 10) (now (get-universal-time)))
  "The newest LIMIT decisions of ROWS, newest first, one line each."
  (if (null rows)
      (format nil "Approvals, ~a: none recorded." title)
      (let* ((newest (last rows (min limit (length rows))))
             (body (loop for row in (reverse newest)
                         collect (list (age (max 0 (- now (row-ts row))))
                                       (or (nlk:json-value row :string "rule") "unknown")
                                       (or (nlk:json-value row :string "surface") "unknown")
                                       (or (nlk:json-value row :string "answer") "unknown")
                                       (duration (nlk:json-value row :number "wait_s"))))))
        (format nil "Approvals, ~a, newest ~d of ~d:~%~%~{~a~%~}"
                title (length body) (length rows)
                (table (list "when" "rule" "surface" "answer" "wait") body)))))
