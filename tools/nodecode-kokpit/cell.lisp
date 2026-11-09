;;;; cell.lisp --- Kokpit: your own Markdown vault, and the day around it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of sahindemirbas's Kokpit Hermes plugin (see NOTICE): one plain
;;;; Markdown vault, captured into from a chat, read back as threads, a short
;;;; brief and a night pass whose every proposal quotes the line it came from.
;;;;
;;;; The harness seams are Nodecode's: the section is the config, the verbs
;;;; are NLE:REGISTER-TOOL registrations named as the original names the same
;;;; work (kokpit_brief, kokpit_brain_search, kokpit_threads, kokpit_capture,
;;;; kokpit_night), and a /kokpit command replaces the original's CLI entry
;;;; points.
;;;;
;;;; What is kept: the vault formats (Threads.md with its Open: items and
;;;; owner marks, To-Do.md with its Open section and due: dates, Last-Session.md
;;;; cards, inbox notes, daily journal lines), append-only writes (a note is a
;;;; new file, a task is one line in the Open section, a journal line is added
;;;; at the end of the day's file), a session card and one receipt when a
;;;; session says it produced something, and a night pass whose proposals each
;;;; carry a verbatim quote from a vault file, accepted only by appending.
;;;;
;;;; What is left out: the Kokpit app's SQLite database (calendar, weather,
;;;; events, jobs, AI usage, job health, training logs, server status) from
;;;; which the original reads read-only, and the optional host-model night
;;;; pass. Both are named in NOTICE and README.

(defpackage #:nodecode-kokpit
  (:use #:cl)
  (:export #:kokpit-error #:kokpit-error-detail))

(in-package #:nodecode-kokpit)

;;; --- what the folder is ------------------------------------------------------
;;; DEFINE-PERIPHERAL gives KOKPIT-ERROR and FAIL, *KOKPIT*, RUNNING-SETTINGS
;;; and the refusal every verb answers while stopped.

(nlk:define-peripheral kokpit :not-running t)

(defvar *captures* (make-hash-table :test #'equal)
  "Per session, the vault files this process wrote for it: a receipt points at them.")

(defvar *session* nil
  "The session a /kokpit or a tool call is running as, when the harness knows one.")

(defparameter +empty-values+
  '("yok" "none" "nothing" "nobody" "-" "n/a" "na" "nessuno" "niente" "nulla" "boş")
  "An open item whose text is one of these owes nothing: the original's rule.")

(defparameter +wait-words+
  '("approve" "approval" "decide" "decision" "waiting on you" "onay" "karar")
  "Words that put an unowned open item on you, as the original reads them.")

(defparameter +active-headings+ '("## Active Threads" "## Active threads" "## Aktif")
  "Where a Threads.md's live threads start.")

(defparameter +closed-headings+ '("## Closed Threads" "## Closed threads" "## Kapanan")
  "Where they end.")

(defparameter +open-markers+ '("Open:" "Açık:"))
(defparameter +status-markers+ '("Where we left off" "Nerede kaldık"))
(defparameter +source-markers+ '("Source:" "Kaynak:"))
(defparameter +earlier-headings+ '("## Earlier sessions" "## Önceki oturumlar"))

(defparameter +skip-dirs+ '("receipts" "archive" ".obsidian" ".git" ".trash")
  "Folders a search does not walk, as the original skips them.")

(defparameter +note-cap+ 2000)
(defparameter +task-cap+ 300)
(defparameter +walk-cap+ 5000)

;;; --- small strings -----------------------------------------------------------
;;; String work only: no dependency beyond what the serving image carries.

(defun blankp-char (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun prefix-p (prefix string)
  (let ((n (length prefix)))
    (and (<= n (length string)) (string-equal prefix string :end2 n))))

(defun first-marker-p (line markers)
  "The first of MARKERS LINE begins with, or NIL."
  (find-if (lambda (marker) (prefix-p marker (trim line))) markers))

(defun cut-at (text markers)
  "TEXT split at the earliest of MARKERS, case-insensitively: (values BEFORE AFTER), AFTER past the marker."
  (let ((best nil))
    (dolist (marker markers)
      (let ((at (search marker text :test #'char-equal)))
        (when (and at (or (null best) (< at (car best))))
          (setf best (cons at marker)))))
    (if best
        (values (subseq text 0 (car best)) (subseq text (+ (car best) (length (cdr best)))))
        (values text ""))))

(defun split-on (string delimiter)
  "STRING's parts between each DELIMITER."
  (loop with start = 0
        for at = (position delimiter string :start start)
        collect (subseq string start at)
        while at
        do (setf start (1+ at))))

(defun unwikilink (text)
  "[[target|label]] read as LABEL, [[folder/target]] as TARGET."
  (let ((out (make-string-output-stream))
        (start 0)
        (done nil))
    (loop while (not done)
          do (let ((at (search "[[" text :start2 start)))
               (if (null at)
                   (progn (write-string (subseq text start) out) (setf done t))
                   (let ((close (search "]]" text :start2 (+ at 2))))
                     (if (null close)
                         (progn (write-string (subseq text start) out) (setf done t))
                         (let* ((inner (subseq text (+ at 2) close))
                                (bar (position #\| inner))
                                (label (if bar
                                           (subseq inner (1+ bar))
                                           (car (last (split-on inner #\/))))))
                           (write-string (subseq text start at) out)
                           (write-string label out)
                           (setf start (+ close 2))))))))
    (get-output-stream-string out)))

(defun clean (text &optional limit)
  "TEXT with wiki links read out, its blanks collapsed, cut to LIMIT characters."
  (let ((one (nlk:one-line (unwikilink (or text "")))))
    (if (and limit (> (length one) limit))
        (format nil "~a..." (string-right-trim '(#\Space) (subseq one 0 (max 1 (- limit 3)))))
        one)))

(defun remove-control (text)
  "TEXT without the control bytes a note must not carry; newlines and tabs stay."
  (coerce (loop for c across (or text "")
                unless (and (< (char-code c) 32) (not (member c '(#\Newline #\Tab))))
                  collect c)
          'string))

(defun iso-date-at (string index)
  "The ten characters at INDEX as an ISO date, or NIL."
  (when (and (<= (+ index 10) (length string))
             (loop for i from index below (+ index 10)
                   for c = (char string i)
                   always (if (member i (list (+ index 4) (+ index 7)))
                              (char= c #\-)
                              (digit-char-p c))))
    (subseq string index (+ index 10))))

(defun first-date (string)
  "The first ISO date in STRING, or NIL."
  (loop with at = (search "20" string)
        while at
        do (let ((date (iso-date-at string at)))
             (when date (return date)))
           (setf at (search "20" string :start2 (1+ at)))))

(defun parse-iso (string)
  "(values YEAR MONTH DAY) for an ISO date, or NIL."
  (when (and string (>= (length string) 10))
    (let ((y (parse-integer string :start 0 :end 4 :junk-allowed t))
          (m (parse-integer string :start 5 :end 7 :junk-allowed t))
          (d (parse-integer string :start 8 :end 10 :junk-allowed t)))
      (when (and y m d) (values y m d)))))

(defun today-iso ()
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d)))

(defun now-stamp ()
  "The minute a capture is made in, as a file name reads it."
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s))
    (format nil "~4,'0d-~2,'0d-~2,'0d-~2,'0d~2,'0d" y mo d h m)))

(defun now-human ()
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))

(defun day-number (iso)
  "An ISO date as a day count, noon UTC so no daylight shift moves it; NIL when unreadable."
  (multiple-value-bind (y m d) (parse-iso iso)
    (when y (encode-universal-time 0 0 12 d m y 0))))

(defun days-from-today (iso)
  "How many days ISO lies ahead of today (negative: past), or NIL."
  (let ((then (day-number iso))
        (now (day-number (today-iso))))
    (when (and then now) (round (- then now) 86400))))

;;; --- the vault on disk -------------------------------------------------------

(defun vault-directory ()
  "The vault's folder, or NIL when the section names none."
  (let ((vault (setting :vault)))
    (and vault (uiop:ensure-directory-pathname vault))))

(defun setting-name (key default)
  "The section's string for KEY, or DEFAULT when it is empty."
  (let ((value (setting key)))
    (if (and (stringp value) (plusp (length (trim value)))) (trim value) default)))

(defun absolute-name-p (name)
  (and (plusp (length name)) (member (char name 0) '(#\/ #\~))))

(defun vault-relative (path)
  (handler-case (uiop:native-namestring (uiop:enough-pathname path (vault-directory)))
    (error () (uiop:native-namestring path))))

(defun vault-file (key default)
  "The file KEY names: a path, else DEFAULT at the vault root, else the one
match of that name below the vault. Two matches are a refusal naming them, as
the original refuses an ambiguous layout."
  (let* ((name (setting-name key default))
         (vault (vault-directory)))
    (if (absolute-name-p name)
        (pathname (nlk:expand-home name))
        (let ((root (merge-pathnames name vault)))
          (if (probe-file root)
              root
              (let* ((base (file-namestring name))
                     (found (ignore-errors (directory (merge-pathnames (format nil "**/~a" base) vault)))))
                (cond ((null found) root)
                      ((null (rest found)) (first found))
                      (t (fail "~a is below the vault ~d times (~{~a~^, ~}); set ~(~a~) in the kokpit section to the one you mean"
                               base (length found)
                               (mapcar #'vault-relative (subseq found 0 (min 4 (length found))))
                               key)))))))))

(defun vault-subdirectory (key default)
  "A folder inside the vault for KEY, made when it is missing."
  (let* ((name (setting-name key default))
         (dir (if (absolute-name-p name)
                  (uiop:ensure-directory-pathname (nlk:expand-home name))
                  (uiop:ensure-directory-pathname (merge-pathnames (format nil "~a/" name) (vault-directory))))))
    (ensure-directories-exist dir)
    dir))

(defun threads-file () (vault-file :threads-file "Threads.md"))
(defun todo-file () (vault-file :todo-file "To-Do.md"))
(defun last-session-file () (vault-file :last-session-file "Last-Session.md"))
(defun inbox-directory () (vault-subdirectory :inbox-dir "inbox"))
(defun receipts-directory () (vault-subdirectory :receipts-dir "receipts"))

(defun file-text (path)
  "PATH as text, or the empty string when it is not there."
  (or (and path (probe-file path) (nlk:read-text path)) ""))

(defun write-file-text (path text)
  (ensure-directories-exist path)
  (nlk:write-file-atomically path text)
  path)

(defun write-new-file (directory base type text)
  "TEXT written to a file of BASE and TYPE that did not exist: the first free
name, so a second capture of the same words never overwrites the first.
=> the PATH written."
  (loop for suffix from 0
        for name = (if (zerop suffix) base (format nil "~a-~d" base suffix))
        for path = (merge-pathnames (format nil "~a~@[.~a~]" name type) directory)
        do (let ((stream (ignore-errors
                           (open path :direction :output :if-exists nil
                                      :if-does-not-exist :create :external-format :utf-8))))
             (when stream
               (unwind-protect (write-string text stream) (close stream))
               (return path)))
           (when (> suffix 200)
             (fail "cannot name a new file under ~a" (namestring directory)))))

;;; --- Threads.md --------------------------------------------------------------

(defun open-item (item line)
  "One item of an Open: list as a plist, or NIL when it owes nothing."
  (let ((item (trim item)))
    (when (>= (length item) 3)
      (multiple-value-bind (owner note rest) (split-owner item)
        (let* ((text (clean (trim (string-right-trim "." rest)) 260))
               (bare (string-downcase (string-right-trim "." text))))
          (unless (or (< (length text) 3) (member bare +empty-values+ :test #'string=))
            (list :text text
                  :owner (or owner "")
                  :on-you (or (and owner (string-equal owner "you"))
                              (and (null owner)
                                   (some (lambda (word) (search word item :test #'char-equal))
                                         +wait-words+))
                              nil)
                  :due (first-date rest)
                  :note (clean note 40)
                  :raw item
                  :line line)))))))

(defun split-owner (item)
  "ITEM's owner mark read off its front -- 'on you:', 'Sende:', 'Bende (30
Ekim):', 'Başkası:' -- => (values OWNER NOTE REST); OWNER is NIL when the item
carries none, and REST is then ITEM whole."
  (labels ((after-colon (from)
             (let ((colon (position #\: item :start from)))
               (and colon (<= (- colon from) 60) colon)))
           (note-before (from to)
             (let* ((open (position #\( item :start from))
                    (close (and open (position #\) item :start open))))
               (when (and open (< open to) close (< close to))
                 (subseq item (1+ open) close))))
           (finish (owner from)
             (let ((colon (after-colon from)))
               (when colon
                 (values owner (or (note-before from colon) "") (subseq item (1+ colon)))))))
    (let ((lower (string-downcase item)))
      (cond ((prefix-p "on you" lower) (finish "you" 6))
            ((prefix-p "on me" lower) (finish "me" 5))
            ((prefix-p "on " lower)
             (let* ((start 3)
                    (stop (or (position-if (lambda (c) (member c '(#\: #\()))
                                           lower :start start)
                              (length lower)))
                    (name (trim (subseq lower start stop))))
               (if (plusp (length name)) (finish name start) (values nil "" item))))
            ((prefix-p "sende" lower) (finish "you" 5))
            ((prefix-p "bende" lower) (finish "me" 5))
            ((prefix-p "başkası" lower) (finish "someone" 8))
            (t (values nil "" item))))))

(defun parse-threads (text)
  "TEXT's active threads, each (:title :date :status :open-line :open-raw :opens)."
  (let* ((lines (nlk:lines text))
         (start (or (position-if (lambda (line) (first-marker-p line +active-headings+)) lines) 0))
         (start (if (first-marker-p (nth start lines) +active-headings+) (1+ start) start))
         (end (or (position-if (lambda (line) (first-marker-p line +closed-headings+)) lines :start start)
                  (length lines)))
         (blocks '())
         (current nil))
    (loop for index from start below end
          for line = (nth index lines)
          do (if (prefix-p "### " (trim line))
                 (progn (when current (push (nreverse current) blocks))
                        (setf current (list (list :heading (string-trim " #" line) :line (1+ index)))))
                 (when current (push (list line (1+ index)) current))))
    (when current (push (nreverse current) blocks))
    (loop for block in (nreverse blocks)
          for heading = (first block)
          for body = (rest block)
          for body-text = (format nil "~{~a~^~%~}" (mapcar #'first body))
          for open-line = (find-if (lambda (pair) (first-marker-p (first pair) +open-markers+)) body)
          collect (let* ((before-open (nth-value 0 (cut-at body-text +open-markers+)))
                         (after-open (nth-value 1 (cut-at body-text +open-markers+)))
                         (open-text (nth-value 0 (cut-at after-open +source-markers+)))
                         (status (nth-value 0 (cut-at before-open +source-markers+))))
                    (list :title (clean (getf heading :heading))
                          :date (first-date (format nil "~a ~a" (getf heading :heading) body-text))
                          :status (clean (reduce (lambda (s marker) (string-replace-all s marker ""))
                                                 +status-markers+ :initial-value status)
                                         520)
                          :open-line (and open-line (second open-line))
                          :open-raw (and open-line (clean (first open-line) 200))
                          :opens (loop for line in (split-on open-text #\Newline)
                                       nconc (loop for item in (split-on line #\;)
                                                   for parsed = (open-item item (and open-line (second open-line)))
                                                   when parsed collect parsed)))))))

(defun string-replace-all (text marker replacement)
  "TEXT with the first occurrence of MARKER taken out (case-insensitively); the original removes one."
  (let ((at (search marker text :test #'char-equal)))
    (if at
        (concatenate 'string (subseq text 0 at) replacement (subseq text (+ at (length marker))))
        text)))

(defun thread-open-items (thread)
  (remove-duplicates (getf thread :opens)
                     :test (lambda (a b) (string-equal (getf a :text) (getf b :text)))))

(defun thread-age (thread)
  "Days since the thread's own date, or NIL when it carries none."
  (let ((date (getf thread :date)))
    (and date (- (days-from-today date)))))

;;; --- To-Do.md ---------------------------------------------------------------

(defun open-heading-line-p (line)
  (when (prefix-p "## " (trim line))
    (let ((title (string-downcase (string-trim " #" (trim line)))))
      (or (prefix-p "open" title) (prefix-p "açık" title) (prefix-p "acik" title)
          (prefix-p "to do" title) (prefix-p "todo" title)))))

(defun done-heading-line-p (line)
  (when (prefix-p "## " (trim line))
    (let ((title (string-downcase (string-trim " #" (trim line)))))
      (or (prefix-p "done" title) (prefix-p "bitti" title) (prefix-p "tamamlan" title)
          (prefix-p "completed" title) (prefix-p "fatto" title) (prefix-p "fatti" title)))))

(defun task-line-text (line)
  "LINE's task text when it is an unchecked task, else NIL."
  (let ((text (string-left-trim '(#\Space #\Tab) line)))
    (when (and (> (length text) 5)
               (member (char text 0) '(#\- #\*))
               (member (char text 1) '(#\Space #\Tab))
               (char= #\[ (char text 2))
               (member (char text 3) '(#\Space #\x #\X))
               (char= #\] (char text 4))
               (not (char-equal (char text 3) #\x)))
      (string-left-trim '(#\Space #\Tab) (subseq text 5)))))

(defun due-of (text)
  "The due: date in TEXT: (values TEXT-WITHOUT-DUE DUE)."
  (let ((at (search "due:" text :test #'char-equal)))
    (if at
        (let ((after (string-left-trim '(#\Space #\Tab) (subseq text (+ at 4)))))
          (if (iso-date-at after 0)
              (values (string-trim " ()-" (concatenate 'string (subseq text 0 at) (subseq after 10)))
                      (iso-date-at after 0))
              (values text "")))
        (values text ""))))

(defun parse-todo (text)
  "TEXT's unchecked tasks, each (:text :due :days-left :overdue :line :raw)."
  (let* ((lines (nlk:lines text))
         (section '()))
    (if (position-if #'open-heading-line-p lines)
        (let* ((start (position-if #'open-heading-line-p lines))
               (end (or (position-if (lambda (line) (prefix-p "## " (trim line)))
                                     lines :start (1+ start))
                        (length lines))))
          (setf section (loop for index from (1+ start) below end
                              collect (cons (nth index lines) (1+ index)))))
        (let ((on t))
          (loop for index from 0 below (length lines)
                for line = (nth index lines)
                do (cond ((prefix-p "## " (trim line)) (setf on (not (done-heading-line-p line))))
                         (on (push (cons line (1+ index)) section))))
          (setf section (nreverse section))))
    (let ((tasks '()))
      (dolist (pair section (nreverse tasks))
        (let ((raw (task-line-text (first pair))))
          (when raw
            (multiple-value-bind (body due) (due-of raw)
              (let* ((days (and due (days-from-today due)))
                     (task (list :text (clean (substitute #\Space #\( (remove-all body "()")) 300)
                                 :due due :days-left days
                                 :overdue (and days (minusp days))
                                 :line (cdr pair)
                                 :raw (clean (first pair) 300))))
                (push task tasks)))))))))

(defun remove-all (text needle)
  "TEXT with every NEEDLE taken out (the empty-parenthesis dregs of a due: mark)."
  (let ((out (make-string-output-stream))
        (start 0))
    (loop for at = (search needle text :start2 start)
          while at
          do (write-string (subseq text start at) out)
             (setf start (+ at (length needle))))
    (write-string (subseq text start) out)
    (get-output-stream-string out)))

(defun upcoming (threads tasks)
  "Due dates in the next four months, soonest first."
  (let ((hits '()))
    (dolist (task tasks)
      (when (getf task :due)
        (push (list :date (getf task :due) :text (getf task :text) :from "To-Do" :days-left (getf task :days-left)) hits)))
    (dolist (thread threads)
      (dolist (item (thread-open-items thread))
        (when (getf item :due)
          (push (list :date (getf item :due) :text (getf item :text) :from (getf thread :title)
                      :days-left (days-from-today (getf item :due)))
                hits))))
    (sort (remove-if-not (lambda (hit) (let ((days (getf hit :days-left)))
                                         (and days (<= -1 days 120))))
                         hits)
          #'string< :key (lambda (hit) (getf hit :date)))))

;;; --- Last-Session.md ---------------------------------------------------------

(defun split-sections (lines)
  "LINES as (values PREAMBLE BLOCKS), each block starting with its '## ' line."
  (let ((preamble '()) (blocks '()) (current nil))
    (dolist (line lines)
      (if (prefix-p "## " line)
          (progn (when current (push (nreverse current) blocks))
                 (setf current (list line)))
          (if current (push line current) (push line preamble))))
    (when current (push (nreverse current) blocks))
    (values (nreverse preamble) (nreverse blocks))))

(defun card-session (block)
  "A session card's session token: its heading's third '·' part, or NIL."
  (let* ((head (first block))
         (parts (mapcar #'trim (split-on head #\·))))
    (third parts)))

(defun parse-sessions (text &optional (limit 8))
  "TEXT's session cards, newest first: (:at :title :open)."
  (multiple-value-bind (preamble blocks) (split-sections (nlk:lines text))
    (declare (ignore preamble))
    (let ((cards '()))
      (dolist (block blocks)
        (let* ((parts (mapcar #'trim (split-on (first block) #\·)))
               (opens (loop for line in (rest block)
                            for item = (card-open-line line)
                            when item collect item)))
          (when (>= (length parts) 2)
            (push (list :at (first parts) :title (clean (second parts) 80)
                        :session (or (third parts) "") :open (subseq opens 0 (min 3 (length opens))))
                  cards))))
      (subseq (nreverse cards) 0 (min limit (length cards))))))

(defun card-open-line (line)
  "The text of a '- Open: ...' or '- Next: ...' line, or NIL."
  (let ((text (string-left-trim '(#\Space #\Tab) line)))
    (when (and (> (length text) 2) (member (char text 0) '(#\- #\*)))
      (let ((body (trim (subseq text 1))))
        (when (or (prefix-p "open" body) (prefix-p "next" body))
          (let ((colon (position #\: body)))
            (when (and colon (< (- colon 12) 40))
              (clean (subseq body (1+ colon)) 160))))))))

;;; --- reading the vault whole ------------------------------------------------

(defun read-vault ()
  "Everything this cell reads, in one place: (values THREADS TASKS SESSIONS UPCOMING)."
  (let* ((threads (parse-threads (file-text (threads-file))))
         (tasks (parse-todo (file-text (todo-file))))
         (sessions (parse-sessions (file-text (last-session-file)))))
    (values threads tasks sessions (upcoming threads tasks))))

;;; --- capture: append only ---------------------------------------------------

(defun slug (text)
  (let* ((lower (string-downcase (clean text 40)))
         (kept (coerce (loop for c across lower
                             when (or (alphanumericp c) (member c '(#\Space #\- #\_)))
                               collect c)
                       'string))
         (dashed (substitute #\- #\Space (trim kept)))
         (short (if (> (length dashed) 48) (subseq dashed 0 48) dashed)))
    (if (plusp (length (string-right-trim "-" short))) (string-right-trim "-" short) "note")))

(defun capture-note (text)
  (let ((text (remove-control (trim text))))
    (unless (and (plusp (length text)) (<= (length text) +note-cap+))
      (fail "a note is 1 to ~d characters; this one is ~d" +note-cap+ (length text)))
    (let ((path (write-new-file (inbox-directory)
                                (format nil "~a-~a" (now-stamp) (slug text))
                                "md"
                                (format nil "---~%captured: ~a~%source: kokpit quick capture~%---~%~a~%"
                                        (nlk:iso-now) text))))
      path)))

(defun capture-task (text &optional due)
  (let ((text (nlk:one-line (remove-control (or text "")))))
    (unless (and (plusp (length text)) (<= (length text) +task-cap+))
      (fail "a task is 1 to ~d characters; this one is ~d" +task-cap+ (length text)))
    (when (and due (plusp (length due)) (null (iso-date-at due 0)))
      (fail "a due date reads YYYY-MM-DD; ~s does not" due))
    (let* ((path (todo-file))
           (body (ensure-open-heading (let ((old (file-text path)))
                                        (if (plusp (length (trim old))) old
                                            (format nil "# To-Do~%~%## Open~%")))))
           (lines (nlk:lines body))
           (start (position-if #'open-heading-line-p lines))
           (after (and start (position-if (lambda (line) (prefix-p "## " (trim line)))
                                          lines :start (1+ start))))
           (line (format nil "- [ ] ~a~@[ (due: ~a)~]" text (and due (plusp (length due)) due)))
           (new (if after
                    (append (subseq lines 0 after) (list line "") (subseq lines after))
                    (append lines (list line)))))
      (write-file-text path (format nil "~{~a~%~}" new))
      line)))

(defun ensure-open-heading (body)
  (if (some #'open-heading-line-p (nlk:lines body))
      body
      (format nil "~a~%~%## Open~%" (string-right-trim '(#\Newline #\Space) body))))

(defun capture-journal (text)
  (let* ((text (nlk:one-line (remove-control (or text ""))))
         (today (today-iso))
         (folder (vault-subdirectory :journal-dir "daily"))
         (path (merge-pathnames (format nil "~a.md" today) folder)))
    (unless (and (plusp (length text)) (<= (length text) +note-cap+))
      (fail "a journal line is 1 to ~d characters; this one is ~d" +note-cap+ (length text)))
    (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
      (declare (ignore s))
      (let* ((line (format nil "- ~2,'0d:~2,'0d ~a" h m text))
             (old (file-text path))
             (new (if (plusp (length (trim old)))
                      (format nil "~a~%~a~%" (string-right-trim '(#\Newline) old) line)
                      (format nil "# ~a~%~%~a~%" today line))))
        (write-file-text path new)
        (values (vault-relative path) line)))))

(defun capture-receipt (text session)
  "One card at the top of Last-Session.md for this session, and one receipt
file that points at what the session wrote."
  (let* ((text (remove-control (trim text)))
         (files (gethash session *captures*))
         (session (or session "session"))
         (title (clean (first-line-of text) 60))
         (card (append (list (format nil "## ~a · ~a · ~a" (now-human) title session)
                             (format nil "- Did: ~a" (clean text 400)))
                       (when files (list (format nil "- Files: ~{~a~^, ~}" files)))
                       (let ((on-you (on-you-lines)))
                         (when on-you (list (format nil "- Open: ~a" (first on-you)))))))
         (path (write-session-card session card))
         (receipt (write-receipt text session files)))
    (values (vault-relative path) (vault-relative receipt) card)))

(defun first-line-of (text)
  (let ((at (position #\Newline text)))
    (if at (subseq text 0 at) text)))

(defun on-you-lines ()
  "The open thread items marked on you, as 'thread: item'."
  (let ((threads (parse-threads (file-text (threads-file))))
        (out '()))
    (dolist (thread threads)
      (dolist (item (thread-open-items thread))
        (when (getf item :on-you)
          (push (format nil "~a: ~a" (getf thread :title) (getf item :text)) out))))
    (nreverse out)))

(defun write-session-card (session card)
  "CARD put at the top of Last-Session.md, this session's older card taken out."
  (let* ((path (last-session-file))
         (lines (nlk:lines (file-text path))))
    (multiple-value-bind (preamble blocks) (split-sections lines)
      (let* ((kept (remove-if (lambda (block)
                                (let ((token (card-session block)))
                                  (and token (string-equal token session))))
                              blocks))
             (text (with-output-to-string (out)
                     (when (plusp (length preamble))
                       (format out "~{~a~%~}~%" preamble))
                     (format out "~{~a~%~}" card)
                     (when kept
                       (format out "~%")
                       (format out "~{~a~%~}" (mapcan (lambda (block) (append block (list ""))) kept))))))
        (write-file-text path text)))))

(defun write-receipt (text session files)
  (let* ((digest (nlk:sha256-text (format nil "~a|~a|~a" session (now-stamp) text)))
         (name (format nil "~a.md" (subseq digest 7 19)))
         (path (merge-pathnames name (receipts-directory))))
    (write-file-text path
                     (format nil "---~%kind: receipt~%harness: nodecode~%session: ~a~%refs: ~{~a~^, ~}~%created: ~a~%---~%~a~%"
                             session (or files '("Last-Session.md")) (nlk:iso-now) (clean text 2000)))
    path))

(defun note-capture (session path)
  (when path
    (pushnew (vault-relative path) (gethash session *captures*) :test #'string=)))

;;; --- the night pass ---------------------------------------------------------

(defun night-file () (nlk:home "kokpit/night.json"))

(defun empty-night ()
  (nlk:json-object "version" 1 "last_run" "" "proposals" #()))

(defun load-night ()
  (let ((path (night-file)))
    (if (probe-file path)
        (let ((value (handler-case (nlk:decode-json (nlk:read-text path))
                       (error () (fail "the night store ~a is not JSON; move it aside" (namestring path))))))
          (if (hash-table-p value) value (fail "the night store ~a holds no JSON object" (namestring path))))
        (empty-night))))

(defun save-night (store)
  (ensure-directories-exist (night-file))
  (nlk:write-file-atomically (night-file) (format nil "~a~%" (nlk:encode-json-object store))
                              :mode #o600 :directory-mode #o700)
  (night-file))

(defun night-proposals (store) (coerce (nlk:json-value store :array "proposals") 'list))

(defun (setf night-proposals) (proposals store)
  (setf (gethash "proposals" store) (coerce proposals 'vector)))

(defun proposal-id (kind source)
  (let ((text (format nil "~a|~a|~a|~a" kind (getf source :path) (getf source :line) (getf source :quote))))
    (subseq (nlk:sha256-text text) 7 19)))

(defun make-proposal (kind text reason source)
  (nlk:json-object "id" (proposal-id kind source)
                    "kind" kind
                    "text" (clean text 300)
                    "reason" (clean reason 160)
                    "source" (nlk:json-object "path" (getf source :path)
                                               "line" (or (getf source :line) 0)
                                               "quote" (or (getf source :quote) ""))
                    "status" "open"))

(defun open-proposals (store)
  (remove-if-not (lambda (p) (string-equal (or (nlk:json-value p :string "status") "open") "open"))
                 (night-proposals store)))

(defun rules-now ()
  "The night pass over the vault alone: a stale thread that is still open, and
a task due in three days or late. Each proposal quotes the line it came from."
  (let ((out '()))
    (dolist (thread (parse-threads (file-text (threads-file))))
      (let* ((opens (thread-open-items thread))
             (age (thread-age thread))
             (stale (or (null age) (> age (setting :stale-days)))))
        (when (and opens stale (getf thread :open-raw))
          (push (make-proposal "stale"
                               (format nil "~a: ~a" (getf thread :title) (getf (first opens) :text))
                               (if age
                                   (format nil "no change to this thread for ~d days" age)
                                   "this thread carries no date")
                               (list :path (vault-relative (threads-file))
                                     :line (getf thread :open-line)
                                     :quote (getf thread :open-raw)))
                out))))
    (dolist (task (parse-todo (file-text (todo-file))))
      (let ((days (getf task :days-left)))
        (when (and days (<= days 3))
          (push (make-proposal "due"
                               (format nil "Finish: ~a" (getf task :text))
                               (if (minusp days)
                                   (format nil "~d days overdue" (- days))
                                   (format nil "due in ~d days" days))
                               (list :path (vault-relative (todo-file))
                                     :line (getf task :line)
                                     :quote (getf task :raw)))
                out))))
    (nreverse out)))

(defun run-night ()
  (let* ((store (load-night))
         (fresh (rules-now))
         (old (remove-if-not (lambda (p)
                               (not (string-equal (or (nlk:json-value p :string "status") "open")
                                                  "open")))
                             (night-proposals store)))
         (known (make-hash-table :test #'equal)))
    (dolist (proposal old) (setf (gethash (nlk:json-value proposal :string "id") known) t))
    (let* ((kept (remove-if (lambda (p) (gethash (nlk:json-value p :string "id") known)) fresh))
           (all (append kept old)))
      (setf (gethash "last_run" store) (nlk:iso-now))
      (setf (night-proposals store) (coerce (subseq all 0 (min 200 (length all))) 'vector))
      (save-night store)
      (values store kept))))

(defun find-proposal (store token)
  (or (find-if (lambda (p) (string-equal (nlk:json-value p :string "id") token)) (open-proposals store))
      (find-if (lambda (p) (let ((id (nlk:json-value p :string "id")))
                             (and id (>= (length id) (length token))
                                  (string-equal (subseq id 0 (length token)) token))))
               (open-proposals store))
      (fail "no open proposal ~s; kokpit_night action=list shows them" token)))

(defun accept-proposal (store proposal)
  "Accepting writes only through the append-only capture writers."
  (let* ((kind (nlk:json-value proposal :string "kind"))
         (text (nlk:json-value proposal :string "text"))
         (written (if (string-equal kind "due") nil (capture-task text))))
    (setf (gethash "status" proposal) "accepted")
    (save-night store)
    (if written
        (format nil "Accepted [~a]: ~a~%  To-Do.md: ~a"
                (subseq (nlk:json-value proposal :string "id") 0 6) text written)
        (format nil "Accepted [~a]: ~a (already a task in To-Do.md)"
                (subseq (nlk:json-value proposal :string "id") 0 6) text))))

(defun dismiss-proposal (store proposal)
  (setf (gethash "status" proposal) "dismissed")
  (save-night store)
  (format nil "Dismissed [~a]: ~a" (subseq (nlk:json-value proposal :string "id") 0 6)
          (nlk:json-value proposal :string "text")))

(defun proposals-text (store &key (limit 10) kind)
  (let* ((open (open-proposals store))
         (picked (if kind
                     (remove-if-not (lambda (p) (string-equal (nlk:json-value p :string "kind") kind)) open)
                     open))
         (shown (subseq picked 0 (min limit (length picked)))))
    (if (null shown)
        (if (nlk:json-value store :string "last_run")
            "No open proposal; kokpit_night action=run makes a new pass."
            "No night pass yet; kokpit_night action=run makes one.")
        (format nil "~d open proposal~:p~@[, last run ~a~]:~%~{~a~%~}"
                (length picked)
                (let ((run (nlk:json-value store :string "last_run")))
                  (and run (plusp (length run)) (subseq run 0 (min 10 (length run)))))
                (loop for p in shown
                      for source = (nlk:json-value p :object "source")
                      collect (format nil "[~a] ~a: ~a~%  ~a~%  source ~a:~d \"~a\"~%  accept /kokpit add ~a, dismiss /kokpit skip ~a"
                                      (subseq (nlk:json-value p :string "id") 0 6)
                                      (nlk:json-value p :string "kind")
                                      (nlk:json-value p :string "text")
                                      (nlk:json-value p :string "reason")
                                      (nlk:json-value source :string "path")
                                      (or (nlk:json-value source :integer "line") 0)
                                      (nlk:json-value source :string "quote")
                                      (subseq (nlk:json-value p :string "id") 0 6)
                                      (subseq (nlk:json-value p :string "id") 0 6)))))))

;;; --- the brief ---------------------------------------------------------------

(defun brief-text ()
  "One plain-text brief from the vault, as the original builds it from the
vault sections: today's tasks, what is on you, the last session, the night
proposals. Under the section's brief_chars."
  (multiple-value-bind (threads tasks sessions upcoming-dates) (read-vault)
    (declare (ignore upcoming-dates))
    (let* ((today (today-iso))
           (overdue (count-if (lambda (task) (getf task :overdue)) tasks))
           (on-you (on-you-lines))
           (soon (remove-if-not (lambda (task) (let ((days (getf task :days-left)))
                                                (and days (<= 0 days 3))))
                                tasks))
           (lines (list (format nil "Kokpit brief ~a" today))))
      (push (format nil "Tasks: ~d open, ~d overdue" (length tasks) overdue) lines)
      (dolist (task (subseq tasks 0 (min 3 (length tasks))))
        (push (format nil "- ~a~@[ (due ~a)~]" (getf task :text) (getf task :due)) lines))
      (when soon
        (push (format nil "Due soon: ~{~a~^; ~}"
                      (loop for task in (subseq soon 0 (min 3 (length soon)))
                            collect (format nil "~a ~a" (getf task :due) (getf task :text))))
              lines))
      (if on-you
          (progn (push (format nil "On you (~d):" (length on-you)) lines)
                 (dolist (item (subseq on-you 0 (min 4 (length on-you))))
                   (push (format nil "- ~a" item) lines)))
          (when threads (push (format nil "Open threads: ~d, nothing marked on you" (length threads)) lines)))
      (when sessions
        (let ((card (first sessions)))
          (push (format nil "Last session: ~a ~a" (getf card :at) (getf card :title)) lines)
          (dolist (item (getf card :open))
            (push (format nil "- Open: ~a" item) lines))))
      (let* ((store (load-night))
             (open (open-proposals store)))
        (if open
            (progn (push (format nil "Night (last run ~a): ~d open"
                                  (let ((run (nlk:json-value store :string "last_run")))
                                    (if (and run (plusp (length run))) (subseq run 0 (min 10 (length run))) "never"))
                                  (length open))
                        lines)
                   (dolist (proposal (subseq open 0 (min 2 (length open))))
                     (push (format nil "- [~a] ~a" (subseq (nlk:json-value proposal :string "id") 0 6)
                                   (nlk:json-value proposal :string "text"))
                           lines)))
            (push "Night: no proposal open, /kokpit night run makes a pass" lines)))
      (push "Act: /kokpit todo <text>, /kokpit note <text>, /kokpit night" lines)
      (let ((text (format nil "~{~a~%~}" (nreverse lines))))
        (if (> (length text) (setting :brief-chars))
            (format nil "~a~%..." (subseq text 0 (max 1 (- (setting :brief-chars) 4))))
            text)))))

;;; --- search -------------------------------------------------------------------

(defun vault-note-files ()
  "Every Markdown file under the vault, leaving out receipts, archive and dot folders."
  (let ((found '()) (seen 0))
    (labels ((walk (dir)
               (when (< seen +walk-cap+)
                 (dolist (file (ignore-errors (uiop:directory-files dir)))
                   (when (and (equal "md" (pathname-type file)) (< seen +walk-cap+))
                     (incf seen)
                     (push file found)))
                 (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                   (let ((name (string-downcase (or (car (last (pathname-directory sub))) ""))))
                     (unless (member name +skip-dirs+ :test #'string=)
                       (walk sub)))))))
      (walk (vault-directory)))
    (nreverse found)))

(defun query-words (query)
  (let ((words '()) (current (make-string-output-stream)))
    (flet ((flush ()
             (let ((word (string-downcase (get-output-stream-string current))))
               (when (> (length word) 1) (pushnew word words :test #'string=)))))
      (loop for c across (or query "")
            do (if (alphanumericp c) (write-char c current) (flush)))
      (flush))
    (nreverse words)))

(defun note-title (path text)
  (let ((heading (find-if (lambda (line) (prefix-p "# " line)) (nlk:lines text))))
    (if heading
        (clean (string-left-trim "# " heading) 80)
        (clean (pathname-name path) 80))))

(defun search-vault (query limit)
  (let ((words (query-words query)))
    (when (null words) (fail "the query has no word to look for"))
    (let ((hits '()))
      (dolist (path (vault-note-files))
        (let* ((text (file-text path))
               (title (note-title path text))
               (haystack (string-downcase (format nil "~a ~a ~a" title (pathname-name path) text))))
          (when (every (lambda (word) (search word haystack)) words)
            (let ((line-number 0) (snippet "") (best 0))
              (loop for line in (nlk:lines text)
                    for index from 1
                    for lower = (string-downcase line)
                    for hits = (count-if (lambda (word) (search word lower)) words)
                    when (and (plusp hits) (plusp (length (trim line))) (> hits best))
                      do (setf best hits line-number index snippet (clean line 200)))
              (push (list :path (vault-relative path) :title title :line line-number :snippet snippet
                          :score (+ (loop for word in words sum (count-word haystack word))
                                    5 (count-if (lambda (word) (search word (string-downcase title))) words)))
                    hits)))))
      (let ((sorted (stable-sort hits #'> :key (lambda (hit) (getf hit :score)))))
        (subseq sorted 0 (min limit (length sorted)))))))

(defun count-word (haystack word)
  (loop with at = (search word haystack)
        while at count 1
        do (setf at (search word haystack :start2 (1+ at)))))

;;; --- the tools -----------------------------------------------------------------

(defun tool-brief (arguments)
  (declare (ignore arguments))
  (brief-text))

(defun tool-search (arguments)
  (let* ((query (or (nlk:json-value arguments :string "query") ""))
         (limit (or (nlk:json-value arguments :integer "limit") 5))
         (hits (search-vault query (max 1 (min 20 limit)))))
    (if (null hits)
        (format nil "No note matched every word of ~s." query)
        (format nil "~d note~:p for ~s:~%~{~a~%~}"
                (length hits) query
                (loop for hit in hits
                      collect (format nil "~a:~d  ~a~%  ~a"
                                      (getf hit :path) (getf hit :line) (getf hit :title) (getf hit :snippet)))))))

(defun tool-threads (arguments)
  (declare (ignore arguments))
  (multiple-value-bind (threads tasks sessions upcoming-dates) (read-vault)
    (declare (ignore tasks upcoming-dates))
    (let ((lines '()))
      (if threads
          (dolist (thread threads)
            (push (format nil "~a~@[ (~a)~]: ~a" (getf thread :title) (getf thread :date)
                          (or (getf thread :status) ""))
                  lines)
            (dolist (item (thread-open-items thread))
              (push (format nil "  - ~@[~a: ~]~a~@[ (due ~a)~]~@[ [~a]~]"
                            (getf item :owner) (getf item :text) (getf item :due)
                            (and (getf item :on-you) "on you"))
                    lines)))
          (push (format nil "~a has no active thread." (vault-relative (threads-file))) lines))
      (when sessions
        (let ((card (first sessions)))
          (push "" lines)
          (push (format nil "Last session: ~a ~a" (getf card :at) (getf card :title)) lines)
          (dolist (item (getf card :open))
            (push (format nil "  - Open: ~a" item) lines))))
      (format nil "~{~a~%~}" (nreverse lines)))))

(defun tool-capture (arguments)
  (let* ((kind (string-downcase (or (nlk:json-value arguments :string "kind") "note")))
         (text (or (nlk:json-value arguments :string "text") ""))
         (due (or (nlk:json-value arguments :string "due") "")))
    (cond ((string-equal kind "receipt")
           (multiple-value-bind (card receipt lines) (capture-receipt text *session*)
             (format nil "Card in ~a, receipt in ~a:~%~{~a~%~}" card receipt lines)))
          ((string-equal kind "task")
           (let ((line (capture-task text due)))
             (note-capture *session* (todo-file))
             (format nil "~a: ~a" (vault-relative (todo-file)) line)))
          ((string-equal kind "journal")
           (multiple-value-bind (path line) (capture-journal text)
             (note-capture *session* (merge-pathnames path (vault-directory)))
             (format nil "~a: ~a" path line)))
          ((string-equal kind "note")
           (let ((path (capture-note text)))
             (note-capture *session* path)
             (format nil "Wrote ~a" (vault-relative path))))
          (t (fail "kind is note, task, journal or receipt; ~s is none of them" kind)))))

(defun tool-night (arguments)
  (let* ((action (string-downcase (or (nlk:json-value arguments :string "action") "list")))
         (kind (nlk:json-value arguments :string "kind"))
         (limit (or (nlk:json-value arguments :integer "limit") 10)))
    (cond ((string-equal action "list")
           (proposals-text (load-night) :limit (max 1 (min 50 limit)) :kind kind))
          ((string-equal action "run")
           (multiple-value-bind (store fresh) (run-night)
             (format nil "Night pass over the vault: ~d new proposal~:p.~%~a"
                     (length fresh) (proposals-text store :limit 10 :kind kind))))
          ((string-equal action "accept")
           (let ((store (load-night)))
             (accept-proposal store (find-proposal store (or (nlk:json-value arguments :string "id") "")))))
          ((string-equal action "dismiss")
           (let ((store (load-night)))
             (dismiss-proposal store (find-proposal store (or (nlk:json-value arguments :string "id") "")))))
          (t (fail "action is list, run, accept or dismiss; ~s is none of them" action)))))

;;; --- /kokpit -------------------------------------------------------------------

(defparameter +primer+
  "Kokpit is your own Markdown vault, read and appended to from here. The
tools: kokpit_brief (the day as one short text), kokpit_brain_search (words
looked for across the vault's notes), kokpit_threads (where you left off, and
what is on you), kokpit_capture (a note, a task, a journal line, or a receipt
that closes a result), kokpit_night (proposals that quote a vault line, and
accept or dismiss by id). /kokpit brief|threads|search|note|todo|j|night|add|skip.
A capture is append-only: a note is a new file in inbox/, a task is one line
under ## Open in To-Do.md, a journal line is added at the end of the day's
file. Nothing is ever edited, moved or deleted, and nothing leaves the machine."
  "What (help :kokpit) answers while the cell runs.")

(defun status-line ()
  (multiple-value-bind (threads tasks sessions upcoming-dates) (read-vault)
    (let ((store (load-night)))
      (format nil "Kokpit vault ~a: ~d thread~:p, ~d open task~:p, ~d session card~:p, ~d proposal~:p open.~%~
                   /kokpit brief shows the day; /kokpit help lists the rest."
              (namestring (vault-directory)) (length threads) (length tasks) (length sessions)
              (length (open-proposals store))))))

(defun run-command (args session-id)
  "One /kokpit invocation: a subcommand and its text."
  (handler-case
      (let* ((*session* session-id)
             (line (trim (or args "")))
             (space (position #\Space line))
             (head (string-downcase (if space (subseq line 0 space) line)))
             (rest-text (if space (trim (subseq line (1+ space))) "")))
        (cond ((member head '("" "help") :test #'string=)
               (format nil "~a~%~%~a" (status-line) +primer+))
              ((member head '("brief" "today" "b") :test #'string=) (brief-text))
              ((member head '("threads" "t") :test #'string=)
               (let ((*session* session-id)) (tool-threads (nlk:json-object))))
              ((string= head "search")
               (tool-search (nlk:json-object "query" rest-text)))
              ((string= head "note")
               (tool-capture (nlk:json-object "text" rest-text "kind" "note")))
              ((string= head "todo")
               (multiple-value-bind (body due) (due-in rest-text)
                 (tool-capture (nlk:json-object "text" body "due" due "kind" "task"))))
              ((member head '("j" "journal") :test #'string=)
               (tool-capture (nlk:json-object "text" rest-text "kind" "journal")))
              ((string= head "night")
               (tool-night (nlk:json-object "action" (if (string= rest-text "run") "run" "list"))))
              ((string= head "add")
               (tool-night (nlk:json-object "action" "accept" "id" rest-text)))
              ((string= head "skip")
               (tool-night (nlk:json-object "action" "dismiss" "id" rest-text)))
              ((string= head "done")
               (let ((*session* session-id))
                 (tool-capture (nlk:json-object "text" rest-text "kind" "receipt"))))
              (t (format nil "Kokpit does not know ~s.~%~%~a" head +primer+))))
    (error (condition) (nle:failure (format nil "ERROR: kokpit: ~a" condition)))))

(defun due-in (text)
  "TEXT with a trailing 'due:YYYY-MM-DD' taken off: (values TEXT DUE)."
  (let ((at (search "due:" text :test #'char-equal)))
    (if at
        (let ((after (string-left-trim '(#\Space #\Tab) (subseq text (+ at 4)))))
          (if (iso-date-at after 0)
              (values (trim (concatenate 'string (subseq text 0 at) (subseq after 10)))
                      (iso-date-at after 0))
              (values text "")))
        (values text ""))))

;;; --- the tool schemas ----------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *brief-schema*
  (tool-schema
   "The day from the Kokpit vault as one short plain-text brief: open tasks and what is overdue, the thread items marked on you, the last session's open points and the night proposals. Read-only."
   '())
  "The kokpit_brief tool.")

(defparameter *search-schema*
  (tool-schema
   "Search the Kokpit vault's Markdown notes (notes/, inbox/, the top level) for words. Answers vault-relative paths, titles, line numbers and one snippet each. Read-only."
   '(("query" "string" "Words to look for; a note must carry all of them." t)
     ("limit" "integer" "How many notes to answer with (default 5, at most 20)." nil)))
  "The kokpit_brain_search tool.")

(defparameter *threads-schema*
  (tool-schema
   "Open threads from Threads.md (where we left off, with the owner of each open item) and the latest Last-Session card. Read-only."
   '())
  "The kokpit_threads tool.")

(defparameter *capture-schema*
  (tool-schema
   "Append to the Kokpit vault: a note (a new file in inbox/), a task (one line under ## Open in To-Do.md), a journal line (in the day's file under daily/), or a receipt that closes a result of this session (a card in Last-Session.md and one short receipt file). Append-only; it never edits a note. Do not use it for lookups or plain chat."
   '(("text" "string" "The note, task or journal line (at most 2000 characters; a task 300); for a receipt, one or two lines on what was done." t)
     ("kind" "string" "note (default), task, journal or receipt." nil)
     ("due" "string" "A due date for a task, YYYY-MM-DD." nil)))
  "The kokpit_capture tool.")

(defparameter *night-schema*
  (tool-schema
   "The Kokpit night pass: proposals read off the vault (a stale thread still open, a task due or late), each quoting the line it came from. action=list shows the open ones, run makes a pass, accept or dismiss takes an id. Accepting writes only through the append-only task writer."
   '(("action" "string" "list (default), run, accept or dismiss." nil)
     ("id" "string" "The proposal's id, or its first 6 characters, for accept and dismiss." nil)
     ("kind" "string" "Only stale or due, for list." nil)
     ("limit" "integer" "How many proposals to show (default 10)." nil)))
  "The kokpit_night tool.")

;;; --- the cell -------------------------------------------------------------------

(defun check-config ()
  "A vault this machine cannot read is a refusal naming it, never a start that
quietly answers from nothing."
  (let ((vault (vault-directory)))
    (cond ((null vault)
           (error 'nlk:config-refusal
                  :detail "kokpit: no vault is set; set vault in the kokpit section to the folder of Markdown notes this cell reads and appends to"))
          ((not (uiop:directory-exists-p vault))
           (error 'nlk:config-refusal
                  :detail (format nil "kokpit: the vault ~a is not a folder; set vault in the kokpit section to your notes folder, or make that folder"
                                  (namestring vault)))))))

(nle:define-cell kokpit
  (:section ("kokpit")
    (:guide "vault is the folder of Markdown notes this cell reads and appends to, and the one thing it needs; the file and folder names are read from it, and stale_days is how long a thread may sit before the night pass proposes a next step")
    ("vault" :directory :required t
     :doc "the Markdown vault this cell reads and appends to; nothing is written outside it")
    ("threads_file" :string :default "Threads.md"
     :doc "the threads file, inside the vault or absolute")
    ("todo_file" :string :default "To-Do.md"
     :doc "the to-do file, inside the vault or absolute")
    ("last_session_file" :string :default "Last-Session.md"
     :doc "the session-card file, inside the vault or absolute")
    ("inbox_dir" :string :default "inbox"
     :doc "the folder a captured note becomes a new file in")
    ("receipts_dir" :string :default "receipts"
     :doc "the folder session receipts are written to")
    ("journal_dir" :string :default "daily"
     :doc "the folder of daily journal files, one YYYY-MM-DD.md a day")
    ("stale_days" :integer :default 7 :min 1
     :doc "days a thread may sit unchanged before the night pass proposes a next step")
    ("brief_chars" :integer :default 3500 :min 200
     :doc "how long the brief may get, in characters"))
  (:help :kokpit
   "kokpit: your Markdown vault - brief, threads, search, capture, night pass; /kokpit"
   +primer+)
  (:command "kokpit" 'run-command
            :description "Your vault: the brief, threads, a capture, the night pass"
            :argument-hint "brief|threads|search|note|todo|j|night|add|skip")
  (:tool "kokpit_brief" *brief-schema* 'tool-brief)
  (:tool "kokpit_brain_search" *search-schema* 'tool-search)
  (:tool "kokpit_threads" *threads-schema* 'tool-threads)
  (:tool "kokpit_capture" *capture-schema* 'tool-capture)
  (:tool "kokpit_night" *night-schema* 'tool-night)
  (:start (lambda ()
            (check-config)
            (setf *captures* (make-hash-table :test #'equal))
            (setf *session* nil)
            (nle:on-stop (lambda ()
                           (setf *captures* (make-hash-table :test #'equal))
                           (setf *session* nil))))))
