;;;; watch.lisp --- the saved watches, the daily check, and its notice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A watch is one saved query plus the speech IDs already seen for it, so
;;;; a daily check announces only what is new. The state is one JSON file
;;;; under the cell's own folder in the home, replaced atomically; nothing
;;;; else is written, and no model is called to check it -- the check is one
;;;; GET per watch and a standing notice when there is something new.
;;;;
;;;; The original kept its state in plugin-data/jp-kokkai/state.json and
;;;; created a Hermes cron job running a script. Here the check is a worker
;;;; inside this image (NLK:WORKER-START), so there is no script, no cron
;;;; job, and nothing left behind after the folder is removed but the file.

(in-package #:nodecode-kokkai)

;;; --- where the watches live -----------------------------------------------------

(defvar *state-file* nil
  "Where the watches are kept. NIL reads it under the home; a test binds a
path of its own.")

(defvar *state* nil
  "The kept state as a hash table while the cell runs, or NIL before the
first read.")

(defparameter +state-relative+ "kokkai/state.json"
  "The file under the home: one folder per cell, one file in it.")

(defparameter +seen-cap+ 5000
  "The most speech IDs one watch remembers. Older ones fall off the front,
which can announce an old speech again after thousands of new ones.")

(defparameter +watch-page+ 100
  "The most rows one watch request asks for: the API's ceiling, and the
number under which a window has to fit in one page.")

(defun state-file ()
  "The file the watches are kept in."
  (or *state-file* (nlk:home +state-relative+)))

(defun empty-state ()
  "A state with nothing saved yet."
  (nlk:make-json-object "watches" (vector) "checked" ""))

(defun read-state ()
  "The kept state. A file that cannot be read is a refusal, never a fresh
start: overwriting it would lose the seen speech IDs."
  (let ((file (state-file)))
    (cond ((not (probe-file file)) (empty-state))
          (t (let ((json (ignore-errors (nlk:decode-json (nlk:read-text file)))))
               (unless (hash-table-p json)
                 (fail "the watch state at ~a is unreadable, so nothing was changed; move it aside to start over"
                       (namestring file)))
               json)))))

(defun state ()
  "The state, read once and kept."
  (or *state* (setf *state* (read-state))))

(defun watches ()
  "The saved watches, oldest first."
  (json-list (json-field (state) "watches")))

(defun save-state (new-watches &key checked)
  "Keep NEW-WATCHES, and the day CHECKED when one is given."
  (setf *state* (nlk:make-json-object
                 "watches" (coerce new-watches 'vector)
                 "checked" (or checked (json-text (state) "checked" ""))))
  (nlk:write-file-atomically (state-file) (nlk:encode-json-object *state*)
                             :mode #o600 :directory-mode #o700)
  *state*)

;;; --- one watch ------------------------------------------------------------------

(defun new-watch-id ()
  "A watch's id: twelve hex characters, enough to name one and short
enough to type."
  (format nil "~12,'0x" (random (expt 16 12))))

(defun watch-any (watch) (json-text watch "any" ""))
(defun watch-speaker (watch) (json-text watch "speaker" ""))
(defun watch-house (watch) (json-text watch "house" ""))
(defun watch-lookback (watch) (max 1 (json-count watch "lookback_days")))
(defun watch-seen (watch) (coerce (json-list (json-field watch "seen")) 'list))

(defun watch-line (watch)
  "One saved watch as a line of the list."
  (format nil "~a  ~@[any ~s ~]~@[speaker ~s ~]~@[house ~a ~]lookback ~d days, ~d speech IDs seen~@[, saved ~a~]"
          (json-text watch "id" "")
          (let ((any (watch-any watch))) (and (plusp (length any)) any))
          (let ((speaker (watch-speaker watch))) (and (plusp (length speaker)) speaker))
          (let ((house (watch-house watch))) (and (plusp (length house)) house))
          (watch-lookback watch) (length (watch-seen watch))
          (let ((added (json-text watch "added" ""))) (and (plusp (length added)) added))))

(defun window-of (lookback)
  "The (values FROM UNTIL) one watch asks for: today in Tokyo, back
LOOKBACK days."
  (let* ((until (tokyo-today))
         (from (multiple-value-bind (second minute hour day month year)
                   (decode-universal-time (- (get-universal-time) (* (1- lookback) 24 60 60)) 9)
                 (declare (ignore second minute hour))
                 (format nil "~4,'0d-~2,'0d-~2,'0d" year month day))))
    (values from until)))

(defun page-rows (any speaker house lookback)
  "One watch page: every meeting row in the window, or a refusal naming
the window as too wide."
  (multiple-value-bind (from until) (window-of lookback)
    (let* ((pairs (page-pairs any speaker house from until +watch-page+))
           (page (call-path +meeting-path+ pairs (setting :timeout-seconds)))
           (total (json-count page "numberOfRecords"))
           (returned (json-count page "numberOfReturn")))
      (when (< returned total)
        (fail "the window holds ~d speeches and one page holds ~d, so nothing was saved: lower lookback_days or narrow the words"
              total returned))
      (json-list (json-field page "meetingRecord")))))

(defun new-speeches (rows seen)
  "The (values SPEECHES URLS) that ROWS carry and SEEN does not."
  (let ((speeches '()) (urls '()))
    (dolist (row rows)
      (let ((fresh '()))
        (dolist (speech (json-list (json-field row "speechRecord")))
          (let ((id (json-text speech "speechID")))
            (when (and id (not (member id seen :test #'string=)))
              (push id fresh)
              (push speech speeches))))
        (when fresh
          (let ((url (json-text row "meetingURL")))
            (when url (push url urls))))))
    (values (nreverse speeches) (nreverse urls))))

(defun keep-seen (seen ids)
  "SEEN with IDS added, at most +SEEN-CAP+ of them kept."
  (let ((all (append seen (remove-duplicates ids :test #'string=))))
    (if (> (length all) +seen-cap+)
        (subseq all (- (length all) +seen-cap+))
        all)))

;;; --- add, remove, list, run -----------------------------------------------------

(defun add-watch (any speaker house lookback-days)
  "Save one watch, marking what the window already holds as seen, so the
first check announces only what comes after it."
  (let* ((any (trimmed* any))
         (speaker (trimmed* speaker))
         (house (check-house house))
         (lookback (or lookback-days 30)))
    (when (and (zerop (length any)) (zerop (length speaker)))
      (fail "a watch needs any or speaker; a date range alone is not a watch"))
    (unless (<= 1 lookback 30)
      (fail "lookback_days is ~a; it is 1 through 30" lookback))
    (let* ((rows (page-rows any speaker house lookback))
           (seen (speech-ids rows))
           (id (new-watch-id))
           (watch (nlk:make-json-object "id" id "any" any "speaker" speaker
                                         "house" house "lookback_days" lookback
                                         "added" (tokyo-today)
                                         "seen" (coerce (subseq seen (max 0 (- (length seen) +seen-cap+))) 'vector))))
      (save-state (append (watches) (list watch)))
      (format nil "Watch ~a saved~@[ for ~s~]~@[ for ~s~]~@[, ~a~]: ~d speech IDs already on the page are marked seen, so the first check announces only what comes after them. It is checked once a day at ~2,'0d:00, looking back ~d days."
              id
              (and (plusp (length any)) any)
              (and (plusp (length speaker)) speaker)
              (and (plusp (length house)) house)
              (length seen) (setting :hour) lookback))))

(defun find-watch (id)
  "The saved watch named ID, or a refusal naming the ones there are."
  (let ((id (trimmed* id)))
    (when (zerop (length id))
      (fail "watch_id is required to remove a watch; the list shows the ids"))
    (or (find id (watches) :key (lambda (watch) (json-text watch "id" "")) :test #'string=)
        (fail "no watch ~s; the list shows the ids there are" id))))

(defun remove-watch (id)
  "Take one watch out of the state."
  (let* ((id (trimmed* id))
         (gone (find-watch id))
         (left (remove gone (watches) :test #'eq)))
    (save-state left)
    (format nil "Watch ~a removed. ~d watch~p still saved." id (length left) (length left))))

(defun list-watches ()
  "The saved watches, one line each, and when the daily check last ran."
  (let ((saved (watches)))
    (if (null saved)
        "No watch is saved. Add one with action=add and any or speaker."
        (format nil "~d watch~p saved~@[, last checked ~a~]:~%~{~a~^~%~}"
                (length saved) (length saved)
                (let ((checked (json-text (state) "checked" ""))) (and (plusp (length checked)) checked))
                (mapcar #'watch-line saved)))))

(defun check-one (watch &key mark)
  "Check one watch => (values SPEECHES URLS): what ROWS carry and the
watch has not seen. MARK puts every ID on the page into the watch's seen
list, the daily check's job, and leaves the manual run's alone."
  (let* ((seen (watch-seen watch))
         (rows (page-rows (watch-any watch) (watch-speaker watch) (watch-house watch)
                           (watch-lookback watch))))
    (multiple-value-bind (speeches urls) (new-speeches rows seen)
      (when mark
        (setf (gethash "seen" watch)
              (coerce (keep-seen seen (speech-ids rows)) 'vector)))
      (values speeches urls))))

(defun run-watches (&key id mark)
  "Check every saved watch, or the one ID names. => (values TEXT NEWS),
NEW a list of (ID . URLS) for the watches that found something. MARK keeps
what was seen: the daily check does, a manual run does not, so a run can
be repeated and read again."
  (let ((saved (if (and id (plusp (length (trimmed* id))))
                   (list (find-watch id))
                   (watches))))
    (when (null saved)
      (fail "no watch is saved, so there was nothing to check"))
    (let ((lines '()) (news '()))
      (dolist (watch saved)
        (let ((id (json-text watch "id" "")))
          (multiple-value-bind (speeches urls) (check-one watch :mark mark)
            (if (null speeches)
                (push (format nil "~a: nothing new~@[ (checked ~{~a~^, ~})~]" id urls) lines)
                (progn
                  (push (cons id urls) news)
                  (push (format nil "~a: ~d new speech~p, at ~d meeting~p~%~{~a~^~%~}"
                                id (length speeches) (length speeches)
                                (length urls) (length urls) urls)
                        lines))))))
      (when mark (save-state (watches) :checked (tokyo-today)))
      (values (format nil "~{~a~^~%~}~@[~%~%The speech text is not included; open a meeting URL to read it.~]"
                      (nreverse lines) news)
              (nreverse news)))))

;;; --- the daily check ------------------------------------------------------------

(defun watch-hour ()
  "The hour the daily check runs at, in the machine's own time, as the
setting names it."
  (setting :hour))

(defun local-hour ()
  "The hour now, on the machine's clock."
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second minute))
    hour))

(defun check-due-p ()
  "T when today's check has not run and the hour has come."
  (and (>= (local-hour) (watch-hour))
       (not (string= (json-text (state) "checked" "") (tokyo-today)))))

(defun watch-step ()
  "The worker's one step: on the watch hour, check every watch and say
what is new under a standing notice; nothing otherwise."
  (when (check-due-p)
    (multiple-value-bind (text news) (run-watches :mark t)
      (declare (ignore text))
      (dolist (row news)
        (nle:notice (format nil "kokkai: new Diet speeches for watch ~a~%~{~a~^~%~}"
                            (car row) (cdr row))
                    :key (format nil "nodecode-kokkai-~a" (car row))))
      ))
  :done)
