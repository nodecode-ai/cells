;;;; cell.lisp --- the transcript, the four verbs, /meeting, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a top-level key next to `websearch':
;;;;   "meeting": {"enabled": true,
;;;;               "dir": "~/.nodecode/meeting",
;;;;               "records": "",
;;;;               "window_chars": 20000,
;;;;               "stale_seconds": 30}
;;;; Nothing here is a secret and nothing here reaches the network: the cell
;;;; reads and writes the two transcript files the original's daemon wrote,
;;;; rolling.txt (a capped tail) and full.txt (every line), and the records.

(in-package #:nodecode-meeting)

;;; --- settings ---------------------------------------------------------------------

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's, with a directory the
operator wrote out and left blank refused here, so it is heard at start rather
than at the first transcript read. An absent key takes the default."
  (let ((given (and (hash-table-p table) (nlk:json-value table :string "dir"))))
    (when (and given (zerop (length (nlk:trimmed given))))
      (nlk:config-error
       "meeting.dir is set but empty: name the directory the transcript files live in, ~
e.g. ~~/.nodecode/meeting, or take the key out to use the default")))
  (setf (getf values :dir) (nlk:trimmed (or (getf values :dir) "")))
  values)

;;; --- where the files are -----------------------------------------------------------

(defun home-first (text)
  "TEXT with a leading ~ read as the home directory."
  (if (and (plusp (length text)) (char= #\~ (char text 0)))
      (concatenate 'string (namestring (user-homedir-pathname))
                   (if (and (> (length text) 1) (char= #\/ (char text 1)))
                       (subseq text 2)
                       (subseq text 1)))
      text))

(defun directory-path (text what)
  "TEXT as a directory pathname, refusing an empty one and naming WHAT it was."
  (let ((text (nlk:trimmed (or text ""))))
    (when (zerop (length text))
      (fail "~a is empty: name a directory" what))
    (uiop:ensure-directory-pathname
     (uiop:parse-native-namestring (home-first text)))))

(defun state-dir ()
  "The directory the live transcript is read from and appended to."
  (directory-path (setting :dir) "meeting.dir"))

(defun records-root ()
  "Where records are saved: the section's own root, else Records/ beside the transcript."
  (let ((records (nlk:trimmed (or (setting :records) ""))))
    (if (zerop (length records))
        (merge-pathnames (make-pathname :directory '(:relative "Records")) (state-dir))
        (directory-path records "meeting.records"))))

(defun transcript-file (name)
  "The transcript file called NAME ("rolling" or "full") in the state directory."
  (merge-pathnames (make-pathname :name name :type "txt") (state-dir)))

(defun file-text (path)
  "PATH's whole text, or the empty string when it is not there yet."
  (if (probe-file path)
      (uiop:read-file-string path :external-format :utf-8)
      ""))

(defun file-lines (path)
  "PATH's lines, as a list, or NIL when it is not there."
  (let ((text (string-right-trim '(#\Newline) (file-text path))))
    (if (zerop (length text)) '() (lines-of text))))

(defun lines-of (text)
  "TEXT split at its newlines."
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline text :start start)
        collect (subseq text start end)
        while end))

;;; --- how fresh it is ----------------------------------------------------------------

(defun age-seconds (path)
  "How long ago PATH was written, in seconds, or NIL when it never was."
  (let ((written (ignore-errors (file-write-date path))))
    (and written (- (get-universal-time) written))))

(defun stale-p (age)
  "True when a transcript AGE seconds old is past the section's stale_seconds."
  (or (null age) (> age (setting :stale-seconds))))

(defun age-text (age)
  "AGE as the operator reads it."
  (cond ((null age) "never written")
        ((< age 60) (format nil "~ds old" age))
        ((< age 3600) (format nil "~dm ~ds old" (floor age 60) (mod age 60)))
        (t (format nil "~dh ~dm old" (floor age 3600) (mod (floor age 60) 60)))))

(defun clock-now ()
  "The wall clock as the original's transcript lines carry it."
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun day-now ()
  "The date as a record's header carries it."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

;;; --- reading the transcript ---------------------------------------------------------

(defun tail-lines (text count)
  "The last COUNT lines of TEXT."
  (let* ((lines (lines-of text))
         (keep (min count (length lines))))
    (format nil "~{~a~^~%~}" (last lines keep))))

(defun tail-chars (text count)
  "The last COUNT characters of TEXT, from a line boundary when one is near."
  (if (<= (length text) count)
      text
      (let* ((start (- (length text) count))
             (newline (position #\Newline text :start start)))
        (subseq text (if newline (1+ newline) start)))))

(defun transcript (&key (lines 40) chars)
  "The newest part of the live transcript, with how fresh it is: the whole of
what the user just heard, not a summary of it."
  (let* ((path (transcript-file "rolling"))
         (text (string-trim '(#\Space #\Tab #\Newline) (file-text path)))
         (age (age-seconds path))
         (body (cond (chars (tail-chars text chars))
                     ((and lines (plusp lines)) (tail-lines text lines))
                     (t text))))
    (with-output-to-string (out)
      (cond ((zerop (length text))
             (format out "meeting: nothing transcribed yet in ~a~@[ (the file is ~a)~]. ~
Append a line with meeting_append, or point meeting.dir at the directory your transcriber writes."
                     (namestring (state-dir)) (and age (age-text age))))
            ((stale-p age)
             (format out "meeting: STALE -- the transcript is ~a, past the section's ~ds: the ~
capture is not running, so say so rather than answering from old words.~%~a"
                     (age-text age) (setting :stale-seconds) body))
            (t (format out "meeting: live -- transcript ~a~%~a" (age-text age) body))))))

(defun status (&key verbose)
  "Whether capture is alive, and where it writes."
  (let* ((path (transcript-file "rolling"))
         (age (age-seconds path))
         (count (length (file-lines path))))
    (with-output-to-string (out)
      (format out "meeting: ~a, ~d line~:p in ~a"
              (if (stale-p age)
                  (format nil "stale (~a)" (age-text age))
                  (format nil "live (~a)" (age-text age)))
              count (namestring (state-dir)))
      (when verbose
        (format out "~%records: ~a~%window_chars ~d, stale_seconds ~d"
                (namestring (records-root))
                (setting :window-chars) (setting :stale-seconds))))))

;;; --- appending one line -------------------------------------------------------------

(defun line-body (line)
  "LINE without its [HH:MM:SS] [tag] prefix, as the original's dedupe read it."
  (let* ((close (position #\] line))
         (rest (and close (string-left-trim '(#\Space) (subseq line (1+ close))))))
    (if (and rest (plusp (length rest)) (char= #\[ (char rest 0)))
        (let ((end (position #\] rest)))
          (if end (string-left-trim '(#\Space) (subseq rest (1+ end))) rest))
        (or rest line))))

(defun echo-p (text previous)
  "True when TEXT says what one of the last few lines already said."
  (let ((wanted (line-body text)))
    (and (>= (length wanted) 8)
         (some (lambda (line) (string= wanted (line-body line)))
               (last previous 10)))))

(defun note-line (text &key (source "mic"))
  "Append one transcribed line: full.txt keeps every line, rolling.txt the last
window_chars of them. An echo of a recent line is dropped rather than doubled,
except from the phone, whose mic wins."
  (let* ((text (nlk:trimmed (or text "")))
         (source (let ((tag (nlk:trimmed (or source "mic"))))
                   (if (zerop (length tag)) "mic" tag)))
         (rolling (transcript-file "rolling"))
         (full (transcript-file "full"))
         (previous (file-lines rolling))
         (line (format nil "[~a] [~a] ~a" (clock-now) source text)))
    (cond ((zerop (length text))
           (fail "meeting_append needs text: the line that was said"))
          ((and (not (string-equal source "phone")) (echo-p line previous))
           "meeting: a recent line already says that, so nothing was appended")
          (t
           (ensure-directories-exist full)
           (with-open-file (out full :direction :output :if-exists :append
                                     :if-does-not-exist :create :external-format :utf-8)
             (write-line line out))
           (let* ((text (concatenate 'string (file-text rolling) line (string #\Newline)))
                  (kept (if (> (length text) (setting :window-chars))
                            (tail-chars text (setting :window-chars))
                            text)))
             (with-open-file (out rolling :direction :output :if-exists :supersede
                                        :if-does-not-exist :create :external-format :utf-8)
               (write-string kept out)))
           (format nil "appended: ~a" line)))))

;;; --- saving a record ----------------------------------------------------------------

(defun slug-of (title)
  "TITLE as a file name: lowercase words joined by single dashes."
  (let ((chars '()) (dash t))
    (loop for char across (string-downcase title)
          do (if (alphanumericp char)
                 (progn (push char chars) (setf dash nil))
                 (unless dash (push #\- chars) (setf dash t))))
    (let ((text (coerce (nreverse chars) 'string)))
      (string-trim "-" (if (zerop (length text)) "meeting" text)))))

(defun monday-of (universal-time)
  "The Monday of the week UNIVERSAL-TIME falls in, as (VALUES DAY MONTH YEAR)."
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (multiple-value-bind (s m h monday-day monday-month monday-year)
        (decode-universal-time (- (encode-universal-time 0 0 12 day month year)
                                   (* 86400 weekday)))
      (declare (ignore s m h))
      (values monday-day monday-month monday-year))))

(defun record-path (title)
  "Where a record called TITLE goes: Records/YYYY/MM/week-YYYY-MM-DD/ as the
original's vault tree had it, the week named by its Monday."
  (let ((now (get-universal-time)))
    (multiple-value-bind (monday-day monday-month monday-year) (monday-of now)
      (multiple-value-bind (second minute hour day month year) (decode-universal-time now)
        (declare (ignore second minute hour day))
        (merge-pathnames
         (make-pathname
          :directory (list :relative
                           (format nil "~4,'0d" year)
                           (format nil "~2,'0d" month)
                           (format nil "week-~4,'0d-~2,'0d-~2,'0d"
                                   monday-year monday-month monday-day))
          :name (slug-of title) :type "md")
         (records-root))))))

(defun save (title &key notes (include-transcript t))
  "One meeting record: the title, the notes, and the live transcript when
INCLUDE-TRANSCRIPT. Answers where it went."
  (let ((title (nlk:trimmed (or title ""))))
    (when (zerop (length title))
      (fail "meeting_save needs a title: what this meeting was about"))
    (let ((path (record-path title))
          (spoken (string-trim '(#\Space #\Tab #\Newline)
                                (file-text (transcript-file "rolling"))))
          (notes (nlk:trimmed (or notes ""))))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create :external-format :utf-8)
        (format out "# ~a~%~%" title)
        (format out "> ~a~%~%" (day-now))
        (when (plusp (length notes))
          (format out "## Notes~%~%~a~%~%" notes))
        (format out "## Transcript~%~%~a~%"
                (if (and include-transcript (plusp (length spoken)))
                    spoken
                    "(not captured)")))
      (format nil "saved ~a" (namestring path)))))

;;; --- the tools -----------------------------------------------------------------------

(nlk:define-contract meeting-status-request
  (:doc "Whether meeting capture is alive: how old the transcript is, how many
lines it holds, and which directory it reads.")
  ("verbose" :boolean :default nil
   :doc "also name the records root and the section's own thresholds"))

(nlk:define-contract meeting-transcript-request
  (:doc "Read the newest part of the live meeting transcript, with how fresh it
is. A STALE header means the capture stopped: say so instead of answering from
old words.")
  ("lines" :integer
   :doc "how many of the newest lines to return; 40 when absent")
  ("chars" :integer
   :doc "the newest characters instead, from the end; it wins over lines"))

(nlk:define-contract meeting-append-request
  (:doc "Append one transcribed line to the live meeting transcript, tagged
with where it was heard. An echo of a recent line is dropped rather than
doubled.")
  ("text" :text :required t
   :doc "the line, as it was said")
  ("source" :string :default "mic"
   :doc "where it came from: mic, audio (the other side of a call), phone, or any tag you like"))

(nlk:define-contract meeting-save-request
  (:doc "Save one meeting record under the records root: the title, the notes,
and the transcript. Answers the path it wrote.")
  ("title" :text :required t
   :doc "what the meeting was about; it names the file")
  ("notes" :text
   :doc "the summary to write under Notes: decisions, action items, open questions")
  ("include_transcript" :boolean :default t
   :doc "write the live transcript into the record"))

(defun tool-status (request)
  "The meeting_status tool."
  (status :verbose (meeting-status-request-verbose request)))

(defun tool-transcript (request)
  "The meeting_transcript tool."
  (transcript :lines (or (meeting-transcript-request-lines request) 40)
              :chars (meeting-transcript-request-chars request)))

(defun tool-append (request)
  "The meeting_append tool."
  (note-line (meeting-append-request-text request)
             :source (meeting-append-request-source request)))

(defun tool-save (request)
  "The meeting_save tool."
  (save (meeting-save-request-title request)
        :notes (meeting-save-request-notes request)
        :include-transcript (meeting-save-request-include-transcript request)))

;;; --- the operator's own door ---------------------------------------------------------

(defun run-slash (args session-id)
  "/meeting: where capture stands and what was last said; /meeting save TITLE
writes the record."
  (declare (ignore session-id))
  (let ((args (nlk:trimmed (or args ""))))
    (cond ((zerop (length args)) (status :verbose t))
          ((uiop:string-prefix-p "save " (string-downcase args))
           (save (nlk:trimmed (subseq args 5)) :include-transcript t))
          (t (transcript :lines 20)))))

;;; --- the words the model reads ------------------------------------------------------

(defparameter *primer*
  "Meeting Overlay is available through the nodecode-meeting cell: the live transcript of a
meeting, and the records saved from it. Four tools, and the same four calls in the MEETING
package from eval.
  (meeting_status &key verbose)
      Whether capture is alive: how old the transcript is, how many lines, where it lives.
  (meeting_transcript &key lines chars)
      The newest part of the transcript, with its age. STALE in the header means the capture
      stopped: say so rather than answering from old words.
  (meeting_append \"what was said\" &key source)
      One transcribed line, tagged mic, audio (the other side of a call), phone or any tag.
      Anything that can write a line feeds the transcript; an echo of a recent line is dropped.
  (meeting_save \"Title\" &key notes include-transcript)
      One markdown record under Records/YYYY/MM/week-.../: the title, the notes, the transcript.
Being a meeting copilot: read the transcript rather than guessing, and answer in one or two
sentences, because the user is reading mid-conversation. Never invent meeting content; when
the transcript is thin, say so in one line. \"What should I say?\" wants the phrasing to say
right now. A follow-up wants three or four questions worth asking next. A recap wants
decisions, action items and open questions, then meeting_save. ERROR: MEETING-ERROR names
the problem."
  "The words the model reads for `meeting'.")

;;; --- the declaration ----------------------------------------------------------------

(defun start ()
  "Make the two directories the cell writes into, so the first append and the
first save never race a missing parent."
  (ensure-directories-exist (state-dir))
  (ensure-directories-exist (records-root)))

(nle:define-cell meeting
  (:section ("meeting")
    (:guide "dir is the directory the transcript is read from and appended to; records is where a saved record goes (empty means Records/ inside dir); window_chars caps rolling.txt; stale_seconds is how old it may be before status calls the capture dead")
    ("dir" :path :default "~/.nodecode/meeting"
           :doc "the directory the live transcript is read from and appended to")
    ("records" :path :default ""
               :doc "where meeting records are saved; empty means Records/ inside dir")
    ("window_chars" :integer :default 20000 :min 500
                    :doc "how much of the newest transcript rolling.txt keeps")
    ("stale_seconds" :integer :default 30 :min 5
                     :doc "how old the transcript may be before status calls the capture dead"))
  (:settings #'read-settings)
  (:tool "meeting_status" (nlk:contract 'meeting-status-request) 'tool-status)
  (:tool "meeting_transcript" (nlk:contract 'meeting-transcript-request) 'tool-transcript)
  (:tool "meeting_append" (nlk:contract 'meeting-append-request) 'tool-append)
  (:tool "meeting_save" (nlk:contract 'meeting-save-request) 'tool-save)
  (:command "meeting" 'run-slash
            :catches 'meeting-error
            :description "The live meeting transcript, and a record saved from it"
            :argument-hint "[STATUS | SAVE TITLE]")
  (:help :meeting
         "meeting: the live meeting transcript and a record saved from it - (meeting:transcript :lines 30)"
         *primer*)
  (:start #'start))
