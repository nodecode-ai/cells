;;;; cell.lisp --- the agent sees what it just made.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Agenziamaia's `eyes' plugin for Hermes Agent, against
;;;; Nodecode's own seams. The seam is the :TOOL point, which wraps one tool
;;;; dispatch: OP is (:name TOOL-NAME :arguments HASH :call-id ID) and NEXT
;;;; runs the tool and answers its text. This cell reads that text -- and the
;;;; strings the call carries -- for image, video and sound paths written in
;;;; the last few minutes, measures each with the ffmpeg the machine already
;;;; has, and hands the model the tool's own answer with a note appended.
;;;;
;;;; Perception, never a block: the hook answers the tool's text whatever
;;;; happens, a file is measured once, at most a few a result, and no note
;;;; ever fails a call. Three tools and a command are the deliberate half: a
;;;; file the model names itself, and what the cell is set to.

(in-package #:nodecode-eyes)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "eyes measures what a tool just made, so the model knows before it says done.
When a tool result names an image, video or sound file written in the last
fresh_seconds, a measured note is appended to that result before the model
reads it: how much of the frame is filled, how much moves, how bright, how
loud, where the silences are, and for a video a contact sheet to look at.
A frame that is black, an empty canvas, a frozen clip and a silent voice-over
each get a WARNING naming what is wrong.
  (eyes:measure \"out.mp4\")   the same note for any file, older than the
      window included; a refusal names what could not be measured.
  eyes_measure                 the tool of the same thing: a path, and
      whether to write the contact sheet.
  eyes_status                  what the cell watches and what it has measured.
  /eyes [path]                 the operator's: no path answers the status.
Each ffprobe and ffmpeg call is capped by timeout_seconds, at most max_notes
files are measured per result, and nothing else reads the file. Needs ffmpeg
and ffprobe on the machine, which the section names."
  "What (help :eyes) answers while the cell runs.")

;;; --- the boundary the tool crosses ------------------------------------------------

(nlk:define-contract eyes-measure
  (:doc "Measure one image, video or sound file: how much of the frame is filled, how much moves, how bright, how loud, where the silences are, and for a video a contact sheet. Local ffprobe and ffmpeg only; the file is read, never written.")
  ("path" :text :required t :doc "the file, by full path or with a leading ~/")
  ("sheet" :boolean :default t :doc "write a contact sheet for a video"))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

;;; --- what has been measured ------------------------------------------------------

(defvar *seen* (make-hash-table :test #'equal)
  "A file's name -> the write date it was measured at, so one write gets one note.")

(defparameter +seen-limit+ 500
  "How many files the table remembers before it starts over.")

(defvar *notes-made* 0 "How many notes this start has appended.")
(defvar *last-note* nil "The newest note, for /eyes and eyes_status.")

(defun watched-tool-p (name)
  "Whether a call to NAME has its result read: the section's list, or any
tool's while the list is empty or says *."
  (let ((tools (setting :tools)))
    (or (null tools)
        (member "*" tools :test #'string=)
        (member name tools :test #'string-equal))))

(defun non-empty-p (file)
  (ignore-errors
    (plusp (with-open-file (in file :element-type '(unsigned-byte 8))
             (file-length in)))))

(defun fresh-p (file)
  "Whether FILE was written within the section's window and holds bytes."
  (let ((written (ignore-errors (file-write-date file))))
    (and written
         (<= (- (get-universal-time) written) (setting :fresh-seconds))
         (non-empty-p file))))

(defun measured-before-p (file)
  (equal (gethash (namestring file) *seen*) (ignore-errors (file-write-date file))))

(defun remember (file)
  (when (> (hash-table-count *seen*) +seen-limit+) (clrhash *seen*))
  (setf (gethash (namestring file) *seen*) (ignore-errors (file-write-date file))))

(defun note-one (path)
  "PATH's note, or NIL: a file that is gone, old, already measured or
unreadable is not a failure, only nothing to say."
  (let ((file (and (plusp (length path)) (probe-file (expand-path path)))))
    (when (and file (fresh-p file) (not (measured-before-p file)))
      (remember file)
      (let ((note (ignore-errors (see file))))
        (when note
          (incf *notes-made*)
          (setf *last-note* note)
          note)))))

(defun append-notes (op text)
  "TEXT, the tool's own answer, with a measured note for each file it names."
  (if (not (watched-tool-p (string (or (getf op :name) ""))))
      text
      (let* ((scan (concatenate 'string text
                                (string #\Newline)
                                (argument-text (getf op :arguments))))
             (notes '()))
        (dolist (path (scan-paths scan))
          (when (>= (length notes) (setting :max-notes)) (return))
          (let ((note (ignore-errors (note-one path))))
            (when note (push note notes))))
        (if notes
            (format nil "~a~%~{~a~}" text (nreverse notes))
            text))))

;;; --- the point ------------------------------------------------------------------

(defun observe-tool (op next)
  "The :TOOL point: the tool's answer, and what it made, in one text. A cell
that is not started, or a tool this cell does not watch, answers unchanged."
  (let ((text (funcall next op)))
    (if (and (stringp text) *eyes*)
        (handler-case (append-notes op text)
          (error () text))
        text)))

;;; --- the two tools and the command -----------------------------------------------

(defun measure-tool (call)
  "eyes_measure: the note for one file."
  (or (and (plusp (length (nlk:trimmed (eyes-measure-path call))))
           (measure (eyes-measure-path call) :sheet (eyes-measure-sheet call)))
      (fail "no path was given; name the image, video or sound file to measure")))

(defun status-text ()
  "What /eyes answers, and eyes_status: the section as the cell runs on it,
and what it has measured since it started."
  (running-settings)
  (format nil "eyes: ~a and ~a, ~d s a call at most; measures a file written in the last ~
~d s, at most ~d a result, for ~:[every tool~;~:*~{~a~^, ~}~]; sheets to ~a; ~
~d note~:p since start~@[; last: ~a~]"
          (ffmpeg-binary) (ffprobe-binary) (setting :timeout-seconds)
          (setting :fresh-seconds) (setting :max-notes) (setting :tools)
          (namestring (or (sheet-directory) ""))
          *notes-made* (nlk:trimmed (or *last-note* ""))))

(defun status-tool (arguments)
  (declare (ignore arguments))
  (status-text))

(defun run-slash (args session-id)
  "One /eyes: no argument answers the status, a path measures that file and
says why when it cannot."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (status-text)
        (handler-case (measure text :sheet nil)
          (error (condition) (princ-to-string condition))))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell eyes
  (:section ("eyes")
    (:guide "sheet_dir is the one thing to set: a folder for the contact-sheet PNGs, made if it is not there; ffmpeg is this machine's, and ffprobe is looked for beside it; next step: /eyes")
    ("sheet_dir" :directory :required t
                 :doc "folder the contact sheets are written to, ~/ expanded; it is made if missing")
    ("ffmpeg" :string :default "ffmpeg"
              :doc "the ffmpeg binary, or its full path")
    ("ffprobe" :string
               :doc "the ffprobe binary; empty means ffprobe, or ffprobe beside ffmpeg")
    ("fresh_seconds" :integer :default 300 :min 1
                     :doc "how recently a file must have been written for a tool result to be measured")
    ("max_notes" :integer :default 3 :min 1
                 :doc "how many files one tool result may be measured for")
    ("timeout_seconds" :integer :default 25 :min 1
                       :doc "one ffprobe or ffmpeg call's ceiling, in seconds")
    ("tools" :list :default '("eval")
             :doc "tool names whose results are read; empty or * reads every tool's"))
  (:tool "eyes_measure" (nlk:contract 'eyes-measure) 'measure-tool)
  (:tool "eyes_status"
         (no-arguments "What the eyes cell measures, on which binaries and with what limits, and how many notes it has appended since it started.")
         'status-tool)
  (:help :eyes
         "eyes: a measured note on every image, video or sound a tool just made - frame filled, motion, brightness, loudness, silences"
         +primer+)
  (:command "eyes" 'run-slash
            :description "Measure a file the way the tool results are measured"
            :argument-hint "[path]")
  (:hook :tool #'observe-tool)
  (:start (lambda ()
            (clrhash *seen*)
            (setf *notes-made* 0
                  *last-note* nil))))
