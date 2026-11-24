;;;; voice.lisp --- the microphone, the speaker, and the folder they leave
;;;; behind.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here talks to OpenAI: this is the machine's half of a voice
;;;; conversation -- a recorder command that writes a WAV, a player command
;;;; that says one, and the transcript the two leave in the folder. The
;;;; plugin this rewrites ran a WebRTC call inside a desktop app, where the
;;;; browser owned both; in an image the microphone is a child process, and
;;;; the command that drives it is the one the operator already has
;;;; (ffmpeg, sox's rec, arecord, afplay, mpv, ffplay, paplay).
;;;;
;;;; Every command is a list of words, with %f standing for the file and %s
;;;; for the seconds, so an operator whose device needs a flag of its own can
;;;; name the whole command in the section instead of hoping detection finds
;;;; it.

(in-package #:nodecode-live-voice)

;;; --- octets -----------------------------------------------------------------

(defun ascii (text)
  "TEXT's characters as the octets a container header stores them as."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun octets-of (data)
  "DATA as octets: NLK:HTTP answers a string when the peer sent text."
  (etypecase data
    (string (map '(vector (unsigned-byte 8)) #'char-code data))
    ((array (unsigned-byte 8) (*)) data)
    ((array t (*)) data)
    (null (make-array 0 :element-type '(unsigned-byte 8)))))

(defun octets-text (data)
  "DATA, text or octets, as a string."
  (if (stringp data) data (map 'string #'code-char data)))

(defun audio-p (octets extension)
  "Whether OCTETS begin like the container EXTENSION names. The API's MIME
header is not trusted: a 200 carrying a JSON error is what this catches."
  (let ((n (length octets)))
    (flet ((at (offset &rest bytes)
             (and (>= n (+ offset (length bytes)))
                  (loop for byte in bytes
                        for index from offset
                        always (= byte (aref octets index))))))
      (cond ((string= extension "wav")
             (and (at 0 #x52 #x49 #x46 #x46) (at 8 #x57 #x41 #x56 #x45)))
            ((string= extension "mp3")
             (or (at 0 #x49 #x44 #x33)
                 (and (>= n 2) (= #xFF (aref octets 0))
                      (= #xE0 (logand #xE0 (aref octets 1))))))
            ((string= extension "flac") (at 0 #x66 #x4C #x61 #x43))
            ((string= extension "opus") (at 0 #x4F #x67 #x67 #x53))
            ((string= extension "aac")
             (and (>= n 2) (= #xFF (aref octets 0)) (= #xF0 (logand #xF0 (aref octets 1)))))
            (t (plusp n))))))

;;; --- what this machine can run -------------------------------------------------

(defun on-path-p (name)
  "Whether NAME is a command on this machine's PATH."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                            :separator #+win32 ";" #-win32 ":")
        for folder = (and (plusp (length directory))
                          (ignore-errors (uiop:parse-native-namestring directory :ensure-directory t)))
        thereis (and folder (probe-file (merge-pathnames name folder)))))

(defvar *path-lookup* #'on-path-p
  "How a command on PATH is found. A test binds this; nothing else does.")

(defun this-platform ()
  "The platform keyword the recorder table is keyed on."
  (or #+darwin :darwin #+linux :linux #+win32 :win32 nil))

(defparameter +recorders+
  '((:linux "arecord" ("-q" "-f" "S16_LE" "-r" "16000" "-c" "1" "-d" "%s" "%f"))
    (:linux "rec" ("-q" "-r" "16000" "-c" "1" "-b" "16" "%f" "trim" "0" "%s"))
    (:linux "ffmpeg" ("-nostdin" "-y" "-loglevel" "error" "-f" "pulse" "-i" "default"
                       "-t" "%s" "-ac" "1" "-ar" "16000" "%f"))
    (:darwin "rec" ("-q" "-r" "16000" "-c" "1" "-b" "16" "%f" "trim" "0" "%s"))
    (:darwin "ffmpeg" ("-nostdin" "-y" "-loglevel" "error" "-f" "avfoundation" "-i" ":0"
                        "-t" "%s" "-ac" "1" "-ar" "16000" "%f"))
    (:win32 "ffmpeg" ("-nostdin" "-y" "-loglevel" "error" "-f" "dshow" "-i" "audio=default"
                       "-t" "%s" "-ac" "1" "-ar" "16000" "%f")))
  "(PLATFORM PROGRAM ARGV-TEMPLATE x3): the recorders this cell knows, in the
order it tries them. The first on PATH is the one a recording runs, unless
the section names a command of its own.")

(defparameter +players+
  '(("afplay" "%f")
    ("mpv" "--no-video" "--really-quiet" "%f")
    ("ffplay" "-nodisp" "-loglevel" "error" "-autoexit" "%f")
    ("play" "%f")
    ("paplay" "%f")
    ("aplay" "%f"))
  "(PROGRAM ARGV-TEMPLATE x2): the players this cell knows, in the order it
tries them. afplay is macOS's, mpv and ffplay read every format the API
returns, sox's play too, paplay and aplay are Linux's (WAV only).")

(defun replace-all (text old new)
  "TEXT with every OLD replaced by NEW."
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old text :start2 start)
          do (if at
                 (progn (write-string text out :start start :end at)
                        (write-string new out)
                        (setf start (+ at (length old))))
                 (progn (write-string text out :start start)
                        (return))))))

(defun words (text)
  "TEXT split into the words a command runs as."
  (uiop:split-string text :separator '(#\Space #\Tab #\Newline)))

(defun template-argv (template file seconds)
  "TEMPLATE -- a command line string or a list of words -- with %f the file
and %s the seconds, as the argv a child process runs. A template naming
neither gets the file appended, the way a player takes it."
  (let* ((parts (if (listp template) template (words template)))
         (named (some (lambda (word) (or (search "%f" word) (search "%s" word))) parts))
         (argv (mapcar (lambda (word)
                         (replace-all (replace-all word "%f" (uiop:native-namestring file))
                                      "%s" (princ-to-string seconds)))
                       parts)))
    (if named argv (append argv (list (uiop:native-namestring file))))))

(defun last-line (text)
  "The last non-empty line of TEXT, clipped: a child's own sentence for why
it failed, without its whole log."
  (let* ((lines (remove-if (lambda (line) (zerop (length (nlk:trimmed line))))
                            (uiop:split-string (or text "") :separator '(#\Newline))))
         (last (car (last lines))))
    (and last (nlk:clip (nlk:trimmed last) 300))))

;;; --- a recording ---------------------------------------------------------------

(defun recorder-template ()
  "The argv template that records on this machine, %f and %s still in it, or
a refusal naming both ways to give one."
  (let ((custom (nlk:trimmed (or (setting :recorder) ""))))
    (if (plusp (length custom))
        (words custom)
        (or (loop for (platform program template) in +recorders+
                  when (and (eq platform (this-platform))
                            (funcall *path-lookup* program))
                    return template)
            (fail "no recorder found on this machine: install ffmpeg, sox (rec) or alsa-utils (arecord), or set live-voice.recorder to a command that writes a WAV to %f and stops after %s seconds")))))

(defun stamp ()
  "The local time as 20261108_101500, the way a file name carries it."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

(defun fresh-id ()
  "Eight hex digits, so two files of one second never collide."
  (format nil "~8,'0x" (random (expt 16 8))))

(defun record (seconds)
  "Record SECONDS from the microphone into a new WAV of this profile's folder,
and answer its pathname. The child is bounded by the length asked for plus
fifteen seconds, so a recorder waiting on a device that answers nothing
still returns."
  (let* ((path (merge-pathnames (format nil "heard_~a_~a.wav" (stamp) (fresh-id))
                                 (setting :directory)))
         (argv (template-argv (recorder-template) path seconds)))
    (ensure-directories-exist path)
    (multiple-value-bind (output errors status)
        (nlk:run-bounded argv :seconds (+ 15 seconds))
      (declare (ignore output))
      (unless (eql status 0)
        (fail "the recorder (~{~a~^ ~}) ~:[exited with status ~a~;did not finish in ~a seconds~]~@[: ~a~]"
              argv (eq status :timeout)
              (if (eq status :timeout) (+ 15 seconds) status)
              (last-line errors)))
      (unless (and (probe-file path)
                   (plusp (or (ignore-errors (nlk:file-bytes path)) 0)))
        (fail "the recorder (~{~a~^ ~}) wrote no audio: check that a microphone is attached and that its command names the right device (live-voice.recorder)"
              argv))
      path)))

;;; --- a spoken file ---------------------------------------------------------------

(defun player-template ()
  "The argv template that plays a file on this machine, or NIL when there is
no player to play it with."
  (let ((custom (nlk:trimmed (or (setting :player) ""))))
    (if (plusp (length custom))
        (words custom)
        (loop for (program . template) in +players+
              when (funcall *path-lookup* program)
                return template))))

(defun play (path)
  "Play PATH here now => (values PLAYED-P NOTE). A machine with no player is
not an error: the file is saved either way, and the note says how to hear
it."
  (let ((template (player-template)))
    (if (null template)
        (values nil "no player found on this machine: set live-voice.player to a command that plays %f")
        (let ((argv (template-argv template path 0)))
          (multiple-value-bind (output errors status)
              (nlk:run-bounded argv :seconds 300)
            (declare (ignore output))
            (if (eql status 0)
                (values t nil)
                (values nil (format nil "the player (~{~a~^ ~}) ~:[exited with status ~a~;ran past five minutes~]~@[: ~a~]"
                                    argv (eq status :timeout)
                                    (if (eq status :timeout) 300 status)
                                    (last-line errors)))))))))

;;; --- the transcript -------------------------------------------------------------

(defun transcript-path ()
  "The file the transcript is kept in: one line per thing heard or said,
beside the audio."
  (merge-pathnames "transcript.jsonl" (setting :directory)))

(defun write-transcript (entry)
  "Append ENTRY, a JSON object, as one line of the transcript. A profile
that loses the line loses a line of history, never the audio or the answer."
  (let ((path (transcript-path)))
    (ignore-errors
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :append
                                 :if-does-not-exist :create)
        (write-line (nlk:encode-json-object entry) out)))))

(defun transcript-entries (&optional (limit 8))
  "The last LIMIT lines of the transcript, newest last, each decoded; a torn
line is skipped."
  (let ((path (transcript-path)))
    (when (probe-file path)
      (let ((lines (with-open-file (in path)
                     (loop for line = (read-line in nil nil)
                           while line
                           when (plusp (length (nlk:trimmed line))) collect line))))
        (loop for line in (last lines limit)
              for entry = (ignore-errors (nlk:decode-json line))
              when (hash-table-p entry) collect entry)))))

(defun entry-line (entry)
  "One transcript entry as the line /voice shows."
  (let ((kind (nlk:json-value entry :string "kind"))
        (at (nlk:json-value entry :string "at"))
        (text (or (nlk:json-value entry :string "text") "")))
    (format nil "~@[~a  ~]~a: ~a"
            (and at (subseq at 0 (min 19 (length at))))
            (or kind "?")
            (nlk:clip (nlk:one-line text) 100))))
