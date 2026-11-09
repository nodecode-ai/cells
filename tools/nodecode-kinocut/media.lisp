;;;; media.lisp --- the local video work: ffprobe in, ffmpeg out, a receipt.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here is a service. Every call runs the ffmpeg the operator
;;;; already has, on a file the operator already has, and writes into the one
;;;; folder the section names: the source is read and never modified, an
;;;; output is new and never overwritten, and every render leaves one line in
;;;; receipts.jsonl naming the exact argv that made it.

(in-package #:nodecode-kinocut)

;;; --- the one seam that runs a program -----------------------------------------

(defun run-argv (argv)
  "Run ARGV: (values OUTPUT STATUS). A program that cannot start answers
status -1 and the condition's text as its output, so the caller can say which
binary it wanted and why nothing ran."
  (handler-case
      (multiple-value-bind (out err status)
          (uiop:run-program argv
                            :output :string
                            :error-output :string
                            :ignore-error-status t
                            :timeout (setting :timeout-seconds))
        (declare (ignore err))
        (values (or out "") (or status 0)))
    (error (condition)
      (values (princ-to-string condition) -1))))

(defvar *run* #'run-argv
  "The one seam that runs a program: (ARGV) => (values OUTPUT STATUS). Tests
replace it; nothing else does.")

(defun exec (argv) (funcall *run* argv))

(defun program (key)
  "The binary the section names for KEY (:ffmpeg, :ffprobe), or a refusal."
  (let ((name (nlk:trimmed (or (setting key) ""))))
    (when (zerop (length name))
      (fail "the kinocut section's ~(~a~) is empty; name the binary, such as \"~(~a~)\"" key key))
    name))

;;; --- small readings -----------------------------------------------------------

(defun last-lines (text limit)
  "TEXT trimmed, keeping its last LIMIT characters: a command's own words,
short enough to ride a tool answer."
  (let* ((text (nlk:trimmed (or text "")))
         (length (length text)))
    (if (> length limit) (subseq text (- length limit)) text)))

(defun first-line (text)
  (let ((text (nlk:trimmed (or text ""))))
    (subseq text 0 (or (position #\Newline text) (length text)))))

(defun number-text (value)
  "VALUE as an ffmpeg argument: an integer as itself, a real to three
places."
  (if (integerp value)
      (princ-to-string value)
      (format nil "~,3f" (coerce value 'double-float))))

(defun to-number (value)
  "VALUE as a number, or NIL: ffprobe answers every duration as a string."
  (typecase value
    (number value)
    (string (let ((*read-eval* nil))
              (ignore-errors (read-from-string (nlk:trimmed value)))))
    (t nil)))

(defun file-name (path)
  (or (ignore-errors (file-namestring path)) (namestring path)))

(defun stamp (&optional (now (nlk:unix-now)))
  (multiple-value-bind (second minute hour day month year) (decode-universal-time now 0)
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun short-id ()
  (format nil "~10,'0x" (random (expt 16 10))))

(defun slug (source)
  "SOURCE's name as a file-safe word: lower case, letters and digits, a dash
for everything else."
  (let* ((name (or (pathname-name (pathname source)) "clip"))
         (slug (string-downcase
                (with-output-to-string (out)
                  (loop for char across name
                        do (write-char (if (alphanumericp char) char #\-) out)))))
         (slug (string-trim "-" slug)))
    (subseq (if (plusp (length slug)) slug "clip") 0 (min 32 (length slug)))))

;;; --- where a render goes ------------------------------------------------------

(defun output-directory ()
  "The folder every render lands in, made if it is not there, with the
receipts log beside the renders; NIL while the section names none."
  (let ((directory (setting :output-dir)))
    (when directory
      (ensure-directories-exist (merge-pathnames "receipts.jsonl" directory))
      directory)))

(defun render-directory ()
  (or (output-directory)
      (fail "no folder for renders: set kinocut.output_dir in /settings, such as a Videos/cuts folder under your home; nothing was written")))

(defun source-file (text)
  "TEXT as an existing file, or a refusal naming what was not there."
  (let* ((text (nlk:trimmed (or text "")))
         (path (and (plusp (length text)) (ignore-errors (probe-file text)))))
    (cond ((null path)
           (fail "no file at ~a; give the full path of the video"
                 (if (plusp (length text)) text "an empty path")))
          ((uiop:directory-pathname-p path)
           (fail "~a is a folder, not a video file" (namestring path)))
          (t path))))

(defun new-output (directory source suffix extension)
  "A path in DIRECTORY no file holds: the source's name, what was done to it,
the minute and a short id."
  (merge-pathnames (format nil "kino_~a-~a_~a_~a.~a"
                            (slug source) suffix (stamp) (short-id) extension)
                   directory))

;;; --- ffprobe -----------------------------------------------------------------

(defun probe-json (path)
  "FFPROBE's JSON for PATH, or a refusal carrying what it said."
  (multiple-value-bind (out status)
      (exec (list (program :ffprobe) "-v" "error" "-print_format" "json"
                   "-show_format" "-show_streams" (namestring path)))
    (unless (zerop status)
      (fail "ffprobe could not read ~a (exit ~a): ~a" (file-name path) status (last-lines out 300)))
    (let ((data (ignore-errors (nlk:decode-json out))))
      (unless (hash-table-p data)
        (fail "ffprobe answered no JSON for ~a: ~a" (file-name path) (last-lines out 200)))
      data)))

(defun probe-streams (data)
  (coerce (or (nlk:json-value data :array "streams") #()) 'list))

(defun probe-stream (data codec-type)
  (find codec-type (probe-streams data)
        :key (lambda (stream) (nlk:json-value stream :string "codec_type"))
        :test #'equal))

(defun probe-duration (data)
  "The file's length in seconds, from its container, else its video stream;
0 when neither says."
  (or (to-number (nlk:json-value data :any "format" "duration"))
      (to-number (nlk:json-value (probe-stream data "video") :any "duration"))
      0))

(defun probe-size (data)
  (or (to-number (nlk:json-value data :any "format" "size")) 0))

(defun probe-width (data)
  (to-number (nlk:json-value (probe-stream data "video") :any "width")))

(defun probe-height (data)
  (to-number (nlk:json-value (probe-stream data "video") :any "height")))

(defun probe-fps (data)
  "Frames a second, from ffprobe's rational rate; NIL when it is unreadable
or 0/0, as a still or an audio-only file answers."
  (let ((rate (nlk:json-value (probe-stream data "video") :string "avg_frame_rate")))
    (when (and rate (plusp (length rate)) (not (equal rate "0/0")))
      (let ((slash (position #\/ rate)))
        (if slash
            (let ((top (to-number (subseq rate 0 slash)))
                  (bottom (to-number (subseq rate (1+ slash)))))
              (and top bottom (plusp bottom) (/ top bottom)))
            (to-number rate))))))

;;; --- the readings a person and a model both want -------------------------------

(defun seconds-text (seconds)
  (if (and (numberp seconds) (plusp seconds))
      (format nil "~,2f s" seconds)
      "an unreadable length"))

(defun frame-text (data)
  (let ((width (probe-width data)) (height (probe-height data)))
    (if (and width height) (format nil "~ax~a" width height) "an unreadable frame")))

(defun rate-text (data)
  (let ((fps (probe-fps data)))
    (and fps (plusp fps) (format nil "~,2f fps" fps))))

(defun audio-text (data)
  "The audio stream in one phrase, or `no audio'."
  (let ((stream (probe-stream data "audio")))
    (if stream
        (format nil "~@[~a~]~@[ ~a ch~]~@[ ~a Hz~]"
                (nlk:json-value stream :string "codec_name")
                (nlk:json-value stream :any "channels")
                (nlk:json-value stream :string "sample_rate"))
        "no audio")))

;;; --- one render -------------------------------------------------------------

(defun render-argv (argv output)
  "Run one render, then prove the file is there: (values PATHNAME BYTES)."
  (let ((output (pathname output)))
    (when (probe-file output)
      (fail "~a is already there, and a render never overwrites another" (file-name output)))
    (ensure-directories-exist output)
    (multiple-value-bind (out status) (exec argv)
      (unless (zerop status)
        (fail "ffmpeg failed (exit ~a): ~a" status (last-lines out 400)))
      (let ((file (probe-file output)))
        (unless file
          (fail "ffmpeg reported success but wrote no file at ~a: ~a"
                (file-name output) (last-lines out 300)))
        (values file
                (with-open-file (in file :element-type '(unsigned-byte 8))
                  (file-length in)))))))

;;; --- the receipts -----------------------------------------------------------------

(defun receipts-path ()
  (merge-pathnames "receipts.jsonl" (render-directory)))

(defun record-receipt (kind source argv output bytes &optional detail)
  "One line in this profile's receipts log: what was asked, what was run,
what came out."
  (with-open-file (out (receipts-path) :direction :output :if-exists :append
                                         :if-does-not-exist :create)
    (write-line (nlk:encode-json-object
                  (nlk:json-object "id" (short-id)
                                   "kind" kind
                                   "created_at" (nlk:iso-time (nlk:unix-now))
                                   "source" (namestring source)
                                   "output" (namestring output)
                                   "bytes" bytes
                                   "argv" (coerce argv 'vector)
                                   :opt "detail" detail))
                out))
  output)

(defun receipts ()
  "Every receipt this profile kept, oldest first; a torn line is skipped
rather than failing the command that reads the log."
  (let ((path (ignore-errors (receipts-path))))
    (if (and path (probe-file path))
        (with-open-file (in path)
          (loop for line = (read-line in nil nil)
                while line
                for data = (ignore-errors (nlk:decode-json line))
                when (hash-table-p data) collect data))
        '())))

(defun receipt-line (data)
  (format nil "~a  ~a  ~a  ~a~%"
          (or (nlk:json-value data :string "created_at") "?")
          (or (nlk:json-value data :string "kind") "?")
          (or (nlk:json-value data :string "output") "?")
          (nlk:size-text (or (nlk:json-value data :any "bytes") 0))))

(defun find-receipt (argument)
  "The newest receipt ARGUMENT names by id or by words in its output path."
  (find-if (lambda (data)
             (or (uiop:string-prefix-p argument (or (nlk:json-value data :string "id") ""))
                 (search (string-downcase argument)
                         (string-downcase (or (nlk:json-value data :string "output") "")))))
           (reverse (receipts))))

(defun receipt-text (data)
  (with-output-to-string (out)
    (format out "~a ~a~%" (nlk:json-value data :string "kind") (nlk:json-value data :string "id"))
    (format out "  source: ~a~%" (nlk:json-value data :string "source"))
    (format out "  output: ~a~%" (nlk:json-value data :string "output"))
    (format out "  ~a, ~a~%" (nlk:json-value data :string "created_at")
            (nlk:size-text (or (nlk:json-value data :any "bytes") 0)))
    (let ((detail (nlk:json-value data :string "detail")))
      (when detail (format out "  ~a~%" detail)))
    (format out "  argv: ~{~a~^ ~}~%" (coerce (or (nlk:json-value data :array "argv") #()) 'list))))

;;; --- doctor, and the /kino command -----------------------------------------------

(defun version-line (name)
  "NAME's first -version line, or how it failed; nothing is written."
  (if (zerop (length (nlk:trimmed (or name ""))))
      "not named in the kinocut section"
      (multiple-value-bind (out status) (exec (list name "-version"))
        (if (zerop status)
            (first-line out)
            (format nil "not runnable (~a)" (last-lines out 120))))))

(defun doctor-text ()
  (with-output-to-string (out)
    (format out "kinocut: local video work on the ffmpeg this machine already has~%")
    (format out "  ffmpeg:   ~a~%" (version-line (nlk:trimmed (or (setting :ffmpeg) ""))))
    (format out "  ffprobe:  ~a~%" (version-line (nlk:trimmed (or (setting :ffprobe) ""))))
    (format out "  renders:  ~a~%"
            (or (ignore-errors (namestring (output-directory)))
                "no output_dir yet -- set kinocut.output_dir in /settings"))
    (let ((receipts (receipts)))
      (format out "  receipts: ~d~%" (length receipts))
      (dolist (data (last receipts (min 5 (length receipts))))
        (format out "    ~a" (receipt-line data))))))

(defun run-slash (args session-id)
  "The /kino command: no argument is the doctor and the last receipts, an
argument is one receipt by id or by words in its file name."
  (declare (ignore session-id))
  (handler-case
      (let ((argument (nlk:trimmed (or args ""))))
        (if (zerop (length argument))
            (doctor-text)
            (let ((found (find-receipt argument)))
              (if found
                  (receipt-text found)
                  (format nil "no receipt matches ~a; /kino lists the last ones~%" argument)))))
    (kinocut-error (condition)
      (format nil "kinocut: ~a~%" (kinocut-error-detail condition)))))
