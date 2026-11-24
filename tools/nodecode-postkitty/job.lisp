;;;; job.lisp --- probing a file, the job folder on disk, the checks, the render.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A job is a folder under the output directory: manifest.sexp says what
;;;; went in and what came out, ready/<platform>/ holds the files and the copy,
;;;; failed/ holds what did not pass, and board.html is the page of the whole
;;;; job. The user's files are read and never rewritten.

(in-package #:nodecode-postkitty)

;;;; --- what the cell runs on -------------------------------------------------

(defvar *output-dir* nil
  "The `output_dir' setting: where job folders go, empty for beside the working directory.")

(defvar *default-platforms* nil
  "The `default_platforms' setting: the platforms a scan takes when none is named.")

(defvar *loudness-lufs* -14
  "The `loudness_lufs' setting: the audio target of a rendered video.")

;;;; --- reading a file's own header -------------------------------------------

(defparameter *image-extensions* '("jpg" "jpeg" "png" "gif" "webp" "heic"))

(defparameter *video-extensions* '("mp4" "mov" "webm" "m4v" "avi" "mkv"))

(defun file-extension (path)
  "PATH's extension, lowercased."
  (string-downcase (or (pathname-type path) "")))

(defun file-kind (path)
  "\"image\", \"video\", or NIL for a file this cell does not read."
  (let ((extension (file-extension path)))
    (cond ((member extension *image-extensions* :test #'string=) "image")
          ((member extension *video-extensions* :test #'string=) "video")
          (t nil))))

(defun read-bytes (path &optional (limit (* 4 1024 1024)))
  "At most LIMIT of PATH's bytes, or NIL when it is not a readable file."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (when in
          (let* ((size (min limit (file-length in)))
                 (data (make-array size :element-type '(unsigned-byte 8))))
            (read-sequence data in)
            data)))
    (error () nil)))

(defun ascii= (data offset string)
  (and (<= (+ offset (length string)) (length data))
       (loop for index below (length string)
             always (= (aref data (+ offset index)) (char-code (char string index))))))

(defun be16 (data offset)
  (+ (* 256 (aref data offset)) (aref data (1+ offset))))

(defun le16 (data offset)
  (+ (aref data offset) (* 256 (aref data (1+ offset)))))

(defun be32 (data offset)
  (+ (* 16777216 (aref data offset)) (* 65536 (aref data (1+ offset)))
     (* 256 (aref data (+ offset 2))) (aref data (+ offset 3))))

(defun be24 (data offset)
  (+ (aref data offset) (* 256 (aref data (1+ offset))) (* 65536 (aref data (+ offset 2)))))

(defun jpeg-size (data)
  "A JPEG's (values WIDTH HEIGHT), read at its start-of-frame marker."
  (let ((index 2) (size (length data)) (found nil))
    (loop while (and (not found) (< index (- size 9))) do
      (if (and (= (aref data index) #xFF)
               (let ((marker (aref data (1+ index))))
                 (and (>= marker #xC0) (<= marker #xCF)
                      (not (member marker '(#xC4 #xC8 #xCC))))))
          (setf found (cons (be16 data (+ index 7)) (be16 data (+ index 5))))
          (if (= (aref data index) #xFF)
              (let ((marker (aref data (1+ index))))
                (if (or (= marker #xD8) (= marker #x01) (and (>= marker #xD0) (<= marker #xD7)))
                    (incf index 2)
                    (incf index (+ 2 (be16 data (+ index 2))))))
              (incf index 1))))
    (values (car found) (cdr found))))

(defun image-size (path)
  "(values WIDTH HEIGHT) from the file's own header, NIL NIL for a format this does not read."
  (let ((data (read-bytes path)))
    (when (and data (> (length data) 32))
      (cond ((and (ascii= data 1 "PNG") (>= (length data) 24)) (values (be32 data 16) (be32 data 20)))
            ((or (ascii= data 0 "GIF87a") (ascii= data 0 "GIF89a")) (values (le16 data 6) (le16 data 8)))
            ((and (= (aref data 0) #xFF) (= (aref data 1) #xD8)) (jpeg-size data))
            ((and (ascii= data 0 "RIFF") (ascii= data 8 "WEBP") (ascii= data 12 "VP8X"))
             (values (1+ (be24 data 24)) (1+ (be24 data 27))))
            (t (values nil nil))))))

(defun parse-number (text)
  "TEXT as a number, or NIL: ffprobe writes a rate as 30000/1001 and a length as 12.345."
  (when (and text (plusp (length text)))
    (let ((slash (position #\/ text)))
      (handler-case
          (if slash
              (/ (read-from-string text) (read-from-string (subseq text (1+ slash))))
              (read-from-string text))
        (error () nil)))))

(defun ffprobe-info (path)
  "ffprobe's own probe when the box has one: a plist of :width, :height, :fps, :duration
and :has-audio, or NIL."
  (let ((executable (nlk::executable-on-path "ffprobe")))
    (when executable
      (handler-case
          (multiple-value-bind (output error status)
              (nlk:run-bounded
               (list (uiop:native-namestring executable) "-v" "error"
                     "-show_entries" "stream=width,height,avg_frame_rate,codec_type:format=duration"
                     "-of" "json" (uiop:native-namestring path))
               :seconds 20)
            (declare (ignore error))
            (when (eql status 0)
              (let* ((document (ignore-errors (nlk:decode-json output)))
                     (streams (nlk:json-array document "streams"))
                     (video (loop for stream across streams
                                  when (equal "video" (nlk:json-value stream :string "codec_type"))
                                    return stream)))
                (when video
                  (list :width (nlk:json-value video :integer "width")
                        :height (nlk:json-value video :integer "height")
                        :fps (parse-number (nlk:json-value video :string "avg_frame_rate"))
                        :duration (parse-number (nlk:json-value document :string "format" "duration"))
                        :has-audio (loop for stream across streams
                                         thereis (equal "audio" (nlk:json-value stream :string "codec_type"))))))))
        (error () nil)))))

(defun probe-input (path id)
  "PATH as a job records it: its kind, size, length, bytes, and a note when a media tool was missing."
  (let* ((truename (handler-case (truename path) (error () nil)))
         (kind (file-kind path)))
    (unless truename (error "~a is not a file" (uiop:native-namestring path)))
    (unless kind (error "~a is neither a photo nor a video this cell reads" (file-namestring path)))
    (let ((bytes (with-open-file (in truename :element-type '(unsigned-byte 8)) (file-length in)))
          (notes '()))
      (multiple-value-bind (width height)
          (if (string= kind "image") (image-size truename) (values nil nil))
        (let ((probed (when (string= kind "video") (ffprobe-info truename))))
          (when (and (string= kind "image") (null width))
            (push "the photo's size could not be read from its header" notes))
          (when (and (string= kind "video") (null probed))
            (push "ffprobe is not on PATH, so the clip's size and length are unknown" notes))
          (list :id id :path (namestring truename) :name (file-namestring truename)
                :kind kind :format (file-extension truename) :bytes bytes
                :width (or width (getf probed :width)) :height (or height (getf probed :height))
                :duration (getf probed :duration) :fps (getf probed :fps)
                :has-audio (getf probed :has-audio) :notes notes))))))

;;;; --- the job folder ----------------------------------------------------------

(defvar *job-counter* 0)

(defun output-directory ()
  "Where job folders go: the setting, else ./postkitty beside the working directory."
  (let ((configured (or *output-dir* "")))
    (if (plusp (length configured))
        (uiop:ensure-directory-pathname (uiop:parse-native-namestring configured))
        (merge-pathnames "postkitty/" (uiop:getcwd)))))

(defun job-root (id)
  "The folder of job ID, refusing a name that is not one."
  (when (or (zerop (length id)) (find #\/ id) (find #\\ id)
            (member id '("." "..") :test #'string=))
    (error "not a job id: ~a" id))
  (merge-pathnames (format nil "~a/" id) (output-directory)))

(defun job-file (id name)
  (merge-pathnames name (job-root id)))

(defun manifest-file (id)
  (job-file id "manifest.sexp"))

(defun read-form (file)
  "FILE read as one Lisp form, or NIL."
  (handler-case
      (when (probe-file file)
        (with-open-file (in file :if-does-not-exist nil)
          (when in
            (with-standard-io-syntax
              (let ((*read-eval* nil))
                (read in nil nil)))))) 
    (error () nil)))

(defun write-form (file value)
  "VALUE written to FILE as one Lisp form, the folders made."
  (ensure-directories-exist file)
  (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t) (*print-length* nil) (*print-level* nil))
        (prin1 value out))))
  file)

(defun new-job-id ()
  (format nil "p~d-~d" (get-universal-time) (incf *job-counter*)))

(defun create-job (&optional id)
  "A job folder, a new one or the ID given; answers its id."
  (let ((id (or id (new-job-id))))
    (job-root id)
    (ensure-directories-exist (job-file id "manifest.sexp"))
    id))

(defun open-job (id)
  "The job ID, when its manifest is there."
  (unless (and id (plusp (length id))) (error "'job' is required; run postkitty_scan first"))
  (unless (probe-file (manifest-file id))
    (error "no job ~a under ~a; run postkitty_scan first" id (uiop:native-namestring (output-directory))))
  id)

(defun load-manifest (id)
  (or (read-form (manifest-file id)) (list :id id :inputs nil :targets nil)))

(defun save-manifest (id manifest)
  (write-form (manifest-file id) manifest)
  manifest)

(defun load-scan (id)
  (read-form (job-file id "scan.sexp")))

(defun save-scan (id scan)
  (write-form (job-file id "scan.sexp") scan)
  scan)

(defun ready-directory (id platform)
  (job-file id (format nil "ready/~a/" platform)))

(defun relative-name (id path)
  "PATH as the manifest names it: under the job folder."
  (enough-namestring path (job-root id)))

;;;; --- checking a file -------------------------------------------------------

(defun limit-of (target key)
  (getf (getf target :limits) key))

(defun soft-p (target key)
  (member key (getf (getf target :limits) :soft) :test #'equal))

(defun check-file (info target)
  "INFO, a probed file, held against TARGET. => (values ROWS PASS-P)."
  (let ((rows '())
        (kind (getf target :kind))
        (width (getf target :width))
        (height (getf target :height)))
    (flet ((row (id status measured limit detail)
             (push (list :id id :status status :measured measured :limit limit :detail detail) rows))
           (bounded (name measured)
             (let* ((key (string-upcase (string name)))
                    (min (getf (getf target :limits) (intern (concatenate 'string key "-MIN") :keyword)))
                    (max (getf (getf target :limits) (intern (concatenate 'string key "-MAX") :keyword)))
                    (soft (member (intern (concatenate 'string key "-MAX") :keyword)
                                  (getf (getf target :limits) :soft) :test #'equal))
                    (label (string-downcase key)))
               (when (and measured min (< measured min))
                 (row (format nil "file.~a" label) "fail" measured min
                      (format nil "~a is ~,2f and the target needs at least ~,2f" label measured min)))
               (when (and measured max (> measured max))
                 (row (format nil "file.~a" label) (if soft "advisory" "fail") measured max
                      (format nil "~a is ~,2f and the target takes at most ~,2f" label measured max))))))
      (unless (or (string= kind "image_or_video") (string= kind (getf info :kind)))
        (row "file.kind" "fail" (getf info :kind) kind
             (format nil "~a takes ~as and this is a ~a" (getf target :id) kind (getf info :kind))))
      (let ((formats (or (limit-of target :formats) (limit-of target :containers))))
        (when (and formats (not (member (getf info :format) formats :test #'string=)))
          (row "file.format" "fail" (getf info :format) formats
               (format nil "~a takes ~{~a~^, ~} and this is a .~a" (getf target :id) formats (getf info :format)))))
      (when (and (getf info :width) (getf info :height))
        (unless (and (= width (getf info :width)) (= height (getf info :height)))
          (row "file.size" "advisory" (format nil "~dx~d" (getf info :width) (getf info :height))
               (format nil "~dx~d" width height)
               (format nil "~a is ~dx~d and the platform scales it to ~dx~d"
                       (getf target :id) width height width height)))
        (bounded :dimension (max (getf info :width) (getf info :height))))
      (bounded :duration (getf info :duration))
      (let ((bytes (getf info :bytes)))
        (when bytes
          (let ((max (limit-of target :bytes-max)) (min (limit-of target :bytes-min)))
            (when (and min (< bytes min))
              (row "file.bytes" "fail" bytes min
                   (format nil "~d bytes is under the ~d this target needs" bytes min)))
            (when (and max (> bytes max))
              (row "file.bytes" (if (soft-p target :bytes-max) "advisory" "fail") bytes max
                   (format nil "~d bytes is over the ~d this target takes" bytes max))))))
      (bounded :fps (getf info :fps))
      (when (and (getf info :width) (getf info :height) (plusp (getf info :height)))
        (let* ((aspect (/ (getf info :width) (getf info :height)))
               (min (limit-of target :aspect-min)) (max (limit-of target :aspect-max)))
          (when (and min (< aspect min))
            (row "file.aspect" "fail" aspect min
                 (format nil "an aspect of ~,3f is narrower than the ~,3f this target takes" aspect min)))
          (when (and max (> aspect max))
            (row "file.aspect" "fail" aspect max
                 (format nil "an aspect of ~,3f is wider than the ~,3f this target takes" aspect max)))))
      (let ((pass (not (find-if (lambda (entry) (string= (getf entry :status) "fail")) rows))))
        (values (nreverse rows) pass)))))

;;;; --- rendering -------------------------------------------------------------

(defun first-line (text)
  "TEXT's first non-empty line, at most 200 characters."
  (let ((line (or (find-if (lambda (one) (plusp (length (string-trim '(#\Space #\Return) one))))
                            (uiop:split-string (or text "") :separator '(#\Newline)))
                  "")))
    (subseq line 0 (min 200 (length line)))))

(defun run-ffmpeg (arguments)
  "FFMPEG with ARGUMENTS. => (values OK MESSAGE)."
  (let ((executable (nlk::executable-on-path "ffmpeg")))
    (if (null executable)
        (values nil "ffmpeg is not on PATH")
        (handler-case
            (multiple-value-bind (output error status)
                (nlk:run-bounded (cons (uiop:native-namestring executable) arguments) :seconds 600)
              (declare (ignore output))
              (values (and (eql status 0) t)
                      (if (eql status 0) nil (string-trim '(#\Space #\Newline) (or error "")))))
          (error () (values nil "ffmpeg could not be started"))))))

(defun scale-filter (target)
  (format nil "scale=~d:~d:force_original_aspect_ratio=increase,crop=~d:~d"
          (getf target :width) (getf target :height) (getf target :width) (getf target :height)))

(defun pad-filter (target)
  "The whole frame, black bars around it: the whole picture, nothing cut off."
  (format nil "scale=~d:~d:force_original_aspect_ratio=decrease,pad=~d:~d:(ow-iw)/2:(oh-ih)/2:color=black"
          (getf target :width) (getf target :height) (getf target :width) (getf target :height)))

(defun render-image (info target output layout)
  "INFO's photo as OUTPUT at TARGET's size. => (values OK MESSAGE)."
  (let ((filter (if (equal layout "pad") (pad-filter target) (scale-filter target))))
    (run-ffmpeg (list "-y" "-v" "error" "-i" (getf info :path) "-vf" filter
                      "-frames:v" "1" "-q:v" "2" (namestring output)))))

(defun render-video (info target output trim cover-at)
  "INFO's video as OUTPUT at TARGET's size, trimmed to its length. => (values OK MESSAGE)."
  (let* ((limit (limit-of target :duration-max))
         (start (or (first trim) 0))
         (finish (or (second trim)
                     (let ((duration (getf info :duration)))
                       (and duration limit (min duration (+ start limit))))))
         (seconds (and finish (- finish start)))
         (arguments (append (list "-y" "-v" "error" "-ss" (format nil "~,3f" start))
                            (when (and seconds (plusp seconds)) (list "-t" (format nil "~,3f" seconds)))
                            (list "-i" (getf info :path) "-vf" (format nil "~a,fps=30" (scale-filter target))
                                  "-c:v" "libx264" "-preset" "veryfast" "-crf" "23" "-pix_fmt" "yuv420p")
                            (when (getf info :has-audio)
                              (list "-af" (format nil "loudnorm=I=~d:TP=-1.5:LRA=11" *loudness-lufs*)
                                    "-c:a" "aac" "-b:a" "128k"))
                            (list "-movflags" "+faststart" (namestring output)))))
    (multiple-value-bind (ok message) (run-ffmpeg arguments)
      (if ok
          (progn (when cover-at (run-ffmpeg (list "-y" "-v" "error" "-ss" (format nil "~,3f" cover-at)
                                                   "-i" (namestring output) "-frames:v" "1" "-q:v" "2"
                                                   (namestring (merge-pathnames "cover.jpg" output)))))
                 (values t nil))
          (values nil message)))))

;;;; --- the page ---------------------------------------------------------------

(defun escape-html (text)
  "TEXT with the four characters an HTML page cannot carry as themselves escaped."
  (with-output-to-string (out)
    (loop for char across (princ-to-string (or text "")) do
      (case char
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\& (write-string "&amp;" out))
        (#\" (write-string "&quot;" out))
        (t (write-char char out))))))

(defun write-board (id manifest)
  "The job's one page: every input, every target and its files, and the copy. => its name."
  (let ((file (job-file id "board.html")))
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out "<!doctype html><html><head><meta charset=\"utf-8\"><title>postkitty ~a</title>" (escape-html id))
      (format out "<style>body{font:15px/1.5 system-ui,sans-serif;margin:2rem;max-width:60rem;color:#1e2a44}table{border-collapse:collapse;width:100%%}td,th{border-bottom:1px solid #ddd;padding:.35rem .5rem;text-align:left;vertical-align:top}pre{white-space:pre-wrap}.ready{color:#2f6f3e}.failed{color:#a33}.skipped{color:#8a6d3b}</style></head><body>~%")
      (format out "<h1>postkitty ~a</h1><h2>Inputs</h2><ul>~%" (escape-html id))
      (dolist (input (getf manifest :inputs))
        (format out "<li>~a - ~a~@[, ~dx~d~]~@[, ~,1fs~]</li>~%"
                (escape-html (getf input :name)) (escape-html (getf input :kind))
                (getf input :width) (getf input :height) (getf input :duration)))
      (format out "</ul><h2>Targets</h2><table><tr><th>Target</th><th>Status</th><th>File</th><th>Size</th><th>Notes</th></tr>~%")
      (dolist (entry (getf manifest :targets))
        (format out "<tr><td>~a</td><td class=\"~a\">~a</td><td>~a</td><td>~a</td><td>~a</td></tr>~%"
                (escape-html (getf entry :id)) (escape-html (getf entry :status)) (escape-html (getf entry :status))
                (escape-html (or (getf entry :file) "-")) (escape-html (or (getf entry :size) "-"))
                (escape-html (or (format nil "~{~a~^; ~}" (append (getf entry :problems)
                                                                     (getf entry :advisory)))
                                 "-"))))
      (format out "</table>~%")
      (when (getf manifest :copy)
        (format out "<h2>Copy</h2>~%")
        (dolist (item (getf manifest :copy))
          (format out "<h3>~a</h3><pre>~a</pre>~%" (escape-html (getf item :platform))
                  (escape-html (getf item :text)))))
      (format out "</body></html>~%"))
    file))
