;;;; cell.lisp --- five local video tools on the ffmpeg this machine has, and
;;;; the section that names it.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Kinocut's idea, kept: an agent never hand-writes an ffmpeg command. It
;;;; asks for an edit, the edit is validated against the file's own probe
;;;; before anything runs, the render lands in one folder as a new file, and
;;;; the answer is a receipt naming the exact argv -- so a person can rerun,
;;;; check or undo it. What the plugin did over MCP with a pinned Python
;;;; runtime, this does in-process against the ffmpeg the operator already
;;;; has.

(in-package #:nodecode-kinocut)

;;; --- what a call asks for ---------------------------------------------------------

(nlk:define-contract kino-probe
  (:doc "Inspect a local video before touching it: duration, frame size and rate, codecs, audio streams and size. Reads only; the file is never modified and nothing is written.")
  ("path" :text :required t :doc "the video file, by full path"))

(nlk:define-contract kino-trim
  (:doc "Cut one clip out of a local video into a new file under the kinocut render folder. accurate re-encodes the exact frame; copy keeps the source's codecs and can only land on a keyframe. The source is never modified.")
  ("path" :text :required t :doc "the source video, by full path")
  ("start" :number :required t :doc "seconds from the start of the source")
  ("end" :number :doc "seconds from the start of the source where the clip stops; give end or duration")
  ("duration" :number :doc "how long the clip runs, in seconds; give end or duration, not both")
  ("mode" :text :options +trim-modes+ :default "accurate"
          :doc "accurate cuts on the exact frame by re-encoding; copy is faster, keeps the source codecs, and snaps to a keyframe"))

(nlk:define-contract kino-reformat
  (:doc "Put a local video in another frame: 9:16 for a phone, 1:1 for a square post, 4:5 for a portrait feed, 16:9 for widescreen. crop fills the frame and loses the edges; pad keeps the whole picture and adds bars. The source is never modified.")
  ("path" :text :required t :doc "the source video, by full path")
  ("aspect" :text :options '("9:16" "1:1" "4:5" "16:9") :required t
            :doc "the frame to make")
  ("fit" :text :options +fits+ :default "crop"
         :doc "crop fills the frame and loses the edges; pad keeps everything and adds bars"))

(nlk:define-contract kino-captions
  (:doc "Burn a subtitle file into a local video's picture, in the section's font and size. The subtitle must be a real .srt or .ass; nothing is transcribed and no model is called. The source is never modified.")
  ("path" :text :required t :doc "the source video, by full path")
  ("subtitles" :text :required t :doc "the .srt or .ass file to burn in, by full path"))

(nlk:define-contract kino-check
  (:doc "A quality gate before publishing a local video: a readable duration, a floor on length when one is named, how much of the clip is black, and whether the audio is audible. Answers a score out of 100 and passes at the section's pass_score. It measures; it does not watch.")
  ("path" :text :required t :doc "the video to check, by full path")
  ("min_seconds" :number :doc "refuse a clip shorter than this, in seconds"))

;;; --- a tool body answers its refusals ----------------------------------------------

(defmacro answering (&body body)
  "Run BODY as a tool's body: a refusal is answered, never signalled, so the
model reads why and repairs the call."
  `(handler-case (progn ,@body)
     (kinocut-error (condition)
       (nle:failure (format nil "ERROR: ~a" (kinocut-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: ~a" condition)))))

(defun render-receipt (summary argv file bytes after)
  "What the model is told about one render: the file, what was done, the
length the file itself reports, the bytes, and the argv that made it."
  (with-output-to-string (out)
    (format out "saved ~a~%" (namestring file))
    (format out "  ~a~%" summary)
    (format out "  ~a, ~a~@[, ~a~]~%"
            (seconds-text (and after (probe-duration after)))
            (if after (frame-text after) "an unreadable frame")
            (and after (rate-text after)))
    (format out "  ~a~%" (nlk:size-text bytes))
    (format out "  argv: ~{~a~^ ~}~%" argv)
    (format out "  the source is untouched; /kino lists this receipt.~%")))

;;; --- kino_probe -----------------------------------------------------------------

(defun probe-video (request)
  "The kino_probe tool: what ffprobe says about a file, in a person's words."
  (answering
    (let* ((source (source-file (kino-probe-path request)))
           (data (probe-json source)))
      (with-output-to-string (out)
        (format out "probe ~a~%" (file-name source))
        (format out "  ~a, ~a~@[, ~a~]~%" (seconds-text (probe-duration data))
                (frame-text data) (rate-text data))
        (format out "  video: ~a~%"
                (or (nlk:json-value (probe-stream data "video") :string "codec_name") "none"))
        (format out "  audio: ~a~%" (audio-text data))
        (format out "  size:  ~a~%" (nlk:size-text (probe-size data)))
        (format out "  reads only: the file is untouched and nothing was written.~%")))))

;;; --- kino_trim ------------------------------------------------------------------

(defun trim-argv (source output start length mode)
  "The ffmpeg argv for one cut: the exact frame by re-encode, or the nearest
keyframe by stream copy."
  (let ((head (list (program :ffmpeg) "-nostdin" "-y" "-hide_banner" "-loglevel" "error"
                    "-ss" (number-text start) "-i" (namestring source)
                    "-t" (number-text length))))
    (if (equal mode "copy")
        (append head (list "-c" "copy" "-avoid_negative_ts" "make_zero" (namestring output)))
        (append head
                (list "-c:v" "libx264" "-preset" (setting :preset)
                      "-crf" (number-text (setting :crf)) "-pix_fmt" "yuv420p"
                      "-c:a" "aac" "-b:a" "192k" "-movflags" "+faststart"
                      (namestring output))))))

(defun trim-video (request)
  "The kino_trim tool: one clip, validated against the source's own length
before anything runs."
  (answering
    (let* ((source (source-file (kino-trim-path request)))
           (start (kino-trim-start request))
           (end (kino-trim-end request))
           (duration (kino-trim-duration request))
           (mode (or (kino-trim-mode request) "accurate"))
           (total (probe-duration (probe-json source))))
      (when (minusp start)
        (fail "start is ~a; a clip cannot begin before the file does" (number-text start)))
      (when (and end duration)
        (fail "give end (where the clip stops) or duration (how long it runs), not both"))
      (unless (or end duration)
        (fail "give end (where the clip stops) or duration (how long it runs)"))
      (let ((length (if end (- end start) duration)))
        (when (<= length 0)
          (fail "the clip would run ~a: start ~a, stop ~a"
                (number-text length) (number-text start) (number-text (+ start length))))
        (when (and (plusp total) (> (+ start length) (+ total 0.05)))
          (fail "the source is ~a long, and a clip from ~a to ~a runs past its end"
                (seconds-text total) (seconds-text start) (seconds-text (+ start length))))
        (let* ((directory (render-directory))
               (extension (if (equal mode "copy")
                              (string-downcase (or (pathname-type (pathname source)) "mp4"))
                              "mp4"))
               (output (new-output directory source "trim" extension))
               (argv (trim-argv source output start length mode)))
          (multiple-value-bind (file bytes) (render-argv argv output)
            (let ((after (ignore-errors (probe-json file))))
              (record-receipt "trim" source argv file bytes
                              (format nil "~a to ~a, ~a mode"
                                      (seconds-text start) (seconds-text (+ start length)) mode))
              (render-receipt (format nil "trim: ~a from ~a of a ~a source, ~a mode"
                                      (seconds-text length) (seconds-text start)
                                      (seconds-text total) mode)
                              argv file bytes after))))))))

;;; --- kino_reformat ----------------------------------------------------------------

(defun even-number (value)
  "VALUE as an even integer of at least 2: x264 takes no odd frame."
  (let ((number (round value)))
    (max 2 (if (evenp number) number (1- number)))))

(defun even-offset (value)
  "VALUE as the even integer at or below it: a crop's x or y, which may be
0 and is never the frame's own size."
  (let ((number (round value)))
    (if (evenp number) number (1- number))))

(defun frame-filter (fit width height tw th)
  "The filter that makes a WIDTHxHEIGHT picture a TWxTH frame, either by
cropping the middle of the picture or by padding it onto black."
  (cond
    ((equal fit "pad")
     (format nil "scale=~d:~d:force_original_aspect_ratio=decrease,pad=~d:~d:(ow-iw)/2:(oh-ih)/2:color=black"
             tw th tw th))
    ((equal fit "crop")
     (let* ((cropping (> (* width th) (* height tw)))
            (cw (if cropping (even-number (* height (/ tw th))) width))
            (ch (if cropping height (even-number (* width (/ th tw)))))
            (x (even-offset (/ (- width cw) 2)))
            (y (even-offset (/ (- height ch) 2))))
       (format nil "crop=~d:~d:~d:~d,scale=~d:~d" cw ch x y tw th)))
    (t (fail "fit is ~a; use crop (fill the frame, losing the edges) or pad (keep everything, add bars)" fit))))

(defun reformat-video (request)
  "The kino_reformat tool: another frame, in one re-encode."
  (answering
    (let* ((source (source-file (kino-reformat-path request)))
           (aspect (kino-reformat-aspect request))
           (fit (or (kino-reformat-fit request) "crop"))
           (data (probe-json source))
           (width (probe-width data))
           (height (probe-height data)))
      (unless (and width height)
        (fail "ffprobe reports no frame for ~a, so there is nothing to reformat" (file-name source)))
      (let* ((target (or (assoc aspect +targets+ :test #'equal)
                         (fail "aspect is ~a; use one of ~{~a~^, ~}" aspect +aspects+)))
             (tw (second target))
             (th (third target))
             (filter (frame-filter fit width height tw th))
             (directory (render-directory))
             (output (new-output directory source "reformat" "mp4"))
             (argv (list (program :ffmpeg) "-nostdin" "-y" "-hide_banner" "-loglevel" "error"
                         "-i" (namestring source) "-vf" filter
                         "-c:v" "libx264" "-preset" (setting :preset)
                         "-crf" (number-text (setting :crf)) "-pix_fmt" "yuv420p"
                         "-c:a" "aac" "-b:a" "192k" "-movflags" "+faststart"
                         (namestring output))))
        (multiple-value-bind (file bytes) (render-argv argv output)
          (let ((after (ignore-errors (probe-json file))))
            (record-receipt "reformat" source argv file bytes
                            (format nil "~a ~a from ~ax~a" aspect fit width height))
            (render-receipt (format nil "reformat: ~a ~a in a ~ax~a frame, from a ~ax~a source"
                                    aspect fit tw th width height)
                            argv file bytes after)))))))

;;; --- kino_captions ------------------------------------------------------------------

(defun subtitle-file (text)
  "TEXT as a subtitle file that has something in it, or a refusal saying what
is wrong with it."
  (let ((path (source-file text)))
    (let ((body (ignore-errors (uiop:read-file-string path))))
      (unless (and body (plusp (length (nlk:trimmed body))))
        (fail "the subtitle file ~a is empty" (file-name path)))
      (unless (or (search "-->" body) (search "[Script Info]" body))
        (fail "~a is not a subtitle file: no SRT cue (\"-->\") and no ASS header ([Script Info]) in it"
              (file-name path)))
      path)))

(defun filter-path (path)
  "PATH as a filtergraph value: the characters the graph reads as syntax are
escaped, so a folder named with a comma or an apostrophe still works."
  (with-output-to-string (out)
    (loop for char across (namestring path)
          do (when (find char "\\':[],;") (write-char #\\ out))
             (write-char char out))))

(defun style-arg ()
  "The libass style the section asks for: the font, its size, a white fill
with a dark edge, and the margin off the bottom."
  (format nil "FontName=~a,FontSize=~d,PrimaryColour=&H00FFFFFF,OutlineColour=&H80000000,BorderStyle=1,Outline=2,Shadow=0,MarginV=~d"
          (setting :font-name) (setting :font-size) (setting :caption-margin)))

(defun caption-video (request)
  "The kino_captions tool: burn a subtitle file into the picture."
  (answering
    (let* ((source (source-file (kino-captions-path request)))
           (subtitle (subtitle-file (kino-captions-subtitles request)))
           (directory (render-directory))
           (output (new-output directory source "captions" "mp4"))
           (filter (format nil "subtitles=filename=~a:force_style='~a'"
                           (filter-path subtitle) (style-arg)))
           (argv (list (program :ffmpeg) "-nostdin" "-y" "-hide_banner" "-loglevel" "error"
                       "-i" (namestring source) "-vf" filter
                       "-c:v" "libx264" "-preset" (setting :preset)
                       "-crf" (number-text (setting :crf)) "-pix_fmt" "yuv420p"
                       "-c:a" "copy" "-movflags" "+faststart"
                       (namestring output))))
      (multiple-value-bind (file bytes) (render-argv argv output)
        (let ((after (ignore-errors (probe-json file))))
          (record-receipt "captions" source argv file bytes
                          (format nil "burned ~a" (file-name subtitle)))
          (render-receipt (format nil "captions: ~a burned in at ~d pt in ~a"
                                  (file-name subtitle) (setting :font-size) (setting :font-name))
                          argv file bytes after))))))

;;; --- kino_check -------------------------------------------------------------------

(defun match-number (pattern text)
  "The first capture of PATTERN in TEXT as a number, or NIL."
  (multiple-value-bind (whole groups) (ignore-errors (cl-ppcre:scan-to-strings pattern (or text "")))
    (declare (ignore whole))
    (and groups (plusp (length groups)) (to-number (aref groups 0)))))

(defun black-seconds (source)
  "Every second the picture is black, from one blackdetect pass; 0 when the
pass itself failed, which is a note, not a finding."
  (multiple-value-bind (out status)
      (exec (list (program :ffmpeg) "-nostdin" "-hide_banner" "-i" (namestring source)
                   "-vf" "blackdetect=d=0.25:pix_th=0.10" "-an" "-f" "null" "-"))
    (if (zerop status)
        (loop for text in (cl-ppcre:all-matches-as-strings "black_duration:[0-9.]+" (or out ""))
              sum (or (to-number (subseq text (length "black_duration:"))) 0))
        0)))

(defun audio-levels (source audio)
  "The loudest and the average moment of the first audio stream in dB, from
one volumedetect pass: (values PEAK MEAN), both NIL without audio."
  (if (null audio)
      (values nil nil)
      (multiple-value-bind (out status)
          (exec (list (program :ffmpeg) "-nostdin" "-hide_banner" "-i" (namestring source)
                       "-map" "0:a:0" "-af" "volumedetect" "-vn" "-f" "null" "-"))
        (declare (ignore status))
        (values (match-number "max_volume:\\s*(-?[0-9.]+)" out)
                (match-number "mean_volume:\\s*(-?[0-9.]+)" out)))))

(defun check-receipt (source data audio peak mean findings score)
  (let ((verdict (if (>= score (setting :pass-score)) "PASS" "FAIL")))
    (with-output-to-string (out)
      (format out "quality check: ~a, ~d/100~%" verdict score)
      (format out "  ~a: ~a, ~a, ~a~%" (file-name source)
              (seconds-text (probe-duration data)) (frame-text data) (audio-text data))
      (dolist (finding (reverse findings))
        (format out "  ! ~a~%" finding))
      (unless findings
        (format out "  no finding: a readable duration, a lit picture and an audible track~%"))
      (when (and audio peak)
        (format out "  the loudest moment is ~,1f dB~@[, the average ~,1f dB~]~%" peak mean))
      (unless audio
        (format out "  note: no audio stream; a silent clip is fine, and captions cannot be checked by ear~%"))
      (format out "  a receipt measures and does not see: look at the frames yourself before publishing.~%"))))

(defun check-video (request)
  "The kino_check tool: the gate a clip passes before anyone publishes it."
  (answering
    (let* ((source (source-file (kino-check-path request)))
           (floor-seconds (kino-check-min-seconds request))
           (data (probe-json source))
           (total (probe-duration data))
           (audio (probe-stream data "audio"))
           (findings '())
           (score 100))
      (unless (plusp total)
        (push "the file reports no duration" findings)
        (setf score (- score 25)))
      (when (and floor-seconds (plusp total) (< total floor-seconds))
        (push (format nil "the clip runs ~a, under the ~a floor"
                      (seconds-text total) (seconds-text floor-seconds))
              findings)
        (setf score (- score 25)))
      (let* ((black (black-seconds source))
             (percent (if (plusp total) (round (* 100 (/ black total))) 0)))
        (when (> percent (setting :max-black-percent))
          (push (format nil "~a of the clip is black, ~d% of it" (seconds-text black) percent)
                findings)
          (setf score (- score 25))))
      (multiple-value-bind (peak mean) (audio-levels source audio)
        (when (and peak (<= peak -50))
          (push (format nil "the audio peaks at ~,1f dB, which is silence" peak) findings)
          (setf score (- score 20)))
        (check-receipt source data audio peak mean findings (max 0 score))))))

;;; --- the cell ---------------------------------------------------------------------

(defparameter +primer+
  "Kinocut is this profile's local video bench: five tools over the ffmpeg the machine
already has, and nothing sent anywhere.
  kino_probe(path)                                  duration, frame, rate, codecs, audio, size; reads only
  kino_trim(path, start, end|duration, mode)        one clip into the render folder; accurate or copy
  kino_reformat(path, aspect, fit)                  9:16, 1:1, 4:5 or 16:9; crop or pad
  kino_captions(path, subtitles)                    burn a real .srt or .ass into the picture
  kino_check(path, min_seconds?)                    duration, black frames, silence; PASS at the section's pass_score
The order that works: probe, then one edit, then check, then look at the frames yourself.
Every tool writes a NEW file under the section's output_dir and never touches the source; the
answer names the file, its real length, and the exact argv, and /kino lists the receipts.
A receipt is a measurement, not a publication. Renders re-encode to H.264/AAC MP4 except a
copy-mode trim, which keeps the source's codecs and can only land on a keyframe."
  "What (help :kino) answers while the cell runs.")

(nle:define-cell kinocut
  (:section ("kinocut")
    (:guide "ffmpeg is already on this machine, and ffmpeg / ffprobe are the usual names; output_dir is the one thing to set, and every render lands there; crf and preset decide how a render looks; font_name, font_size and caption_margin decide how burned captions look")
    ("ffmpeg" :string :default "ffmpeg" :doc "the ffmpeg binary, or its full path")
    ("ffprobe" :string :default "ffprobe" :doc "the ffprobe binary, or its full path")
    ("output_dir" :directory :required t
                  :doc "where renders land, ~/ expanded; a render never writes outside it")
    ("crf" :integer :default 20 :min 0
           :doc "x264 quality: lower is better and bigger, 18 to 23 is the useful range")
    ("preset" :choice :options '("veryfast" "fast" "medium" "slow") :default "medium"
              :doc "x264 speed against file size")
    ("font_name" :string :default "DejaVu Sans"
                 :doc "the caption font; it must be one fontconfig on this machine knows")
    ("font_size" :integer :default 20 :min 8 :doc "caption size in points")
    ("caption_margin" :integer :default 40 :min 0
                      :doc "how far captions sit above the bottom edge")
    ("max_black_percent" :integer :default 50 :min 0
                         :doc "a check fails when more of the clip than this is black")
    ("pass_score" :integer :default 80 :min 0 :doc "the score kino_check must reach to pass")
    ("timeout_seconds" :integer :default 900 :min 10 :doc "one render's ceiling"))
  (:tool "kino_probe" (nlk:contract 'kino-probe) 'probe-video)
  (:tool "kino_trim" (nlk:contract 'kino-trim) 'trim-video)
  (:tool "kino_reformat" (nlk:contract 'kino-reformat) 'reformat-video)
  (:tool "kino_captions" (nlk:contract 'kino-captions) 'caption-video)
  (:tool "kino_check" (nlk:contract 'kino-check) 'check-video)
  (:help :kino "kino: five local video tools -- probe, trim, reformat, captions, check -- on the ffmpeg this machine already has" +primer+)
  (:command "kino" 'run-slash
            :description "The ffmpeg this profile runs, its render folder, and the last receipts"
            :argument-hint "[receipt id or words]")
  (:start (lambda ()
            ;; The render folder exists from the first start, so a render's
            ;; naming never depends on making it.
            (output-directory))))
