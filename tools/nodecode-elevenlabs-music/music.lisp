;;;; music.lisp --- the eight tools, the files they leave, the jobs they run in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every billed request (a song, a plan's song, an upload, stems, a video
;;;; score) runs on a background thread with a record in *JOBS*, so a tool can
;;;; answer with the job id instead of holding the turn for a minute: the
;;;; elevenlabs_music_job tool then reports or waits for the rest. The free
;;;; calls (a drafted plan, the finetune list) answer directly.
;;;;
;;;; Files land in output_dir (the organism's cache/generated/audio/ by
;;;; default) and each is recorded in the cell's own tracks.jsonl, so a later
;;;; stems or upload call can name an earlier track by its track_id.

(in-package #:nodecode-elevenlabs-music)

(defparameter +max-prompt-chars+ 4100)
(defparameter +min-song-seconds+ 3)
(defparameter +max-song-seconds+ 600)
(defparameter +max-chunks+ 30)
(defparameter +min-chunk-ms+ 3000)
(defparameter +max-chunk-ms+ 120000)
(defparameter +max-lines+ 30)
(defparameter +max-line-chars+ 200)
(defparameter +max-styles+ 50)
(defparameter +max-conditioning-ms+ 30000)
(defparameter +min-range-ms+ 50)
(defparameter +song-deadline+ 900)
(defparameter +plan-deadline+ 120)
(defparameter +audio-exts+ '("mp3" "wav" "flac" "m4a" "aac" "ogg" "opus" "webm" "aiff" "aif"))
(defparameter +video-exts+ '("mp4" "mov" "m4v" "webm" "mkv" "avi"))
(defparameter +max-audio-upload-bytes+ (* 100 1024 1024))
(defparameter +max-video-bytes+ (* 200 1024 1024))
(defparameter +max-stem-total+ (* 400 1024 1024))
(defparameter +deliver+
  (concatenate 'string
               "Each media_tag line in the result is MEDIA:<path>: put each one on a line of its own "
               "in your reply so the audio reaches the user."))

;;; --- arguments ------------------------------------------------------------

(defun argument (args key)
  "KEY's member of the decoded tool argument table ARGS, or NIL."
  (and (hash-table-p args) (gethash key args)))

(defun text-argument (args key limit &key required)
  "KEY as trimmed text of at most LIMIT characters; a refusal says which."
  (let ((raw (argument args key)))
    (cond ((null raw)
           (when required (fail "~a is required" key))
           nil)
          ((not (stringp raw)) (fail "~a must be text" key))
          (t (let ((text (string-trim '(#\Space #\Tab #\Return #\LineFeed) raw)))
               (when (and required (zerop (length text)))
                 (fail "~a must not be empty" key))
               (when (> (length text) limit)
                 (fail "~a is ~d characters long; shorten it to at most ~d"
                       key (length text) limit))
               (and (plusp (length text)) text))))))

(defun number-argument (args key low high)
  "KEY as a number in [LOW, HIGH], or NIL when it was left out."
  (let ((raw (argument args key)))
    (cond ((null raw) nil)
          ((not (realp raw))
           (fail "~a must be a number between ~a and ~a" key low high))
          ((or (< raw low) (> raw high))
           (fail "~a must be between ~a and ~a (got ~a)" key low high raw))
          (t raw))))

(defun integer-argument (args key low high)
  (let ((value (number-argument args key low high)))
    (when (and value (/= value (truncate value)))
      (fail "~a must be a whole number between ~a and ~a" key low high))
    (and value (truncate value))))

(defun choice-argument (args key allowed default)
  "KEY when it is one of ALLOWED, DEFAULT when it was left out."
  (let ((raw (argument args key)))
    (cond ((null raw) default)
          ((and (stringp raw) (member raw allowed :test #'string=)) raw)
          (t (fail "~a must be one of ~{~a~^, ~} (got ~s)" key allowed raw)))))

(defun boolean-argument (args key)
  "KEY when it is JSON true, NIL otherwise."
  (let ((raw (argument args key)))
    (cond ((null raw) nil)
          ((eq raw t) t)
          (t (fail "~a must be true or false" key)))))

(defun string-list-argument (args key limit &key (item-chars 300))
  "KEY's array of non-empty strings, at most LIMIT of them."
  (let ((raw (argument args key)))
    (cond ((null raw) '())
          ((or (not (vectorp raw)) (stringp raw)) (fail "~a must be a list of strings" key))
          (t (let ((items (loop for item across raw
                                 when (and (stringp item)
                                           (plusp (length (string-trim '(#\Space) item))))
                                   collect (string-trim '(#\Space #\Tab) item))))
               (when (> (length items) limit)
                 (fail "~a has ~d entries; at most ~d are allowed" key (length items) limit))
               (when (some (lambda (item) (> (length item) item-chars)) items)
                 (fail "each entry of ~a must be at most ~d characters" key item-chars))
               items)))))

;;; --- tracks and files -----------------------------------------------------

(defun track-id ()
  "A fresh track id: twelve lowercase hex characters."
  (format nil "~(~{~2,'0x~}~)" (loop repeat 6 collect (random 256))))

(defun stamp ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

(defun slug (text)
  "TEXT as a file-name stem: lowercase letters and digits, runs of anything
else as one dash, at most 48 of them."
  (let ((out (make-string-output-stream))
        (dash nil))
    (loop for char across (string-downcase (or text ""))
          do (cond ((alphanumericp char) (write-char char out) (setf dash nil))
                   ((not dash) (write-char #\- out) (setf dash t))))
    (let ((text (string-trim "-" (get-output-stream-string out))))
      (if (plusp (length text)) (subseq text 0 (min 48 (length text))) "track"))))

(defun audio-dir ()
  "Where a track is written: the section's output_dir, else the organism's
own cache/generated/audio/."
  (let ((configured (setting :output-dir)))
    (if (and (stringp configured) (plusp (length (string-trim '(#\Space) configured))))
        (uiop:ensure-directory-pathname
         (uiop:parse-native-namestring (string-trim '(#\Space) configured)))
        (nlk:home "cache/generated/audio/"))))

(defun save-audio (octets prefix label ext id)
  "OCTETS as a new file under AUDIO-DIR; never overwrites an earlier track."
  (let* ((directory (audio-dir))
         (path (merge-pathnames (format nil "~a_~a_~a_~a.~a"
                                         prefix (slug label) (stamp) id ext)
                                 directory)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :error
                               :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    path))

(defun library-file ()
  "The cell's own record of the tracks it made, one JSON object a line."
  (nlk:home "plugin-data/nodecode-elevenlabs-music/tracks.jsonl"))

(defun record-track (object)
  "File OBJECT, one line, in the track library."
  (let ((path (library-file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :append
                               :if-does-not-exist :create)
      (write-line (nlk:encode-json-object object) out))
    object))

(defun find-track (id)
  "The library record of track ID, or NIL."
  (let ((path (library-file)))
    (when (probe-file path)
      (with-open-file (in path)
        (loop for line = (read-line in nil nil)
              while line
              for object = (handler-case (nlk:decode-json line) (error () nil))
              when (and (hash-table-p object)
                        (equal id (nlk:json-value object :string "id")))
                return object)))))

(defun expanded-path (text)
  "TEXT as a pathname: ~/ read against the operator's home, a relative path
against the working directory."
  (let ((text (string-trim '(#\Space) text)))
    (cond ((uiop:string-prefix-p "~/" text)
           (merge-pathnames (subseq text 2) (user-homedir-pathname)))
          (t (let ((path (uiop:parse-native-namestring text)))
               (if (uiop:absolute-pathname-p path)
                   path
                   (merge-pathnames path (uiop:getcwd))))))))

(defun local-file (text what extensions limit)
  "TEXT as an existing file of one of EXTENSIONS, no larger than LIMIT bytes."
  (let* ((path (probe-file (expanded-path text)))
         (extension (and path (pathname-type path))))
    (unless path
      (fail "~a: no file at ~s" what text))
    (unless (and extension (member extension extensions :test #'string-equal))
      (fail "~a must be one of ~{~a~^, ~} (got ~s)" what extensions text))
    (when (> (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))
             limit)
      (fail "~a is larger than ~d MB" what (round limit (* 1024 1024))))
    (uiop:native-namestring path)))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buffer in)
      buffer)))

(defparameter +mime-types+
  (list (cons "mp3" "audio/mpeg") (cons "wav" "audio/wav") (cons "flac" "audio/flac")
        (cons "ogg" "audio/ogg") (cons "opus" "audio/ogg") (cons "m4a" "audio/mp4")
        (cons "aac" "audio/aac") (cons "aiff" "audio/aiff") (cons "aif" "audio/aiff")
        (cons "webm" "audio/webm") (cons "mp4" "video/mp4") (cons "m4v" "video/mp4")
        (cons "mov" "video/quicktime") (cons "mkv" "video/x-matroska")
        (cons "avi" "video/x-msvideo"))
  "The content type each extension a tool accepts travels as.")

(defun mime-type (name &key video)
  (let ((extension (string-downcase (or (pathname-type (pathname name)) ""))))
    (or (and video (string= extension "webm") "video/webm")
        (cdr (assoc extension +mime-types+ :test #'string=))
        "application/octet-stream")))

(defun source-audio (args)
  "The audio a tool was pointed at, by track_id or file_path (exactly one).
=> (values FILE-NAME OCTETS TRACK-RECORD-OR-NIL)."
  (let ((track (text-argument args "track_id" 100))
        (file (text-argument args "file_path" 4096)))
    (cond ((and track file)
           (fail "pass exactly one of track_id (a track these tools made) or file_path (an audio file)"))
          (track
           (let ((row (find-track track)))
             (unless row
               (fail "no track ~s in this profile's music library" track))
             (let ((path (nlk:json-value row :string "file_path")))
               (unless (and path (probe-file path))
                 (fail "track ~s has no audio file on this machine any more" track))
               (values (file-namestring path) (read-octets path) row))))
          (file
           (let ((path (local-file file "file_path" +audio-exts+ +max-audio-upload-bytes+)))
             (values (file-namestring path) (read-octets path) nil)))
          (t (fail "pass one of track_id (a track these tools made) or file_path (an audio file)")))))

;;; --- what a plan says about the song --------------------------------------

(defun sections (plan)
  "Each chunk's label and time range in the finished song: what an edit keeps
or regenerates."
  (let ((out '())
        (at 0))
    (loop for index from 0
          for chunk across (nlk:json-array plan "chunks")
          when (hash-table-p chunk)
            do (let* ((range (nlk:json-value chunk :object "range"))
                      (length (if range
                                  (- (or (nlk:json-value range :integer "end_ms") 0)
                                     (or (nlk:json-value range :integer "start_ms") 0))
                                  (or (nlk:json-value chunk :integer "duration_ms") 0)))
                      (head (first (uiop:split-string
                                    (or (nlk:json-value chunk :string "text") "")
                                    :separator '(#\Newline))))
                      (label (cond (range "kept slice")
                                   ((and head (uiop:string-prefix-p "[" head))
                                    (string-trim "[]" head))
                                   (head (subseq head 0 (min 40 (length head))))
                                   (t "section"))))
                 (push (nlk:make-json-object "index" index "label" label
                                              "start_ms" at "end_ms" (+ at length))
                       out)
                 (incf at length)))
    (coerce (nreverse out) 'vector)))

(defun lyrics (plan)
  (let ((lines (loop for chunk across (nlk:json-array plan "chunks")
                     for text = (and (hash-table-p chunk)
                                     (nlk:json-value chunk :string "text"))
                     when text collect (string-trim '(#\Space) text))))
    (and lines (format nil "~{~a~^~%~%~}" lines))))

;;; --- the song answer -------------------------------------------------------

(defun detailed (body format prompt requested parent)
  "POST /v1/music/detailed, save the audio it answers, and return the tool's
result object."
  (multiple-value-bind (octets headers)
      (multiple-value-call #'ok
        (request :post "/v1/music/detailed"
                 :query (format nil "output_format=~a" format)
                 :content (nlk:encode-json-object body)
                 :content-type "application/json"
                 :timeout +song-deadline+))
    (multiple-value-bind (metadata audio) (split-multipart octets headers)
      (let* ((song (nlk:json-value metadata :object "song_metadata"))
             (plan (nlk:json-value metadata :object "composition_plan"))
             (id (track-id))
             (title (or (nlk:json-value song :text "title")
                        (and parent (concatenate 'string parent " (edit)"))))
             (ext (or (cdr (assoc format +formats+ :test #'string=)) "mp3"))
             (path (save-audio audio "music" (or title prompt "song") ext id))
             (song-id (header-value headers "song-id"))
             (stored (and song-id (setting :store-for-editing)
                          (member (nlk:json-value body :string "model_id") +plan-models+
                                  :test #'string=)
                          t))
             (where (uiop:native-namestring path)))
        (record-track (nlk:make-json-object
                       "id" id "kind" "song" "title" title "prompt" prompt
                       "model" (nlk:json-value body :string "model_id")
                       "description" (nlk:json-value song :string "description")
                       "genres" (nlk:json-value song :array "genres")
                       "requested_seconds" requested "output_format" format
                       "file_path" where "bytes" (length audio) "song_id" song-id
                       "stored_for_editing" stored))
        (nlk:make-json-object
         "success" t "track_id" id "title" title
         "description" (nlk:json-value song :string "description")
         "genres" (nlk:json-value song :array "genres")
         "file_path" where "media_tag" (concatenate 'string "MEDIA:" where)
         "bytes" (length audio) "requested_seconds" requested
         "model" (nlk:json-value body :string "model_id") "output_format" format
         "elevenlabs_song_id" song-id "stored_for_editing" stored
         "sections" (sections plan) "lyrics" (lyrics plan)
         "composition_plan" plan "deliver" +deliver+)))))

;;; --- the jobs a billed call runs in ----------------------------------------

(defstruct (job (:constructor make-job (id kind label estimate)))
  "One billed request running on a thread of its own."
  id kind label estimate (status "running") result error started)

(defvar *jobs* (make-hash-table :test #'equal)
  "The music jobs this process started, by id. A job's id is good for as long
as the organism runs.")

(defun job-elapsed (job)
  (max 0 (/ (- (get-internal-real-time) (job-started job))
            internal-time-units-per-second)))

(defun start-job (kind label estimate thunk)
  "Run THUNK on a thread, recording what it answers; the job id names it."
  (let ((job (make-job (track-id) kind label estimate)))
    (setf (job-started job) (get-internal-real-time))
    (setf (gethash (job-id job) *jobs*) job)
    (bt2:make-thread
     (lambda ()
       (handler-case
           (progn (setf (job-result job) (funcall thunk)
                        (job-status job) "done"))
         (elevenlabs-music-error (condition)
           (setf (job-error job) (elevenlabs-music-error-detail condition)
                 (job-status job) "failed"))
         (error (condition)
           (setf (job-error job)
                 (format nil "unexpected ~a after the request was sent (~a). ElevenLabs may already have billed it. ~a"
                         (type-of condition) condition +no-retry+)
                 (job-status job) "failed"))))
     :name (format nil "elevenlabs-music-~a" (job-id job)))
    job))

(defun job-wait (job seconds)
  "JOB, once SECONDS have passed or it has finished."
  (let ((deadline (+ (get-internal-real-time)
                     (* (max 0 seconds) internal-time-units-per-second))))
    (loop while (and (equal (job-status job) "running")
                     (< (get-internal-real-time) deadline))
          do (sleep 0.05))
    job))

(defun job-pending (job)
  (let* ((elapsed (round (job-elapsed job)))
         (left (- (or (job-estimate job) 60) elapsed)))
    (nlk:make-json-object
     "success" t "status" "running" "job_id" (job-id job) "label" (job-label job)
     "elapsed_seconds" elapsed
     "estimated_seconds_left" (if (plusp left) left "past the estimate, any moment now")
     "next_step" (format nil "Still generating in the background. Call elevenlabs_music_job with job_id ~s and wait_seconds 240 to wait for it, then send each media_tag line. Never start the same request again: it would bill twice."
                          (job-id job)))))

(defun job-text (job)
  "JOB's result object, a refusal when it failed, or what is known so far."
  (cond ((equal (job-status job) "done")
         (let ((result (job-result job)))
           (when (hash-table-p result)
             (setf (gethash "job_id" result) (job-id job)))
           result))
        ((equal (job-status job) "failed")
         (fail "music job ~a (~a) failed: ~a" (job-id job) (job-label job) (job-error job)))
        (t (job-pending job))))

(defun billed (kind label estimate thunk)
  "Run THUNK in the background, wait the section's wait_seconds for it, and
answer its result or the job id to ask about later."
  (let ((job (start-job kind label estimate thunk)))
    (answer (job-text (job-wait job (or (setting :wait-seconds) 240))))))

(defun answer (object)
  "OBJECT as the tool's text."
  (if (stringp object) object (nlk:encode-json-object object)))

;;; --- compose --------------------------------------------------------------

(defun estimate (seconds)
  "How long a request for SECONDS of audio is likely to take, end to end."
  (round (+ 5 (* 0.2 (or seconds 120)))))

(defun compose (args)
  "The elevenlabs_music_compose tool: a song or instrumental from a prompt."
  (running-settings)
  (let* ((prompt (text-argument args "prompt" +max-prompt-chars+ :required t))
         (length (number-argument args "length_seconds" +min-song-seconds+ +max-song-seconds+))
         (model (choice-argument args "model" +models+ (setting :music-model)))
         (instrumental (boolean-argument args "instrumental"))
         (finetune (text-argument args "finetune_id" 100))
         (format (output-format))
         (body (nlk:make-json-object "prompt" prompt "model_id" model "with_timestamps" t
                                      "force_instrumental" (and instrumental t)
                                      "store_for_inpainting"
                                      (and (setting :store-for-editing)
                                           (member model +plan-models+ :test #'string=)
                                           t))))
    (api-key)
    (when length
      (setf (gethash "music_length_ms" body) (round (* 1000 length))))
    (when finetune
      (setf (gethash "finetune_id" body) finetune))
    (billed "song" (subseq prompt 0 (min 80 (length prompt))) (estimate length)
            (lambda () (detailed body format prompt length nil)))))

(defun json-object-of (octets what)
  "OCTETS decoded as a JSON object, or a refusal naming WHAT came back."
  (let ((value (handler-case (nlk:decode-json octets) (error () nil))))
    (unless (hash-table-p value)
      (fail "ElevenLabs returned ~a this tool cannot read" what))
    value))

;;; --- a composition plan ----------------------------------------------------

(defparameter +chunk-keys+
  (list "text" "duration_ms" "positive_styles" "negative_styles" "context_adherence"
        "conditioning_ref" "condition_strength"))

(defun plan-range (value where)
  (unless (hash-table-p value)
    (fail "~a.range must be {start_ms, end_ms}" where))
  (let ((start (gethash "start_ms" value))
        (end (gethash "end_ms" value)))
    (unless (and (integerp start) (>= start 0))
      (fail "~a.range.start_ms must be a whole number of milliseconds from 0" where))
    (unless (and (integerp end) (plusp end))
      (fail "~a.range.end_ms must be a whole number of milliseconds" where))
    (when (< (- end start) +min-range-ms+)
      (fail "~a.range must cover at least ~d ms (end_ms after start_ms)" where +min-range-ms+))
    value))

(defun kept-slice (chunk where)
  "CHUNK as a kept slice {song_id, range}, checked."
  (let ((song (gethash "song_id" chunk)))
    (unless (and (stringp song) (plusp (length song)))
      (fail "~a.song_id must be an elevenlabs_song_id from an earlier result" where))
    (plan-range (gethash "range" chunk) where)
    (nlk:make-json-object "song_id" song "range" (gethash "range" chunk))))

(defun generation-chunk (chunk where)
  "CHUNK as new audio {text, duration_ms, positive_styles...}, checked."
  (dolist (key (loop for key being the hash-keys of chunk when (stringp key) collect key))
    (unless (member key +chunk-keys+ :test #'string=)
      (fail "~a has an unknown field ~s; the fields are ~{~a~^, ~}" where key +chunk-keys+)))
  (let ((text (gethash "text" chunk))
        (duration (gethash "duration_ms" chunk))
        (styles (gethash "positive_styles" chunk)))
    (unless (and (stringp text) (plusp (length (string-trim '(#\Space) text))))
      (fail "~a.text is required: a section name in brackets, then lyric lines" where))
    (let ((lines (uiop:split-string text :separator '(#\Newline))))
      (when (> (length lines) +max-lines+)
        (fail "~a.text has ~d lines; at most ~d are allowed per chunk" where (length lines) +max-lines+))
      (when (some (lambda (line) (> (length line) +max-line-chars+)) lines)
        (fail "~a.text has a line over ~d characters" where +max-line-chars+)))
    (unless (and (integerp duration) (<= +min-chunk-ms+ duration +max-chunk-ms+))
      (fail "~a.duration_ms must be a whole number of milliseconds from ~d to ~d"
            where +min-chunk-ms+ +max-chunk-ms+))
    (unless (and (vectorp styles) (plusp (length styles)) (every #'stringp styles))
      (fail "~a.positive_styles must list at least one style" where))
    (when (> (length styles) +max-styles+)
      (fail "~a.positive_styles has ~d entries; at most ~d are allowed"
            where (length styles) +max-styles+))
    chunk))

(defun plan-chunks (raw what)
  "RAW as the array of plan chunks ElevenLabs takes, each checked."
  (unless (and (vectorp raw) (not (stringp raw)) (<= 1 (length raw) +max-chunks+))
    (fail "~a must be 1 to ~d chunks" what +max-chunks+))
  (coerce (loop for chunk across raw
                for index from 0
                for where = (format nil "~a[~d]" what index)
                do (unless (hash-table-p chunk)
                     (fail "~a must be an object" where))
                collect (if (gethash "song_id" chunk)
                            (kept-slice chunk where)
                            (generation-chunk chunk where)))
          'vector))

(defun source-track (chunks)
  "The library record of the first stored song CHUNKS reuse, so an edit is
listed under that song's title."
  (loop for chunk across chunks
        for id = (and (hash-table-p chunk)
                       (or (gethash "song_id" chunk)
                           (nlk:json-value (nlk:json-value chunk :object "conditioning_ref")
                                           :string "song_id")))
        when id return (find-track id)))

(defun compose-plan (args)
  "The elevenlabs_music_compose_plan tool: music from an ordered plan of new
sections and kept slices."
  (running-settings)
  (let* ((chunks (plan-chunks (argument args "chunks") "chunks"))
         (configured (setting :music-model))
         (model (choice-argument args "model" +plan-models+
                                 (if (member configured +plan-models+ :test #'string=)
                                     configured "music_v2_5")))
         (seed (integer-argument args "seed" 0 2147483647))
         (finetune (text-argument args "finetune_id" 100))
         (format (output-format))
         (total (/ (loop for chunk across chunks
                         summing (let ((range (gethash "range" chunk)))
                                   (if range
                                       (- (gethash "end_ms" range) (gethash "start_ms" range))
                                       (gethash "duration_ms" chunk))))
                   1000))
         (styles (loop for chunk across chunks
                       for styles = (gethash "positive_styles" chunk)
                       when styles return (format nil "~{~a~^, ~}"
                                                  (loop for style across styles
                                                        repeat 6 collect style))))
         (first-text (loop for chunk across chunks
                           for text = (gethash "text" chunk)
                           when text return (first (uiop:split-string text :separator '(#\Newline)))))
         (parent (source-track chunks))
         (body (nlk:make-json-object
                "composition_plan" (nlk:make-json-object "chunks" chunks)
                "model_id" model "with_timestamps" t
                "store_for_inpainting" (and (setting :store-for-editing) t))))
    (api-key)
    (when seed (setf (gethash "seed" body) seed))
    (when finetune (setf (gethash "finetune_id" body) finetune))
    (billed "song" (format nil "plan, ~d chunks, ~d s" (length chunks) (round total))
            (estimate total)
            (lambda ()
              (detailed body format (or styles first-text) total
                        (and parent (nlk:json-value parent :string "title")))))))

;;; --- a drafted plan, and the finetunes --------------------------------------

(defun draft-plan (args)
  "The elevenlabs_music_draft_plan tool: ElevenLabs writes the plan, free."
  (running-settings)
  (let* ((prompt (text-argument args "prompt" +max-prompt-chars+ :required t))
         (length (number-argument args "length_seconds" +min-song-seconds+ +max-song-seconds+))
         (configured (setting :music-model))
         (model (choice-argument args "model" +plan-models+
                                  (if (member configured +plan-models+ :test #'string=)
                                      configured "music_v2_5")))
         (source (argument args "source_chunks"))
         (body (nlk:make-json-object "prompt" prompt "model_id" model)))
    (api-key)
    (when length
      (setf (gethash "music_length_ms" body) (round (* 1000 length))))
    (when (and (vectorp source) (plusp (length source)))
      (setf (gethash "source_composition_plan" body)
            (nlk:make-json-object "chunks" (plan-chunks source "source_chunks"))))
    (let ((plan (json-object-of
                 (multiple-value-call #'ok
                   (request :post "/v1/music/plan"
                            :content (nlk:encode-json-object body)
                            :content-type "application/json"
                            :timeout +plan-deadline+))
                 "a plan")))
      (let ((chunks (nlk:json-value plan :array "chunks")))
        (unless (and chunks (plusp (length chunks)))
          (fail "ElevenLabs returned a plan with no chunks"))
        (answer (nlk:make-json-object
                 "success" t "model" model "billed" nil
                 "chunks" chunks "sections" (sections plan) "lyrics" (lyrics plan)
                 "next_step" (concatenate
                              'string
                              "Show the user the sections and lyrics; after any edits, pass these chunks "
                              "(and this model) to elevenlabs_music_compose_plan to make it.")))))))

(defun finetunes (args)
  "The elevenlabs_music_finetunes tool: the account's Music finetunes, free."
  (running-settings)
  (let* ((created (choice-argument args "created_by"
                                    '("self" "workspace" "elevenlabs") nil))
         (query (format nil "page_size=100~@[&created_by=~a~]" created))
         (data (json-object-of
                (multiple-value-call #'ok
                  (request :get "/v1/music/finetunes" :query query :timeout 60))
                "a finetune list"))
         (rows (nlk:json-value data :array "finetunes")))
    (unless rows
      (fail "ElevenLabs returned a finetune list this tool cannot read"))
    (answer (nlk:make-json-object
             "success" t "more" (nlk:json-value data :boolean "has_more") "finetunes"
             (coerce (loop for row across rows
                           when (hash-table-p row)
                             collect (nlk:make-json-object
                                      "id" (nlk:json-value row :string "id")
                                      "name" (nlk:json-value row :string "name")
                                      "tags" (nlk:json-value row :array "tags")
                                      "model_id" (nlk:json-value row :string "model_id")
                                      "status" (nlk:json-value row :string "status")
                                      "created_by" (nlk:json-value row :string "created_by")))
                     'vector)))))

;;; --- your own audio, and its stems -----------------------------------------

(defun upload (args)
  "The elevenlabs_music_upload tool: store a song at ElevenLabs so a later
plan can edit, extend or sample it."
  (running-settings)
  (multiple-value-bind (name audio row) (source-audio args)
    (when (and row
               (nlk:json-value row :string "song_id")
               (nlk:json-value row :boolean "stored_for_editing"))
      (fail "track ~a is already stored at ElevenLabs for editing: use elevenlabs_song_id ~s in a compose plan's chunks (uploading it again bills again)"
            (nlk:json-value row :string "id") (nlk:json-value row :string "song_id")))
    (let ((extract (let ((raw (argument args "extract_plan")))
                     (if (null raw) t (eq raw t))))
          (configured (setting :music-model)))
      (api-key)
      (let ((extract-model (if (member configured +plan-models+ :test #'string=)
                               configured "music_v2_5")))
        (billed "upload" (format nil "upload ~a" name)
                (round (+ 30 (/ (length audio) 200000)))
                (lambda ()
                  (multiple-value-bind (body content-type)
                      (multipart-body
                       (append (list (list :field "with_timestamps" "true")
                                     (list :file "file" name (mime-type name) audio))
                               (and extract
                                    (list (list :field "extract_composition_plan" extract-model)))))
                    (let* ((stored (json-object-of
                                    (multiple-value-call #'ok
                                      (request :post "/v1/music/upload" :content body
                                               :content-type content-type
                                               :timeout +song-deadline+))
                                    "an upload answer"))
                           (song-id (nlk:json-value stored :string "song_id"))
                           (plan (nlk:json-value stored :object "composition_plan")))
                      (unless song-id
                        (fail "ElevenLabs accepted the upload (billed) but returned no song id. ~a"
                              +no-retry+))
                      (nlk:make-json-object
                       "success" t "elevenlabs_song_id" song-id
                       "source" (or (and row (nlk:json-value row :string "id")) name)
                       "sections" (sections plan) "lyrics" (lyrics plan)
                       "composition_plan" plan
                       "next_step" (concatenate
                                    'string
                                    "Use elevenlabs_song_id in elevenlabs_music_compose_plan chunks "
                                    "(song_id + range) to keep, extend or condition on parts of this song."))))))))))

(defun save-stems (blob prefix label row)
  "Save every audio member of BLOB, a ZIP archive, as its own track."
  (let ((members (remove-if-not (lambda (member)
                                  (member (string-downcase (or (pathname-type (pathname (car member))) ""))
                                          +audio-exts+ :test #'string=))
                                (zip-members blob)))
        (tracks '()))
    (unless members
      (fail "ElevenLabs' ZIP archive held no audio files (the request was billed). ~a" +no-retry+))
    (when (> (loop for member in members summing (length (cdr member))) +max-stem-total+)
      (fail "ElevenLabs' ZIP archive unpacks to more than ~d MB (the request was billed). ~a"
            (round +max-stem-total+ (* 1024 1024)) +no-retry+))
    (dolist (member members)
      (let* ((name (car member))
             (stem (slug (pathname-name (pathname name))))
             (ext (string-downcase (or (pathname-type (pathname name)) "mp3")))
             (id (track-id))
             (path (save-audio (cdr member) prefix (format nil "~a-~a" stem label) ext id))
             (where (uiop:native-namestring path)))
        (record-track (nlk:make-json-object
                       "id" id "kind" "stem" "title" (format nil "~a (~a)" label stem)
                       "stem" stem "file_path" where "bytes" (length (cdr member))
                       "parent_id" (and row (nlk:json-value row :string "id"))))
        (push (nlk:make-json-object "track_id" id "stem" stem "file_path" where
                                     "media_tag" (concatenate 'string "MEDIA:" where)
                                     "bytes" (length (cdr member)))
              tracks)))
    (coerce (nreverse tracks) 'vector)))

(defun stems (args)
  "The elevenlabs_music_stems tool: split a song into two or six stems."
  (running-settings)
  (multiple-value-bind (name audio row) (source-audio args)
    (let ((variation (if (string= (or (choice-argument args "stems" '("two" "six") "six") "six")
                                  "two")
                         "two_stems_v1" "six_stems_v1"))
          (format (legacy-format))
          (label (or (and row (nlk:json-value row :string "title")) (pathname-name (pathname name)))))
      (api-key)
      (billed "stems" (format nil "stems of ~a" label)
              (round (+ 45 (/ (length audio) 100000)))
              (lambda ()
                (multiple-value-bind (body content-type)
                    (multipart-body (list (list :field "stem_variation_id" variation)
                                          (list :file "file" name (mime-type name) audio)))
                  (let* ((blob (multiple-value-call #'ok
                                 (request :post "/v1/music/stem-separation"
                                          :query (format nil "output_format=~a" format)
                                          :content body :content-type content-type
                                          :timeout +song-deadline+)))
                         (tracks (save-stems blob "stem" label row)))
                    (nlk:make-json-object
                     "success" t "source" (or (and row (nlk:json-value row :string "id")) name)
                     "stems" tracks
                     "media_tags" (loop for track across tracks
                                         collect (nlk:json-value track :string "media_tag"))
                     "deliver" +deliver+))))))))


;;; --- scoring a video --------------------------------------------------------

(defparameter +content-exts+
  (list (cons "audio/mpeg" "mp3") (cons "audio/mp3" "mp3") (cons "audio/ogg" "opus")
        (cons "audio/opus" "opus") (cons "audio/wav" "wav") (cons "audio/x-wav" "wav")
        (cons "audio/flac" "flac") (cons "audio/aac" "aac") (cons "audio/mp4" "m4a"))
  "The extension each content type an audio answer arrives as is.")

(defun zip-p (octets)
  (and (>= (length octets) 2) (= (aref octets 0) 80) (= (aref octets 1) 75)))

(defun save-video-answer (octets headers label format)
  "The scored audio as tracks: one file, or every member of a ZIP answer."
  (if (zip-p octets)
      (save-stems octets "score" label nil)
      (let* ((type (string-downcase (or (header-value headers "content-type") "audio/mpeg")))
             (kind (subseq type 0 (or (position #\; type) (length type))))
             (ext (or (cdr (assoc kind +content-exts+ :test #'string=))
                      (cdr (assoc format +formats+ :test #'string=))
                      "mp3"))
             (id (track-id))
             (path (save-audio octets "score" label ext id))
             (where (uiop:native-namestring path)))
        (record-track (nlk:make-json-object "id" id "kind" "score" "title" label
                                             "file_path" where "bytes" (length octets)))
        (vector (nlk:make-json-object "track_id" id "file_path" where
                                       "media_tag" (concatenate 'string "MEDIA:" where)
                                       "bytes" (length octets))))))

(defun from-video (args)
  "The elevenlabs_music_from_video tool: background music timed to videos."
  (running-settings)
  (let ((raw (argument args "video_paths")))
    (unless (and (vectorp raw) (<= 1 (length raw) 10))
      (fail "video_paths must list 1 to 10 video files"))
    (let* ((paths (loop for index from 0
                        for item across raw
                        do (unless (stringp item)
                             (fail "video_paths[~d] must be a path" index))
                        collect (local-file item (format nil "video_paths[~d]" index)
                                             +video-exts+ +max-video-bytes+)))
           (total (loop for path in paths
                        summing (with-open-file (in path :element-type '(unsigned-byte 8))
                                  (file-length in))))
           (description (text-argument args "description" +max-prompt-chars+))
           (tags (string-list-argument args "tags" 10 :item-chars 100))
           (model (choice-argument args "model" +models+ (setting :music-model)))
           (flags (loop for key in '("add_sfx" "separate_tracks" "use_audio_as_reference")
                        collect (cons key (boolean-argument args key))))
           (format (legacy-format))
           (label (or description (and tags (format nil "~{~a~^, ~}" tags))
                      (pathname-name (pathname (first paths))))))
      (when (> total +max-video-bytes+)
        (fail "the videos add up to more than ~d MB; ElevenLabs accepts at most that in total"
              (round +max-video-bytes+ (* 1024 1024))))
      (api-key)
      (billed "score" (format nil "score for ~a" (pathname-name (pathname (first paths))))
              (round (+ 90 (/ total 2000000)))
              (lambda ()
                (multiple-value-bind (body content-type)
                    (multipart-body
                     (append (list (list :field "model_id" model))
                             (loop for (key . value) in flags
                                   collect (list :field key (if value "true" "false")))
                             (and description (list (list :field "description" description)))
                             (loop for tag in tags collect (list :field "tags" tag))
                             (loop for path in paths
                                   collect (list :file "videos" (file-namestring path)
                                                 (mime-type path :video t)
                                                 (read-octets path)))))
                  (multiple-value-bind (octets headers)
                      (multiple-value-call #'ok
                        (request :post "/v1/music/video-to-music"
                                 :query (format nil "output_format=~a" format)
                                 :content body :content-type content-type
                                 :timeout +song-deadline+))
                    (let ((tracks (save-video-answer octets headers label format)))
                      (nlk:make-json-object
                       "success" t "model" model "tracks" tracks
                       "videos" (coerce (loop for path in paths collect (file-namestring path))
                                         'vector)
                       "media_tags" (loop for track across tracks
                                           collect (nlk:json-value track :string "media_tag"))
                       "deliver" +deliver+)))))))))

;;; --- a job already running ------------------------------------------------

(defun job-tool (args)
  "The elevenlabs_music_job tool: check on, or wait for, a generation."
  (running-settings)
  (let* ((id (text-argument args "job_id" 40 :required t))
         (wait (or (number-argument args "wait_seconds" 0 240) 0))
         (job (gethash id *jobs*)))
    (unless job
      (fail "no music job ~s in this process (job ids last as long as the organism runs)" id))
    (answer (job-text (job-wait job wait)))))
