;;;; cell.lisp --- what a session did, cut to a length, drawn as captions, encoded by ffmpeg.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-showreel: the original recorded a session through six
;;;; observer hooks into a SQLite table of its own; Nodecode already keeps that
;;;; history as facts in the event log, so this cell reads it there -- the ask
;;;; (turn.input_committed), each tool call and its result
;;;; (turn.tool_call_started, turn.tool_result), the answer
;;;; (turn.assistant_message_completed) and the spend (turn.usage) -- and
;;;; writes nothing of its own but the video, its .srt and its poster.
;;;;
;;;; The storyboard is a function of the log: the same session and settings
;;;; always give the same beats. A failing step and the step that fixed it
;;;; are kept together, the whole cut fits a hard 89 s, and every string is
;;;; scrubbed of secrets twice: once when it is read out of the log, once
;;;; right before it is drawn.
;;;;
;;;; Rendering is your own ffmpeg: no Pillow, no bundled binary, nothing
;;;; downloaded. Each beat is one drawtext filter over a coloured frame and
;;;; its caption is a file beside the filter, so no caption text is ever
;;;; parsed as a filter argument.

(in-package #:nodecode-showreel)

;;; --- the manual ---------------------------------------------------------------
;;; (help :showreel) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "Showreel turns a session into a short demo video, through the nodecode-showreel
cell: one Lisp function called through eval, and the showreel_render tool.
  (showreel:render &key session-id length aspect title)
      Cuts SESSION-ID (default: this session) into an MP4 of LENGTH seconds
      (10 to 89, default: the section's default_length) in ASPECT (16:9, 9:16
      or 1:1) with burned-in captions, plus an .srt and a poster PNG, and
      answers where they went. It reads the session's own recorded history --
      the ask, the tool calls and their results, the answer, the spend -- so a
      session with nothing done yet has nothing to show.
  /showreel            the bench: this session, the folder renders land in, ffmpeg
  /showreel cut [seconds] [16:9|9:16|1:1] [title...]   render this session
  /showreel plan [seconds]   the beats and captions, rendered nothing
  /showreel list            recent sessions
  /showreel status          capture, ffmpeg and folder status
  /showreel demo [seconds] [aspect]   a built-in sample session, no session needed
What it needs: ffmpeg on PATH (or the section's ffmpeg_path) and a font file
(the section's font_file, or one found in the usual places). Nothing is
uploaded: rendering is local, and no narration is made."
  "What (help :showreel) answers while the cell runs.")

;;; --- the words ----------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *aspects* '("16:9" "9:16" "1:1")
  "The three shapes a cut can be.")

(defparameter *hard-cap* 89
  "The longest cut, in seconds: the hard budget the storyboard never exceeds.")

(defparameter +usage+
  "Usage: /showreel [cut|plan|list|status|demo] [seconds] [16:9|9:16|1:1] [title...]"
  "What /showreel answers when it does not know the verb.")

(defparameter +shell-tools+
  '("snippet" "run" "bash" "shell" "sh" "exec_command" "eval")
  "The tools whose call is a command run, as the engine spells them.")

(defparameter +detail-keys+
  '("command" "cmd" "path" "file" "file_path" "pattern" "query" "url" "text" "expression")
  "Where a call's one useful line lives in its arguments, in the order tried.")

(defparameter *run-program* nil
  "How a program is run, or NIL for the default runner over UIOP: (values
OUTPUT EXIT-STATUS). A test rebinds it.")

;;; --- small text ---------------------------------------------------------------

(defun field (object key)
  "KEY's value in OBJECT, a decoded JSON object, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun field-string (object key)
  "KEY's value in OBJECT as a string, or NIL when it is absent or a container."
  (let ((value (field object key)))
    (cond ((stringp value) value)
          ((or (null value) (hash-table-p value) (vectorp value)) nil)
          (t (princ-to-string value)))))

(defun field-number (object key)
  "KEY's value in OBJECT as a number, or 0."
  (let ((value (field object key)))
    (if (realp value) value 0)))

(defun content-text (object)
  "The text a wire message, or the payload holding one, carries."
  (cond ((null object) "")
        ((stringp object) object)
        ((hash-table-p object)
         (or (let ((text (field-string object "text")))
               (and text (plusp (length text)) text))
             (let ((content (field object "content")))
               (if content (content-text content) nil))
             (let ((message (field object "message")))
               (if message (content-text message) nil))
             ""))
        ((vectorp object)
         (format nil "~{~a~^ ~}"
                 (remove "" (map 'list #'content-text (coerce object 'list)) :test #'string=)))
        (t "")))

(defun one-line (text)
  "TEXT on one line, runs of space made single."
  (nlk:one-line (or text "")))

(defun clipped (text limit)
  "TEXT cut to LIMIT characters, an ellipsis where it was cut."
  (let ((text (one-line text)))
    (if (<= (length text) limit)
        text
        (format nil "~a…" (string-right-trim " ,;:-" (subseq text 0 (1- limit)))))))

(defun headline (text)
  "The first sentence of TEXT, without its punctuation."
  (let* ((text (one-line text))
         (stop (or (position-if (lambda (c) (find c ".!?")) text) (length text))))
    (string-right-trim " .!?:;" (subseq text 0 stop))))

(defun wrapped (text columns &optional (rows 3))
  "TEXT broken at COLUMNS into at most ROWS lines."
  (let ((lines '()) (line ""))
    (dolist (word (uiop:split-string (one-line text) :separator '(#\Space #\Tab)))
      (unless (zerop (length word))
        (cond ((zerop (length line)) (setf line word))
              ((<= (+ (length line) 1 (length word)) columns)
               (setf line (concatenate 'string line " " word)))
              (t (push line lines) (setf line word)))))
    (when (plusp (length line)) (push line lines))
    (let* ((lines (nreverse lines))
           (kept (subseq lines 0 (min rows (length lines)))))
      (if (> (length lines) rows)
          (append (butlast kept) (list (clipped (car (last kept)) columns)))
          kept))))

;;; --- scrubbing ----------------------------------------------------------------
;;; Best effort, twice over: once on the way out of the log and again on the
;;; way into a frame. It is a scrubber, not a guarantee.

(defparameter +secret-shapes+
  '("sk-" "sk_" "ghp_" "gho_" "ghu_" "ghs_" "ghr_" "github_pat_" "github_pat_"
    "xoxb-" "xoxp-" "AKIA" "ASIA" "AIza" "hf_" "npm_" "eyJ" "-----BEGIN")
  "What a token starts with when it is a credential.")

(defparameter +secret-words+
  '("key" "token" "secret" "password" "passwd" "auth" "credential" "cookie" "apikey" "api_key")
  "What a `name=value' pair's name holds when its value is a credential.")

(defun suspicious-token-p (token)
  "Whether TOKEN looks like a credential rather than a word."
  (and (>= (length token) 20)
       (some (lambda (prefix) (uiop:string-prefix-p prefix token)) +secret-shapes+)))

(defun secret-pair-p (token)
  "Whether TOKEN is a `secret_name=value' or `secret_name: value' pair."
  (let ((at (position-if (lambda (c) (find c "=:")) token)))
    (and at (> at 0)
         (let ((name (string-downcase (subseq token 0 at))))
           (and (some (lambda (word) (search word name)) +secret-words+)
                (> (length token) (1+ at)))))))

(defun substitute-all (text old new)
  "TEXT with every OLD replaced by NEW."
  (if (zerop (length old))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search old text :start2 start)
              do (cond (at (write-string text out :start start :end at)
                           (write-string new out)
                           (setf start (+ at (length old))))
                       (t (write-string text out :start start)
                          (return)))))))

(defun email-p (token)
  "Whether TOKEN looks like an email address."
  (let ((at (position #\@ token)))
    (and at (> at 0) (find #\. token :start at) (> (length token) (1+ at)))))

(defun scrub (text)
  "TEXT with credentials and the home path masked. => (values CLEAN COUNT)."
  (let ((count 0)
        (home (ignore-errors (namestring (nlk:home-path "")))))
    (when (and home (> (length home) 3))
      (let ((home (string-right-trim "/" home)))
        (when (search home text)
          (setf text (substitute-all text home "~")))))
    (let ((out '()))
      (dolist (token (uiop:split-string text :separator '(#\Space #\Tab #\Newline)))
        (push (cond ((suspicious-token-p token) (incf count) "[redacted]")
                    ((secret-pair-p token)
                     (incf count)
                     (let ((at (position-if (lambda (c) (find c "=:")) token)))
                       (concatenate 'string (subseq token 0 (1+ at)) "[redacted]")))
                    ((email-p (string-trim "()[],;\"'" token))
                     (incf count)
                     (substitute-all token (string-trim "()[],;\"'" token) "[email]"))
                    (t token))
              out))
      (values (format nil "~{~a~^ ~}" (nreverse out)) count))))

;;; --- what a session did ---------------------------------------------------------

(defun payloads (session-id kind &key (order :oldest) limit)
  "The payload objects of SESSION-ID's facts of KIND, in ORDER."
  (apply #'nlk:events :session-id session-id :kind kind :order order :as :payloads
                      (when limit (list :limit limit))))

(defun ask-of (session-id)
  "The first thing SESSION-ID was asked."
  (let ((object (first (payloads session-id nlk::+kind-input-committed+ :order :newest :limit 1))))
    (nlk:trimmed (content-text object))))

(defun reply-of (session-id)
  "The newest answer SESSION-ID gave."
  (let ((object (first (payloads session-id nlk::+kind-assistant-completed+
                                   :order :newest :limit 1))))
    (nlk:trimmed (content-text object))))

(defun argument-line (arguments)
  "The one useful line among a call's ARGUMENTS, or \"\"."
  (let ((arguments (if (stringp arguments)
                       (or (ignore-errors (nlk:decode-json arguments)) arguments)
                       arguments)))
    (or (loop for key in +detail-keys+
            for value = (field-string arguments key)
              when (and value (plusp (length (nlk:trimmed value))))
                return value)
        "")))

(defun steps-of (session-id)
  "The tool calls SESSION-ID made, oldest first, each a plist of :name
:detail :ok :ms :text."
  (let ((results (make-hash-table :test #'equal))
        (steps '()))
    (dolist (object (payloads session-id nlk::+kind-tool-result+))
      (let ((call (field-string object "call-id")))
        (when call (setf (gethash call results) object))))
    (dolist (object (payloads session-id nlk::+kind-tool-call-started+))
      (let* ((call (field-string object "call-id"))
             (name (or (field-string object "tool-name") "tool"))
             (result (and call (gethash call results)))
             (text (or (and result (field-string result "result")) ""))
             (failed (uiop:string-prefix-p "ERROR:" text)))
        (push (list :name name
                    :detail (clipped (argument-line (field object "arguments")) 90)
                    :ok (not failed)
                    :ms (and result (field-number result "duration-ms"))
                    :text text)
              steps)))
    (nreverse steps)))

(defun usage-of (session-id)
  "SESSION-ID's spend: (values INPUT-TOKENS OUTPUT-TOKENS ROUNDS)."
  (let ((input 0) (output 0) (rounds 0))
    (dolist (object (payloads session-id nlk::+kind-turn-usage+))
      (unless (eql 1 (field object "voided"))
        (incf input (field-number object "input-tokens"))
        (incf output (field-number object "output-tokens"))
        (incf rounds)))
    (values input output rounds)))

(defun session-name (session-id)
  "SESSION-ID's title, or its id."
  (or (ignore-errors (nlk::session-title session-id)) session-id))

(defun session-id-of (given)
  "GIVEN, or the session this call is for, or a refusal."
  (or (and given (plusp (length (nlk:trimmed given))) (nlk:trimmed given))
      (getf (nle:turn) :session-id)
      (fail "no session: name one, or call this from a session's turn")))

;;; --- the storyboard ---------------------------------------------------------------

(defun beat (kind label caption score base)
  "One beat: what it is, the word above its caption, the caption, its score
and its base seconds."
  (list :kind kind :label label :caption caption :score score :base base))

(defun step-beat (step)
  "One beat for a tool call STEP, scored by what it showed."
  (let* ((name (getf step :name))
         (shell (member name +shell-tools+ :test #'string=))
         (detail (getf step :detail))
         (body (or (getf step :text) ""))
         (caption (if (plusp (length (nlk:trimmed body)))
                      (or (first (remove "" (uiop:split-string (one-line body)
                                                                :separator '(#\Newline))
                                         :test #'string=))
                          detail)
                      detail)))
    (beat (if shell "terminal" "tool")
          (if (getf step :ok) (string-upcase name) (format nil "~a · FAILED" (string-upcase name)))
          (if (plusp (length (nlk:trimmed detail)))
              (format nil "~a~@[ → ~a~]" detail (clipped caption 70))
              (clipped caption 90))
          (cond ((not (getf step :ok)) 3.0)
                ((and shell (plusp (length (nlk:trimmed body)))) 2.0)
                (shell 1.6)
                (t 1.0))
          (if shell 4.0 3.0))))

(defun picked-steps (steps limit)
  "The LIMIT steps that carry the story: the best scored, a failure kept with
the step that fixed it, in the order they happened."
  (let* ((indexed (loop for step in steps for i from 0 collect (cons i step)))
         (failed (remove-if (lambda (pair) (getf (cdr pair) :ok)) indexed))
         (fixed (loop for (i . step) in failed
                      append (loop for (j . later) in indexed
                                   when (and (> j i) (getf later :ok)
                                             (string= (getf later :name) (getf step :name)))
                                     return (list (cons j later)))))
         (chosen (remove-duplicates (append failed fixed) :key #'car :test #'=))
         (rest (sort (remove-if (lambda (pair) (member (car pair) chosen :key #'car))
                                indexed)
                     #'> :key (lambda (pair) (getf (cdr pair) :score)))))
    (let ((chosen (sort (append chosen
                                 (subseq rest 0 (min (length rest)
                                                     (max 0 (- limit (length chosen))))))
                        #'< :key #'car)))
      (mapcar #'cdr chosen))))

(defun fit (beats target)
  "BEATS' durations scaled to TARGET seconds, never over the hard cap, each
started where the one before it ended. => the beats, each with :start."
  (let* ((total (reduce #'+ beats :key (lambda (b) (getf b :base)) :initial-value 0.0))
         (target (min (float target 1.0) (float *hard-cap* 1.0)))
         (factor (if (plusp total) (max 0.3 (min 1.4 (/ target total))) 1.0))
         (start 0.0))
    (mapcar (lambda (b)
              (let ((duration (max 1.2 (round (* factor (getf b :base)) 1))))
                (prog1 (list* :start start :duration duration b)
                  (incf start duration))))
            beats)))

(defun storyboard (session-id &key title limit target)
  "The beats a cut of SESSION-ID carries, fitted to TARGET seconds; NIL when
the session recorded nothing."
  (let* ((ask (ask-of session-id))
         (reply (reply-of session-id))
         (steps (steps-of session-id))
         (name (or title (session-name session-id)))
         (beats (append
                 (list (beat "title" "SHOWREEL" (clipped name 60) 0 3.0))
                 (when (plusp (length ask))
                   (list (beat "ask" "THE ASK" (clipped (headline ask) 120) 0 4.5)))
                 (mapcar #'step-beat (picked-steps steps (or limit 5)))
                 (when (plusp (length reply))
                   (list (beat "result" "THE ANSWER" (clipped reply 140) 0 4.5)))
                 (let ((input 0) (output 0) (rounds 0))
                   (multiple-value-setq (input output rounds) (usage-of session-id))
                   (list (beat "stats" "THE SPEND"
                               (format nil "~d step~:p · ~d round~:p · ~d~@[ + ~d~] tokens"
                                       (length steps) rounds input output)
                               0 3.0)))
                 (list (beat "outro" "SHOWREEL"
                             "written by Nodecode · rendered locally, nothing uploaded"
                             0 2.6)))))
    (if (or (plusp (length ask)) steps (plusp (length reply)))
        (fit beats (or target (setting :default-length)))
        nil)))

(defun demo-storyboard (&key target)
  "The built-in sample session's beats, so a cut can be made with no session."
  (fit (list (beat "title" "SHOWREEL" "A tiny snake game, in one session" 0 3.0)
             (beat "ask" "THE ASK" "Build a snake game that runs in the browser" 0 4.0)
             (beat "terminal" "SNIPPET" "npm test → 1 failing: the snake eats itself" 2.0 4.0)
             (beat "terminal" "SNIPPET · FIXED"
                   "npm test → 12 passing, the body skips its own tail" 3.0 4.5)
             (beat "tool" "WRITE_FILE" "snake.html → 214 lines" 1.0 3.0)
             (beat "result" "THE ANSWER" "Open snake.html: arrows turn, food scores." 0 4.5)
             (beat "stats" "THE SPEND" "7 steps · 4 rounds · 41k + 6k tokens" 0 3.0)
             (beat "outro" "SHOWREEL" "written by Nodecode · rendered locally" 0 2.6))
       (or target (setting :default-length))))

;;; --- drawing ------------------------------------------------------------------------

(defun render-dir ()
  "The folder renders land in, made when it is not there."
  (let ((dir (uiop:ensure-directory-pathname (nlk:expand-home (setting :output-dir)))))
    (ensure-directories-exist dir)
    dir))

(defparameter +font-candidates+
  '("/usr/share/fonts/dejavu-sans-fonts/DejaVuSans.ttf"
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    "/usr/share/fonts/dejavu/DejaVuSans.ttf"
    "/usr/share/fonts/TTF/DejaVuSans.ttf"
    "/usr/share/fonts/liberation-sans-fonts/LiberationSans-Regular.ttf"
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
    "/usr/share/fonts/noto-sans-fonts/NotoSans-Regular.ttf"
    "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    "/usr/local/share/fonts/DejaVuSans.ttf"
    "/System/Library/Fonts/Helvetica.ttc"
    "C:/Windows/Fonts/arial.ttf")
  "Where a font is looked for when the section names none.")

(defun find-on-path (name)
  "The first NAME on PATH, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                             :separator '(#\: #\;))
        for path = (and (plusp (length directory))
                        (probe-file (merge-pathnames name
                                                      (uiop:ensure-directory-pathname directory))))
        when path return path))

(defun font-file ()
  "The font a cut draws with, or a refusal naming the setting."
  (or (let ((named (setting :font-file)))
        (when (and named (plusp (length (nlk:trimmed named))))
          (let ((path (uiop:parse-native-namestring (nlk:expand-home (nlk:trimmed named)))))
            (if (probe-file path) path (fail "font_file is set to ~a, which is not a file" named)))))
      (loop for candidate in +font-candidates+
            for path = (probe-file candidate)
            when path return path)
      (let ((home (ignore-errors (probe-file (nlk:home-path ".fonts/DejaVuSans.ttf")))))
        home)
      (fail "no font found: set font_file in the showreel section to a TTF this machine has")))

(defun size-for (resolution aspect)
  "The frame (values WIDTH HEIGHT) a cut is drawn at."
  (let ((tall (equal resolution "720p")))
    (cond ((string= aspect "9:16") (if tall (values 720 1280) (values 1080 1920)))
          ((string= aspect "1:1") (if tall (values 720 720) (values 1080 1080)))
          (t (if tall (values 1280 720) (values 1920 1080))))))

(defun beat-text (beat)
  "What one beat's caption file holds: its word, a blank line, its lines."
  (format nil "~a~%~%~{~a~^~%~}~%"
          (string-downcase (getf beat :label))
          (wrapped (scrub (getf beat :caption)) 44 3)))

(defun drawtext (index start end font-size)
  "One beat's drawtext filter: its caption file, centred, for its span."
  (format nil "drawtext=fontfile=font.ttf:textfile=cap~d.txt:fontcolor=0xf2f5ff:fontsize=~d:line_spacing=18:x=(w-text_w)/2:y=(h-text_h)/2:enable='between(t,~,2f,~,2f)'"
          index font-size start end))

(defun filter-graph (beats width)
  "Every beat's drawtext, chained."
  (format nil "~{~a~^,~}"
          (loop for beat in beats for i from 0
                collect (drawtext i (getf beat :start)
                                  (+ (getf beat :start) (getf beat :duration))
                                  (round (/ width 34))))))

(defun srt-stamp (seconds)
  "SECONDS as an .srt clock."
  (multiple-value-bind (seconds fraction) (floor seconds)
    (multiple-value-bind (minutes seconds) (floor seconds 60)
      (multiple-value-bind (hours minutes) (floor minutes 60)
        (format nil "~2,'0d:~2,'0d:~2,'0d,~3,'0d"
                hours minutes seconds (round (* fraction 1000)))))))

(defun write-srt (beats path)
  "BEATS as a subtitle file at PATH. => PATH."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (loop for beat in beats for i from 1
          do (format out "~d~%~a --> ~a~%~a~%~%"
                     i (srt-stamp (getf beat :start))
                     (srt-stamp (+ (getf beat :start) (getf beat :duration)))
                     (scrub (getf beat :caption)))))
  path)

(defun default-runner (argv &key directory)
  "Run ARGV through UIOP; => (values OUTPUT EXIT-STATUS)."
  (let ((status 0))
    (values (with-output-to-string (out)
              (setf status
                    (let ((values (multiple-value-list
                                   (uiop:run-program argv :directory directory
                                                          :output out :error-output out
                                                          :ignore-error-status t))))
                      (or (find-if #'integerp values) 0))))
            status)))

(defun run (argv &key directory)
  "Run ARGV; => (values OUTPUT EXIT-STATUS)."
  (funcall (or *run-program* #'default-runner) argv :directory directory))

(defun render-video (beats path &key aspect resolution)
  "Encode BEATS into an MP4 at PATH with the user's ffmpeg, write its .srt and
its poster beside it, and answer a plist of where they went."
  (let* ((ffmpeg (or (let ((named (setting :ffmpeg-path)))
                       (and named (plusp (length (nlk:trimmed named)))
                            (uiop:parse-native-namestring (nlk:expand-home (nlk:trimmed named)))))
                     (or (find-on-path "ffmpeg")
                         (find-on-path "ffmpeg.exe")
                         (fail "ffmpeg is not on PATH: install it, or set ffmpeg_path"))))
         (font (font-file))
         (work (merge-pathnames (format nil "showreel-work-~d/" (get-universal-time))
                                (render-dir)))
         (duration (reduce #'+ beats :key (lambda (b) (+ (getf b :start) (getf b :duration)))
                           :initial-value 0.0)))
    (multiple-value-bind (width height) (size-for resolution aspect)
      (ensure-directories-exist work)
      (unwind-protect
           (progn
             (uiop:copy-file font (merge-pathnames "font.ttf" work))
             (loop for beat in beats for i from 0
                   do (with-open-file (out (merge-pathnames (format nil "cap~d.txt" i) work)
                                           :direction :output :if-exists :supersede
                                           :if-does-not-exist :create :external-format :utf-8)
                        (write-string (beat-text beat) out)))
             (let ((argv (list (namestring ffmpeg) "-hide_banner" "-loglevel" "error" "-y"
                               "-f" "lavfi"
                               "-i" (format nil "color=c=0x0d1020:s=~dx~d:r=25:d=~,2f" width height duration)
                               "-vf" (filter-graph beats width)
                               "-c:v" "libx264" "-preset" "veryfast" "-crf" "21"
                               "-pix_fmt" "yuv420p" "-movflags" "+faststart"
                               (namestring path))))
               (multiple-value-bind (output status) (run argv :directory work)
                 (unless (and (eql status 0) (probe-file path))
                   (fail "ffmpeg exited ~d: ~a" status (clipped (or output "") 300)))))
             (let ((poster (merge-pathnames (make-pathname :type "png" :defaults path))))
               (run (list (namestring ffmpeg) "-hide_banner" "-loglevel" "error" "-y"
                          "-i" (namestring path) "-frames:v" "1" (namestring poster))
                    :directory work)
               (list :file (namestring path)
                     :srt (namestring (write-srt beats (make-pathname :type "srt" :defaults path)))
                     :poster (and (probe-file poster) (namestring poster))
                     :duration (round duration 1)
                     :size (format nil "~dx~d" width height))))
        (ignore-errors (uiop:delete-directory-tree work :validate t :if-does-not-exist :ignore))))))

(defun render-name (title aspect)
  "A file name for a cut: the title made a slug, the aspect, the clock."
  (let* ((slug (string-downcase
                (with-output-to-string (out)
                  (let ((dash nil))
                    (loop for char across (one-line (or title "session"))
                          do (cond ((or (alphanumericp char) (find char "._-"))
                                    (write-char char out) (setf dash nil))
                                   ((not dash) (write-char #\- out) (setf dash t))))))))
         (slug (string-trim "-." (if (> (length slug) 40) (subseq slug 0 40) slug))))
    (format nil "showreel-~a-~a-~d.mp4"
            (if (plusp (length slug)) slug "session")
            (substitute #\x #\: aspect)
            (get-universal-time))))

;;; --- the verb ------------------------------------------------------------------

(define-verb render (&key session-id length aspect title)
  "Cut a session into a captioned demo video with the user's own ffmpeg.
=> where the MP4, its .srt and its poster went, and what the cut carries."
  (let* ((session (session-id-of session-id))
         (aspect (or (and aspect (plusp (length aspect)) aspect) (setting :default-aspect)))
         (length (or length (setting :default-length)))
         (title (and title (nlk:trimmed title)))
         (beats (progn
                  (unless (member aspect *aspects* :test #'string=)
                    (fail "aspect must be one of ~{~a~^, ~}" *aspects*))
                  (when (or (< length 10) (> length *hard-cap*))
                    (fail "a cut is 10 to ~d seconds long, not ~a" *hard-cap* length))
                  (storyboard session :title title :target length))))
    (when (null beats)
      (fail "session ~a has nothing recorded yet" session))
    (let* ((path (merge-pathnames (render-name (or title (session-name session)) aspect)
                                   (render-dir)))
           (where (render-video beats path :aspect aspect :resolution (setting :resolution)))
           (secrets (loop for beat in beats sum (nth-value 1 (scrub (getf beat :caption))))))
      (format nil "Showreel cut: ~as ~a · ~d beats (title → ~{~a~^ → ~} → outro)~%~
                   video: ~a~%captions: ~a~@[~%poster: ~a~]~%~
                   ~d secret~:p redacted · ~a · rendered locally with ffmpeg, nothing uploaded"
              (getf where :duration) aspect (length beats)
              (mapcar (lambda (beat) (getf beat :kind)) (subseq beats 1 (1- (length beats))))
              (getf where :file) (getf where :srt) (getf where :poster)
              secrets (getf where :size)))))

(defun plan-text (session-id &key title length)
  "The beats and captions a cut would carry, rendered nothing."
  (let ((beats (storyboard (session-id-of session-id) :title title
                                                      :target (or length (setting :default-length)))))
    (when (null beats)
      (fail "session ~a has nothing recorded yet" session-id))
    (format nil "Showreel plan: ~d beats, ~as of ~as~%~{~a~^~%~}"
            (length beats)
            (reduce #'+ beats :key (lambda (b) (getf b :duration)) :initial-value 0.0)
            (or length (setting :default-length))
            (loop for beat in beats
                  collect (format nil "  ~,1f  ~10a ~a"
                                  (getf beat :start) (getf beat :kind)
                                  (clipped (scrub (getf beat :caption)) 84))))))

(defun list-text (&optional (limit 10))
  "The sessions this profile has, newest first."
  (let ((rows (ignore-errors (nlk:list-sessions))))
    (if (null rows)
        "showreel: no sessions on this profile yet"
        (format nil "showreel: ~d session~:p, newest first~%~{  ~a  ~a~^~%~}"
                (length rows)
                (loop for row in (subseq rows 0 (min limit (length rows)))
                      collect (list (clipped (or (getf row :title) (getf row :id) "") 56)
                                    (or (getf row :id) "")))))))

(defun status-text ()
  "What this profile has to cut with."
  (let ((ffmpeg (or (ignore-errors (find-on-path "ffmpeg"))
                    (ignore-errors (find-on-path "ffmpeg.exe"))))
        (font (ignore-errors (font-file)))
        (dir (ignore-errors (render-dir))))
    (format nil "showreel: ffmpeg ~a · font ~a · renders in ~a · default ~as ~a~%~
                 ~d session~:p on this profile"
            (or ffmpeg "NOT FOUND (set ffmpeg_path)")
            (or font "NOT FOUND (set font_file)")
            (or dir "?")
            (setting :default-length) (setting :default-aspect)
            (length (ignore-errors (nlk:list-sessions))))))

(defun render-demo (&key length aspect)
  "A cut of the built-in sample session, no session needed."
  (let* ((aspect (or aspect (setting :default-aspect)))
         (beats (demo-storyboard :target (or length (setting :default-length))))
         (path (merge-pathnames (render-name "showreel-demo" aspect) (render-dir)))
         (where (render-video beats path :aspect aspect :resolution (setting :resolution))))
    (format nil "Showreel demo: ~as ~a · ~d beats (built-in sample session)~%video: ~a~%captions: ~a"
            (getf where :duration) aspect (length beats) (getf where :file) (getf where :srt))))

;;; --- /showreel ------------------------------------------------------------------

(defun run-command (args session-id)
  "/showreel typed alone: the bench and a panel of what it can do; its lines
are the subcommands. => the answer's text, and the panel when there is one."
  (let* ((text (nlk:trimmed (or args "")))
         (parts (remove "" (uiop:split-string text :separator '(#\Space #\Tab)) :test #'string=))
         (sub (string-downcase (or (first parts) "")))
         (rest (rest parts))
         (length (and rest (ignore-errors (parse-integer (first rest) :junk-allowed t))))
         (aspect (find-if (lambda (word) (member word *aspects* :test #'string=)) rest)))
    (cond
      ((member sub '("help" "-h" "--help") :test #'string=) +usage+)
      ((string= sub "list") (list-text))
      ((string= sub "status") (status-text))
      ((string= sub "plan")
       (plan-text (or session-id (fail "this shell has no session")) :length length))
      ((string= sub "demo") (render-demo :length length :aspect aspect))
      ((member sub '("" "cut") :test #'string=)
       (if (null session-id)
           (values (status-text)
                   (nle:list-dialog
                    :title "Showreel" :summary "No session in this shell: try the demo"
                    :rows (list (nle:run-row "Render the built-in demo" "/showreel demo"
                                             :detail "a sample session, about 30 seconds")
                                (nle:run-row "Where renders land" "/showreel status"
                                             :detail "ffmpeg, font and folder"))))
           (values (render :session-id session-id :length length :aspect aspect
                           :title (and (> (length rest) 0)
                                       (format nil "~{~a~^ ~}"
                                               (remove-if (lambda (word)
                                                            (or (member word *aspects* :test #'string=)
                                                                (every #'digit-char-p word)))
                                                          rest))))
                   (nle:list-dialog
                    :title "Showreel" :summary "Cut it, or look first"
                    :rows (list (nle:run-row "Plan the beats" "/showreel plan"
                                             :detail "the captions, rendered nothing")
                                (nle:run-row "Knock it down to 30 s" "/showreel cut 30"
                                             :detail "a shorter cut of this session")
                                (nle:run-row "Vertical, for a phone" "/showreel cut 45 9:16"
                                             :detail "1080x1920"))))))
      (t +usage+))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract showreel-render
  (:doc "Render a short demo video (an MP4 with burned-in captions, plus an .srt and a poster PNG) of a Nodecode session's recorded history: the ask, the key steps, the answer and the spend. Rendering is local with the user's own ffmpeg, secrets are redacted before drawing, and nothing is uploaded. Defaults to this session. Use it when the user asks for a demo video, a recap video, a reel or a clip of what was done.")
  ("session_id" :text
                :doc "the session to cut; omit for this one")
  ("length_seconds" :integer
                    :doc "target length, 10 to 89 seconds; omit for the section's default")
  ("aspect" :string :options '("16:9" "9:16" "1:1")
            :doc "the frame's shape; omit for the section's default")
  ("title" :text
           :doc "a title for the card and the file name; omit for the session's name"))

(defun render-tool (arguments)
  "The showreel_render tool: one cut of one session, its line and paths."
  (render :session-id (showreel-render-session-id arguments)
          :length (showreel-render-length-seconds arguments)
          :aspect (showreel-render-aspect arguments)
          :title (showreel-render-title arguments)))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell showreel
  (:section ("showreel")
    (:guide "set output_dir to the folder renders land in, and make sure ffmpeg is on PATH or ffmpeg_path; font_file only when no DejaVu, Liberation, Noto, Helvetica or Arial is installed; then /showreel demo renders a sample")
    ("output_dir" :path :required t
                  :doc "the folder a cut, its .srt and its poster are written into")
    ("ffmpeg_path" :string :default ""
                   :doc "the ffmpeg to run; empty finds ffmpeg on PATH")
    ("font_file" :string :default ""
                 :doc "a TTF to draw with; empty looks for DejaVu, Liberation, Noto, Helvetica or Arial")
    ("default_length" :integer :default 45 :min 10
                      :doc "the target length of a cut, in seconds; no cut is longer than 89")
    ("default_aspect" :choice :options '("16:9" "9:16" "1:1") :default "16:9"
                       :doc "the frame's shape when a cut names none")
    ("resolution" :choice :options '("1080p" "720p") :default "1080p"
                  :doc "1080p draws 1920x1080 (1080x1920 upright), 720p draws half that"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((dir (getf values :output-dir)))
                 (unless (and (stringp dir) (plusp (length (nlk:trimmed dir))))
                   (nlk:config-error "showreel.output_dir names the folder a cut is written into; set it to one of yours"))
                 values)))
  (:help :showreel
         "showreel:render cuts a session into a captioned demo video with your own ffmpeg - (help :showreel)"
         +primer+)
  (:command "showreel" 'run-command
            :catches 'showreel-error
            :description "Cut this session into a demo video"
            :argument-hint "[cut|plan|list|status|demo] [seconds] [aspect]")
  (:tool "showreel_render" (nlk:contract 'showreel-render) 'render-tool))
