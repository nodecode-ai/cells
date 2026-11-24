;;;; cell.lisp --- one text becomes a spoken audio file, and the section, the
;;;; tool and /say that make it reachable.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The speech is made on this machine. The original plugin loads k2-fsa's
;;;; OmniVoice model inside Hermes' Python process; a Lisp image cannot, so this
;;;; cell runs the runtime that same install provides: the `omnivoice-hermes-tts'
;;;; command, which reads a text file and writes an audio file
;;;; (--text-file, --output, --model, --voice, --speed, --format). Its tuning
;;;; knobs are the OMNIVOICE_* environment variables the plugin documents, so
;;;; every control the original read from ~/.hermes/config.yaml is a setting
;;;; here: model, device, dtype, voice, instruct, ref_audio, ref_text,
;;;; language, speed, num_step, guidance_scale.
;;;;
;;;; Nothing leaves the machine but the runtime's own first download of the
;;;; weights (Hugging Face, k2-fsa/OmniVoice).

(in-package #:nodecode-omnivoice)

(defparameter +tool-name+ "omnivoice_say"
  "The tool's name: the cell's, then the verb, as the other rewrites spell
one (mem0_add, ha_call_service).")

(defparameter +primer+
  "OmniVoice speech is available through the nodecode-omnivoice cell: one tool,
omnivoice_say, and the command /say.
  omnivoice_say(text, voice?, speed?, format?, play?)
      Speak TEXT with OmniVoice, locally: the answer names the audio file it wrote and
      whether it was played. Give the operator that path when the text was for a file or
      a voiceover.
  voice: `auto' lets OmniVoice pick. `instruct:<a described voice>' designs one --
      `female, low pitch, british accent', `older male, deep voice', `female, whisper'.
      `ref:<path to a short clip>' clones the voice in that clip (install ffmpeg and give
      the clip's transcript in the section's ref_text for the best result). Any other
      text is a design instruction too. The section's voice is used when a call names
      none.
  speed: 1.0 is normal; 0.8 slower, 1.2 faster.
  format: wav (default), mp3, flac or ogg. MP3 needs ffmpeg on the PATH; without it the
      runtime writes a WAV instead and the answer names that file.
  play: true (default) hands the file to the first player on the PATH -- ffplay, afplay,
      paplay or aplay -- and does not wait for it. Pass false when the file is for later.
The model runs on this machine, so the first call is slow (it loads, and downloads its
weights once). Only clone voices you have the right to use. /say <text> speaks from the
command line, and /say alone shows what the cell runs on."
  "What (help :omnivoice) answers while the cell runs.")

;;; --- the shape of one call -----------------------------------------------------------

(nlk:define-contract speech-request
  (:doc "Speak a text aloud on this machine with OmniVoice, and answer the audio file it wrote. Use it when the operator asks to hear something, wants a voiceover or a spoken file, or asks for a voice they described or a clip they gave. The speech is generated locally: nothing but the text is sent anywhere. Give the operator the path in the answer.")
  ("text"  :text :required t :doc "what to say, in any language OmniVoice knows (up to 5000 characters)")
  ("voice" :text :doc "`auto', `instruct:<a described voice>' (e.g. 'female, low pitch, british accent'), `ref:<path to a short clip>' to clone, or any other text as a design instruction; the section's voice when omitted")
  ("speed" :number :slot pace
           :doc "the pace: 1.0 normal, 0.8 slower, 1.2 faster; omitted, the section's speed")
  ("format" :text :slot container :options +formats+
           :doc "the container: wav by default, or mp3, flac, ogg; MP3 falls back to wav when ffmpeg is missing")
  ("play"  :boolean :slot playing
            :doc "hand the file to a player on this machine as it is written (default true); false when the file is for later"))

;;; --- where a program is, and how one runs --------------------------------------------

(defun executable-path (command)
  "The program COMMAND names: the path itself when it carries a separator,
else the first file of that name on the PATH, or NIL."
  (let ((command (nlk:trimmed (or command ""))))
    (when (plusp (length command))
      (flet ((found (path)
               (let ((probe (ignore-errors (probe-file path))))
                 (and probe (not (uiop:directory-pathname-p probe)) (namestring probe)))))
        (if (or (find #\/ command) (find #\\ command))
            (found command)
            (loop with path = (funcall *getenv* "PATH")
                  for directory in (and path (uiop:split-string path :separator '(#\: #\;)))
                  for candidate = (and (plusp (length directory))
                                       (found (merge-pathnames
                                               command
                                               (uiop:ensure-directory-pathname directory))))
                  when candidate return candidate))))))

(defun expand-path (text)
  "TEXT as it names a file here, with a leading ~ resolved. NIL for blank."
  (let ((text (nlk:trimmed (or text ""))))
    (cond ((zerop (length text)) nil)
          ((uiop:string-prefix-p "~/" text)
           (namestring (merge-pathnames (subseq text 2) (user-homedir-pathname))))
          ((string= text "~") (namestring (user-homedir-pathname)))
          (t text))))

(defun run-program-now (command arguments &key environment)
  "Run COMMAND with ARGUMENTS and wait. => (values OUTPUT STATUS). A test
binds *RUN* instead of executing anything."
  (multiple-value-bind (output status)
      (uiop:run-program (cons command arguments)
                         :environment (environment-of environment)
                         :output '(:string :stripped t)
                         :error-output :output
                         :ignore-error-status t)
    (values (or output "") (or status 0))))

(defun launch-program-now (command arguments)
  "Start COMMAND with ARGUMENTS and do not wait for it: a player reading one
file."
  (uiop:launch-program (cons command arguments) :output :ignore :error-output :ignore))

(defparameter *run* #'run-program-now
  "How the runtime is run, and waited for. A test binds this.")

(defparameter *launch* #'launch-program-now
  "How a player is started, unwaited. A test binds this.")

(defun environment-of (extra)
  "EXTRA as "NAME=VALUE" strings laid over this process's own environment, so
the runtime still reads its own PATH, HOME and caches."
  (if extra
      (append extra #+sbcl (sb-ext:posix-environ) #-sbcl nil)
      #+sbcl (sb-ext:posix-environ) #-sbcl nil))

;;; --- a voice value, read the way the plugin read it ------------------------------------

(defun voice-mode (instruct ref-audio)
  "Which of OmniVoice's three modes a pair of defaults names."
  (cond (ref-audio :ref) (instruct :instruct) (t :auto)))

(defun resolve-voice (voice instruct ref-audio ref-text)
  "The plugin's own mapping of a voice value onto OmniVoice's prompt arguments
(provider.py:90): (values MODE INSTRUCT REF-AUDIO REF-TEXT), MODE one of
:AUTO, :INSTRUCT, :REF.

  auto, default, random  the model picks           (the defaults are cleared)
  instruct:<text>        a described voice         (the defaults are cleared)
  ref:<path>             that clip, cloned
  <a file that is there> that clip, cloned
  any other text         a described voice"
  (let ((raw (nlk:trimmed (or voice ""))))
    (cond
      ((zerop (length raw))
       (values (voice-mode instruct ref-audio) instruct ref-audio ref-text))
      ((member (string-downcase raw) '("auto" "default" "random") :test #'string=)
       (values :auto nil nil nil))
      ((uiop:string-prefix-p "instruct:" (string-downcase raw))
       (let ((text (nlk:trimmed (subseq raw (length "instruct:")))))
         (if (zerop (length text))
             (values :auto nil nil nil)
             (values :instruct text nil nil))))
      ((uiop:string-prefix-p "ref:" (string-downcase raw))
       (let ((path (nlk:trimmed (subseq raw (length "ref:")))))
         (if (zerop (length path))
             (values (voice-mode instruct ref-audio) instruct ref-audio ref-text)
             (values :ref instruct (expand-path path) ref-text))))
      ((probe-file (expand-path raw))
       (values :ref instruct (expand-path raw) ref-text))
      (t (values :instruct raw ref-audio ref-text)))))

(defun voice-value (mode instruct ref-audio)
  "The runtime voice value that reproduces MODE, as the plugin spells one."
  (ecase mode
    (:auto "auto")
    (:instruct (if instruct (format nil "instruct:~a" instruct) "auto"))
    (:ref (format nil "ref:~a" ref-audio))))

(defun voice-text (mode instruct ref-audio)
  "MODE for a person, naming the voice it resolved to."
  (ecase mode
    (:auto "auto (the model picks)")
    (:instruct (format nil "designed: ~a" instruct))
    (:ref (format nil "cloned from ~a" ref-audio))))

;;; --- the files one call makes --------------------------------------------------------

(defun datestamp ()
  "The moment a file is made, to the second, in UTC."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
            year month day hour minute second)))

(defun file-id ()
  "Twelve hex digits: the name two files made in the same second differ by."
  (format nil "~12,'0x" (random (expt 16 12))))

(defun slug (text &optional (cap 40))
  "TEXT as a file name reads: lowercase words, dashes between them, at most
CAP characters."
  (let ((out (make-string-output-stream))
        (space nil))
    (loop for character across (string-downcase (subseq text 0 (min (length text) 200)))
          do (cond ((alphanumericp character) (write-char character out) (setf space nil))
                   ((not space) (write-char #\- out) (setf space t))))
    (let ((written (string-trim "-" (get-output-stream-string out))))
      (subseq written 0 (min cap (length written))))))

(defun write-text-file (path text)
  "TEXT into PATH as UTF-8: the runtime reads it back as its --text-file."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string text out)))

(defun written-file (output-path)
  "The file the runtime wrote: OUTPUT-PATH, or the same stem in another
container -- the plugin asks soundfile for mp3 and falls back to WAV when
ffmpeg is missing (provider.py:341)."
  (or (probe-file output-path)
      (loop for format in +formats+
            for candidate = (probe-file (make-pathname :type format :defaults output-path))
            when candidate return candidate)))

(defun written-seconds (path)
  "The playing time a canonical RIFF/WAVE file states, or NIL for any other
container -- the answer quotes a length only when it read one."
  (ignore-errors
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((head (make-array 44 :element-type '(unsigned-byte 8))))
        (when (and (= 44 (read-sequence head in))
                   (equalp (subseq head 0 4) (octets-of "RIFF"))
                   (equalp (subseq head 8 12) (octets-of "WAVE")))
          (let ((byte-rate (little-u32 head 28))
                (data (little-u32 head 40)))
            (when (and (plusp byte-rate) (plusp data))
              (/ data (float byte-rate)))))))))

(defun octets-of (text)
  "TEXT's bytes, for a header's four letters."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun little-u32 (bytes at)
  "The four bytes at AT of BYTES, little-endian."
  (loop for index below 4
        sum (ash (aref bytes (+ at index)) (* 8 index))))

;;; --- running the runtime --------------------------------------------------------------

(defun checked-real (values key name default)
  "VALUES' KEY as a float, or a CONFIG-REFUSAL naming NAME: a section written
by hand can hold text where a number belongs."
  (let ((text (nlk:trimmed (let ((value (getf values key)))
                              (if value (princ-to-string value) "")))))
    (if (zerop (length text))
        default
        (multiple-value-bind (number end)
            (let ((*read-eval* nil)) (ignore-errors (read-from-string text)))
          (if (and (realp number) (= end (length text)))
              (coerce number 'double-float)
              (nlk:config-error "~a is ~s; it is a number, such as ~a" name text default))))))

(defun checked-settings (values table)
  "The section as the cell runs on: the command named, the numbers parsed."
  (declare (ignore table))
  (let ((command (nlk:trimmed (or (getf values :command) ""))))
    (when (zerop (length command))
      (nlk:config-error "omnivoice.command is empty: it is the command the OmniVoice runtime installs, ~
 omnivoice-hermes-tts, or the path to it"))
    (setf (getf values :command) command))
  (setf (getf values :speed) (checked-real values :speed "omnivoice.speed" 1.0d0)
        (getf values :guidance-scale)
        (checked-real values :guidance-scale "omnivoice.guidance_scale" 2.0d0))
  values)

(defun number-text (number)
  "NUMBER as the runtime's own flags carry one."
  (format nil "~,4f" (coerce number 'double-float)))

(defun child-environment (instruct ref-audio ref-text)
  "The OMNIVOICE_* variables the plugin documents, for the runtime it runs:
its own configuration knobs, handed to the child rather than read by us."
  (flet ((pair (name value)
           (let ((text (and value (nlk:trimmed (princ-to-string value)))))
             (when (and text (plusp (length text)))
               (format nil "~a=~a" name text)))))
    (remove nil
            (list (pair "OMNIVOICE_MODEL" (setting :model))
                  (pair "OMNIVOICE_DEVICE" (setting :device))
                  (pair "OMNIVOICE_DTYPE" (setting :dtype))
                  (pair "OMNIVOICE_LANGUAGE" (setting :language))
                  (pair "OMNIVOICE_NUM_STEP" (setting :num-step))
                  (pair "OMNIVOICE_GUIDANCE_SCALE" (setting :guidance-scale))
                  (pair "OMNIVOICE_SPEED" (setting :speed))
                  (pair "OMNIVOICE_INSTRUCT" instruct)
                  (pair "OMNIVOICE_REF_AUDIO" ref-audio)
                  (pair "OMNIVOICE_REF_TEXT" ref-text)))))

(defun speak-arguments (text-path output-path format-name voice speed)
  "The runtime's flags, as its own command line reads them (cli.py:15)."
  (append (list "--text-file" (namestring text-path)
                "--output" (namestring output-path)
                "--format" format-name
                "--model" (setting :model)
                "--voice" voice)
          (when speed (list "--speed" (number-text speed)))))

(defun tail-text (text)
  "TEXT's last +OUTPUT-TAIL+ characters: a failed run's own last words."
  (let ((text (nlk:trimmed (or text ""))))
    (if (> (length text) +output-tail+)
        (format nil "...~a" (subseq text (- (length text) +output-tail+)))
        text)))

;;; --- the player ---------------------------------------------------------------------

(defun player-command ()
  "The player the section names, or the first one on the PATH. => a list of
(program argument...), or NIL when nothing here can play audio."
  (let ((named (nlk:trimmed (or (setting :player) ""))))
    (if (and (plusp (length named)) (not (string-equal named "auto")))
        (let ((parts (uiop:split-string named :separator '(#\Space #\Tab))))
          (cons (or (executable-path (first parts)) (first parts)) (rest parts)))
        (loop for candidate in '(("ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet")
                                 ("afplay") ("paplay") ("aplay" "-q"))
              for path = (executable-path (first candidate))
              when path return (cons path (rest candidate))))))

(defun play-file (file)
  "Hand FILE to a player without waiting. => the player's name, or NIL."
  (let ((player (player-command)))
    (when player
      (ignore-errors
        (funcall *launch* (first player) (append (rest player) (list (namestring file))))
        (file-namestring (first player))))))

;;; --- the one call -------------------------------------------------------------------

(defun report (file text format-name mode instruct ref-audio played)
  "What the model reads: the file, what it is, and whether it is being heard."
  (let ((size (with-open-file (in file :element-type '(unsigned-byte 8)) (file-length in)))
        (seconds (written-seconds file)))
    (with-output-to-string (out)
      (format out "wrote ~a~%" (namestring file))
      (format out "said: ~s~%" (subseq text 0 (min 120 (length text))))
      (format out "~@[~,2f s, ~]~a, ~a, voice ~a, model ~a~%"
              seconds (nlk:size-text size) format-name (voice-text mode instruct ref-audio)
              (setting :model))
      (if played
          (format out "playing with ~a (it does not wait for the end)~%" played)
          (format out "not played: ~a~%"
                  (if (player-command)
                      "the player did not start"
                      "no ffplay, afplay, paplay or aplay on the PATH")))
      (format out "Give the operator that path: the file plays in any audio player and can be ~
 copied into a project. /say lists this profile's speech."))))

(defun speak (text &key voice speed format play)
  "TEXT into an audio file under the section's directory, spoken by OmniVoice
on this machine. Answers what REPORT says, or signals OMNIVOICE-ERROR."
  (let ((text (nlk:trimmed (or text ""))))
    (when (zerop (length text))
      (fail "there is no text to speak: pass the sentence to say"))
    (when (> (length text) +max-text-chars+)
      (fail "the text is ~d characters and one OmniVoice call takes at most ~d: ~
 split it into a few calls" (length text) +max-text-chars+))
    (let ((format-name (string-downcase (nlk:trimmed (or format (setting :format) "wav")))))
      (unless (find format-name +formats+ :test #'string=)
        (fail "the format is ~s; this cell writes ~{~a~^, ~}" format-name +formats+))
      (multiple-value-bind (mode instruct ref-audio ref-text)
          (resolve-voice (or voice (setting :voice)) (setting :instruct)
                         (setting :ref-audio) (setting :ref-text))
        (when (and ref-audio (not (probe-file ref-audio)))
          (fail "the reference audio ~a is not there: voice cloning reads a short clip of ~
 one speaker on this machine, and its path is the section's ref_audio or ref:<path>"
                ref-audio))
        (speak-now text format-name mode instruct ref-audio ref-text
                   (or speed (setting :speed)) play)))))

(defun speak-now (text format-name mode instruct ref-audio ref-text speed play)
  "SPEAK past the checks: write the text, run the runtime, find what it wrote."
  (let* ((command (or (executable-path (setting :command))
                      (fail "the command ~s is not on the PATH: install the OmniVoice runtime ~
 (pip install \"hermes-omnivoice[omnivoice] @ ~
 git+https://github.com/ThaungThanHan/hermes-omnivoice.git\"), or set command to its ~
 path. Nothing was run." (setting :command))))
         (directory (setting :directory))
         (id (file-id))
         (text-path (merge-pathnames (format nil "omnivoice-~a.txt" id) directory))
         (output-path (merge-pathnames
                       (format nil "speech_~a_~a_~a.~a" (slug text) (datestamp) id format-name)
                       directory))
         (voice (voice-value mode instruct ref-audio)))
    (ensure-directories-exist text-path)
    (unwind-protect
         (progn
           (write-text-file text-path text)
           (multiple-value-bind (output status)
               (funcall *run* command
                        (speak-arguments text-path output-path format-name voice speed)
                        :environment (child-environment instruct ref-audio ref-text))
             (unless (zerop status)
               (fail "OmniVoice exited ~d: ~a" status (tail-text output)))
             (let ((file (written-file output-path)))
               (unless file
                 (fail "OmniVoice exited 0 but wrote no audio at ~a~@[ -- it said: ~a~]"
                       output-path (let ((said (nlk:trimmed output)))
                                     (and (plusp (length said)) (tail-text said)))))
               (report file text format-name mode instruct ref-audio
                       (and play (play-file file))))))
      (ignore-errors (delete-file text-path)))))

(defun say (request)
  "The omnivoice_say tool. Every refusal is answered, never signalled: the
model reads why in the result and repairs the call."
  (handler-case
      (speak (speech-request-text request)
             :voice (speech-request-voice request)
             :speed (speech-request-pace request)
             :format (speech-request-container request)
             :play (speech-request-playing request))
    (omnivoice-error (condition)
      (nle:failure (format nil "ERROR: ~a" (omnivoice-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /say ---------------------------------------------------------------------------

(defun recent-files (&optional (count 5))
  "The newest audio files in the speech folder, newest first."
  (let ((files (loop for type in +formats+
                     append (directory (merge-pathnames
                                        (make-pathname :name :wild :type type)
                                        (setting :directory))))))
    (subseq (sort files #'> :key (lambda (file) (or (ignore-errors (file-write-date file)) 0)))
            0 (min count (length files)))))

(defun settings-text ()
  "What the cell runs on, and the last few files it made."
  (with-output-to-string (out)
    (format out "OmniVoice speech on this machine: ~a~%" (setting :command))
    (format out "model ~a on ~a (~a), voice ~a, ~a, speed ~a~%"
            (setting :model) (setting :device) (setting :dtype)
            (setting :voice) (setting :format) (setting :speed))
    (format out "folder ~a~%" (namestring (setting :directory)))
    (format out "voice values: auto | instruct:<a described voice> | ref:<a clip> | <a described voice>~%")
    (let ((files (recent-files)))
      (if files
          (progn (format out "newest:~%")
                 (dolist (file files)
                   (format out "  ~a, ~a~%" (file-namestring file)
                           (file-write-date-text file))))
          (format out "no speech made here yet~%")))))

(defun file-write-date-text (file)
  "FILE's write time, as a sentence reads it."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (or (ignore-errors (file-write-date file)) (get-universal-time)))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun run-slash (arguments session-id)
  "Speak ARGUMENTS, or show what the cell runs on when there are none."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or arguments ""))))
    (handler-case
        (if (zerop (length text)) (settings-text) (speak text))
      (omnivoice-error (condition)
        (format nil "ERROR: ~a" (omnivoice-error-detail condition)))
      (error (condition)
        (format nil "ERROR: ~a" condition)))))

;;; --- the cell ------------------------------------------------------------------------

(nle:define-cell omnivoice
  (:section ("omnivoice")
    (:guide "install the OmniVoice runtime once: pip install \"hermes-omnivoice[omnivoice] @ git+https://github.com/ThaungThanHan/hermes-omnivoice.git\" (torch and the model weights come with it; the first call downloads them); command names that install's omnivoice-hermes-tts, or the path to it; voice is auto, a described voice such as female, low pitch, british accent, or ref:<a short clip> to clone; language, speed, num_step and guidance_scale are the model's own knobs; format and directory say what is written and where; player is auto (ffplay, afplay, paplay or aplay) or a player's command")
    ("command" :string :default "omnivoice-hermes-tts"
               :doc "the OmniVoice runtime's command, or the path to it")
    ("model" :string :default "k2-fsa/OmniVoice"
             :doc "the model the runtime loads; the weights come from Hugging Face on the first call")
    ("device" :choice :options '("auto" "cpu" "cuda" "mps") :default "auto"
              :doc "auto picks the best device here; cpu works and is slow, a GPU is much faster")
    ("dtype" :choice :options '("float16" "float32" "bfloat16") :default "float16"
             :doc "float16 on a GPU; float32 on a CPU that cannot do half precision")
    ("voice" :string :default "auto"
             :doc "the voice a call names none: auto, a described voice, or ref:<a short clip> to clone")
    ("instruct" :string
                :doc "a voice description used when voice is empty, such as older male, deep voice")
    ("ref_audio" :string
                 :doc "a short clip of one speaker to clone, when voice is empty")
    ("ref_text" :string
                :doc "what that clip says: with it the runtime does not transcribe the clip itself")
    ("language" :string
                :doc "the language code to speak in, such as th or my; unset, OmniVoice reads the text")
    ("speed" :string :default "1.0"
             :doc "the pace: 1.0 normal, 0.8 slower, 1.2 faster")
    ("num_step" :integer :default 32 :min 1
                :doc "diffusion steps: more is slower and finer, 32 is the model's own default")
    ("guidance_scale" :string :default "2.0"
                      :doc "how closely the speech follows the text and the voice description")
    ("format" :choice :options '("wav" "mp3" "flac" "ogg") :default "wav"
              :doc "the container written; MP3 needs ffmpeg and falls back to WAV without it")
    ("directory" :directory :default "speech/"
                 :doc "where the spoken files are kept, under the home")
    ("player" :string :default "auto"
              :doc "auto picks ffplay, afplay, paplay or aplay; or name a player, with its flags"))
  ;; The declaration cannot say it: a section written by hand may hold text where
  ;; a number belongs, and an empty command is no command at all.
  (:settings (lambda (values table) (checked-settings values table)))
  (:tool +tool-name+ (nlk:contract 'speech-request) 'say)
  (:help :omnivoice "omnivoice: OmniVoice speech on this machine - omnivoice_say turns a text into an audio file, and /say speaks one" +primer+)
  (:command "say" 'run-slash
            :description "Speak a sentence with OmniVoice, or show what it runs on"
            :argument-hint "[text]")
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first start,
            ;; even before the first sentence.
            (ensure-directories-exist (setting :directory)))))
