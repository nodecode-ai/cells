;;;; cell.lisp --- one described sound becomes an audio file, and the section,
;;;; the tool and /sfx that make it reachable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; POST {api_base_url}/v1/sound-generation?output_format=... with
;;;; {text, duration_seconds?, prompt_influence?, loop, model_id} and the
;;;; xi-api-key header answers the audio itself (ElevenLabs' documented Sound
;;;; Effects endpoint). The bytes are checked against the container that was
;;;; asked for, saved under the section's directory, and appended to the
;;;; library; the tool answer names the file, and /sfx lists what this profile
;;;; has made.
;;;;
;;;; Everything that could waste a billed call -- a missing key, a prompt over
;;;; the API's limit, a length named in the prompt instead of in
;;;; duration_seconds, a length outside 0.5 to 30 -- is refused before the
;;;; request goes out.

(in-package #:nodecode-sound-effects)

(defparameter +tool-name+ "elevenlabs_sound_effect"
  "The tool's name, the plugin's, so a prompt written for it still reads right.")

(defparameter +primer+
  "ElevenLabs sound effects are available through the nodecode-sound-effects cell: one tool,
elevenlabs_sound_effect, and the library command /sfx.
  elevenlabs_sound_effect(prompt, duration_seconds?, loop?, prompt_influence?, model?)
      ONE sound per call, described concretely: source, action, material, space -- \"glass
      shattering on a concrete floor\", \"heavy wooden door creaking open in a stone hall\".
      A sequence says then / followed by; music names genre, tempo and key (\"90s hip-hop
      drum loop, 90 BPM\"); foley says one-shot or ambience. At most 450 characters.
  duration_seconds: 0.5 to 30. Set it whenever the user names a length or the sound must
      fit a slot: an unset length is billed at a flat, higher rate and ElevenLabs picks
      one. A length written in the prompt is not read -- pass it here.
  loop: true for ambience, music beds and engine hums. A loop can come back longer than
      asked, so quote the length the result reports, never the request.
  prompt_influence: 0 to 1, 0.3 unless sent; higher follows the prompt more literally.
Every call is billed to the operator's ElevenLabs account, so make one call per sound and
say what a regeneration costs before spending it. The file is saved in this profile's
sounds folder and the result names its path: tell the operator that path, since the file
plays in any audio player and can be copied into a project. /sfx lists what this profile
has made, and /sfx <id or words> shows one.
Post-processing is local and free: trim, normalise, convert or fix a loop seam with any
ffmpeg on the machine rather than generating the sound again."
  "What (help :sfx) answers while the cell runs.")

;;; --- the shape of one call -----------------------------------------------------------

(nlk:define-contract sound-effect-request
  (:doc "Generate one sound effect with ElevenLabs, up to 30 seconds: foley, impacts, ambience, UI sounds, creature noises, stingers, drum loops. Describe it concretely (source, material, space, intensity), e.g. 'heavy wooden door creaking open in a stone hall'; a length the user names goes in duration_seconds, never in the prompt. The answer names the saved file: give that path to the operator. Billed to the operator's ElevenLabs account.")
  ("prompt" :text :required t
            :doc "what the sound is: source, action, material, space (at most 450 characters)")
  ("duration_seconds" :number
                      :doc "the clip's length in seconds, 0.5 to 30; omitted, ElevenLabs picks one at a flat, higher price")
  ("loop" :boolean :slot looping
                    :doc "a seamless loop, for ambience, music beds and engine hums")
  ("prompt_influence" :number
                      :doc "0 to 1: higher follows the prompt more literally, lower varies more (0.3 unless sent)")
  ("model" :text :options +models+
           :doc "omit for the profile's sound_model setting"))

;;; --- what a call runs on ---------------------------------------------------------------

(defun api-key ()
  "The key a call is billed to: the section's, else the variable api_key_env
names. A refusal here costs nothing, so it is raised before any request."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    (when (zerop (length key))
      (fail "no ElevenLabs key: paste one into the sound-effects section's api_key, or set ~
             the variable ~a (create a key at https://elevenlabs.io/app/settings/api-keys, ~
             with Sound Effects enabled). Nothing was sent and nothing was billed."
            (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the ElevenLabs key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was billed."))
    key))

(defun checked-base-url (text)
  "TEXT as the endpoint root, without its trailing slash, or a CONFIG-REFUSAL.
Plain http is allowed only for a loopback host: anywhere else it would send
the xi-api-key header in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "sound-effects.api_base_url is empty; it is https://api.elevenlabs.io unless a residency endpoint is wanted"))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest host (plusp (length host)))
        (nlk:config-error "sound-effects.api_base_url is ~s; it must be an https URL, such as https://api.elevenlabs.io" url))
      (when (and plain (not (member (string-downcase host) '("localhost" "127.0.0.1" "::1" "[::1]")
                                     :test #'string=)))
        (nlk:config-error "sound-effects.api_base_url is ~s; plain http would send the key in clear, so it is allowed only for a localhost stand-in" url))
      url)))

(defun length-in-prompt (prompt)
  "The first `N seconds' PROMPT names, or NIL."
  (cl-ppcre:scan-to-strings "(?i)[0-9]+([.][0-9]+)?[ \t]*-?[ \t]*(secs?|seconds?)" (or prompt "")))

(defun check-call (prompt duration influence)
  "Refuse anything the API would refuse, or bill for, before the request goes out."
  (when (> (length prompt) +max-prompt-chars+)
    (fail "the prompt is ~d characters and ElevenLabs accepts at most ~d; describe the sound in fewer words"
          (length prompt) +max-prompt-chars+))
  (when (and (null duration) (length-in-prompt prompt))
    (fail "the prompt names a length (~a) but duration_seconds is not set: ElevenLabs reads no length ~
           from the text and bills an unset length at a flat, higher rate. Pass duration_seconds (0.5 ~
           to 30) with the clip's length, and keep only timing that belongs to the sound itself, ~
           such as \"a beep every 2 seconds\"." (length-in-prompt prompt)))
  (when (and duration (or (< duration +min-seconds+) (> duration +max-seconds+)))
    (fail "duration_seconds is ~a; ElevenLabs generates ~a to ~a seconds. Omit it to let ElevenLabs choose."
          duration +min-seconds+ +max-seconds+))
  (when (and influence (or (< influence 0) (> influence 1)))
    (fail "prompt_influence is ~a; it runs from 0 (free) to 1 (literal)" influence)))

(defun request-sound (key output-format body)
  "One POST to the sound-generation endpoint, under the audio size cap:
(values BODY STATUS HEADERS TRUNCATED-P). A stub in a test stands here."
  (nlk:http :post (format nil "~a/v1/sound-generation?output_format=~a"
                           (setting :api-base-url) output-format)
            :headers (list (cons "xi-api-key" key)
                           (cons "accept" "audio/*")
                           (cons "content-type" "application/json"))
            :content body
            :binary t
            :max-bytes +max-audio-bytes+
            :timeout 120))

;;; --- what came back ------------------------------------------------------------------

(defun api-error (status body)
  "ElevenLabs' own words for a failed STATUS, and the fix its detail names.
=> (values MESSAGE NOTE): a 401 can be a bad key, a missing permission or no
credits, and each needs a different fix."
  (let* ((text (octets-text body))
         (payload (ignore-errors (nlk:decode-json text)))
         (detail (nlk:json-value payload :any "detail"))
         (code (and (hash-table-p detail) (nlk:json-value detail :string "status")))
         (message
           (cond ((hash-table-p detail)
                  (or (nlk:json-value detail :string "message")
                      (nlk:json-value detail :string "status")
                      "no detail"))
                 ((stringp detail) detail)
                 ((vectorp detail)
                  (format nil "~{~a~^; ~}"
                          (loop for item across detail
                                collect (or (nlk:json-value item :string "msg") "?"))))
                 (t (let ((text (nlk:trimmed text)))
                      (if (plusp (length text))
                          (subseq text 0 (min 300 (length text)))
                          "no body"))))))
    (values message
            (or (cdr (assoc code '(("invalid_api_key" . " (that is not a valid ElevenLabs key)")
                                   ("missing_permissions" . " (enable Sound Effects for this key)")
                                   ("quota_exceeded" . " (the ElevenLabs account is out of credits)"))
                             :test #'equal))
                (cdr (assoc status '((401 . " (check the key and the plan's credits)")
                                     (429 . " (rate limited or out of credits; try again later)"))))))))

(defun seconds-text (seconds)
  "SECONDS as a reading, or the words for a length the bytes could not give."
  (if (and (realp seconds) (plusp seconds))
      (format nil "~,2f s" seconds)
      "an unreadable length"))

(defun save-and-report (octets prompt extension output-format model looping duration influence headers)
  "Write the audio under the sounds folder, one line to the library, and answer
the model: where the file is, what it is, and what it cost."
  (let* ((id (sound-id))
         (directory (setting :directory))
         (name (format nil "sfx_~a_~a_~a.~a" (slug prompt) (datestamp) id extension))
         (path (merge-pathnames name directory))
         (length (audio-seconds octets extension))
         (billed (cdr (assoc "character-cost" headers :test #'string-equal))))
    (ensure-directories-exist path)
    ;; IF-EXISTS :ERROR: a stamp and an id make a collision absurd, and a sound
    ;; never overwrites the sound before it.
    (with-open-file (out path :direction :output :if-exists :error
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    (record (nlk:json-object "id" id
                              "created_at" (nlk:iso-time (nlk:unix-now))
                              "prompt" prompt
                              "duration_seconds" duration
                              "length_seconds" length
                              "loop" looping
                              "prompt_influence" influence
                              "model" model
                              "output_format" output-format
                              "file_name" name
                              "bytes" (length octets)
                              "billed_characters" billed
                              "session_id" (getf (nle:turn) :session-id)))
    (with-output-to-string (out)
      (format out "saved ~a~%" (namestring path))
      (format out "prompt: ~a~%" prompt)
      (format out "~a long, ~:[no loop~;loop~], ~a, ~a, ~a~@[, ~a characters billed~]~%"
              (seconds-text length) looping model output-format
              (format nil "~a" (nlk:size-text (length octets))) billed)
      (when (and length duration (>= (abs (- length duration)) 0.05))
        (format out "ElevenLabs returned ~a for the ~a asked for (the bill follows the request): ~
                     quote the length of the file, not the request.~%"
                (seconds-text length) (seconds-text duration)))
      (format out "id ~a -- /sfx lists this profile's sounds~%" id)
      (format out "Give the operator that path: the file plays in any audio player and can be copied ~
                   into a project.~%"))))

(defun generate-sound (request)
  "The tool's one call, from a materialized SOUND-EFFECT-REQUEST."
  (let* ((prompt (sound-effect-request-prompt request))
         (duration (sound-effect-request-duration-seconds request))
         (looping (and (sound-effect-request-looping request) t))
         (influence (sound-effect-request-prompt-influence request))
         (model (or (sound-effect-request-model request) (setting :sound-model)))
         (output-format (setting :output-format))
         (extension (or (extension-for output-format)
                        (fail "the sound-effects section's output_format is ~s; use one of ~{~a~^, ~}"
                              output-format (format-names)))))
    (check-call prompt duration influence)
    (let ((key (api-key)))
      (multiple-value-bind (answer status headers truncated)
          (request-sound key output-format
                         (nlk:encode-json-object
                          (nlk:json-object "text" prompt
                                           "loop" looping
                                           "model_id" model
                                           :opt "duration_seconds" duration
                                           :opt "prompt_influence" influence)))
        (when truncated
          (fail "ElevenLabs sent more than ~a, which is no single sound effect"
                (nlk:size-text +max-audio-bytes+)))
        (let ((octets (octets-of answer)))
          (unless (= status 200)
            (multiple-value-bind (message note) (api-error status octets)
              (fail "ElevenLabs returned HTTP ~d: ~a~a" status message note)))
          (when (zerop (length octets))
            (fail "ElevenLabs returned an empty audio response"))
          (unless (audio-p octets extension)
            (fail "ElevenLabs answered HTTP 200 with ~a, but the bytes are not ~a audio"
                  (or (cdr (assoc "content-type" headers :test #'string-equal)) "no content type")
                  extension))
          (save-and-report octets prompt extension output-format model looping duration influence headers))))))

(defun generate (request)
  "The elevenlabs_sound_effect tool. Every refusal is answered, never signalled:
the model reads why in the result and repairs the call."
  (handler-case (generate-sound request)
    (sound-effects-error (condition)
      (nle:failure (format nil "ERROR: ~a" (sound-effects-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /sfx ---------------------------------------------------------------------------

(defun run-slash (args session-id)
  "The library: every sound this profile made, newest first, or the one ARGS
names by id or by words in its prompt."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (zerop (length argument))
        (library-text)
        (one-sound-text argument))))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell sound-effects
  (:section ("sound-effects")
    (:guide "create a key at https://elevenlabs.io/app/settings/api-keys with Sound Effects enabled; paste it in api_key, or name the variable that holds it in api_key_env (ELEVENLABS_API_KEY by default); sound_model, output_format and api_base_url are what every call uses, and directory is where the files and the library are kept; every generation is billed to that account")
    ("api_key" :secret
               :doc "the ElevenLabs API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "ELEVENLABS_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("sound_model" :choice :options '("eleven_text_to_sound_v2" "eleven_text_to_sound_v3")
                   :default "eleven_text_to_sound_v2"
                   :doc "the model a call uses when it names none")
    ("output_format" :choice :options '("mp3_44100_128" "mp3_44100_192" "opus_48000_128"
                                        "opus_48000_192" "wav_44100" "wav_48000")
                     :default "mp3_44100_128"
                     :doc "mp3 plays inline in most chat apps; Ogg Opus and WAV are for game engines and video editing")
    ("api_base_url" :string :default "https://api.elevenlabs.io"
                    :doc "change only for an ElevenLabs data-residency endpoint, such as https://api.eu.residency.elevenlabs.io")
    ("directory" :directory :default "sounds/"
                 :doc "where the audio files and their library line are kept, under the home"))
  ;; The one thing the declaration cannot say: a base URL is a web address,
  ;; and plain http is a cleartext key.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :api-base-url) (checked-base-url (getf values :api-base-url)))
               values))
  (:tool +tool-name+ (nlk:contract 'sound-effect-request) 'generate)
  (:help :sfx "sfx: elevenlabs_sound_effect turns a described sound into an audio file, kept in this profile's library" +primer+)
  (:command "sfx" 'run-slash
            :description "The sounds this profile made: newest first, or one by id or words"
            :argument-hint "[id or words]")
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first start,
            ;; even before the first sound.
            (ensure-directories-exist (library-path)))))
