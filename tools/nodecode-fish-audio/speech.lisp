;;;; speech.lisp --- text becomes audio, and local audio becomes text.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; POST {api_base_url}/v1/tts with {text, reference_id?, format, prosody}
;;;; and the model in Fish Audio's `model' header answers the audio itself.
;;;; POST {api_base_url}/v1/asr as a multipart upload answers JSON: text,
;;;; segments, speaker turns. Both are billed, so everything the API would
;;;; refuse -- an empty text, a speed out of range, an unreadable file, an
;;;; oversize upload -- is refused before the request goes out.

(in-package #:nodecode-fish-audio)

;;; --- text to speech ---------------------------------------------------------------

(defun speak-now (request)
  "The fish_speak call: one text, one voice (or two to four speakers), one
audio file in the section's directory."
  (let* ((text (fish-speech-text request))
         (voice (trimmed-text (fish-speech-voice request)))
         (speakers (fish-speech-speakers request))
         (format (or (fish-speech-format-name request) (setting :output-format)))
         (model (or (fish-speech-model request) (setting :tts-model)))
         (speed (fish-speech-pace request))
         (language (trimmed-text (fish-speech-language request))))
    (unless (and (stringp text) (plusp (length (nlk:trimmed text))))
      (fail "Nothing to say: text is empty."))
    (when (> (length text) +max-text-chars+)
      (fail "text is ~d characters and one request says at most ~d; speak it in paragraphs"
            (length text) +max-text-chars+))
    (unless (extension-for format)
      (fail "format is ~s; use ~{~a~^, ~}" format +formats+))
    (unless (model-name model)
      (fail "model is ~s; use ~{~a~^, ~}" model +tts-models+))
    (when (and speed (not (and (realp speed) (<= 0.5 speed 2.0))))
      (fail "speed is ~a; Fish Audio takes 0.5 to 2.0" speed))
    (when (and language (not (every (lambda (char)
                                      (or (alphanumericp char) (find char "-_" :test #'char=)))
                                    language)))
      (fail "language is ~s; pass a BCP-47 tag such as en, es-MX or zh-CN" language))
    (when voice
      (unless (every (lambda (char) (or (alphanumericp char) (find char "-_" :test #'char=))) voice)
        (fail "voice is ~s; pass a Fish Audio voice id (letters, digits, - and _)" voice)))
    (when speakers
      (unless (<= 2 (length speakers) 4)
        (fail "speakers takes 2 to 4 voice ids; ~d were given" (length speakers)))
      (when voice
        (fail "name either one voice or a speakers list, not both")))
    (let* ((key (api-key))
           (body (nlk:encode-json-object
                  (nlk:json-object
                   "text" text
                   "format" (api-format format)
                   "chunk_length" 200
                   "normalize" t
                   "prosody" (nlk:json-object "speed" (coerce (or speed 1.0) 'double-float))
                   :opt "reference_id" (cond (speakers (coerce speakers 'vector))
                                             (voice voice)))))
           (label (if speakers "multi-speaker" text)))
      (multiple-value-bind (body status response-headers truncated)
          (request :post "/v1/tts" :key key :model model :content body
                                  :content-type "application/json" :binary t
                                  :timeout 180 :max-bytes +max-audio-bytes+)
        (declare (ignore response-headers))
        (when truncated
          (fail "Fish Audio sent more than ~a of audio for one call" (nlk:size-text +max-audio-bytes+)))
        (let ((octets (octets-of body)))
          (ensure-ok status octets)
          (when (zerop (length octets))
            (fail "Fish Audio answered with no audio at all"))
          (unless (audio-p octets format)
            (fail "Fish Audio answered HTTP 200 with bytes that are not ~a audio" format))
          (let ((path (save-bytes octets format label)))
            (with-output-to-string (out)
              (format out "saved ~a~%" (namestring path))
              (format out "~a, ~a, ~a, ~a~%"
                      (if speakers (format nil "~d speakers" (length speakers))
                          (if voice (format nil "voice ~a" voice) "Fish Audio's default voice"))
                      model format (nlk:size-text (length octets)))
              (format out "Billed to the Fish Audio account. Give the operator that path: the file plays in ~
                           any audio player and can be attached to a message.~@[ Language: ~a.~]~%"
                      language))))))))

(defun speak (request)
  "The fish_speak tool. Every refusal is answered, never signalled: the model
reads why in the result and repairs the call."
  (handler-case (speak-now request)
    (fish-audio-error (condition)
      (nle:failure (format nil "ERROR: ~a" (fish-audio-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- speech to text ----------------------------------------------------------------

(defun bool-text (value)
  (if value "true" "false"))

(defun srt-stamp (seconds)
  "SECONDS as 00:00:00,000."
  (let ((ms (max 0 (round (* 1000 (or seconds 0))))))
    (multiple-value-bind (rest frac) (floor ms 1000)
      (multiple-value-bind (rest secs) (floor rest 60)
        (multiple-value-bind (hours mins) (floor rest 60)
          (format nil "~2,'0d:~2,'0d:~2,'0d,~3,'0d" hours mins secs frac))))))

(defun srt-text (segments &key (turns #()))
  "SubRip cues from Fish Audio's segments, or from its speaker turns when it
gave them: one cue per segment, capped at 500."
  (flet ((cues (rows label-start)
           (loop for row across rows
                 for i from 1 to 500
                 collect (format nil "~d~%~a --> ~a~%~a~%"
                                 i
                                 (srt-stamp (nlk:json-value row :number "start"))
                                 (srt-stamp (nlk:json-value row :number "end"))
                                 (format nil "~@[~a: ~]~a"
                                         (nlk:json-value row :string "speaker")
                                         (or (nlk:json-value row :string "text") ""))))))
    (declare (ignore label-start))
    (format nil "~{~a~^~%~}" (cues (if (plusp (length turns)) turns segments) nil))))

(defun transcribe-now (request)
  "The fish_transcribe call: one local audio file, Fish Audio's text back,
and an SRT beside it when the call asks for one."
  (multiple-value-bind (path kind mime bytes) (checked-input (fish-transcript-file-path request))
    (declare (ignore bytes))
    (let* ((model (or (fish-transcript-model request) (setting :stt-model)))
           (diarize (or (fish-transcript-diarize request) "auto"))
           (timestamps (fish-transcript-timestamps request))
           (events (fish-transcript-tag-audio-events request))
           (srt (fish-transcript-srt request))
           (language (trimmed-text (fish-transcript-language request)))
           (hints (remove nil (list (fish-transcript-num-speakers request)
                                    (fish-transcript-min-speakers request)
                                    (fish-transcript-max-speakers request)))))
      (unless (stt-model-name model)
        (fail "model is ~s; use ~{~a~^, ~}" model +stt-models+))
      (when (and hints (or (string= model "transcribe-1") (string= diarize "false")))
        (fail "speaker counts need transcribe-1-pro and diarize other than false"))
      (let* ((key (api-key))
             (fields (append (list (cons "audio" path)
                                   (cons "ignore_timestamps" (bool-text (not (or timestamps srt))))
                                   (cons "diarize" diarize)
                                   (cons "tag_audio_events" (bool-text events)))
                             (when language (list (cons "language" language)))
                             (when (fish-transcript-num-speakers request)
                               (list (cons "num_speakers" (princ-to-string (fish-transcript-num-speakers request)))))
                             (when (fish-transcript-min-speakers request)
                               (list (cons "min_speakers" (princ-to-string (fish-transcript-min-speakers request)))))
                             (when (fish-transcript-max-speakers request)
                               (list (cons "max_speakers" (princ-to-string (fish-transcript-max-speakers request))))))))
        (multiple-value-bind (payload status body)
            (json-answer :post "/v1/asr" :key key :model model :content fields
                                        :timeout 900)
          (ensure-ok status body)
          (let ((text (nlk:json-value payload :string "text"))
                (segments (nlk:json-array payload "segments"))
                (turns (nlk:json-array payload "speaker_turns"))
                (duration (nlk:json-value payload :number "duration")))
            (unless text
              (fail "Fish Audio answered with no text for ~a" (file-namestring path)))
            (with-output-to-string (out)
              (format out "~a~%" text)
              (format out "---~%~a~@[, ~a long~]~@[, language ~a~]~@[, ~d speaker turns~]~%"
                      (file-namestring path) duration
                      (or (nlk:json-value payload :string "language_code") language)
                      (length turns))
              (when srt
                (let ((path (save-bytes (map '(vector (unsigned-byte 8)) #'char-code
                                              (srt-text segments :turns turns))
                                         "srt" (file-namestring path))))
                  (format out "subtitles: ~a~%" (namestring path))))
              (format out "Billed to the Fish Audio account (~a)." model))))))))

(defun transcribe (request)
  "The fish_transcribe tool, answering its refusals as text."
  (handler-case (transcribe-now request)
    (fish-audio-error (condition)
      (nle:failure (format nil "ERROR: ~a" (fish-audio-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))
