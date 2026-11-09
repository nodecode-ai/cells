;;;; speech.lisp --- one spoken answer saved as a file, one recording read
;;;; back as text, and every refusal that costs nothing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; POST {tts_url} with {"model", "language", "voice", "audio_format",
;;;; "text"} and an Authorization: Bearer header answers the audio itself
;;;; (Soniox TTS realtime). Speech-to-text is asynchronous, as Soniox serves
;;;; it: POST /files uploads the recording, POST /transcriptions asks for the
;;;; work, GET /transcriptions/{id} is polled until it completes,
;;;; GET /transcriptions/{id}/transcript is the words, and the transcription
;;;; and the file are deleted afterwards.

(in-package #:nodecode-soniox)

;;; --- speak -------------------------------------------------------------------------

(defun clamped-speed (speed)
  "SPEED held inside what Soniox speaks at, to three places; NIL when the call
named none. The plugin clamped rather than refusing, and so does this: a pace is
a preference, not a mistake."
  (when (realp speed)
    (let ((bounded (max +min-speed+ (min +max-speed+ speed))))
      (coerce (/ (round (* bounded 1000)) 1000) 'single-float))))

(defun speak-payload (text &key voice (language +tts-language+) (format-name "mp3") speed)
  "The JSON body one text-to-speech call sends."
  (let ((payload (nlk:json-object "model" (setting :model)
                                  "language" language
                                  "voice" voice
                                  "audio_format" (second (format-entry format-name))
                                  "text" text))
        (speed (clamped-speed speed)))
    (when speed
      (setf (gethash "speed" payload) speed))
    payload))

(defun speak-now (text &key voice language format speed)
  "TEXT spoken by Soniox and saved as a file; => the answer the model reads,
whose first line names the path."
  (let* ((text (nlk:trimmed (or text "")))
         (voice (or (filled voice) (setting :voice)))
         (language (or (normalize-language language) (setting :language) +tts-language+))
         (format-name (string-downcase (or (and format (plusp (length format)) format)
                                           (setting :audio-format))))
         (entry (or (format-entry format-name)
                    (fail "the format is ~s; Soniox speaks ~{~a~^, ~}"
                          format-name +format-names+))))
    (declare (ignore entry))
    (when (zerop (length text))
      (fail "there is nothing to say: give the tool the text to speak"))
    (when (> (length text) +max-speak-chars+)
      (fail "the text is ~d characters and one call speaks at most ~d: split it into two calls and stop at a sentence"
            (length text) +max-speak-chars+))
    (let ((key (api-key)))
      (multiple-value-bind (body status headers truncated)
          (request "TTS" :post (tts-url)
                   :headers (bearer-headers key)
                   :content (nlk:encode-json-object
                             (speak-payload text :voice voice :language language
                                                 :format-name format-name :speed speed))
                   :binary t :max-bytes +max-audio-bytes+ :timeout 90)
        (declare (ignore headers))
        (when truncated
          (fail "Soniox sent more than ~a of audio for one call: speak it in shorter pieces"
                (nlk:size-text +max-audio-bytes+)))
        (let ((octets (octets-of body)))
          (unless (= status 200)
            (let* ((text (octets-text body))
                   (payload (ignore-errors (nlk:decode-json text)))
                   (error-type (and (hash-table-p payload)
                                    (nlk:json-value payload :string "error_type")))
                   (message (or (and (hash-table-p payload)
                                     (or (nlk:json-value payload :string "error_message")
                                         (nlk:json-value payload :string "message")))
                                (compact-text body))))
              (fail "Soniox returned HTTP ~d: ~a~a"
                    status (redact message key) (or (error-note status error-type) ""))))
          (when (zerop (length octets))
            (fail "Soniox returned an empty audio response"))
          (let ((extension (audio-extension octets format-name)))
            (unless extension
              (fail "Soniox answered HTTP 200 with ~a, but the bytes are not ~a audio"
                    (compact-text octets 40) format-name))
            (let* ((directory (setting :directory))
                   (name (format nil "soniox_~a_~a_~a.~a"
                                  (slug text) (datestamp) (call-id) extension))
                   (path (merge-pathnames name directory)))
              (ensure-directories-exist path)
              (with-open-file (out path :direction :output :if-exists :error
                                         :if-does-not-exist :create
                                         :element-type '(unsigned-byte 8))
                (write-sequence octets out))
              (with-output-to-string (out)
                (format out "saved ~a~%" (namestring path))
                (format out "said: ~a~%" text)
                (format out "~a, ~a, voice ~a, ~a, ~a~%"
                        (setting :model) language voice format-name
                        (nlk:size-text (length octets)))
                (format out "Give the operator that path: the file plays in any audio player and ~
                             can be moved into a project, a video or a game.~%")))))))))

;;; --- transcribe ----------------------------------------------------------------------

(defun upload-file (path key)
  "PATH uploaded to Soniox as one file; => its id. The recording rides as
the one part of a multipart body, so the bytes are the file's own."
  (multiple-value-bind (body status headers truncated)
      (request "file upload" :post (files-url)
               :headers (bearer-headers key nil)
               :content (list (cons "file" path))
               :timeout 300)
    (declare (ignore headers))
    (when truncated (fail "Soniox's answer to the upload was cut short; try a smaller recording"))
    (unless (= status 200)
      (let* ((text (octets-text body))
             (payload (ignore-errors (nlk:decode-json text)))
             (error-type (and (hash-table-p payload) (nlk:json-value payload :string "error_type")))
             (message (or (and (hash-table-p payload)
                               (or (nlk:json-value payload :string "error_message")
                                   (nlk:json-value payload :string "message")))
                          (compact-text body))))
        (fail "Soniox returned HTTP ~d for the upload: ~a~a"
              status (redact message key) (or (error-note status error-type) ""))))
    (answer-id "file upload" (json-body "file upload" body))))

(defun create-transcription (file-id key &key model language prompt)
  "A transcription of FILE-ID asked for; => its id."
  (let ((payload (nlk:json-object "model" model "file_id" file-id))
        (hints (language-hints language)))
    (when hints
      (setf (gethash "language_hints" payload) (coerce hints 'vector)))
    (when (filled prompt)
      (let ((text (subseq prompt 0 (min +max-context-chars+ (length prompt)))))
        (setf (gethash "context" payload) (nlk:json-object "text" text))))
    (multiple-value-bind (answer status)
        (call-json "transcription create" :post (transcriptions-url) key
                   :content (nlk:encode-json-object payload)
                   :timeout 30)
      (declare (ignore status))
      (answer-id "transcription create" answer))))

(defun fetch-transcript (id key)
  "The words of the finished transcription ID: its text field, or its tokens
joined when Soniox answers those instead."
  (multiple-value-bind (payload status)
      (call-json "transcript fetch" :get (transcriptions-url id "transcript") key :timeout 30)
    (declare (ignore status))
    (let ((text (nlk:json-value payload :string "text"))
          (tokens (nlk:json-value payload :any "tokens")))
      (nlk:trimmed
       (cond
         (text text)
         ((and (vectorp tokens) (plusp (length tokens)))
          (with-output-to-string (out)
            (loop for token across tokens
                  when (hash-table-p token)
                    do (write-string (or (nlk:json-value token :string "text") "") out))))
         (t (fail "Soniox's transcript response was malformed: neither text nor tokens")))))))

(defun delete-quietly (op url key)
  "Ask Soniox to drop one resource, and let a failure go: a leftover is a
bill, not an answer the model has to read."
  (ignore-errors
    (request op :delete url :headers (bearer-headers key nil) :timeout 30)))

(defun transcribe-now (path &key model language prompt)
  "The recording at PATH read by Soniox; => the transcript and what was read,
as the answer the model passes on."
  (let* ((path (resolve-recording path))
         (octets (read-recording path))
         (model-name (or (filled model) (setting :stt-model) +stt-model+))
         (hints (language-hints (or (filled language) (setting :stt-language))))
         (key (api-key))
         (file-id nil)
         (transcription-id nil))
    (unwind-protect
         (progn
           (setf file-id (upload-file path key))
           (setf transcription-id
                 (create-transcription file-id key
                                       :model model-name
                                       :language (format nil "~{~a~^,~}" hints)
                                       :prompt prompt))
           (wait-for-transcription transcription-id key)
           (let ((transcript (fetch-transcript transcription-id key)))
             (when (zerop (length (nlk:trimmed transcript)))
               (fail "Soniox heard no words in ~a: the recording may be silent, or not speech"
                     (file-name path)))
             (with-output-to-string (out)
               (format out "transcript: ~a~%" transcript)
               (format out "~a~@[, language ~{~a~^, ~}~]~%" model-name hints)
               (format out "from ~a, ~a uploaded~%" (file-name path) (nlk:size-text (length octets)))
               (format out "Pass the transcript on as the words of the recording: what left the ~
                           machine was that file's audio, and Soniox drops it again when the ~
                           transcription is read.~%"))))
      (when transcription-id
        (delete-quietly "transcription delete" (transcriptions-url transcription-id) key))
      (when file-id
        (delete-quietly "file delete" (files-url file-id) key)))))
