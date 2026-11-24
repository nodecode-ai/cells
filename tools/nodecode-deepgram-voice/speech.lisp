;;;; speech.lisp --- one spoken answer saved as a file, one recording read
;;;; back as text, and every refusal that costs nothing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; POST {api_base_url}/speak?model=...&encoding=... with {"text": ...} and
;;;; an Authorization: Token header answers the audio itself (Deepgram Aura 2);
;;;; POST {api_base_url}/listen?model=... with the recording as the body
;;;; answers JSON whose results hold the transcript (Deepgram Nova). Both are
;;;; the documented REST endpoints, so no Deepgram SDK is needed.

(in-package #:nodecode-deepgram-voice)

;;; --- what a call runs on ----------------------------------------------------------

(defun api-key ()
  "The key a call is billed to: the section's, else the variable api_key_env
names. A refusal here costs nothing, so it is raised before any request."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    (when (zerop (length key))
      (fail "no Deepgram key: paste one into the deepgram-voice section's api_key, or set the ~
             variable ~a (create a key at https://console.deepgram.com/). Nothing was sent ~
             and nothing was billed."
            (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the Deepgram key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was billed."))
    key))

(defun checked-base-url (text)
  "TEXT as the API root, without its trailing slash, or a CONFIG-REFUSAL.
Plain http is allowed only for a loopback host: anywhere else it would send
the key in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "deepgram-voice.api_base_url is empty; it is https://api.deepgram.com/v1 unless a proxy is wanted"))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest host (plusp (length host)))
        (nlk:config-error "deepgram-voice.api_base_url is ~s; it must be an https URL, such as https://api.deepgram.com/v1" url))
      (when (and plain (not (member (string-downcase host) '("localhost" "127.0.0.1" "::1" "[::1]")
                                     :test #'string=)))
        (nlk:config-error "deepgram-voice.api_base_url is ~s; plain http would send the key in clear, so it is allowed only for a localhost stand-in" url))
      url)))

(defun auth-headers (key &optional (content-type "application/json"))
  "The headers every Deepgram call carries: its token, and what the body is."
  (append (list (cons "authorization" (format nil "Token ~a" key)))
          (when content-type (list (cons "content-type" content-type)))))

(defun url-encode (text)
  "TEXT as one query parameter value."
  (with-output-to-string (out)
    (loop for char across text
          do (cond ((or (alphanumericp char) (find char "-._~" :test #'char=))
                    (write-char char out))
                   (t (format out "%~2,'0X" (char-code char)))))))

(defun query-url (base pairs)
  "BASE with PAIRS, an alist of (NAME . VALUE), as its query string; a pair
whose value is NIL or empty is left out."
  (let ((parts (loop for (name . value) in pairs
                     when (and value (plusp (length (princ-to-string value))))
                       collect (format nil "~a=~a" (url-encode name)
                                       (url-encode (princ-to-string value))))))
    (if parts
        (format nil "~a?~{~a~^&~}" base parts)
        base)))

;;; --- what came back ---------------------------------------------------------------

(defun api-error (status body)
  "Deepgram's own words for a failed STATUS, and the fix its code names.
=> (values MESSAGE NOTE)."
  (let* ((text (octets-text body))
         (payload (ignore-errors (nlk:decode-json text)))
         (code (nlk:json-value payload :string "err_code"))
         (message (or (nlk:json-value payload :string "err_msg")
                      (let ((text (nlk:trimmed text)))
                        (if (plusp (length text))
                            (subseq text 0 (min 300 (length text)))
                            "no body")))))
    (values message
            (or (cdr (assoc code '(("INVALID_AUTH" . " (that Deepgram key is not valid)")
                                   ("INSUFFICIENT_PERMISSIONS" . " (this key's role cannot use that endpoint)")
                                   ("PAYMENT_REQUIRED" . " (the Deepgram account has no credit)")
                                   ("TOO_MANY_REQUESTS" . " (rate limited; try again later)"))
                             :test #'string=))
                (cdr (assoc status '((401 . " (check the key: create one at https://console.deepgram.com/)")
                                     (402 . " (add credit to the Deepgram account)")
                                     (403 . " (this key's role cannot use that endpoint)")
                                     (429 . " (rate limited; try again later)"))))))))

(defun transcribe-body (payload)
  "The transcript Deepgram's JSON PAYLOAD holds: every channel's best
alternative, paragraphs preferred when the call asked for them."
  (let ((channels (nlk:json-value payload :any "results" "channels"))
        (texts '()))
    (when (vectorp channels)
      (loop for channel across channels
            do (let* ((alternatives (and (hash-table-p channel)
                                         (nlk:json-value channel :any "alternatives")))
                      (first (and (vectorp alternatives) (plusp (length alternatives))
                                  (aref alternatives 0)))
                      (paragraphs (and (hash-table-p first)
                                       (nlk:json-value first :any "paragraphs")))
                      (text (or (and (hash-table-p paragraphs)
                                     (nlk:json-value paragraphs :text "transcript"))
                                (and (hash-table-p first)
                                     (nlk:json-value first :text "transcript")))))
                 (when text (push (nlk:trimmed text) texts)))))
    (format nil "~{~a~^ ~}" (nreverse texts))))

(defun detected-language (payload)
  "The language Deepgram decided the recording is in, when it detected one."
  (let ((channels (nlk:json-value payload :any "results" "channels"))
        (found nil))
    (when (vectorp channels)
      (loop for channel across channels
            do (when (hash-table-p channel)
                 (setf found (or found (nlk:json-value channel :text "detected_language"))))))
    found))

;;; --- speak ------------------------------------------------------------------------

(defun speak-now (text &key voice format speed)
  "TEXT spoken by Deepgram Aura 2 and saved as a file; => the answer the model
reads, whose first line names the path."
  (let* ((text (nlk:trimmed (or text "")))
         (voice (or (filled voice) (setting :voice)))
         (format-name (string-downcase (or (and format (plusp (length format)) format)
                                           (setting :audio-format))))
         (entry (or (format-entry format-name)
                    (fail "the format is ~s; Deepgram Aura answers one of ~{~a~^, ~}"
                          format-name +format-names+)))
         (extension (second entry))
         (encoding (third entry))
         (container (fourth entry))
         (key (api-key)))
    (when (zerop (length text))
      (fail "there is nothing to say: give the tool the text to speak"))
    (when (> (length text) +max-speak-chars+)
      (fail "the text is ~d characters and one call speaks at most ~d: split it into two calls and stop at a sentence"
            (length text) +max-speak-chars+))
    (when (and speed (or (< speed +min-speed+) (> speed +max-speed+)))
      (fail "speed is ~a; Deepgram speaks from ~a to ~a" speed +min-speed+ +max-speed+))
    (multiple-value-bind (answer status headers truncated)
        (nlk:http :post
                  (query-url (format nil "~a/speak" (setting :api-base-url))
                             (list (cons "model" voice)
                                   (cons "encoding" encoding)
                                   (cons "container" container)
                                   (cons "speed" (and speed (format nil "~,2f" speed)))))
                  :headers (auth-headers key)
                  :content (nlk:encode-json-object (nlk:json-object "text" text))
                  :binary t
                  :max-bytes +max-audio-bytes+
                  :timeout 90)
      (when truncated
        (fail "Deepgram sent more than ~a of audio for one call: speak it in shorter pieces"
              (nlk:size-text +max-audio-bytes+)))
      (let ((octets (octets-of answer)))
        (unless (= status 200)
          (multiple-value-bind (message note) (api-error status octets)
            (fail "Deepgram returned HTTP ~d: ~a~a" status message note)))
        (when (zerop (length octets))
          (fail "Deepgram returned an empty audio response"))
        (unless (audio-p octets extension)
          (fail "Deepgram answered HTTP 200 with ~a, but the bytes are not ~a audio"
                (or (cdr (assoc "content-type" headers :test #'string-equal)) "no content type")
                extension))
        (let* ((directory (setting :directory))
               (name (format nil "voice_~a_~a_~a.~a"
                              (slug text) (datestamp) (voice-id) extension))
               (path (merge-pathnames name directory)))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :if-exists :error
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
            (write-sequence octets out))
          (with-output-to-string (out)
            (format out "saved ~a~%" (namestring path))
            (format out "said: ~a~%" text)
            (format out "~a, ~a, ~a~%" voice format-name (nlk:size-text (length octets)))
            (format out "Give the operator that path: the file plays in any audio player and ~
                         can be moved into a project, a video or a game.~%")))))))

;;; --- transcribe --------------------------------------------------------------------

(defun transcribe-now (path &key model language diarize (smart-format t))
  "The recording at PATH read by Deepgram Nova; => the transcript and what was
read, as the answer the model passes on."
  (let* ((path (resolve-recording path))
         (octets (read-recording path))
         (model-name (or (filled model) (setting :stt-model)))
         (language-name (or (filled language) ""))
         (key (api-key)))
    (multiple-value-bind (answer status headers truncated)
        (nlk:http :post
                  (query-url (format nil "~a/listen" (setting :api-base-url))
                             (list (cons "model" model-name)
                                   (cons "smart_format" (if smart-format "true" "false"))
                                   (cons "punctuate" "true")
                                   (cons "diarize" (and diarize "true"))
                                   (cons "paragraphs" (and diarize "true"))
                                   (cons "language" language-name)
                                   (cons "detect_language" (and (zerop (length language-name)) "true"))))
                  :headers (auth-headers key (content-type-of path))
                  :content octets
                  :binary t
                  :max-bytes (* 4 1024 1024)
                  :timeout 180)
      (declare (ignore headers))
      (when truncated
        (fail "Deepgram's transcript answer was longer than 4 MB: transcribe this recording in shorter pieces"))
      (let ((body (octets-text answer)))
        (unless (= status 200)
          (multiple-value-bind (message note) (api-error status (octets-of answer))
            (fail "Deepgram returned HTTP ~d: ~a~a" status message note)))
        (let* ((payload (nlk:decode-json body))
               (transcript (transcribe-body payload))
               (duration (nlk:json-value payload :number "metadata" "duration"))
               (detected (detected-language payload)))
          (when (zerop (length (nlk:trimmed transcript)))
            (fail "Deepgram heard no words in ~a: the recording may be silent, or not speech"
                  (file-name path)))
          (with-output-to-string (out)
            (format out "transcript: ~a~%" transcript)
            (format out "~a, ~a~@[, language ~a~]~@[, ~a of audio~]~%"
                    model-name
                    (if diarize "diarized" "plain text")
                    detected
                    (and duration (format nil "~,1f s" duration)))
            (format out "from ~a, ~a uploaded~%"
                    (file-name path) (nlk:size-text (length octets)))
            (format out "Pass the transcript on as the words of the recording: what leaves the ~
                         machine is that file's audio, nothing else from the conversation.~%")))))))
