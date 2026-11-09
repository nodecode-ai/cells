;;;; cell.lisp --- TTS Buddy speech as three tools, a /ttsbuddy command and a
;;;; manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites registered TTS Buddy as Hermes' own TTS
;;;; provider and shelled out to the user-installed `ttsbuddy' CLI for every
;;;; call. A cell has no voice pipeline to join, and needs no second binary:
;;;; it speaks to the same endpoint the CLI spoke to, as three tools the model
;;;; calls:
;;;;
;;;;   ttsbuddy_speak    POST {api}/v1/agent-tts -> a job -> poll -> MP3 file
;;;;   ttsbuddy_voices   the CLI's curated catalogue, offline -> ids to choose
;;;;   ttsbuddy_recover  GET {api}/v1/agent-tts?id=... -> download one job
;;;;
;;;; The first is the plugin's providers.synthesize (internal/api's Speak and
;;;; SpeakRequest, cmd/speak.go's poll and download); the second is what
;;;; `ttsbuddy voices' answered from its embedded list; the third is the
;;;; plugin's own ttsbuddy_recover tool, which never submits speech. Which
;;;; voice, pace, language and ceiling each call uses is the section's, the
;;;; values the plugin kept per Hermes profile.
;;;
;;; Everything that could waste a billed call -- a missing key, a missing
;;; voice, a pace outside 0.5 to 1.5, a language on a voice that takes none --
;;; is refused before any request goes out.

(in-package #:nodecode-ttsbuddy)

;;; --- the key and the endpoint ----------------------------------------------------

(defun api-key ()
  "The TTS Buddy key a call is billed to: the section's, else the variable
api_key_env names. A refusal here costs nothing, so it is raised before any
request."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    (when (zerop (length key))
      (fail "no TTS Buddy key: paste an agent key into the ttsbuddy section's api_key, ~
             or set the variable ~a (keys are made at https://www.ttsbuddy.com). Nothing ~
             was sent and nothing was billed." (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the TTS Buddy key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was billed."))
    key))

(defun checked-api-url (text)
  "TEXT as the endpoint, without its trailing slash, or a CONFIG-REFUSAL. Plain
http is allowed only for a loopback host: anywhere else it would send the key
in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "ttsbuddy.api_base_url is empty; it is ~a unless a stand-in is wanted"
                         +default-api-url+))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest host (plusp (length host)))
        (nlk:config-error "ttsbuddy.api_base_url is ~s; it must be an https URL, such as ~a"
                          url +default-api-url+))
      (when (and plain (not (member (string-downcase host) '("localhost" "127.0.0.1" "::1" "[::1]")
                                     :test #'string=)))
        (nlk:config-error "ttsbuddy.api_base_url is ~s; plain http would send the key in clear, so it is allowed only for a localhost stand-in" url))
      url)))

(defun api-host ()
  "The host the endpoint answers on, which a download may also come from."
  (let* ((url (setting :api-base-url))
         (rest (subseq url (if (uiop:string-prefix-p "https://" url) 8 7))))
    (string-downcase (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                        (length rest))))))

(defun parse-speed (text)
  "TEXT as a pace, or NIL when empty; a CONFIG-REFUSAL when it is not a
number from 0.5 to 1.5. The plugin kept a float in a YAML file; a section
field is a string."
  (let ((text (nlk:trimmed (or text ""))))
    (if (zerop (length text))
        nil
        (let ((value (ignore-errors
                       (multiple-value-bind (number end) (read-from-string text nil nil)
                         (and (realp number) (= end (length text))
                              (coerce number 'double-float))))))
          (unless value
            (nlk:config-error "ttsbuddy.speed is ~s; it must be a number from ~a to ~a, or empty"
                              text +min-speed+ +max-speed+))
          (when (or (< value +min-speed+) (> value +max-speed+))
            (nlk:config-error "ttsbuddy.speed is ~a; TTS Buddy speaks from ~a to ~a"
                              text +min-speed+ +max-speed+))
          value))))

(defun checked-language (text)
  "TEXT as a language code, or NIL when empty, or a refusal. Only a Supertonic
voice takes one; the CLI's own pattern is letters with an optional region."
  (let ((text (string-downcase (nlk:trimmed (or text "")))))
    (when (zerop (length text))
      (return-from checked-language nil))
    (unless (every (lambda (char) (or (alphanumericp char) (char= char #\-))) text)
      (fail "language is ~s; give a code such as en, fr, de or pt-BR, or leave it empty" text))
    text))

;;; --- one exchange, and what its failure says ---------------------------------------

(defun error-code-text (code)
  "The fix CODE names, as the plugin's own sentences said it."
  (let ((code (string-upcase (or code ""))))
    (cond ((member code '("AUTH_REQUIRED" "SESSION_EXPIRED" "INVALID_KEY" "NO_API_ACCESS")
                   :test #'string=)
           "check the key in ttsbuddy.api_key, or the variable api_key_env names")
          ((member code '("USAGE_LIMIT_EXCEEDED" "INACTIVE_SUBSCRIPTION") :test #'string=)
           "the account's plan or minutes do not allow this call; this cell never buys one")
          ((string= code "RATE_LIMITED") "wait before starting another call")
          ((string= code "TEXT_TOO_LONG") "say it in fewer words, or split it into two calls")
          ((string= code "FILE_EXPIRED") "that audio file has expired; submit the text again")
          ((string= code "TTS_PROVIDER_ERROR") "TTS Buddy's speech provider could not finish it; try again")
          (t nil))))

(defun error-sentence (status body)
  "TTS Buddy's own words for a failed STATUS, and the fix it names."
  (let* ((text (octets-text body))
         (payload (ignore-errors (nlk:decode-json text)))
         (error (and (hash-table-p payload) (nlk:json-value payload :object "error")))
         (code (and error (nlk:json-value error :string "code")))
         (message (and error (nlk:json-value error :string "message")))
         (hint (or (error-code-text code)
                   (cdr (assoc status '((401 . "check the key")
                                        (403 . "the key is inactive, or the account lacks access")
                                        (429 . "rate limited, or out of minutes; try again shortly"))
                             :test #'=)))))
    (format nil "TTS Buddy returned HTTP ~d~@[ (~a)~]~@[: ~a~]: ~a"
            status code hint
            (or message (one-line text) "no detail"))))

(defun payload-error (payload status)
  "A refusal built from a JSON failure PAYLOAD, or NIL when it is not one."
  (let* ((error (and (hash-table-p payload) (nlk:json-value payload :object "error")))
         (code (and error (nlk:json-value error :string "code")))
         (message (and error (nlk:json-value error :string "message"))))
    (when (or code message (and status (>= status 400)))
      (format nil "TTS Buddy~@[ returned HTTP ~d~]~@[ (~a)~]~@[: ~a~]: ~a"
              status code (or (error-code-text code) "the account, the key or the request")
              (or message "no detail")))))

;;; --- submit, poll, download --------------------------------------------------------

(defun request-speak (key body)
  "One POST to the agent endpoint, as the CLI sent it. A stub in a test
stands here."
  (nlk:http :post (setting :api-base-url)
            :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                           (cons "content-type" "application/json")
                           (cons "accept" "application/json")
                           (cons "idempotency-key" (idempotency-key)))
            :content (nlk:encode-json-object body)
            :timeout 60))

(defun request-status (key job-id)
  "One GET for a known job. A stub in a test stands here."
  (nlk:http :get (format nil "~a?id=~a" (setting :api-base-url) job-id)
            :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                           (cons "accept" "application/json"))
            :timeout 20))

(defun read-payload (body status)
  "BODY as the decoded JSON object, or a refusal naming what came back."
  (let ((payload (ignore-errors (nlk:decode-json (octets-text body)))))
    (unless (hash-table-p payload)
      (fail "~a" (error-sentence status body)))
    payload))

(defun wait-before-poll (delay)
  "Wait DELAY seconds, at most a minute at a time, through *SLEEP*."
  (funcall *sleep* (max 1 (min 60 (round delay)))))

(defun completed-payload (payload status)
  "The PAYLOAD of a finished job, or a refusal when it failed. STATUS is the
HTTP status the response carried."
  (let ((state (string-downcase (or (nlk:json-value payload :string "status") ""))))
    (cond ((string= state "completed") payload)
          ((string= state "failed") (fail "~a" (or (payload-error payload status)
                                                    "TTS Buddy reported the job failed")))
          ((string= state "expired") (fail "~a" (or (payload-error payload status)
                                                     "that job's audio has expired and been deleted; submit the text again")))
          (t (fail "~a" (or (payload-error payload status)
                             (format nil "TTS Buddy answered an unexpected state: ~a"
                                     (if (plusp (length state)) state "none"))))))))

(defun job-id-of (payload)
  "PAYLOAD's job id, or a refusal."
  (let ((id (nlk:json-value payload :string "job_id")))
    (unless (and id (plusp (length id)))
      (fail "TTS Buddy accepted the request but named no job id; nothing can be recovered from it"))
    id))

(defun processing-p (payload status)
  "Whether PAYLOAD is a job still running."
  (let ((state (string-downcase (or (nlk:json-value payload :string "status") ""))))
    (or (= status 202) (= status 201)
        (member state '("processing" "pending" "queued" "running") :test #'string=))))

(defun running-state-p (state)
  "Whether STATE names a job still working."
  (member (string-downcase (or state "")) '("processing" "pending" "queued" "running")
          :test #'string=))

(defun await-job (key payload)
  "Follow the job PAYLOAD names until it finishes, or the section's ceiling
runs out. The plugin polled the same way, honouring retry_after_seconds."
  (let* ((job-id (job-id-of payload))
         (deadline (+ (funcall *now*) (setting :poll-timeout-seconds)))
         (delay (or (nlk:json-value payload :integer "retry_after_seconds") +default-delay+))
         (last payload))
    (loop
      (when (> (funcall *now*) deadline)
        (fail "TTS Buddy was still working on job ~a after ~a seconds. The hosted job ~
               may still finish: call ttsbuddy_recover with that exact job id and a new ~
               path, which downloads it without submitting anything again."
              job-id (setting :poll-timeout-seconds)))
      (wait-before-poll delay)
      (multiple-value-bind (body status-code) (request-status key job-id)
        (setf last (read-payload body status-code))
        (let ((state (nlk:json-value last :string "status")))
          (cond ((and state (string= (string-downcase state) "completed")) (return last))
                ((running-state-p state)
                 (setf delay (or (nlk:json-value last :integer "retry_after_seconds")
                                 +default-delay+)))
                (t (return (completed-payload last status-code)))))))))

(defun download-host-ok-p (url)
  "Whether URL may be fetched for audio: https to the API host or its object
storage, as the CLI's ValidateDownloadURL allowed, or loopback for a stand-in."
  (let* ((secure (uiop:string-prefix-p "https://" url))
         (plain (uiop:string-prefix-p "http://" url)))
    (when (or secure plain)
      (let* ((rest (subseq url (if secure 8 7)))
             (host (string-downcase (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#"))
                                                                      rest)
                                                        (length rest))))))
        (cond ((zerop (length host)) nil)
              ((member host '("localhost" "127.0.0.1" "::1" "[::1]") :test #'string=) t)
              ((not secure) nil)
              ((string= host (api-host)) t)
              ((uiop:string-suffix-p ".amazonaws.com" host) t)
              (t nil))))))

(defun audio-url (payload)
  "The finished job's audio URL, or a refusal."
  (let ((url (nlk:json-value payload :string "audio_url")))
    (unless (and url (plusp (length url)))
      (fail "TTS Buddy finished the job but named no audio URL; nothing could be downloaded"))
    (unless (download-host-ok-p url)
      (fail "TTS Buddy pointed at an audio host this cell does not trust, so nothing was ~
             downloaded: ~a" (one-line url 120)))
    url))

(defun save-mp3 (octets text voice)
  "Write OCTETS under the section's folder, never over an existing file, and
answer the model where it is."
  (let* ((directory (setting :directory))
         (name (format nil "ttsbuddy_~a_~a_~a.mp3" (slug text) (datestamp) (fresh-id)))
         (path (merge-pathnames name directory)))
    (ensure-directories-exist path)
    ;; IF-EXISTS :ERROR: a stamp and an id make a collision absurd, and one
    ;; utterance never overwrites the one before it.
    (with-open-file (out path :direction :output :if-exists :error
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    (with-output-to-string (out)
      (format out "saved ~a~%" (namestring path))
      (format out "~a in voice ~a, mp3, ~a~%" (setting :directory) voice (nlk:size-text (length octets)))
      (format out "Give the operator that path: any audio player opens it.~%")
      (format out "Billed to the TTS Buddy account the key belongs to.~%"))))

(defun fetch-audio (url)
  "URL as an octet vector, refused unless it looks like MPEG audio."
  (multiple-value-bind (body status headers truncated)
      (nlk:http :get url :binary t :max-bytes +max-audio-bytes+ :timeout 300)
    (declare (ignore headers))
    (when truncated
      (fail "TTS Buddy sent more than ~a of audio, which is no single answer"
            (nlk:size-text +max-audio-bytes+)))
    (let ((octets (octets-of body)))
      (unless (= status 200)
        (fail "TTS Buddy's audio download answered HTTP ~d" status))
      (when (zerop (length octets))
        (fail "TTS Buddy's audio download was empty"))
      (unless (mp3-p octets)
        (fail "TTS Buddy answered HTTP 200 for the audio, but the bytes are not an MP3"))
      octets)))

;;; --- speaking --------------------------------------------------------------------

(nlk:define-contract speak-request
  (:doc "Say text out loud with TTS Buddy and save it as an MP3 on this machine: one call, one file. The answer names the saved path -- give that path to the operator. Billed to the operator's TTS Buddy account. Pick a voice with ttsbuddy_voices when none is selected in the section.")
  ("text" :text :required t
          :doc "what to say, in the words to be spoken; numbers and names are read as written")
  ("voice" :text
           :doc "a TTS Buddy voice id, such as af_heart or st_m1; omit for the ttsbuddy section's voice")
  ("speed" :number :slot pace
           :doc "0.5 to 1.5; omit for the section's speed")
  ("language" :text
              :doc "the language code a Supertonic st_* voice speaks, such as en or fr; Kokoro voices take none. Omit for the section's language"))

(defun speak-now (request)
  "The ttsbuddy_speak call, from a materialized SPEAK-REQUEST."
  (let* ((text (speak-request-text request))
         (voice (let ((named (nlk:trimmed (or (speak-request-voice request) ""))))
                  (if (plusp (length named)) named (nlk:trimmed (or (setting :voice) "")))))
         (speed (or (speak-request-pace request) (setting :speed)))
         (language (or (checked-language (speak-request-language request))
                       (checked-language (setting :language))))
         (key (api-key)))
    (when (zerop (length voice))
      (fail "no voice is selected: list them with ttsbuddy_voices, then set ttsbuddy.voice, ~
             or pass voice to this call. Nothing was sent and nothing was billed."))
    (when (and speed (or (< speed +min-speed+) (> speed +max-speed+)))
      (fail "speed is ~a; TTS Buddy speaks from ~a to ~a. Nothing was sent and nothing was billed."
            speed +min-speed+ +max-speed+))
    (when (and language (not (uiop:string-prefix-p "st_" (string-downcase voice))))
      (fail "language applies only to the Supertonic st_* voices, and ~a is not one; ~
             a Kokoro voice speaks its own language. Nothing was sent and nothing was billed."
            voice))
    (multiple-value-bind (body status)
        (request-speak key (nlk:json-object "text" text
                                             "voice" voice
                                             :opt "speed" speed
                                             :opt "language" language
                                             "client_context" (nlk:json-object
                                                               "client" "nodecode-ttsbuddy"
                                                               "version" "0.1.0"
                                                               "execution_context" "agent")))
      (let* ((payload (read-payload body status))
             (done (if (processing-p payload status)
                       (await-job key payload)
                       (completed-payload payload status)))
             (octets (fetch-audio (audio-url done))))
        (save-mp3 octets text voice)))))

(defun speak (request)
  "The ttsbuddy_speak tool, one refusal away from the model's own words."
  (handler-case (speak-now request)
    (ttsbuddy-error (condition)
      (nle:failure (format nil "ERROR: ~a" (ttsbuddy-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- recovery of one known job ------------------------------------------------------

(nlk:define-contract recover-request
  (:doc "Download the MP3 of a TTS Buddy job that was already submitted -- one exact job id, never a new synthesis, so nothing is billed twice. Use it when a speak call timed out or was interrupted and the answer named that job id.")
  ("job_id" :text :required t
            :doc "the exact TTS Buddy job id an earlier answer named")
  ("path" :text
          :doc "where to save the MP3: a path with no file at it yet, absolute or under the home; omit for a fresh name in the section's folder"))

(defun job-id-shaped-p (text)
  "Whether TEXT reads as a TTS Buddy job id."
  (and (plusp (length text))
       (<= (length text) 128)
       (alphanumericp (char text 0))
       (every (lambda (char) (or (alphanumericp char) (char= char #\-) (char= char #\_)))
              text)))

(defun output-path (text job-id)
  "TEXT, the call's own path or NIL, as a fresh MP3 pathname."
  (let ((text (nlk:trimmed (or text ""))))
    (if (zerop (length text))
        (merge-pathnames (format nil "ttsbuddy_recovered_~a_~a.mp3" job-id (datestamp))
                         (setting :directory))
        (let ((path (cond ((char= (char text 0) #\/) (uiop:parse-native-namestring text))
                          ((and (> (length text) 1) (char= (char text 0) #\~)
                                (char= (char text 1) #\/))
                           (nlk:home (subseq text 2)))
                          (t (nlk:home text)))))
          (make-pathname :type "mp3" :defaults path)))))

(defun recover-now (request)
  "The ttsbuddy_recover call: one GET for one known job, then its audio."
  (let* ((job-id (nlk:trimmed (or (recover-request-job-id request) "")))
         (key (api-key))
         (path (output-path (recover-request-path request) (if (job-id-shaped-p job-id) job-id "job"))))
    (unless (job-id-shaped-p job-id)
      (fail "job_id is ~s; give the exact id an earlier TTS Buddy answer named" job-id))
    (when (probe-file path)
      (fail "there is already a file at ~a; give a path nothing is at yet -- this cell never overwrites one"
            (namestring path)))
    (ensure-directories-exist path)
    (multiple-value-bind (body status) (request-status key job-id)
      (let* ((payload (read-payload body status))
             (done (completed-payload payload status)))
        (save-mp3-into (fetch-audio (audio-url done)) path job-id)))))

(defun save-mp3-into (octets path job-id)
  "Write OCTETS at PATH, never over an existing file, and answer where it is."
  (with-open-file (out path :direction :output :if-exists :error
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
    (write-sequence octets out))
  (format nil "saved ~a~%job ~a, mp3, ~a~%Give the operator that path. Nothing was submitted again and nothing was billed again.~%"
          (namestring path) job-id (nlk:size-text (length octets))))

(defun recover (request)
  "The ttsbuddy_recover tool, refusing the same way as speak."
  (handler-case (recover-now request)
    (ttsbuddy-error (condition)
      (nle:failure (format nil "ERROR: ~a" (ttsbuddy-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- the voice catalogue, offline ---------------------------------------------------

(nlk:define-contract voice-request
  (:doc "List the TTS Buddy voices this cell knows offline, filtered by words: language names, male or female, kokoro or supertonic, or an id. Nothing is sent and nothing is billed; give an id to ttsbuddy_speak.")
  ("query" :text
           :doc "words to match anywhere in a voice's id, name, language, gender or engine; omit for the whole catalogue")
  ("language" :text
              :doc "only voices for one language, as a name or a code: French, ja")
  ("limit" :integer
           :doc "how many voices to list, 1 to 100; 50 unless asked"))

(defun words-of (text)
  "TEXT's words, downcased."
  (let ((text (or text "")) (out '()) (start 0))
    (loop for index from 0 to (length (or text ""))
          do (when (or (= index (length (or text "")))
                        (member (char text index) '(#\Space #\Tab #\, #\;)))
               (let ((word (string-downcase (subseq text start index))))
                 (when (plusp (length word)) (push word out)))
               (setf start (1+ index))))
    (nreverse out)))

(defun voice-text (row)
  "ROW as the line a search answers."
  (destructuring-bind (id name gender language engine) row
    (format nil "~a  ~a, ~a, ~a~@[ (language: ~a)~]"
            id name gender language
            (and (string= engine "supertonic") "pass language for the accent"))))

(defun voice-matches-p (row words language)
  "Whether ROW answers every one of WORDS and the LANGUAGE filter."
  (destructuring-bind (id name gender voice-language engine) row
    (let ((haystack (string-downcase (format nil "~a ~a ~a ~a ~a"
                                             id name gender voice-language engine))))
      (and (every (lambda (word) (search word haystack :test #'char=)) words)
           (or (null language)
               (let ((language (string-downcase language)))
                 (or (search language haystack :test #'char=)
                     (and (string= engine "supertonic")
                          (member language +supertonic-languages+ :test #'string=)))))))))

(defun voices-now (request)
  "The ttsbuddy_voices answer: the matching rows and the id to pass on."
  (let* ((words (words-of (voice-request-query request)))
         (language (nlk:trimmed (or (voice-request-language request) "")))
         (limit (or (voice-request-limit request) 50))
         (rows (remove-if-not (lambda (row) (voice-matches-p row words language)) (voice-rows)))
         (shown (if (> (length rows) limit) (subseq rows 0 limit) rows)))
    (when (zerop (length rows))
      (fail "no curated voice matches ~{~a~^ ~}; try a language name (French), a gender, kokoro or supertonic, or part of an id"
            words))
    (with-output-to-string (out)
      (format out "~d voice~:p~@[ matching ~{~a~^ ~}~]~@[, language ~a~]~%~%"
              (length rows) words language)
      (dolist (row shown)
        (format out "~a~%" (voice-text row)))
      (when (> (length rows) (length shown))
        (format out "... and ~d more; say limit to list them~%" (- (length rows) (length shown))))
      (format out "~%Pass an id as voice to ttsbuddy_speak, or keep it in ttsbuddy.voice for every call.~%")
      (format out "This list is the CLI's curated catalogue, so it costs nothing: nothing was sent.~%"))))

(defun voices (request)
  "The ttsbuddy_voices tool, refusing the same way as speak."
  (handler-case (voices-now request)
    (ttsbuddy-error (condition)
      (nle:failure (format nil "ERROR: ~a" (ttsbuddy-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /ttsbuddy -----------------------------------------------------------------

(defun key-state ()
  "Whether a key is ready, and where it comes from -- never the key itself."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (cond ((plusp (length key)) "set in ttsbuddy.api_key")
          ((plusp (length (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
           (format nil "read from the variable ~a" (setting :api-key-env)))
          (t (format nil "not set: fill api_key in /settings, or export ~a"
                     (setting :api-key-env))))))

(defun status-text ()
  "What /ttsbuddy says with no argument: the settings every call runs on."
  (with-output-to-string (out)
    (format out "TTS Buddy~%")
    (format out "  key: ~a~%" (key-state))
    (format out "  voice: ~a~@[, speed ~a~]~@[, language ~a~]~%"
            (setting :voice) (setting :speed) (setting :language))
    (format out "  files: ~a~%" (namestring (setting :directory)))
    (format out "  endpoint: ~a, job ceiling ~a s~%"
            (setting :api-base-url) (setting :poll-timeout-seconds))
    (format out "  /ttsbuddy voices <words> -- the catalogue; /ttsbuddy say <text> -- speak it~%")))

(defun say-text (text)
  "TEXT spoken through the call the tool makes, its answer as the shell shows it."
  (speak-now (make-speak-request :text text)))

(defun command-text (argument)
  "ARGUMENT, the words after /ttsbuddy, as the text the shell shows."
  (let* ((argument (nlk:trimmed (or argument "")))
         (space (position #\Space argument))
         (word (string-downcase (if space (subseq argument 0 space) argument)))
         (rest (if space (nlk:trimmed (subseq argument (1+ space))) "")))
    (cond ((zerop (length argument)) (status-text))
          ((string= word "voices")
           (if (zerop (length rest))
               (voices-now (make-voice-request))
               (voices-now (make-voice-request :query rest))))
          ((string= word "say")
           (if (zerop (length rest))
               "Say what to speak: /ttsbuddy say Good morning."
               (say-text rest)))
          ((string= word "status") (status-text))
          (t (format nil "Unknown: /ttsbuddy ~a. Try /ttsbuddy, /ttsbuddy voices <words>, or /ttsbuddy say <text>.~%"
                     argument)))))

(defun run-command (args session-id)
  "The /ttsbuddy command: the settings, the voice catalogue, or one utterance."
  (declare (ignore session-id))
  (handler-case (command-text args)
    (ttsbuddy-error (condition) (ttsbuddy-error-detail condition))
    (error (condition) (format nil "ttsbuddy: ~a" condition))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "TTS Buddy is available through the nodecode-ttsbuddy cell: three tools and the
/ttsbuddy command, on one TTS Buddy key billed to the operator's account.
  ttsbuddy_speak(text, voice?, speed?, language?)
      one utterance, saved as an MP3 in this profile's voice folder; the answer names
      the path -- give it to the operator. At most a paragraph or two per call: the
      file is played, not read. A Kokoro voice speaks its own language; a Supertonic
      st_* voice takes language (en, fr, de, ja...). Speed is 0.5 to 1.5.
  ttsbuddy_voices(query?, language?, limit?)
      the curated catalogue the CLI shipped, filtered by words: \"French female\",
      \"kokoro\", \"st_\". Offline -- nothing is sent and nothing is billed.
  ttsbuddy_recover(job_id, path?)
      download one job that was already submitted, by its exact id, to a fresh path.
      Nothing is submitted and nothing is billed twice; use it after a timeout.
Every call that reaches TTS Buddy is billed to that account. The section holds the
defaults: voice, speed, language, directory, api_base_url, poll_timeout_seconds,
and the key (api_key, or the variable api_key_env names)."
  "What (help :ttsbuddy) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell ttsbuddy
  (:section ("ttsbuddy")
    (:guide "make an agent key at https://www.ttsbuddy.com and paste it in api_key, or name the variable that holds it in api_key_env; pick a voice with ttsbuddy_voices (or /ttsbuddy voices <words>) and keep its id in voice; speed is 0.5 to 1.5 and language applies only to the Supertonic st_* voices; directory is where the MP3s are kept; every call is billed to that TTS Buddy account")
    ("api_key" :secret
               :doc "the TTS Buddy agent key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "TTSBUDDY_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("voice" :string :default "st_m1"
             :doc "the voice every call speaks in unless it names one; list them with ttsbuddy_voices. Kokoro ids such as af_heart speak their own language, the Supertonic st_* voices take language")
    ("speed" :string
             :doc "the pace every call speaks at, 0.5 to 1.5, written as a number; empty, the API's own")
    ("language" :string
                :doc "the language code a Supertonic voice speaks, such as en, fr or de; Kokoro voices ignore it")
    ("directory" :directory :default "voice/"
                 :doc "where the synthesized MP3s are kept, under the home")
    ("api_base_url" :string :default "https://www.ttsbuddy.com/v1/agent-tts"
                    :doc "the TTS Buddy agent endpoint; change it only for a stand-in")
    ("poll_timeout_seconds" :integer :default 600 :min 30
                            :doc "how long one job is followed before the call answers the job id for ttsbuddy_recover; 30 to 1800"))
  ;; The one thing the declaration cannot say: a pace is a number in a string
  ;; field, and 1800 is the ceiling the plugin enforced.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :api-base-url) (checked-api-url (getf values :api-base-url)))
               (setf (getf values :speed) (parse-speed (getf values :speed)))
               (let ((timeout (getf values :poll-timeout-seconds)))
                 (when (or (> timeout +max-poll-timeout+) (< timeout +min-poll-timeout+))
                   (nlk:config-error "ttsbuddy.poll_timeout_seconds is ~a; it must be from ~a to ~a"
                                     timeout +min-poll-timeout+ +max-poll-timeout+)))
               values))
  (:tool "ttsbuddy_speak" (nlk:contract 'speak-request) 'speak)
  (:tool "ttsbuddy_voices" (nlk:contract 'voice-request) 'voices)
  (:tool "ttsbuddy_recover" (nlk:contract 'recover-request) 'recover)
  (:help :ttsbuddy "ttsbuddy: TTS Buddy speech - speak text into an MP3, pick a voice, recover a job" +primer+)
  (:command "ttsbuddy" 'run-command
            :description "TTS Buddy: the settings, the voice catalogue, or one utterance"
            :argument-hint "[status|voices <words>|say <text>]"
            :session nil)
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first
            ;; start, even before the first utterance.
            (ensure-directories-exist (merge-pathnames "voice.keep" (setting :directory))))))
