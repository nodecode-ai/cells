;;;; cell.lisp --- Gemini Live voice, one spoken turn per call.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-gemini-live)

(nlk:define-peripheral gemini-live :not-running t)

;;; --- what the endpoint is ---------------------------------------------------------

(defparameter +ws-uri+
  "wss://generativelanguage.googleapis.com/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent")

(defparameter +default-model+ "gemini-3.8-live")
(defparameter +default-voice+ "Aoede")
(defparameter +in-rate+ 16000 "Mic bytes go up at 16 kHz; the model speaks at 24 kHz.")
(defparameter +out-rate+ 24000)
(defparameter +max-chars+ 4000 "The longest text turn sent, the original's cap.")

(defparameter +instructions+
  (concatenate 'string
               "You are a spoken assistant. Keep answers short and speakable: no markdown, "
               "no bullet lists, no code blocks, no long paths. You hold no present-tense "
               "knowledge: no files, no terminal, no web. For anything real or current, your "
               "first action is a call to hermes_task - never guess and never claim you "
               "cannot reach it. After calling it, say one brief sentence that you are on it, "
               "then stop talking and wait for the result."))

(defparameter +declarations+
  (concatenate 'string
               "[{\"name\":\"hermes_task\",\"description\":\"Hand real work to the agent: "
               "files, terminals, web, code, memory. The call returns a short task id "
               "immediately; the result arrives later. After calling it, say in one short "
               "sentence that you are on it, then stay quiet. Never invent the result.\","
               "\"parameters\":{\"type\":\"OBJECT\",\"properties\":{\"task\":{\"type\":\"STRING\","
               "\"description\":\"what to do, self-contained\"},\"context\":{\"type\":\"STRING\","
               "\"description\":\"background the worker needs\"}},\"required\":[\"task\"]},"
               "\"behavior\":\"NON_BLOCKING\"},"
               "{\"name\":\"hermes_tasks\",\"description\":\"Read the task board: every task "
               "this call started and where it stands.\","
               "\"parameters\":{\"type\":\"OBJECT\",\"properties\":{}},\"behavior\":\"NON_BLOCKING\"},"
               "{\"name\":\"hermes_task_update\",\"description\":\"Act on a task you started, "
               "by its id. You cannot approve one: only the user can.\","
               "\"parameters\":{\"type\":\"OBJECT\",\"properties\":{\"task\":{\"type\":\"STRING\"},"
               "\"action\":{\"type\":\"STRING\",\"enum\":[\"deny\",\"steer\",\"stop\"]},"
               "\"answer\":{\"type\":\"STRING\"}},\"required\":[\"task\",\"action\"]},"
               "\"behavior\":\"NON_BLOCKING\"}]"))

(defparameter +primer+
  (concatenate 'string
               "Gemini Live voice is available through the nodecode-gemini-live cell: one tool, "
               "gemini_live, and /live."
               "  gemini_live(text, audio_path?, delegate?)"
               "      ONE spoken turn per call: the text goes to the Gemini Live model, which "
               "answers out loud. The answer holds the spoken transcript, the path of the "
               "saved WAV (24 kHz mono), and any task the voice model delegated through "
               "hermes_task - that task is not run here, so do it, or tell the operator "
               "what it asked for."
               "      audio_path sends a 16-bit PCM WAV before the text, so a recorded clip "
               "is answered too. Each call is billed to the operator's Google AI Studio key "
               "and holds a metered, listening socket while it runs: one call per thing the "
               "operator wants said aloud. /live with no argument shows the configured model "
               "and voice and the calls made so far; /live <what to say> runs one turn."))

;;; --- JSON, built by hand so nothing hides in a codec --------------------------------

(defun json-string (text)
  "TEXT as a JSON string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across (or text "") do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (if (< (char-code char) 32)
               (format out "\\u~4,'0x" (char-code char))
               (write-char char out)))))
    (write-char #\" out)))

(defun jget (object key)
  (and (hash-table-p object) (values (gethash key object))))

(defun jlist (value)
  (cond ((vectorp value) (coerce value 'list))
        ((consp value) value)
        (t nil)))

(defun wire-model (model)
  (if (uiop:string-prefix-p "models/" model) model (concatenate 'string "models/" model)))

(defun instructions ()
  (let ((extra (nlk:trimmed (or (setting :instructions) ""))))
    (if (plusp (length extra))
        (concatenate 'string +instructions+ " " extra)
        +instructions+)))

(defun setup-frame (&key (tools t))
  (format nil (concatenate 'string
                           "{\"setup\":{\"model\":~a,\"generationConfig\":{"
                           "\"responseModalities\":[\"AUDIO\"],\"speechConfig\":{\"voiceConfig\":{"
                           "\"prebuiltVoiceConfig\":{\"voiceName\":~a}}}},\"systemInstruction\":{"
                           "\"parts\":[{\"text\":~a}]},\"inputAudioTranscription\":{},"
                           "\"outputAudioTranscription\":{},\"sessionResumption\":{},"
                           "\"contextWindowCompression\":{\"slidingWindow\":{}}~a}}")
          (json-string (wire-model (setting :model)))
          (json-string (setting :voice))
          (json-string (instructions))
          (if tools (format nil ",\"tools\":[{\"functionDeclarations\":~a}]" +declarations+) "")))

(defun audio-frame (base64)
  (format nil "{\"realtimeInput\":{\"audio\":{\"mimeType\":\"audio/pcm;rate=~d\",\"data\":~a}}}"
          +in-rate+ (json-string base64)))

(defun text-frame (text)
  (format nil "{\"realtimeInput\":{\"text\":~a}}" (json-string text)))

(defun tool-response-frame (id name result)
  (format nil "{\"toolResponse\":{\"functionResponses\":[{\"id\":~a,\"name\":~a,\"response\":{\"result\":~a}}]}}"
          (json-string id) (json-string name) (json-string result)))

;;; --- one frame in, the events it carries -------------------------------------------

(defun frame-events (frame)
  "FRAME decoded as a JSON object; the events it carries, oldest first. Never signals:
unknown fields are dropped rather than trusted."
  (let ((events '()))
    (when (jget frame "setupComplete") (push '(:ready) events))
    (let ((error (jget frame "error")))
      (when (hash-table-p error)
        (push (list :error (or (jget error "message") "the Live API refused the session")) events)))
    (let ((content (jget frame "serverContent")))
      (when (hash-table-p content)
        (let ((turn (jget content "modelTurn")))
          (dolist (part (jlist (and (hash-table-p turn) (jget turn "parts"))))
            (when (hash-table-p part)
              (let ((inline (jget part "inlineData")))
                (when (and (hash-table-p inline) (jget inline "data"))
                  (push (list :audio (jget inline "data")) events)))
              (let ((text (jget part "text")))
                (when (and (stringp text) (plusp (length (nlk:trimmed text))))
                  (push (list :text text) events))))))
        (when (jget content "turnComplete") (push '(:turn-complete) events))))
    (dolist (field '("inputTranscription" "outputTranscription"))
      (let* ((part (jget frame field))
             (text (and (hash-table-p part) (jget part "text"))))
        (when (and (stringp text) (plusp (length text)))
          (push (list :text text) events))))
    (let ((call (jget frame "toolCall")))
      (when (hash-table-p call)
        (dolist (item (jlist (jget call "functionCalls")))
          (when (hash-table-p item)
            (push (list :tool-call (or (jget item "id") "") (or (jget item "name") "")
                        (jget item "args"))
                  events)))))
    (nreverse events)))

(defun event-kind (event)
  (if (consp event) (first event) event))

;;; --- WAV out, PCM in ----------------------------------------------------------------

(defun poke-tag (array offset text)
  (loop for char across text for index from offset
        do (setf (aref array index) (char-code char))))

(defun poke-u16 (array offset value)
  (dotimes (index 2) (setf (aref array (+ offset index)) (ldb (byte 8 (* 8 index)) value))))

(defun poke-u32 (array offset value)
  (dotimes (index 4) (setf (aref array (+ offset index)) (ldb (byte 8 (* 8 index)) value))))

(defun wav-bytes (pcm &key (rate +out-rate+))
  "PCM octets as one 16-bit mono WAV of RATE."
  (let* ((length (length pcm))
         (out (make-array (+ 44 length) :element-type '(unsigned-byte 8) :initial-element 0)))
    (poke-tag out 0 "RIFF") (poke-u32 out 4 (+ 36 length))
    (poke-tag out 8 "WAVE") (poke-tag out 12 "fmt ")
    (poke-u32 out 16 16) (poke-u16 out 20 1) (poke-u16 out 22 1)
    (poke-u32 out 24 rate) (poke-u32 out 28 (* rate 2))
    (poke-u16 out 32 2) (poke-u16 out 34 16)
    (poke-tag out 36 "data") (poke-u32 out 40 length)
    (replace out pcm :start1 44)
    out))

(defun read-octets (path)
  "Every byte of PATH, as one vector."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun wav-pcm (path)
  "The sample bytes of a 16-bit PCM WAV FILE, header stripped."
  (let* ((octets (read-octets path))
         (at (search #(100 97 116 97) octets)))
    (if (and at (<= (+ at 8) (length octets)))
        (subseq octets (+ at 8))
        octets)))

(defun audio-seconds (pcm)
  (/ (length pcm) 2.0 +out-rate+))

;;; --- the socket -------------------------------------------------------------------

(defvar *transport-maker* nil
  "NIL for the real websocket, or a function of one URL answering a transport plist
(:send :recv :close) - the seam every test drives.")

(defun websocket-transport (url)
  "A transport over websocket-driver, the runtime's RFC-6455 client."
  (let* ((ws (make-instance 'websocket-driver.ws.client:client :url url))
         (lock (sb-thread:make-mutex :name "gemini-live"))
         (wait (sb-thread:make-waitqueue :name "gemini-live"))
         (incoming '())
         (closed nil))
    (websocket-driver:on :message ws
                         (lambda (message)
                           (sb-thread:with-mutex (lock)
                             (setf incoming (nconc incoming (list message)))
                             (sb-thread:condition-broadcast wait))))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (sb-thread:with-mutex (lock)
                             (setf closed t)
                             (sb-thread:condition-broadcast wait))))
    (websocket-driver:start-connection ws)
    (list :send (lambda (text) (websocket-driver:send ws text))
          :recv (lambda (seconds)
                  (sb-thread:with-mutex (lock)
                    (loop unless (or incoming closed)
                          do (unless (sb-thread:condition-wait wait lock :timeout seconds)
                               (return)))
                    (or (pop incoming) (and closed :closed))))
          :close (lambda () (ignore-errors (nlk:sever-websocket ws))))))

(defun open-transport (url)
  (if *transport-maker* (funcall *transport-maker* url) (websocket-transport url)))

(defun transport-send (transport text)
  (funcall (getf transport :send) text))

(defun transport-close (transport)
  (ignore-errors (funcall (getf transport :close))))

(defun now ()
  (/ (get-internal-real-time) (float internal-time-units-per-second)))

(defun next-frame (transport deadline)
  "The next decoded frame, or NIL when the deadline passed."
  (let* ((raw (funcall (getf transport :recv) (max 0.1 (- deadline (now))))))
    (cond ((null raw) nil)
          ((eq raw :closed) nil)
          ((stringp raw) (nlk:decode-json raw))
          ((vectorp raw) (nlk:decode-json (sb-ext:octets-to-string raw :external-format :utf-8)))
          (t (ignore-errors (nlk:decode-json (princ-to-string raw)))))))

(defun redact (text key)
  "TEXT with every occurrence of KEY replaced, since a transport error quotes its URL."
  (if (and (stringp text) (stringp key) (>= (length key) 6))
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search key text :start2 start)
              do (if at
                     (progn (write-string text out :start start :end at)
                            (write-string "***" out)
                            (setf start (+ at (length key))))
                     (progn (write-string text out :start start) (return)))))
      (princ-to-string (or text ""))))

;;; --- the key, and the folder -------------------------------------------------------

(defun api-key ()
  "The key a call is billed to, or a refusal that names the fix. Raised before any socket."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (let ((variable (nlk:trimmed (or (setting :api-key-env) ""))))
        (when (plusp (length variable))
          (setf key (nlk:trimmed (or (uiop:getenv variable) "")))))
    (when (zerop (length key))
      (fail "no Gemini key: paste one into the gemini-live section's api_key, or set the variable ~a. A key from https://aistudio.google.com/apikey works on the free tier. Nothing was sent."
            (setting :api-key-env)))
    key))

(defun call-directory ()
  (let ((directory (nlk:trimmed (or (setting :directory) ""))))
    (if (plusp (length directory))
        (uiop:ensure-directory-pathname directory)
        (nlk:home "gemini-live/"))))

(defun recent-calls (&optional (count 5))
  "The last COUNT reply files written, newest first, as a line each."
  (let* ((directory (call-directory))
         (files (and (uiop:directory-exists-p directory)
                     (sort (remove-if-not (lambda (path) (equal "wav" (pathname-type path)))
                                          (uiop:directory-files directory))
                           #'> :key (lambda (path) (or (file-write-date path) 0))))))
    (if files
        (format nil "~{~a~%~}"
                (loop for path in (subseq files 0 (min count (length files)))
                      for date = (file-write-date path)
                      collect (format nil "  ~a~@[  (~a)~]"
                                      (file-namestring path)
                                      (and date (multiple-value-bind (s m h d mo y) (decode-universal-time date)
                                                  (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))))))
        "no calls saved yet")))

;;; --- one turn ---------------------------------------------------------------------

(defun await-ready (transport deadline key)
  (loop
    (let ((frame (next-frame transport deadline)))
      (unless frame
        (fail "the Gemini Live API did not acknowledge the session setup in time. Nothing was said."))
      (dolist (event (frame-events frame))
        (case (event-kind event)
          (:ready (return-from await-ready t))
          (:error (fail "the Gemini Live API refused the session: ~a"
                        (redact (or (second event) "") key))))
          (t nil))))))

(defun collect-reply (transport deadline)
  "Whatever the model said before its turn completed: (values PCM TEXT TASKS FAILURE)."
  (let ((chunks '()) (said "") (tasks '()) (done nil) (failure nil))
    (loop while (and (not done) (< (now) deadline))
          for frame = (next-frame transport deadline)
          do (if (null frame)
                 (setf done t)
                 (dolist (event (frame-events frame))
                   (case (event-kind event)
                     (:audio (push (cl-base64:base64-string-to-usb8-array (second event)) chunks))
                     (:text (setf said (concatenate 'string said (or (second event) ""))))
                     (:tool-call
                      (let ((id (second event)) (name (third event)) (args (fourth event)))
                        (push (or (and (hash-table-p args) (jget args "task")) "") tasks)
                        (transport-send transport
                                         (tool-response-frame
                                          id name
                                          (format nil "WORK_STARTED ~a: the agent is running it. Do not describe any result yet." id)))))
                     (:turn-complete (setf done t))
                     (:error (setf failure (second event) done t))))))
    (values (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse chunks))
            (nlk:trimmed said) (nreverse tasks) failure)))

(defun live-turn (request)
  "One spoken turn: text in, the reply's transcript and a saved WAV out."
  (let ((text (nlk:trimmed (or (live-request-text request) "")))
        (clip (nlk:trimmed (or (live-request-audio-path request) ""))))
    (when (zerop (length text))
      (fail "text is empty: say what the operator wants said aloud, or what to ask."))
    (when (> (length text) +max-chars+)
      (fail "text is ~d characters; one spoken turn takes at most ~d. Split it, or say the short version here."
            (length text) +max-chars+))
    (when (and (plusp (length clip)) (not (probe-file clip)))
      (fail "audio_path ~a is not a file this machine can read." clip))
    (let* ((key (api-key))
           (seconds (setting :timeout-seconds))
           (transport (open-transport (format nil "~a?key=~a" +ws-uri+ key)))
           (deadline (+ (now) seconds)))
      (unwind-protect
           (progn
             (transport-send transport (setup-frame :tools (live-request-delegate request)))
             (await-ready transport deadline key)
             (when (plusp (length clip))
               (let ((pcm (wav-pcm clip)))
                 (loop for start from 0 below (length pcm) by 3200
                       do (transport-send
                           transport
                           (audio-frame (cl-base64:usb8-array-to-base64-string
                                         (subseq pcm start (min (length pcm) (+ start 3200)))))))))
             (transport-send transport (text-frame text))
             (multiple-value-bind (pcm said tasks failure) (collect-reply transport deadline)
               (when failure
                 (fail "the Gemini Live API reported: ~a" (redact (princ-to-string failure) key)))
               (let* ((directory (call-directory))
                      (path (merge-pathnames
                             (format nil "live-~d-~2,'0d.wav" (get-universal-time) (random 100))
                             directory)))
                 (ensure-directories-exist path)
                 (when (plusp (length pcm))
                   (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                              :if-exists :supersede)
                     (write-sequence (wav-bytes pcm) out)))
                 (format nil "Gemini Live (~a, voice ~a) said: ~a~%Saved the reply audio (~,1f s, 24 kHz mono WAV): ~a~@[~%The voice model asked for work, which is not run here, so do it or report it: ~{\n  - ~a~}~]"
                         (setting :model) (setting :voice)
                         (if (plusp (length said)) said "(no transcription came back)")
                         (audio-seconds pcm) (namestring path)
                         (and tasks (mapcar (lambda (task) (if (plusp (length task)) task "(no task text)")) tasks))))))
        (transport-close transport)))))

(defun live-command (args session-id)
  "(/live) shows what is configured and what has been saved; (/live <what to say>) runs one turn."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (format nil "Gemini Live: model ~a, voice ~a. Replies are saved in ~a.~%~a~%~a"
                (setting :model) (setting :voice) (namestring (call-directory))
                (recent-calls)
                "Type /live <what to say> to run one spoken turn.")
        (live-turn (make-live-request :text text)))))

;;; --- the cell ---------------------------------------------------------------------

(nlk:define-contract live-request
  (:doc "One spoken turn with the Gemini Live model: text in, the reply's spoken transcript and a saved 24 kHz mono WAV out. Billed to the operator's Google AI Studio key. A task the voice model delegates through hermes_task is named in the answer and is NOT run here.")
  ("text" :text :required t
          :doc "what to say to the voice model: one question or one thing to say aloud")
  ("audio_path" :string
                 :doc "a 16-bit PCM WAV sent before the text, so a recorded clip is answered too")
  ("delegate" :boolean :default t
              :doc "offer the voice model the three delegate verbs (hermes_task, hermes_tasks, hermes_task_update)"))

(nle:define-cell gemini-live
  (:section ("gemini-live")
    (:guide "a key from aistudio.google.com/apikey (free tier works); voice is one of the five Live voices")
    ("api_key" :secret :doc "the Gemini API key, pasted in")
    ("api_key_env" :string :default "GEMINI_API_KEY" :doc "or the variable holding the key")
    ("model" :string :default +default-model+ :doc "the Live model id")
    ("voice" :choice :options '("Puck" "Charon" "Kore" "Fenrir" "Aoede") :default +default-voice+
             :doc "the voice it answers in, case-sensitive")
    ("instructions" :string :default "" :doc "extra system instruction, appended to the spoken-brevity one")
    ("timeout_seconds" :integer :default 45 :min 5 :doc "how long one turn may take")
    ("directory" :string :default "" :doc "where reply WAVs are saved; empty means this profile's own"))
  (:tool "gemini_live" (nlk:contract 'live-request) 'live-turn)
  (:command "live" 'live-command
            :description "One Gemini Live spoken turn, saved as a WAV"
            :argument-hint "WHAT TO SAY")
  (:help :live "Gemini Live voice: one spoken turn per call, the reply saved as a WAV" +primer+)
  (:start (lambda ()
            (nle:notice (format nil "gemini-live: ~a, voice ~a" (setting :model) (setting :voice))
                        :key "nodecode-gemini-live")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-gemini-live"))))))
