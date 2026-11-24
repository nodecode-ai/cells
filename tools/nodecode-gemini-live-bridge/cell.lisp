;;;; cell.lisp --- one Live conversation, its catalog, and /gemini-live-bridge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original gave a user one long voice session in a desktop minibar:
;;;; ask, hear the answer, keep the context across drops, watch the token
;;;; meter. A terminal has no microphone, so what carries over is the
;;;; session: GEMINI_LIVE_BRIDGE_TALK sends one turn into a Live session that stays
;;;; open between tool calls, resumes from Google's session handle after a
;;;; drop (dashboard/plugin_api.py:176-190), and answers the reply with the
;;;; session's token spend -- the minibar's transcript and meter as text.

(in-package #:nodecode-gemini-live-bridge)

(defstruct (conversation (:constructor make-conversation (&key model link handle usage turns)))
  "One open Live session: its model, socket, resumption handle and meter."
  model link handle usage (turns 0))

(defparameter +thinking-levels+ (list "low" "high")
  "The thinking levels the extended-thinking models accept.")

(defvar *conversation* nil "The conversation the tools keep open, or NIL.")

(defun setup-frame (model &key thinking-level handle)
  "The setup message the original sends (dashboard/plugin_api.py:206-225),
answering text instead of audio: a terminal plays no PCM."
  (let ((generation (nlk:json-object "responseModalities" (vector "TEXT"))))
    (when (search "extended-thinking" model)
      (setf (gethash "thinkingConfig" generation)
            (nlk:json-object "thinkingLevel" (or thinking-level "low"))))
    (nlk:json-object
     "setup"
     (nlk:json-object
      "model" model
      "generationConfig" generation
      "inputAudioTranscription" (nlk:json-object)
      "outputAudioTranscription" (nlk:json-object)
      "sessionResumption" (if handle
                               (nlk:json-object "handle" handle)
                               (nlk:json-object))))))

(defun error-text (error)
  "ERROR, a Live API error member, as one line."
  (or (and (hash-table-p error) (field-text error "message"))
      (princ-to-string error)))

(defun open-conversation (model &key thinking-level handle)
  "Mint a token, dial MODEL and wait for setupComplete; a LINK."
  (let* ((token (mint-token))
         (link (funcall *dial* (format nil "~a?access_token=~a" +live-ws-url+ token)))
         (open t))
    (unwind-protect
         (progn
           (link-send link (nlk:encode-json-object
                           (setup-frame model :thinking-level thinking-level :handle handle)))
           (prog1 (loop for frame = (link-recv link :timeout 15)
                        do (cond
                             ((eq frame :timeout) (fail "~a did not finish setup in 15s" model))
                             ((eq frame :closed) (fail "the socket closed during setup"))
                             (t (let ((message (ignore-errors (nlk:decode-json frame))))
                                  (cond ((field message "setupComplete") (return link))
                                        ((field message "error")
                                         (fail "~a: ~a" model
                                               (error-text (field message "error")))))))))
             (setf open nil)))
      (when open (link-close link)))))

(defun read-turn (link)
  "Read frames until turnComplete, answering (VALUES TEXT USAGE HANDLE)."
  (let ((text (make-string-output-stream)) (usage nil) (handle nil) (pieces 0))
    (loop
      (let ((frame (link-recv link :timeout 90)))
        (cond
          ((eq frame :timeout) (fail "no answer in 90s"))
          ((eq frame :closed) (fail "the socket closed mid-turn"))
          (t
           (let* ((message (ignore-errors (nlk:decode-json frame)))
                  (content (field message "serverContent"))
                  (resumption (field message "sessionResumptionUpdate")))
             (when (field message "usageMetadata") (setf usage (field message "usageMetadata")))
             (when (and (field resumption "newHandle") (field resumption "resumable"))
               (setf handle (field-text resumption "newHandle")))
             (when (field message "error")
               (fail "upstream: ~a" (error-text (field message "error"))))
             (dolist (part (coerce (or (field (field content "modelTurn") "parts") #()) 'list))
               (let ((piece (field-text part "text")))
                 (when piece (incf pieces) (write-string piece text))))
             (when (field content "turnComplete")
               (return (values (get-output-stream-string text) usage handle pieces))))))))))

(defun token-count (usage)
  "USAGE's total token count, or NIL when it is absent."
  (let ((value (and usage (field usage "totalTokenCount"))))
    (and (numberp value) value)))

(defun meter-line (conversation)
  "The minibar's token counter, one line."
  (format nil "[~a; ~d turn~:p]"
          (if (token-count (conversation-usage conversation))
              (format nil "~d tokens in this session" (token-count (conversation-usage conversation)))
              "no token count yet")
          (conversation-turns conversation)))

(defun close-conversation ()
  "Close the open session, if any."
  (when *conversation*
    (link-close (conversation-link *conversation*))
    (setf *conversation* nil))
  nil)

(defun say (text &key model thinking-level (fresh nil))
  "Send TEXT into the session (opening or resuming one) and answer its reply."
  (let* ((text (string-trim " \t\n" text))
         (name (if *conversation*
                   (conversation-model *conversation*)
                   (chosen-model model))))
    (when (and *conversation*
               (or fresh (and model (not (equal name (conversation-model *conversation*))))))
      (close-conversation))
    (unless *conversation*
      (let* ((handle (and (not fresh) (conversation-handle-of name)))
             (resumed (and handle
                           (ignore-errors
                            (make-conversation
                             :model name
                             :link (open-conversation name :thinking-level thinking-level
                                                       :handle handle)
                             :handle handle)))))
        (setf *conversation*
              (or resumed
                  (make-conversation :model name
                                     :link (open-conversation name
                                                               :thinking-level thinking-level))))))
    (let ((conversation *conversation*))
      (link-send (conversation-link conversation)
                 (nlk:encode-json-object
                  (nlk:json-object
                   "clientContent"
                   (nlk:json-object
                    "turns" (vector (nlk:json-object "role" "user"
                                                      "parts" (vector (nlk:json-object "text" text))))
                    "turnComplete" t))))
      (multiple-value-bind (reply usage handle pieces)
          (read-turn (conversation-link conversation))
        (setf (conversation-usage conversation) (or usage (conversation-usage conversation))
              (conversation-handle conversation) (or handle (conversation-handle conversation)))
        (incf (conversation-turns conversation))
        (values (if (plusp pieces) reply "(no text in the reply)")
                conversation)))))

(defvar *last-handle* nil "The resumption handle of the session last closed, for the next dial.")
(defvar *last-handle-model* nil "The model *LAST-HANDLE* belongs to.")

(defun conversation-handle-of (model)
  "The stored handle for MODEL, as the original's .resume.json kept it."
  (and (equal model *last-handle-model*) *last-handle*))

(defun remember-handle ()
  "Keep the closed session's handle for the next dial of the same model."
  (when *conversation*
    (setf *last-handle* (conversation-handle *conversation*)
          *last-handle-model* (conversation-model *conversation*))))

;;; --- the tools ---------------------------------------------------------------------

(defun models-tool (arguments)
  "List the Live-capable models on the key, in the original's fixed order."
  (declare (ignore arguments))
  (handler-case
      (format nil "~{~a~%~}" (live-models))
    (gemini-live-bridge-error (c) (format nil "ERROR: ~a" c))))

(defun talk-tool (turn)
  "One turn in the Live session, answered as text with the session meter."
  (handler-case
      (multiple-value-bind (reply conversation)
          (say (gemini-live-bridge-turn-prompt turn)
               :model (gemini-live-bridge-turn-model turn)
               :thinking-level (gemini-live-bridge-turn-thinking-level turn)
               :fresh (gemini-live-bridge-turn-new-session turn))
        (format nil "~a~%[gemini-live-bridge ~a; ~a]" reply (conversation-model conversation)
                (meter-line conversation)))
    (gemini-live-bridge-error (c) (format nil "ERROR: ~a" c))))

(defun run-slash (arguments session-id)
  "The /gemini-live-bridge command: status, models, say TEXT, stop."
  (declare (ignore session-id))
  (let ((arguments (string-trim " \t" (or arguments ""))))
    (handler-case
        (cond
          ((zerop (length arguments))
           (if *conversation*
               (format nil "gemini-live-bridge: ~a live, ~a" (conversation-model *conversation*)
                       (meter-line *conversation*))
               (format nil "gemini-live-bridge: idle; ~a" (format nil "the next talk dials ~a" (or (ignore-errors (chosen-model)) +default-model+)))))
          ((equal arguments "models") (format nil "~{~a~%~}" (live-models)))
          ((equal arguments "stop")
           (remember-handle)
           (close-conversation)
           "gemini-live-bridge: session closed")
          ((uiop:string-prefix-p "say " arguments)
           (multiple-value-bind (reply conversation) (say (subseq arguments 4))
             (format nil "~a~%[~a]" reply (meter-line conversation))))
          (t "gemini-live-bridge: status | models | say <text> | stop"))
      (gemini-live-bridge-error (c) (format nil "gemini-live-bridge: ~a" c)))))

(defun stop-conversation ()
  "Take the session down with the cell."
  (remember-handle)
  (close-conversation))

;;; --- the declaration ----------------------------------------------------------------

(nlk:define-contract gemini-live-bridge-turn
  (:doc "One turn in the open Gemini Live session: the text to say, and,
optionally, the model to dial, the thinking level and a fresh session.")
  ("prompt" :text :required t :doc "what to say to Gemini Live")
  ("model" :string :doc "a Live model name, models/...; empty keeps the open session's")
  ("thinking_level" :text :options +thinking-levels+
                     :doc "reasoning depth, for *-extended-thinking models only")
  ("new_session" :boolean :doc "close the open session and dial a fresh one"))

(defparameter *models-schema*
  (list :description "List every Live-capable Gemini model on the key, in the catalog's fixed order: 3.8-live first, then extended-thinking, then native-audio."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "GEMINI_LIVE_BRIDGE_MODELS takes no arguments.")

(nle:define-cell gemini-live-bridge
  (:section ("gemini-live-bridge")
    (:guide "api_key_env names the environment variable holding a Google AI Studio key with Live API access -- get one at aistudio.google.com/apikey; model names one Live model (empty = the catalog's first, the original's order); thinking_level only reaches *-extended-thinking")
    ("api_key_env" :env :required t
                    :doc "environment variable holding the Google AI Studio key")
    ("model" :string :doc "a Live model, models/...; empty picks the catalog's first candidate")
    ("thinking_level" :choice :options +thinking-levels+ :default "low"
                       :doc "reasoning depth, for *-extended-thinking models only"))
  (:tool "gemini_live_bridge_talk" (nlk:contract 'gemini-live-bridge-turn) 'talk-tool)
  (:tool "gemini_live_bridge_models" *models-schema* 'models-tool)
  (:command "gemini-live-bridge" 'run-slash
            :description "The Live session: status, models, say TEXT, stop"
            :argument-hint "status | models | say TEXT | stop")
  (:help :gemini-live-bridge
         "gemini-live-bridge: one Gemini Live session across turns - gemini_live_bridge_talk, gemini_live_bridge_models, /gemini-live-bridge"
         (format nil "~a~%~a~%~a"
                 "gemini-live-bridge talks to Gemini Live: GEMINI_LIVE_BRIDGE_TALK sends one turn into a session that stays open between calls and answers the reply plus the session's token count; GEMINI_LIVE_BRIDGE_MODELS lists the Live-capable models on your key in the catalog's fixed order; /gemini-live-bridge shows the session, lists models, says one turn (say TEXT) or closes it (stop)."
                 "A session that drops is redialled once with Google's session-resumption handle, and the last handle is kept in memory only, so a new session starts clean."
                 "The key is read from the section's api_key_env and goes to generativelanguage.googleapis.com only."))
  (:start (lambda () (setf *conversation* nil) (nle:on-stop #'stop-conversation))))
