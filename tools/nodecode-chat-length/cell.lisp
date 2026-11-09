;;;; cell.lisp --- a notice when a chat nears its context window.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original Hermes plugin hooks POST_API_REQUEST to keep the numbers the
;;;; provider billed for the last call and PRE_LLM_CALL to put a one-turn
;;;; instruction into the next user message. Nodecode has both facts already:
;;;; every round's billed counts are the turn.usage facts of the session, and
;;;; the :ROUND point runs before each request is built, where
;;;; NLK:RECORD-TURN-CONTEXT-PROMPT puts a line into that very request. So the
;;;; fill is read from the log, not from a store of the cell's own; the window
;;;; is the model's, kept per session under the cell's own state key because a
;;;; new turn opens without a measurement.
;;;;
;;;; Two thresholds, each announced once per chat, re-armed when the fill
;;;; drops well below the first (eviction, a cleared chat, a smaller model).
;;;; Everything here fails silently: no fill, no notice, never a broken turn.

(defpackage #:nodecode-chat-length
  (:use #:cl))

(in-package #:nodecode-chat-length)

(nlk:define-peripheral chat-length :not-running t)

;;; --- what the module keeps --------------------------------------------------

(defparameter *rearm-gap-percent* 15
  "How far below the first threshold the fill must fall before both notices
arm again: past an eviction the numbers drop at once, a small wobble does not.")

(defparameter *window-key* "chat-length.window"
  "The session-state key holding the context window last measured, so the
first round of a later turn can judge its fill before this turn measured one.")

(defparameter *level-key* "chat-length.level"
  "The session-state key holding the level already announced: 0, 1 or 2.")

(defparameter *headline*
  '((:en "**Heads-up: this chat is getting long** (about ~d % of the context window). Replies get slower and less precise from here on. Best to open a new chat and paste this opener:"
         "**Urgent: this chat is almost full** (about ~d % of the context window). Older parts will be dropped from the window soon and details get lost. Please open a new chat now and paste this opener:")
    (:de "**Hinweis: Dieser Chat wird lang** (ca. ~d % des Kontextfensters). Antworten werden ab hier langsamer und ungenauer. Am besten einen neuen Chat oeffnen und diesen Startsatz einfuegen:"
         "**Dringend: Dieser Chat ist fast voll** (ca. ~d % des Kontextfensters). Bald fallen aeltere Teile aus dem Fenster und Details gehen verloren. Bitte jetzt einen neuen Chat oeffnen und diesen Startsatz einfuegen:"))
  "The notice's opening line, first for the first level, second for the urgent
one: EN, and DE for the fixed German wording.")

(defparameter *opener*
  '((:en "Continue our chat ~a. The earlier chat is still saved: read it back with `nodecode resume ~a` if you need what we said there.~%Status: <2 to 3 bullet points>~%Open: <open items, short>")
    (:de "Knuepfe an unseren Chat ~a an. Der fruehere Chat ist noch da: lies ihn mit `nodecode resume ~a` nach, wenn du brauchst, was wir dort gesagt haben.~%Stand: <2 bis 3 Stichpunkte>~%Offen: <offene Punkte, kurz>"))
  "The opener's body: the chat as it is named, then the session id.")

(defparameter *manual*
  "chat-length: how full this chat's context window is.

The cell reads the counts the provider billed for the session's newest round
(the turn.usage facts) and the model's context window, and puts a short
notice into the reply in which the fill first passes a threshold: 70 % by
default, and an urgent one at 90 %. The notice carries an opener for a fresh
chat that names the chat and its session id, and the chat is still saved:
`nodecode resume SESSION-ID` reads it back.

Each level is announced once per chat. When the fill drops well below the
first threshold (an eviction, a cleared chat, a smaller model) both arm again.

The /chat-length command and the chat_length tool answer the same numbers on
demand, and the opener with them once a threshold is passed.

(chat-length:chat-fill \"s-ABC123\")            => (values TOKENS WINDOW RATIO) or NIL
(chat-length:notice-text 1 0.72 \"s-ABC123\")    => the notice text for a level
(chat-length:report \"s-ABC123\")                => one line, the command's answer"
  "The (help :chat-length) text.")

;;; --- settings ---------------------------------------------------------------

(defun urgent-percent ()
  "The urgent threshold, never below the first one."
  (max (setting :notice-percent) (setting :urgent-percent)))

(defun language-setting ()
  (let ((language (string-downcase (string (setting :language)))))
    (if (member language '("en" "de") :test #'equal) language "auto")))

;;; --- the log, the window, the session state ----------------------------------

(defun state-number (session key)
  "The number stored under KEY for SESSION, or NIL."
  (let ((text (ignore-errors (nlk:session-state-get session key))))
    (when (and (stringp text) (plusp (length text)))
      (ignore-errors (parse-integer text :junk-allowed t)))))

(defun note-number (session key value)
  "Keep VALUE under KEY for SESSION, silently: a store that refuses is no
reason to fail a turn."
  (ignore-errors (nlk:session-state-put session key (princ-to-string value))))

(defun prompt-tokens (usage)
  "The prompt USAGE placed in the window: input, cache reads and cache
writes, the rule the shell's own meter uses."
  (when usage
    (let ((counts (remove nil (list (nlk:json-value usage :integer "input-tokens")
                                    (nlk:json-value usage :integer "cached-input-tokens")
                                    (nlk:json-value usage :integer "cache-write-tokens")))))
      (when counts (reduce #'+ counts)))))

(defun newest-usage (session)
  "The payload of the newest turn.usage fact of SESSION that carried counts --
the numbers the provider billed for that round -- or NIL before one reported.
A round whose lane reported no usage records a fact without counts, and that
one measures nothing."
  (ignore-errors
    (loop for usage in (nlk:events :session-id session :kind "turn.usage" :order :newest
                                    :limit 8 :as :payloads)
          when (prompt-tokens usage) return usage)))

(defun output-tokens (usage)
  (and usage (or (nlk:json-value usage :integer "output-tokens") 0)))

(defun context-window (session usage)
  "The window SESSION's chat fills: this turn's measurement once one round
reported, else the catalog's window for the model of the newest round, else
the one a former round measured."
  (or (getf (nle:turn) :context-window)
      (let ((model (and usage (nlk:json-value usage :text "model"))))
        (and model (plusp (length model))
             (ignore-errors (nle:model-context-tokens
                             model (nlk:json-value usage :text "provider")))))
      (let ((kept (state-number session *window-key*)))
        (and kept (plusp kept) kept))))

(defun chat-fill (session)
  "=> (values TOKENS WINDOW RATIO): the tokens the session's newest round
placed in the window plus its reply, the window, and their share of it; NIL
tokens when no round reported yet."
  ;; The reply of the newest round is in the history of the next one, which is
  ;; why the original adds its output count too.
  (let* ((usage (newest-usage session))
         (prompt (prompt-tokens usage))
         (window (context-window session usage)))
    (when window (note-number session *window-key* window))
    (when (and prompt window (plusp window))
      (values (+ prompt (output-tokens usage)) window
              (min 1.0 (/ (float (+ prompt (output-tokens usage))) window))))))

(defun percent (ratio)
  "RATIO as the whole percent the notice says: 0 to 99."
  (max 0 (min 99 (round (* 100 ratio)))))

(defun chat-title (session)
  "The name SESSION wears, or an empty string."
  (or (ignore-errors
        (getf (find session (nlk:list-sessions) :key (lambda (entry) (getf entry :id))
                    :test #'equal)
              :title))
      ""))

(defun tokens-text (tokens)
  (if (fboundp 'nle:format-token-count)
      (nle:format-token-count tokens)
      (princ-to-string tokens)))

;;; --- the notice --------------------------------------------------------------

(defun notice-text (level ratio session)
  "The notice for LEVEL (1 or 2) at RATIO of the window of SESSION, as the
one-turn instruction that makes the reply carry it."
  (let* ((language (language-setting))
         (wording (if (equal language "auto") :en language))
         (headlines (rest (assoc wording *headline*)))
         (title (chat-title session))
         (chat (if (plusp (length title))
                   (if (equal wording :de)
                       (format nil "\"~a\" (Session ~a)" title session)
                       (format nil "\"~a\" (session ~a)" title session))
                   (format nil "session ~a" session)))
         (opener (format nil (second (assoc wording *opener*)) chat session)))
    (format nil "[chat-length, instruction for this one reply only]~%~
First answer the message above completely and as usual. Then append exactly one notice block at the very end of your reply, in this form:~%~%---~%~a~%~%```text~%~a~%```~%~%~
Rules for the block: replace the placeholders in angle brackets with the real state of this chat. At most 3 bullet points under the status line, open items short, at most 80 words in the opener. Copy the chat's name and the session id exactly, and keep `nodecode resume` unchanged. ~a Do not repeat the block in later replies on your own."
            (format nil (nth (if (= level 2) 1 0) headlines) (percent ratio))
            opener
            (if (equal language "auto")
                "Write the whole block in the language of this conversation (translate the template if needed)."
                "Keep the wording of the template; fill in the placeholders in the same language."))))

(defun due-level (ratio &aux (first (/ (setting :notice-percent) 100.0))
                              (urgent (/ (urgent-percent) 100.0)))
  "The level RATIO earns, 1 or 2, or NIL below the first threshold."
  (cond ((>= ratio urgent) 2)
        ((>= ratio first) 1)))

;;; --- the :ROUND point -------------------------------------------------------

(defun announce (turn session level ratio)
  "Put LEVEL's notice into the request about to be built, once."
  (let ((text (notice-text level ratio session)))
    (nlk:record-turn-context-prompt turn :text text)
    (nle:notice (format nil "chat-length: ~d % of the context window~@[, urgent~]"
                        (percent ratio) (and (= level 2) t))
                :key "nodecode-chat-length")
    text))

(defun before-round (op)
  "The :ROUND point: read the fill of OP's session before its request is
built, and when a threshold is passed for the first time in this chat, put the
notice into that request. Silent when nothing is measured yet."
  (let ((session (getf op :session-id))
        (turn (getf op :turn)))
    (when (and session turn (ignore-errors (nlk:store-open-p)))
      (let ((level (or (state-number session *level-key*) 0)))
        (multiple-value-bind (tokens window ratio) (chat-fill session)
          (declare (ignore tokens window))
          (when ratio
            (let ((due (due-level ratio)))
              (cond ((and due (> due level))
                     (note-number session *level-key* due)
                     (announce turn session due ratio))
                    ((and (plusp level)
                          (< ratio (/ (- (setting :notice-percent) *rearm-gap-percent*) 100.0)))
                     (note-number session *level-key* 0))))))))))

;;; --- the report the command and the tool answer ------------------------------

(defun report (session)
  "One line: how full SESSION's chat is, and the opener once a threshold is
passed."
  (multiple-value-bind (tokens window ratio) (chat-fill session)
    (if (null ratio)
        "chat-length: this chat has no measurement yet; the first provider round measures it."
        (let* ((level (due-level ratio))
               (line (format nil "chat-length: ~a of ~a tokens, ~d % of the context window (first notice at ~d %, urgent at ~d %)."
                              (tokens-text tokens) (tokens-text window)
                              (percent ratio) (setting :notice-percent) (urgent-percent))))
          (cond ((eq level 2)
                 (format nil "~a~%~%~a" line
                         (format nil (second (assoc (if (equal (language-setting) "de") :de :en)
                                                    *opener*))
                                 (if (plusp (length (chat-title session)))
                                     (format nil "\"~a\" (session ~a)" (chat-title session) session)
                                     (format nil "session ~a" session))
                                 session)))
                (t line))))))

(defparameter *tool-schema*
  (nlk:json-object "type" "object" "properties" (nlk:make-json-object) "required" #())
  "The chat_length tool's parameter object: the numbers of the chat this turn
answers are read, and nothing is passed.")

(defun chat-length-tool (arguments)
  "The chat_length tool: the report for the session this turn answers, or for
the session ARGUMENTS names."
  (declare (ignore arguments))
  (let ((session (or (getf (nle:turn) :session-id) nlk:*scribe-session-id*)))
    (if session
        (report session)
        "chat-length: no session to measure.")))

(defun run-slash (args session)
  "/chat-length: the report for SESSION."
  (declare (ignore args))
  (if session
      (report session)
      "chat-length: no session to measure."))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell chat-length
  (:section ("chat-length")
    (:guide "nothing is required; the two thresholds are percentages of the context window; language is auto, en or de")
    ("notice_percent" :integer :default 70 :min 1
     :doc "Fill level, in percent of the context window, at which the first notice appears once per chat.")
    ("urgent_percent" :integer :default 90 :min 1
     :doc "Fill level, in percent, at which the urgent notice appears once per chat.")
    ("language" :choice :options '("auto" "en" "de") :default "auto"
     :doc "auto = the conversation's language (English template, translated by the model); en or de = fixed wording."))
  (:tool "chat_length"
         (list :description "How full this chat's context window is, in tokens and percent, and the opener for a fresh chat once it is getting long."
               :parameters *tool-schema*)
         #'chat-length-tool)
  (:command "chat-length" #'run-slash
            :description "How full this chat is, and the opener for a fresh one")
  (:help :chat-length
         "chat-length: how full this chat's context window is, with a notice at 70 % and 90 %"
         *manual*)
  (:hook :round (nle:observer #'before-round "chat-length"))
  (:start (lambda ()
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-chat-length"))))))
