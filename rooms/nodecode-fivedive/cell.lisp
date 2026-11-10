;;;; cell.lisp --- the 5dive app chat, through the box's local spool folder.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of 5dive-ai/5dive-plugins' `fivedive' platform adapter (see
;;;; NOTICE). The spool protocol, the admission rule, the reply word cap and
;;;; the voice-note reply are the original's; the harness seams are
;;;; Nodecode's: the section is the config, a message is NLE:SUBMIT, and an
;;;; answer is the session's own.
;;;;
;;;; On a 5dive box the dashboard bridge owns everything that talks to 5dive.
;;;; It leaves one JSON file per chat message in <spool>/inbox/, and this
;;;; cell is the other end of that spool: every message is handed to the
;;;; agent in the chat's own session, and the finished answer is written to
;;;; <spool>/outbox/<chat>/<id>.json, where the bridge picks it up and sends
;;;; it to the app. Nothing here dials anything: the folder is the wire, and
;;;; it is created for the agent's own user, so whoever can write into it is
;;;; the agent's user.
(defpackage #:nodecode-fivedive
  (:use #:cl)
  (:export #:fivedive-error #:fivedive-error-detail #:start-cell
           #:take-inbox #:write-outbox #:count-reply-words #:reply-too-long
           #:cut-it-prompt #:session-of #:without-fenced-code))
(in-package #:nodecode-fivedive)
(define-condition fivedive-error (error)
  ((detail :initarg :detail :reader fivedive-error-detail))
  (:report (lambda (condition stream) (write-string (fivedive-error-detail condition) stream)))
  (:documentation "The spool could not be read or written."))
(nlk:define-peripheral fivedive :not-running t :lock "fivedive")
(defparameter +cut-tries+ 2
  "How many rewrites one answer gets before it goes as written.")
(defparameter +cut-target+ 60
  "The word count a rewrite is asked for, when the cap is at least this.")
(defparameter +default-chat+ "dashboard"
  "The chat the spool speaks for: the bridge's own outbox folder.")
(defvar *worker* nil
  "The spool worker (NLK:WORKER-START) while the cell runs, or NIL.")
(defvar *owed* '()
  "Chats with a turn still owed an answer: (CHAT-ID . SESSION) entries,
newest first. Guarded by the cell's own lock.")
(defvar *tries* '()
  "Per chat, how many rewrites this answer has had: (CHAT-ID . N).")
(defvar *fallback* '()
  "Per chat, the over-cap answer held in case the rewrite never comes.")
(defvar *voice* '()
  "Chats whose next reply is spoken, one entry per voice note that arrived.")
(defvar *sequence* 0
  "A counter for the command ids this cell mints.")
;;; --- the section, read back ---------------------------------------------------
(defun spool ()
  "The spool directory the section names, or NIL when it names none."
  (let ((dir (ignore-errors (setting :spool-dir))))
    (when (and (stringp dir) (plusp (length (nlk:trimmed dir)))) (nlk:trimmed dir))))
(defun chat-name (chat-id)
  "CHAT-ID as a folder name: letters, digits and dashes."
  (let ((out (make-string-output-stream)))
    (loop for char across (or chat-id +default-chat+)
          when (or (alphanumericp char) (char= char #\-)) do (write-char char out))
    (let ((text (get-output-stream-string out)))
      (if (plusp (length text)) text +default-chat+))))
(defun spool-root ()
  (uiop:ensure-directory-pathname
   (or (spool) (error 'fivedive-error :detail "fivedive: the section names no spool_dir"))))
(defun inbox-dir () (merge-pathnames "inbox/" (spool-root)))
(defun outbox-dir (&optional (chat-id nil))
  (merge-pathnames (format nil "outbox/~a/" (chat-name chat-id)) (spool-root)))
(defun ensure-dir (path)
  "PATH as a directory that exists, created 0700 for the agent's own user."
  (ensure-directories-exist (uiop:ensure-directory-pathname path) :mode #o700))
(defun allowed-p (sender)
  "Whether SENDER may talk to the agent: everyone with allow_all, else the
list. A message naming no sender is admitted too: it can only come from the
bridge's own folder, which is the trust boundary."
  (let ((sender (nlk:trimmed (or sender ""))))
    (or (setting :allow-all)
        (zerop (length sender))
        (member sender (setting :allowed-senders) :test #'equal))))
(defun check-config ()
  "Refuse a section the cell could never run on, naming the member."
  (unless (spool)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "fivedive: no spool folder: set \"spool_dir\" in the channels.fivedive "
                                 "section to the folder the 5dive bridge reads and writes "
                                 "(inbox/ and outbox/<chat>/ inside it)"))))
;;; --- the spool ---------------------------------------------------------------
(defun message-file-p (file)
  "Whether FILE is a complete message: a *.json name the bridge wrote."
  (let ((name (file-namestring file)))
    (and (plusp (length name))
         (not (char= #\. (char name 0)))
         (equal "json" (string-downcase (or (pathname-type file) ""))))))
(defun message-files (dir)
  "The message files in DIR, oldest name first."
  (if (uiop:directory-exists-p dir)
      (sort (remove-if-not #'message-file-p (uiop:directory-files dir))
            #'string< :key #'file-namestring)
      '()))
(defun route-of (message)
  (let ((route (nlk:json-value message :object "route")))
    (and (hash-table-p route) route)))
(defun message-p (message)
  "Whether MESSAGE is one the bridge meant for the agent: a dashboard route,
and either text or an upload."
  (let ((route (route-of message)))
    (and route
         (equal "dashboard" (nlk:json-value route :string "source"))
         (or (let ((text (nlk:json-value message :string "text")))
               (and text (plusp (length (nlk:trimmed text)))))
             (let ((upload (nlk:json-value message :string "image_path")))
               (and upload (plusp (length (nlk:trimmed upload)))))))))
(defun message-text (message)
  (nlk:trimmed (or (nlk:json-value message :string "text") "")))
(defun message-upload (message)
  (let ((upload (nlk:json-value message :string "image_path")))
    (and upload (plusp (length (nlk:trimmed upload))) (nlk:trimmed upload))))
(defun message-sender (message)
  (or (nlk:json-value message :string "sender") (nlk:json-value message :string "user")))
(defun message-chat (message)
  (let ((chat (nlk:json-value (route-of message) :string "chat_id")))
    (if (and chat (plusp (length (nlk:trimmed chat)))) (nlk:trimmed chat) +default-chat+)))
(defun message-voice-p (message)
  (not (null (nlk:json-value (route-of message) :boolean "voice"))))
(defun take-inbox (&optional (root nil))
  "Read and remove every complete message waiting in the inbox, oldest first.

A file is removed before its message is handed on, so a restart never
answers the same message twice. A file that is not a valid message is
removed too: leaving it would make every lap trip on it."
  (let ((messages '())
        (files (if root
                   (message-files (merge-pathnames "inbox/" (uiop:ensure-directory-pathname root)))
                   (message-files (inbox-dir)))))
    (dolist (file files (nreverse messages))
      (let ((raw (handler-case (uiop:read-file-string file :external-format :utf-8)
                   (error () nil))))
        (handler-case (delete-file file) (error () (setf raw nil)))
        (let ((message (and raw (plusp (length (nlk:trimmed raw)))
                            (handler-case (nlk:decode-json raw) (error () nil)))))
          (cond ((not (and (hash-table-p message) (message-p message)))
                 (warn "fivedive: dropped an incomplete inbox file ~a" (file-namestring file)))
                (t (push message messages))))))))
(defun reply-id ()
  (format nil "~d-~8,'0x" (get-universal-time) (random #xffffffff)))
(defun write-outbox (chat-id text &key files voice (root nil))
  "Leave one reply in the chat's outbox for the bridge to send => its id."
  (let* ((dir (ensure-dir (if root
                              (merge-pathnames (format nil "outbox/~a/" (chat-name chat-id))
                                               (uiop:ensure-directory-pathname root))
                              (outbox-dir chat-id))))
         (id (reply-id))
         (tmp (merge-pathnames (format nil ".~a.tmp" id) dir))
         (final (merge-pathnames (format nil "~a.json" id) dir))
         (body (apply #'nlk:make-json-object
                      "source" "dashboard" "chat_id" (chat-name chat-id) "text" text
                      (append (when files (list "files" (coerce files 'vector)))
                              (when voice (list "voice" t))))))
    (handler-case
        (progn
          (with-open-file (out tmp :direction :output :if-exists :supersede
                                :if-does-not-exist :create :external-format :utf-8)
            (write-string (nlk:encode-json-object body) out)
            (terpri out))
          (rename-file tmp final)
          id)
      (error (condition)
        (ignore-errors (delete-file tmp))
        (error 'fivedive-error
               :detail (format nil "fivedive: could not write a reply: ~a" condition))))))
;;; --- the reply word cap -------------------------------------------------------
(defun fence-marker (line)
  "LINE's fence marker in its first three characters, or NIL."
  (let ((line (string-left-trim '(#\Space #\Tab) line)))
    (when (and (>= (length line) 3)
               (or (char= #\` (char line 0)) (char= #\~ (char line 0)))
               (char= (char line 0) (char line 1))
               (char= (char line 0) (char line 2)))
      (subseq line 0 3))))
(defun without-fenced-code (text)
  "TEXT with every fenced code block taken out: the words in one are not
the answer's prose."
  (let ((keep '()) (open nil))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((marker (fence-marker line)))
        (cond ((and marker (null open)) (setf open marker))
              ((and marker open (char= (char marker 0) (char open 0))
                    (>= (length marker) (length open))
                    (string= (string-trim '(#\Space #\Tab #\Return) line) marker))
               (setf open nil))
              ((null open) (push line keep)))))
    (format nil "~{~a~^~%~}" (nreverse keep))))
(defun count-reply-words (text)
  "How many words TEXT answers with, as the original counts them: outside
fenced code, every token carrying a letter or a digit."
  (let ((count 0))
    (dolist (token (uiop:split-string (without-fenced-code text)
                                       :separator '(#\Space #\Tab #\Newline #\Return)))
      (when (some #'alphanumericp token) (incf count)))
    count))
(defun reply-too-long (text &optional (cap nil))
  "The refusal TEXT earns when it is over CAP words, or NIL when it is not:
the original's REPLY_TOO_LONG."
  (let ((cap (or cap (ignore-errors (setting :reply-max-words)) 70)))
    (when (and (stringp text) (plusp cap))
      (let ((count (count-reply-words text)))
        (when (> count cap)
          (format nil "too long: ~d words; cut to ~d or attach a file and name it in one line. ~
Nothing was sent."
                  count (min +cut-target+ cap)))))))
(defun cut-it-prompt (refusal)
  "The turn a held answer starts: the refusal, and what to do about it."
  (format nil "[5dive chat: your last reply was not sent. ~a Write your answer again, ~
shorter. Only the new version reaches the chat.]" refusal))
;;; --- the turn: one session per chat -------------------------------------------
(defun session-of (chat-id)
  "The session a chat's messages run in: one per chat, so a conversation
there is a conversation in the agent's history."
  (format nil "fivedive-~a" (chat-name chat-id)))
(defun submit-prompt (session prompt command-id source-id)
  "PROMPT into SESSION through the one in-process ingress; tests stub this."
  (nle:submit session prompt :command-id command-id :source "fivedive" :source-id source-id))
(defun ensure-session (session)
  (when (nlk:store-open-p)
    (unless (nlk:session-exists-p session)
      (nlk:standby-session :id session :cwd (uiop:getcwd)))))
(defun prompt-for (chat-id text &key upload voice sender)
  "TEXT as the agent reads it: where it came from, and how to answer."
  (format nil "[5dive app chat, chat ~a~@[, from ~a~]]~%~@[~a~%~]~a~@[~%~%A file arrived with this ~
message: ~a - read it with your own tools if it matters.~]"
          chat-id sender
          (and voice (concatenate 'string
                                  "[voice note] this message was spoken: answer in one to three short "
                                  "sentences with no lists, links or code, because the reply is spoken back."))
          text upload))
(defun ask-agent (message)
  "Hand MESSAGE to the agent in its chat's session, and remember to answer."
  (let* ((chat-id (message-chat message))
         (session (session-of chat-id))
         (text (message-text message))
         (upload (message-upload message)))
    (handler-case
        (progn
          (ensure-session session)
          (submit-prompt session
                         (prompt-for chat-id text :upload upload :voice (message-voice-p message)
                                     :sender (message-sender message))
                         (format nil "fivedive:~a:~a" chat-id (incf *sequence*))
                         chat-id)
          (with-fivedive-lock
            (setf *tries* (remove chat-id *tries* :key #'car :test #'equal)
                  *fallback* (remove chat-id *fallback* :key #'car :test #'equal))
            (when (message-voice-p message) (push chat-id *voice*))
            (unless (assoc chat-id *owed* :test #'equal)
              (push (cons chat-id session) *owed*))))
      (error (condition)
        (warn "fivedive: could not hand a message on: ~a" condition)))))
;;; --- the answer ---------------------------------------------------------------
(defun tries-of (chat-id)
  (or (cdr (assoc chat-id *tries* :test #'equal)) 0))
(defun fallback-of (chat-id)
  (cdr (assoc chat-id *fallback* :test #'equal)))
(defun forget (chat-id)
  (setf *owed* (remove chat-id *owed* :key #'car :test #'equal)
        *tries* (remove chat-id *tries* :key #'car :test #'equal)
        *fallback* (remove chat-id *fallback* :key #'car :test #'equal)))
(defun spoken-p (chat-id)
  "Whether the next reply in CHAT-ID is spoken, and stop holding the note."
  (when (member chat-id *voice* :test #'equal)
    (setf *voice* (remove chat-id *voice* :test #'equal))
    t))
(defun deliver (chat-id text &key files)
  "Write one finished answer to the chat's outbox, spoken when a voice note
asked for it => the reply's id."
  (write-outbox chat-id text :files files :voice (spoken-p chat-id)))
(defun hold-for-cut-p (chat-id answer)
  "True when ANSWER is over the cap and this answer still has a rewrite left:
the answer is held and the agent is asked for a shorter one."
  (let* ((cap (ignore-errors (setting :reply-max-words)))
         (refusal (and cap (plusp cap) (< (tries-of chat-id) +cut-tries+)
                       (reply-too-long answer cap))))
    (when refusal
      (setf *tries* (acons chat-id (1+ (tries-of chat-id)) *tries*)
            *fallback* (acons chat-id answer (remove chat-id *fallback* :key #'car :test #'equal)))
      t)))
(defun ask-for-cut (chat-id session)
  "Ask the session for a shorter answer to the one just held; a failure sends
the held answer as written."
  (handler-case
      (submit-prompt session (cut-it-prompt (reply-too-long (fallback-of chat-id)))
                     (format nil "fivedive:~a:cut:~a" chat-id (incf *sequence*)) chat-id)
    (error (condition)
      (warn "fivedive: could not ask for a rewrite: ~a" condition)
      (let ((held (fallback-of chat-id)))
        (when held
          (deliver chat-id held)
          (forget chat-id))))))
(defun deliver-pending ()
  "Write each finished answer back to the chat that asked for it, holding an
over-cap one for a rewrite first."
  (with-fivedive-lock
    (dolist (entry (copy-list *owed*))
      (let ((chat-id (car entry)) (session (cdr entry)))
        (unless (nlk:active-turn-p session)
          (let ((answer (nlk:trimmed (or (nlk:turn-answer session) ""))))
            (cond ((zerop (length answer)) (forget chat-id))
                  ((hold-for-cut-p chat-id answer) (ask-for-cut chat-id session))
                  (t (handler-case (deliver chat-id answer)
                       (error (condition)
                         (warn "fivedive: an answer did not reach ~a: ~a" chat-id condition)))
                     (forget chat-id)))))))))
;;; --- the lap -----------------------------------------------------------------
(defun handle-message (message)
  "One message from the spool: admit it, then hand it to the chat's session."
  (when (allowed-p (message-sender message))
    (ask-agent message)))
(defun lap ()
  "The worker's step: deliver what is due, then take what came in. An error
is a warn, not a stop - the next lap tries again."
  (handler-case (deliver-pending) (error (condition) (warn "fivedive: delivery: ~a" condition)))
  (handler-case (dolist (message (take-inbox)) (handle-message message))
    (error (condition) (warn "fivedive: spool: ~a" condition)))
  nil)
;;; --- the manual, /fivedive and the tool -----------------------------------------
(defparameter +primer+
  "The 5dive app chat is the web and mobile chat of a 5dive box, and it reaches this agent
through a folder: the box's bridge leaves one JSON file per message in <spool>/inbox/ and sends
whatever appears in <spool>/outbox/<chat>/ back to the app. This cell reads the inbox every
section.poll_seconds, hands each message to the agent in the session fivedive-<chat>, and writes
the finished answer to the outbox. Two more things happen there: an answer over
section.reply_max_words words is held and the agent is asked to shorten it (up to two rewrites,
then it goes as written), and the first answer after a voice note is marked voice, so the app
speaks it. The folder is the trust boundary: no token, no listener, no network.
/fivedive shows the spool and what is owed an answer; /fivedive TEXT writes TEXT to the chat now.
The fivedive_send tool does the same from a turn, with files."
  "What (help :fivedive) answers while the cell runs.")
(defun status-line ()
  (format nil "5dive app chat: spool ~a, chat ~a. Allowed: ~a. Cap: ~a. ~d answer~:p owed.~%~
The section is channels.fivedive: spool_dir is the bridge's folder, allowed_senders names who may ~
talk (the bridge sends as 5dive-app), or allow_all lets every sender in."
          (or (spool) "(none)") (chat-name nil)
          (if (setting :allow-all) "anyone"
              (format nil "~{~a~^, ~}" (setting :allowed-senders)))
          (let ((cap (setting :reply-max-words)))
            (if (plusp cap) (format nil "~d words" cap) "off"))
          (length *owed*)))
(defun run-command (args session-id)
  "One /fivedive invocation: no argument answers the spool's state; TEXT goes
to the chat now."
  (declare (ignore session-id))
  (unless *worker*
    (fail "fivedive: the cell is not running: turn it on in /cells, or check its channels.fivedive section"))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (status-line)
        (progn (deliver +default-chat+ text)
               "Sent to the 5dive app chat."))))
(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:make-json-object "type" type
                                                                          "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))
(defparameter *send-schema*
  (tool-schema
   "Send a reply to the 5dive app chat through its spool, without waiting for a turn's answer. Use it to report back to the owner's chat, or to hand files over: each path is named in the reply and the bridge attaches it."
   '(("text" "string" "What to send." t)
     ("files" "array" "Absolute paths of the files to send with it." nil)
     ("chat_id" "string" "The chat to answer in; the section's chat when omitted." nil)))
  "The fivedive_send tool: the original's standalone sender, as a verb.")
(defun reply-files (arguments)
  "The paths an fivedive_send call names, as a list."
  (let ((value (nlk:json-value arguments :any "files")))
    (remove-if-not (lambda (file) (and (stringp file) (plusp (length (nlk:trimmed file)))))
                   (cond ((vectorp value) (coerce value 'list))
                         ((listp value) value)
                         ((stringp value) (list value))
                         (t '())))))
(defun tool-send (arguments)
  "fivedive_send: one reply to one chat, with files."
  (let* ((text (nlk:trimmed (or (nlk:json-value arguments :string "text") "")))
         (chat (nlk:json-value arguments :string "chat_id"))
         (chat-id (if (and chat (plusp (length (nlk:trimmed chat)))) (nlk:trimmed chat) +default-chat+))
         (files (reply-files arguments)))
    (unless (plusp (length text)) (fail "fivedive_send needs text"))
    (when (null *worker*) (fail "fivedive_send: the cell is not running: turn it on in /cells"))
    (handler-case
        (let ((id (deliver chat-id (subseq text 0 (min (length text) (setting :max-length)))
                           :files files)))
          (format nil "Sent to the 5dive app chat (~a)." id))
      (fivedive-error (condition) (fivedive-error-detail condition)))))
;;; --- the cell -------------------------------------------------------------------
(nle:define-cell fivedive
  (:section ("channels" "fivedive")
    (:guide "spool_dir is the folder the 5dive bridge reads and writes on a 5dive box; next step: /fivedive shows what the cell reads")
    ("spool_dir" :path :required t
     :doc "the 5dive spool folder (inbox/ and outbox/<chat>/ inside it); the bridge creates it")
    ("allowed_senders" :list :default '("5dive-app")
     :doc "the senders allowed to talk; the 5dive bridge sends as 5dive-app")
    ("allow_all" :boolean :default nil
     :doc "let every sender in the spool through (they run with your machine's authority)")
    ("reply_max_words" :integer :default 70 :min 0
     :doc "word cap on one chat answer; a longer one goes back to the agent to shorten (0 turns it off)")
    ("poll_seconds" :integer :default 1 :min 1
     :doc "how often the spool is read")
    ("max_length" :integer :default 16000 :min 100
     :doc "the chat's per-message character limit"))
  (:help :fivedive "fivedive: the 5dive app chat through a local spool - /fivedive shows the spool, /fivedive TEXT sends to the chat" +primer+)
  (:command "fivedive" 'run-command
            :description "Show the 5dive spool, or send a message to the app chat"
            :argument-hint "TEXT")
  (:tool "fivedive_send" *send-schema* 'tool-send)
  (:start (lambda ()
            (check-config)
            (ensure-dir (inbox-dir))
            (ensure-dir (outbox-dir))
            (setf *owed* '() *tries* '() *fallback* '() *voice* '() *sequence* 0)
            (setf *worker* (nlk:worker-start "fivedive-spool" #'lap
                                              :wake (lambda () (setting :poll-seconds))))
            (nle:on-stop (lambda ()
                           (when *worker* (nlk:worker-stop *worker*))
                           (setf *worker* nil *owed* '() *tries* '() *fallback* '() *voice* '()))))))
