;;;; cell.lisp --- how long a session is, and the handoff that outlives it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A session passes its size limit when the newest round filled enough of
;;;; the model's context, when its input tokens reach the limit, or when it
;;;; has taken the rounds the operator allows. At that request's round the
;;;; session is told once -- one line recorded into the turn's own prompt
;;;; (NLK:RECORD-TURN-CONTEXT-PROMPT) -- and the operator gets one standing
;;;; notice with the path the handoff goes to. In ADVISE mode the line is a
;;;; nudge to finish the task and start fresh; in HANDOFF mode it asks for a
;;;; structured handoff, which the `handoff' tool files: one markdown file,
;;;; standing orders, the state of each workstream, the open items, the next
;;;; action, what was not touched, for a fresh session to read first.
;;;;
;;;; Where a session stands lives in its own session state
;;;; (HANDYDDANDY.ARMED, HANDYDDANDY.HANDOFF), so it survives a restart and a
;;;; second process on the same store reads it. Nothing runs while a session
;;;; idles: the one hook is the :ROUND point, which pokes nothing and starts
;;;; nothing.

(in-package #:nodecode-handydandy)

;;; --- the manual ---------------------------------------------------------------
;;; (help :handydandy) answers it while the cell runs; a request carries the
;;; one line the :HELP clause below gives, never this text.

(defparameter +primer+
  "This session's size is watched by the nodecode-handydandy cell: once it
passes its limit -- the share of the model's context its newest round filled,
its input tokens, or its rounds -- it is told so once, and the operator gets a
notice naming the file its handoff goes to. The `handoff' tool writes that
file: a markdown page with the orders and approvals still in force, the state
of each workstream with file paths and ids, the open items and who owns each,
the next action, and what was not touched, for a fresh session to read first.
handydandy.mode is advise, a nudge to finish and start fresh, or handoff,
where the tool is asked for. /handydandy says where this session stands and
forgets the notice when asked; (handydandy:status) says the same in eval."
  "What (help :handydandy) answers while the cell runs.")

;;; --- the words -------------------------------------------------------------------
;;; Data, so a layer can reword what a long session and its operator are told.

(defparameter *notice*
  "[handydandy] This session is getting long (~a). Finish the current task; if the work continues, ~
write a handoff with the `handoff` tool, then start a fresh session from it."
  "What an over-long session is told in advise mode, over the reasons it is
over its limit.")

(defparameter *instruction*
  "[handydandy] This session has passed its size limit (~a). First finish the operator's current ~
request. Then, in this same turn, write a handoff for a fresh session with the `handoff` tool, ~
which files it at ~a: the orders and approvals still in force; the state of each workstream, with ~
file paths and ids; the open items and who owns each; the next action; what was not touched. Write ~
it for the next session, not as a recap, and never copy a secret value into it. Then tell the ~
operator in one line where the handoff is."
  "What an over-long session is told in handoff mode, over the reasons and the
path the handoff goes to.")

(defparameter *saved*
  "[handydandy] The handoff is at ~a. A fresh session starts by reading that file; nothing is ~
carried over by itself."
  "What a session is told once its handoff is on disk, over its path.")

;;; --- settings and paths ---------------------------------------------------------

(defun setting-text (key)
  "SETTING KEY as a trimmed string; a member left out reads as one."
  (let ((value (setting key)))
    (if (stringp value) (nlk:trimmed value) "")))

(defun expand-home (text)
  "TEXT with a leading ~/ meant as this machine's home."
  (if (and (>= (length text) 2) (char= #\~ (char text 0)) (char= #\/ (char text 1)))
      (merge-pathnames (subseq text 2) (user-homedir-pathname))
      text))

(defparameter +id-chars+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.-"
  "What a session id may keep in a handoff file's name; every other character is a dash.")

(defun safe-piece (text fallback limit)
  "TEXT as a file name's piece: its last LIMIT characters, each outside
+ID-CHARS+ a dash; FALLBACK when nothing is left."
  (let* ((clean (map 'string (lambda (char) (if (find char +id-chars+) char #\-))
                      (princ-to-string text)))
         (piece (if (> (length clean) limit) (subseq clean (- (length clean) limit)) clean)))
    (if (plusp (length piece)) piece fallback)))

(defun handoff-directory ()
  "Where handoffs are filed: handydandy.handoff_dir, else the home's
handoffs folder."
  (let ((named (setting-text :handoff-dir)))
    (if (plusp (length named))
        (uiop:ensure-directory-pathname (expand-home named))
        (nlk:home "handoffs/"))))

(defun handoff-path (session)
  "Where SESSION's handoff goes: HANDOFF-<date>-<session>.md, dated today."
  (merge-pathnames (format nil "HANDOFF-~a-~a.md"
                           (subseq (nlk:iso-now) 0 10) (safe-piece session "session" 24))
                   (handoff-directory)))

;;; --- where a session stands ------------------------------------------------------
;;; Session state: one JSON text a key. ARMED is when the session was told it
;;; is long; HANDOFF is the file it was told about, present once written.
;;; Both outlive the process and both are read inside one store.

(defparameter +armed-key+ "handydandy.armed"
  "Session state key: when this session was told its size, as a universal time.")

(defparameter +handoff-key+ "handydandy.handoff"
  "Session state key: the handoff file this session wrote, as JSON text.")

(defparameter +told-key+ "handydandy.told"
  "Session state key: the handoff this session was told about, so it is told once.")

(defun state-text (session key)
  "SESSION's KEY as the string it holds, or NIL."
  (let ((raw (and session (nlk:store-open-p) (ignore-errors (nlk:session-state-get session key)))))
    (and (stringp raw) raw)))

(defun state-put (session key value)
  "Hold VALUE, a JSON text, under SESSION's KEY; a NIL VALUE drops it."
  (when (and session (nlk:store-open-p))
    (if value
        (ignore-errors (nlk:session-state-put session key value))
        (ignore-errors (nlk:session-state-delete session key))))
  value)

(defun armed-at (session)
  "When SESSION was told it is long, as a universal time, or NIL."
  (let ((raw (state-text session +armed-key+)))
    (and raw (ignore-errors (parse-integer raw)))))

(defun (setf armed-at) (stamp session)
  (state-put session +armed-key+ (and stamp (princ-to-string stamp)))
  stamp)

(defun handoff-written (session)
  "The path SESSION's handoff was written to, or NIL while none was."
  (let ((raw (state-text session +handoff-key+)))
    (and raw (ignore-errors (nlk:decode-json raw)))))

(defun (setf handoff-written) (path session)
  (state-put session +handoff-key+ (and path (shasht:write-json (namestring path) nil)))
  path)

(defun handoff-told (session)
  "The handoff SESSION was told about, or NIL."
  (let ((raw (state-text session +told-key+)))
    (and raw (ignore-errors (nlk:decode-json raw)))))

(defun (setf handoff-told) (path session)
  (state-put session +told-key+ (and path (shasht:write-json (namestring path) nil)))
  path)

(defun session-path (session)
  "The path SESSION's handoff goes to: the one it was told, else a fresh one."
  (or (handoff-written session) (handoff-path session)))

;;; --- the limits -------------------------------------------------------------------

(defun reasons-over (tokens window rounds)
  "The reasons this session is over its limits, each a phrase; empty when
it is fine. TOKENS is what the newest round filled, WINDOW the model's
context, ROUNDS how many it has taken."
  (let ((reasons '())
        (max-tokens (setting :max-input-tokens))
        (percent (setting :context-percent))
        (max-rounds (setting :max-rounds)))
    (when (and (plusp max-tokens) (>= tokens max-tokens))
      (push (format nil "~:d input tokens, limit ~:d" tokens max-tokens) reasons))
    (when (and window (plusp window) (plusp percent) (>= tokens (floor (* window percent) 100)))
      (push (format nil "~d% of the model's ~:d-token context" (round (* 100 tokens) window) window)
            reasons))
    (when (and (plusp max-rounds) (>= rounds max-rounds))
      (push (format nil "~d round~:p, limit ~d" rounds max-rounds) reasons))
    (nreverse reasons)))

(defun reasons-text (reasons)
  (format nil "~{~a~^; ~}" reasons))

;;; --- the one seam: the round about to be asked -----------------------------------

(defun say (turn text)
  "Put TEXT in TURN's prompt, in the round about to be asked."
  (nlk:record-turn-context-prompt turn :text text))

(defun record (session kind &rest payload)
  "Append one KIND fact about this cell's work in SESSION's log."
  (ignore-errors (nlk:record-event session kind payload)))

(defun arm (session reasons rounds)
  "Tell SESSION once that it is long: its ARMED stamp, one fact, and the
operator's standing notice naming the path. => the path."
  (let ((path (handoff-path session)))
    (setf (armed-at session) (get-universal-time))
    (record session "handydandy.armed" :reasons reasons :rounds rounds
            :handoff (namestring path))
    (nle:notice (format nil "handydandy: this session is getting long (~a); its handoff goes to ~a"
                        (reasons-text reasons) (namestring path))
                :level :info :key "nodecode-handydandy")
    path))

(defun nudge (op)
  "At one model request: SESSION's size, and the one line it is told about
it -- the notice once it passes its limit, the ask for the handoff at each
turn's first round until the file is there, and one line once it is."
  (let* ((session (getf op :session-id))
         (turn (getf op :turn))
         (round (or (getf op :round) 0)))
    (when (and (stringp session) turn (nlk:store-open-p))
      (let* ((meter (nle:turn))
             (tokens (or (getf meter :context-tokens) 0))
             (window (getf meter :context-window))
             (written (handoff-written session)))
        (cond
          ;; the handoff this cell asked for is on disk: said once, then quiet
          ((and written (not (equal written (handoff-told session))))
           (setf (handoff-told session) written)
           (say turn (format nil *saved* written)))
          ;; armed, the handoff still owed: the ask, at each turn's first round
          ((and (armed-at session) (not written) (equal "handoff" (setting :mode)) (zerop round))
           (say turn (format nil *instruction* "past its size limit"
                              (namestring (session-path session)))))
          ;; not told yet, and over its limit: ask once
          ((null (armed-at session))
           (let ((reasons (reasons-over tokens window (1+ round))))
             (when reasons
               (let ((path (arm session reasons (1+ round))))
                 (say turn (if (equal "handoff" (setting :mode))
                               (format nil *instruction* (reasons-text reasons) (namestring path))
                               (format nil *notice* (reasons-text reasons)))))))))))))

;;; --- the handoff file ---------------------------------------------------------------

(defun handoff-section (heading text)
  "One section of a handoff: HEADING, then TEXT, or that nothing was said."
  (format nil "## ~a~%~%~a~%"
          heading
          (let ((text (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
            (if (and text (plusp (length text))) text "(nothing to say)"))))

(defun handoff-text (brief session)
  "BRIEF, the model's handoff, as the file a fresh session reads first."
  (format nil "# Handoff~@[ ~a~]~%~%Written ~a for the session ~a, for a fresh session to read first.~%~
It is what the next session needs, not a recap of this one.~%~%~{~a~}"
          (let ((title (handoff-brief-title brief)))
            (and title (plusp (length title)) title))
          (nlk:iso-now) session
          (list (handoff-section "Standing orders and approvals still in force"
                                 (handoff-brief-standing-orders brief))
                (handoff-section "State of each workstream" (handoff-brief-workstreams brief))
                (handoff-section "Open items and who owns them" (handoff-brief-open-items brief))
                (handoff-section "Next action" (handoff-brief-next-action brief))
                (handoff-section "What was not touched" (handoff-brief-untouched brief)))))

(defun csv-cell (value)
  "\"VALUE\" as one CSV cell."
  (format nil "\"~a\"" (princ-to-string value)))

(defun append-log (session path)
  "One row for the handoff PATH in the CSV handydandy.session_log_csv names,
its header when the file is new; nothing when it names none."
  (let ((csv (setting-text :session-log-csv)))
    (when (plusp (length csv))
      (let ((file (pathname (expand-home csv))))
        (ignore-errors
          (ensure-directories-exist file)
          (let ((new (not (probe-file file))))
            (with-open-file (out file :direction :output :if-exists :append
                                       :if-does-not-exist :create :external-format :utf-8)
              (when new
                (write-line "timestamp,session_id,handoff" out))
              (write-line (format nil "~a,~a,~a" (nlk:iso-now) (csv-cell session)
                                  (csv-cell (namestring path)))
                          out))))))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract handoff-brief
  (:doc "Write this session's handoff for a fresh one, once the work outgrows the session. It files the brief as one markdown page and answers its path; a fresh session reads that file first. Write it for the next session, not as a recap, and never copy a secret value into it.")
  ("title" :string :doc "one line naming the work, for the file's heading")
  ("standing_orders" :text
                     :doc "orders, decisions and approvals still in force, one a line, with what each covers")
  ("workstreams" :text :required t
                 :doc "the state of each workstream: what is done, what is half done, file paths, ids, branch or commit names")
  ("open_items" :text :doc "what is open, who owns each, and what it waits on")
  ("next_action" :text :required t :doc "the one step a fresh session takes first")
  ("untouched" :text :doc "what was not touched, and why"))

(defun write-handoff (brief)
  "The handoff tool: file BRIEF as this session's handoff. => its path."
  (let* ((session (or (getf (nle:turn) :session-id) nlk:*scribe-session-id*
                      (fail "a handoff belongs to a session: write it inside a turn")))
         (path (handoff-path session)))
    (nlk:write-file-atomically path (handoff-text brief session) :mode #o600 :directory-mode #o700)
    (setf (handoff-written session) path)
    (record session "handydandy.handoff" :handoff (namestring path))
    (append-log session path)
    (nle:notice (format nil "handydandy: handoff saved at ~a" (namestring path))
                :level :info :key "nodecode-handydandy")
    (format nil "handoff written to ~a. A fresh session starts by reading that file; nothing is ~
carried over by itself."
            (namestring path))))

;;; --- where the session stands -----------------------------------------------------

(defun status-line (session)
  "One line on where SESSION stands with its size limit."
  (if (and session (nlk:store-open-p))
      (let ((armed (armed-at session))
            (written (handoff-written session))
            (meter (nle:turn)))
        (format nil "handydandy ~a: ~@[told it is long, ~]~@[handoff at ~a, ~]~
context ~@[~:d tokens ~]~@[of ~:d ~]~@[at round ~d~], limit ~:d input tokens / ~d% of the window / ~d rounds; ~
the handoff would go to ~a"
                (setting :mode)
                (and armed "yes")
                written
                (getf meter :context-tokens)
                (getf meter :context-window)
                (and meter (1+ (getf meter :rounds)))
                (setting :max-input-tokens) (setting :context-percent) (setting :max-rounds)
                (namestring (session-path session))))
      (format nil "handydandy ~a: no store is open, so no session is watched; a handoff goes to ~a"
              (setting :mode) (namestring (handoff-path (or session "session"))))))

(define-verb status ()
  "Where this session stands with its size limit: the mode, whether it has
been told it is long, the handoff it wrote, the limits, and the path a
handoff goes to. => one line."
  (status-line (or nlk:*scribe-session-id* (getf (nle:turn) :session-id))))

(defun forget (session)
  "Drop SESSION's notice, so the limits are checked again from its next
request; a handoff it wrote stays on disk and in its state."
  (setf (armed-at session) nil
        (handoff-written session) nil
        (handoff-told session) nil)
  (nle:notice nil :key "nodecode-handydandy")
  (format nil "forgotten: this session's size is checked again from its next request; ~
a handoff already written stays at ~a"
          (namestring (handoff-path session))))

;;; --- /handydandy -----------------------------------------------------------------

(defun panel (session)
  "The dialog /handydandy answers with: what it would forget, and where a
handoff goes."
  (nle:list-dialog
   :title "HandyDandy"
   :summary (if (armed-at session) "This session was told it is long" "This session is under its limit")
   :rows (list (nle:run-row "Forget the notice for this session" "/handydandy reset"
                            :detail "the size is checked again from the next request")
               (nle:run-row "Where the handoff goes" "/handydandy path"
                            :detail "the file a fresh session reads first"))))

(defun run-slash (args session-id)
  "The answer to one /handydandy invocation: where the session stands, its
handoff's path, or the notice forgotten."
  (let* ((nlk:*scribe-session-id* session-id)
         (word (string-downcase (or (first (nlk:split-words (or args ""))) ""))))
    (nlk:dispatch word string=
      ("" (let ((line (status-line session-id)))
            (values line (and session-id (panel session-id)))))
      ("status" (status-line session-id))
      ("path" (format nil "the handoff goes to ~a" (namestring (session-path session-id))))
      ("reset" (if session-id
                    (forget session-id)
                    "no session here: /handydandy reset forgets one session's notice"))
      (t (format nil "~a is not one of its choices: /handydandy, status, path, reset" word)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell handydandy
  (:section ("handydandy")
    (:guide "mode advise nudges, handoff asks for a handoff file; the limits are context_percent of the model's window, max_input_tokens and max_rounds, each 0 for off; handoffs are filed under handoff_dir")
    ("mode" :choice :options '("advise" "handoff") :default "advise"
            :doc "advise: one nudge to finish and start fresh; handoff: the agent writes a handoff with the tool")
    ("max_input_tokens" :integer :default 100000 :min 0
                        :doc "a session's input tokens that count as too long; 0 turns this check off")
    ("context_percent" :integer :default 50 :min 0
                       :doc "the share of the model's context window that counts as too long, in percent; 0 turns this check off")
    ("max_rounds" :integer :default 40 :min 0
                  :doc "rounds in one session that count as too long; 0 turns this check off")
    ("handoff_dir" :path :doc "where handoffs are filed; empty uses the home's handoffs folder")
    ("session_log_csv" :path :doc "a CSV that gains one row per handoff (timestamp, session, path); empty writes none"))
  (:help :handydandy "a session past its size limit is told once, and its handoff is filed for the session after it; /handydandy" +primer+)
  (:hook :round (nlk:observer #'nudge "nodecode-handydandy"))
  (:tool "handoff" (nlk:contract 'handoff-brief) 'write-handoff)
  (:command "handydandy" #'run-slash
            :catches 'handydandy-error
            :description "Where this session stands with its size limit, and its handoff"
            :argument-hint "status|path|reset"))
