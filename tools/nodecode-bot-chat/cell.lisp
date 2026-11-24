;;;; cell.lisp --- a bot-chat reply style, and a guard that only ever blocks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Tormen28/hermes-bot-chat-agent (MIT). The original
;;;; is two Hermes hooks, a pre_llm_call that injects a concise style
;;;; contract and a pre_tool_call that blocks a risky call, both scoped to the
;;;; sessions it recognizes as a canonical chat with a bot. Here the same two
;;;; acts are Nodecode seams:
;;;;
;;;;   - one NLE:HOOK on the :TOOL point, which refuses a call that breaks a
;;;;     rule (the guard cell's shape). A call is READ, not grepped: the spawn
;;;;     and file calls inside an EVAL form are found, and their literal
;;;;     arguments are what the rules see, so a comment that names a rule is
;;;;     not the rule;
;;;;   - one NLE:HOOK on NLE::READ-HARNESS-SECTIONS, which rides the style on
;;;;     every request in a session the mode is on for;
;;;;   - one NLE:HOOK on NLE:ASK-OPERATOR, so one turn asks at most one
;;;;     question;
;;;;   - one /bot-chat command, which turns the mode on and off.
;;;;
;;;; WHERE THE MODE IS ON. The original reads a session's title out of
;;;; Hermes's state.db and applies only there. Nodecode has no session titles,
;;;; so the operator says it: /bot-chat on in the session that should behave
;;;; this way, or scope all in the section for every session. Nothing starts
;;;; on by itself, so a fresh install changes nothing at all.
;;;;
;;;; The guard only ever BLOCKS; it never approves, rewrites or allows
;;;; anything a rule did not already refuse. Nothing leaves this machine and
;;;; nothing is written anywhere.

(defpackage #:nodecode-bot-chat
  (:use #:cl))

(in-package #:nodecode-bot-chat)

(nlk:define-peripheral bot-chat :not-running t)

;;; --- where the mode is on ---------------------------------------------------

(defvar *sessions* (make-hash-table :test #'equal :synchronized t)
  "Session id -> T while bot chat is on for that session.")

(defvar *questions* (make-hash-table :test #'equal :synchronized t)
  "Turn key -> T once that turn has asked its one question.")

(defvar *here* nil
  "(:SESSION-ID ... :TURN-ID ...) for a call made outside a turn, which is
what a test binds; inside a turn NLE:TURN answers instead.")

(defun here ()
  "The turn this call belongs to, as a plist, or *HERE* outside one."
  (or (let ((turn (ignore-errors (nle:turn))))
        (and turn (list :session-id (getf turn :session-id)
                        :turn-id (getf turn :turn-id))))
      *here*))

(defun session-id ()
  "The session this call belongs to, or the empty string."
  (or (getf (here) :session-id) ""))

(defun turn-key ()
  "A key naming this session's turn, or NIL outside one."
  (let ((turn (here)))
    (and turn (getf turn :turn-id)
         (format nil "~a:~a" (getf turn :session-id) (getf turn :turn-id)))))

(defun scoped-p (session)
  "Whether the mode is on for SESSION: every session under scope all,
otherwise the ones /bot-chat on named."
  (or (equal (setting :scope) "all")
      (and (stringp session) (plusp (length session)) (gethash session *sessions*))))

;;; --- the presets, as data ---------------------------------------------------

(defparameter +rules+
  '("secrets" "deletes" "destructive_git" "protected_push" "external_sends"
    "expensive_models" "one_question")
  "Every rule this cell knows, in the order the original checks them.")

(defparameter +presets+
  '(("strict" . ("secrets" "deletes" "destructive_git" "protected_push" "external_sends"
                 "expensive_models" "one_question"))
    ("relaxed" . ("secrets" "destructive_git" "protected_push")))
  "The original's presets: strict and default are the same seven rules, and
relaxed keeps only the three that lose data or leave the machine.")

(defun active-rules ()
  "The rules the section's preset runs."
  (let ((preset (string-downcase (or (setting :preset) "default"))))
    (or (cdr (assoc preset +presets+ :test #'equal))
        (cdr (assoc "default" +presets+ :test #'equal))
        +rules+)))

(defun rule-on-p (rule)
  (and (member rule (active-rules) :test #'equal) t))

;;; --- the rules, as patterns -------------------------------------------------

(defparameter +secret-re+
  (concatenate 'string
               "(?i)(?:^|[^a-z0-9_-])(?:\\.env(?:\\.[a-z0-9_-]+)?|auth\\.json|"
               "credentials(?:\\.json)?|id_rsa|id_ed25519|id_ecdsa|id_dsa|\\.npmrc|"
               "\\.pypirc|\\.netrc|[a-z0-9_-]*secret[a-z0-9_-]*\\.(?:json|ya?ml|txt)|"
               "[a-z0-9_-]+\\.(?:pem|key|p12|pfx))(?:$|[^a-z0-9_-])")
  "A path or command that names a credential file, as the original read it.")

(defparameter +secret-placeholder-re+
  "(?i)\\.env\\.(?:example|sample|template|dist|schema)\\b"
  "The .env names that are documentation, not credentials.")

(defparameter +secret-command-re+
  (concatenate 'string
               "(?i)(?:^|[;&|()\\s])(?:printenv\\b|env\\s*(?=[;&|>]|$)|"
               "export\\s+-p\\b|set\\s*(?=[;&|>]|$)|"
               "(?:Get-ChildItem|gci|dir)\\s+env:)")
  "A command that prints the whole environment.")

(defparameter +delete-re+
  (concatenate 'string
               "(?i)(?:^|[;&|()\\s]|(?:sudo|xargs)\\s+)(?:rm\\s+-[a-z]*[rf]|rm\\s+\\S|"
               "rmdir\\b|del\\s|erase\\s|Remove-Item\\b|ri\\s|shred\\b|unlink\\b|"
               "rimraf\\b|format\\s+[a-z]:)|\\bfind\\b[^\\n]*\\s-delete\\b|\\bgit\\s+rm\\b|"
               "\\bDROP\\s+(?:TABLE|DATABASE|SCHEMA)\\b|\\bTRUNCATE\\s+TABLE\\b|"
               "\\bDELETE\\s+FROM\\b|shutil\\.rmtree|os\\.remove|os\\.unlink|\\.unlink\\(")
  "What the original refused as a delete: a removal, a destructive SQL
statement, or a library call that removes a file.")

(defparameter +git-prefix+
  "\\bgit\\s+(?:-[Cc]\\s+\\S+\\s+|-(?![Cc]\\s)\\S+\\s+){0,8}"
  "GIT and the short options before a subcommand, bounded so the pattern
cannot backtrack away the turn.")

(defparameter +destructive-git-re+
  (concatenate 'string
               +git-prefix+
               "(?:push\\s+[^\\n]*(?:--force\\b|-f\\b|--force-with-lease)|reset\\s+--hard|"
               "clean\\s+-\\w*f|branch\\s+-D|checkout\\s+--\\s|restore\\s+[^\\n]*--source|"
               "rebase\\b|filter-branch|push\\s+[^\\n]*--delete|reflog\\s+expire|"
               "gc\\s+--prune|stash\\s+(?:drop|clear)\\b)")
  "What the original refused as destructive git.")

(defun scan-p (pattern text)
  "Whether PATTERN matches TEXT; a broken pattern matches nothing."
  (and (stringp text) (plusp (length text))
       (ignore-errors (and (cl-ppcre:scan pattern text) t))))

(defun secret-hit-p (text)
  "Whether TEXT names a credential file; a documented placeholder does not."
  (scan-p +secret-re+ (cl-ppcre:regex-replace-all +secret-placeholder-re+ text " ")))

(defun protected-push-p (command)
  "Whether COMMAND pushes or merges into a protected branch."
  (let ((branches (remove "" (setting :protected-branches) :test #'equal)))
    (when branches
      (let ((alt (format nil "(?:~{~a~^|~})"
                         (mapcar #'cl-ppcre:quote-meta-chars branches))))
        (or (scan-p (format nil "~apush\\b[^\\n]*(?:\\s|:|refs/heads/|origin/)(?:~a)(?=\\s|$|[;&|)])"
                             +git-prefix+ alt)
                    command)
            (and (scan-p "\\bgit\\s+merge\\b" command)
                 (scan-p (format nil "\\bgit\\s+(?:checkout|switch)\\s+(?:~a)\\b" alt) command))
            (scan-p "\\bgh\\s+pr\\s+merge\\b" command))))))

(defun external-send-p (tool-name)
  "Whether a call of TOOL-NAME sends something to the outside world."
  (or (member tool-name (setting :external-send-tools) :test #'string-equal)
      (scan-p "(?:^|[-_])(?:send|post|reply|publish|tweet|email|mail|message)(?:s|es)?(?:$|[-_])"
              tool-name)))

(defun expensive-text-p (text)
  "Whether TEXT names a model the section calls expensive."
  (some (lambda (pattern) (scan-p pattern text)) (setting :expensive-models)))

;;; --- what a call names, read from the call ----------------------------------

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defparameter +file-operators+
  '("read-file" "write-file" "edit" "delete-file" "copy-file" "rename-file"
    "probe-file" "file-exists-p" "directory-files" "ensure-directories-exist")
  "The heads whose argument is a path the call touches.")

(defun operator-name (x)
  "X's symbol name, lowercased, or NIL, so a form read in any package is read."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun dollar-name (x)
  "The strings-object name a ($ name) form reads, or NIL."
  (and (consp x) (equal "$" (operator-name (car x))) (stringp (second x)) (second x)))

(defun literal (x strings)
  "The text X alone names, or NIL when the form hides it."
  (cond ((stringp x) x)
        ((dollar-name x) (and strings (gethash (dollar-name x) strings)))
        ((and (consp x) (member (operator-name (car x)) '("list" "quote") :test #'equal))
         (let ((items (if (equal "quote" (operator-name (car x))) (second x) (rest x))))
           (when (and (listp items) items
                      (every (lambda (item) (or (stringp item) (dollar-name item))) items))
             (format nil "~{~a~^ ~}"
                     (mapcar (lambda (item) (or (dollar-name item) item)) items)))))
        (t nil)))

(defun read-forms (text)
  "TEXT's forms, or NIL when it does not read."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil)
              (*package* (find-package :nodecode-bot-chat))
              (forms '()))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push form forms))
          (nreverse forms)))
    (error () nil)))

(defun scan-forms (forms strings)
  "What FORMS run and name: => (:COMMANDS ... :PATHS ... :MODELS ... :HIDDEN)."
  (let ((commands '()) (paths '()) (models '()) (hidden nil))
    (labels ((collect (x place)
               ;; PUSH onto the lexical list itself, never onto a parameter:
               ;; a PUSH on a binding the caller handed in grows nothing.
               (let ((text (literal x strings)))
                 (cond (text (case place
                               (:commands (push text commands))
                               (:paths (push text paths))
                               (:models (push text models))))
                       ((eq place :commands) (setf hidden t)))))
             (arguments (x place)
               (dolist (argument (rest x))
                 (unless (keywordp argument) (collect argument place))))
             (walk (x)
               (cond ((consp x)
                      (let ((name (operator-name (car x))))
                        (cond ((member name +spawn-operators+ :test #'equal)
                               (arguments x :commands))
                              ((member name +file-operators+ :test #'equal)
                               ;; The target is the first argument; a body's own
                               ;; text is not the path that was written.
                               (collect (second x) :paths))
                              ((member name (setting :expensive-tools) :test #'string-equal)
                               (arguments x :models))))
                      (walk (car x))
                      (walk (cdr x)))
                     (t nil))))
      (dolist (form forms) (walk form)))
    (list :commands (nreverse commands) :paths (nreverse paths)
          :models (nreverse models) :hidden hidden)))

(defun argument (arguments key)
  "ARGUMENTS' member KEY, or NIL when it is not an object."
  (and (hash-table-p arguments) (gethash key arguments)))

(defun field (arguments &rest keys)
  "The first of KEYS ARGUMENTS names as a non-empty string."
  (loop for key in keys
        for value = (argument arguments key)
        when (and (stringp value) (plusp (length value)))
          return value))

(defun carried-values (arguments)
  "Every string ARGUMENTS carries, space separated."
  (if (hash-table-p arguments)
      (with-output-to-string (out)
        (loop for value being the hash-values of arguments
              when (stringp value) do (format out " ~a" value)))
      ""))

(defun eval-call-text (arguments)
  "An EVAL call's form as text."
  (let ((form (argument arguments "form")))
    (cond ((stringp form) form)
          ((null form) "")
          (t (format nil "~s" form)))))

(defun call-scan (tool-name arguments)
  "What a call of TOOL-NAME names, as a plist of :COMMANDS, :PATHS, :MODELS
and :TEXT. An EVAL call is read through its form; any other tool is read
through the fields it names beside it, as the original read every call."
  (let ((name (string-downcase (or tool-name ""))))
    (if (equal name "eval")
        (let* ((text (eval-call-text arguments))
               (strings (argument arguments "strings"))
               (forms (and (plusp (length text)) (read-forms text))))
          (if (null forms)
              (list :commands (and (plusp (length text)) (list text)) :paths nil
                    :models nil :text (concatenate 'string text (carried-values arguments)))
              (append (scan-forms forms strings)
                      (list :text (concatenate 'string text
                                               (carried-values arguments))))))
        (list :commands (let ((one (field arguments "command" "cmd" "code")))
                          (and one (list one)))
              :paths (let ((one (field arguments "path" "file_path" "filename"
                                       "target_file" "file")))
                       (and one (list one)))
              :models nil
              :text (carried-values arguments)))))

;;; --- the violation ---------------------------------------------------------

(defun violation (tool-name arguments)
  "The rule a call of TOOL-NAME with ARGUMENTS breaks, or NIL."
  (let* ((scan (call-scan tool-name arguments))
         (text (or (getf scan :text) ""))
         (commands (getf scan :commands))
         (paths (getf scan :paths))
         (command (and commands (format nil "~{~a~^~%~}" commands)))
         (path (and paths (format nil " ~{~a~^ ~} " paths)))
         (name (string-downcase (or tool-name ""))))
    (flet ((secret-p ()
             (or (and path (secret-hit-p path))
                 (and command (or (secret-hit-p (format nil " ~a " command))
                                  (scan-p +secret-command-re+ command)))))
           (delegating-p ()
             (member name (setting :expensive-tools) :test #'string-equal)))
      (cond ((and (rule-on-p "secrets") (secret-p)) "secrets")
            ((and (rule-on-p "destructive_git") command
                  (scan-p +destructive-git-re+ command))
             "destructive_git")
            ((and (rule-on-p "protected_push") command (protected-push-p command))
             "protected_push")
            ((and (rule-on-p "deletes") command (scan-p +delete-re+ command))
             "deletes")
            ((and (rule-on-p "deletes")
                  (member name '("delete-file" "remove-file" "rm") :test #'equal))
             "deletes")
            ((and (rule-on-p "external_sends") (external-send-p name)) "external_sends")
            ((and (rule-on-p "expensive_models") (expensive-text-p text)
                  (or (delegating-p)
                      (and (equal name "eval")
                           (some (lambda (tool) (search tool text :test #'char-equal))
                                 (setting :expensive-tools)))))
             "expensive_models")
            (t nil)))))

;;; --- what the model and the operator read -----------------------------------

(defparameter +style+
  '(:en ("[Bot Chat style]"
         "1. Act, don't narrate: do the work, then reply briefly with the result."
         "2. Start long tasks with one short acknowledgement line, then work."
         "3. Don't ask permission for safe, reversible, in-scope steps. Ask at most ONE question per turn, only when truly blocked."
         "4. Keep replies short and plain; no inner monologue or tool-by-tool logs."
         "5. Never reveal or paste secrets, tokens, keys, .env or auth files."
         "6. Deleting, destructive git, pushing to protected branches, sending messages to the outside world and using expensive models require the user's explicit OK."
         "7. If a tool is blocked by the safety guard, stop that step, tell the user in one line what needs approval, and continue with the rest."
         "8. Report honestly: say what was verified and what was not.")
    :es ("[Estilo Bot Chat]"
         "1. Actua, no narres: haz el trabajo y responde breve con el resultado."
         "2. En tareas largas, empieza con una sola linea de acuse y trabaja."
         "3. No pidas permiso para pasos seguros, reversibles y dentro del alcance. Haz como maximo UNA pregunta por turno, solo si estas bloqueado de verdad."
         "4. Respuestas cortas y claras; sin monologo interno ni logs de herramientas."
         "5. Nunca muestres secretos, tokens, claves, .env ni archivos de auth."
         "6. Borrar, git destructivo, push a ramas protegidas, enviar mensajes al exterior y usar modelos caros requieren el OK explicito del usuario."
         "7. Si la guardia bloquea una herramienta, deten ese paso, di en una linea que necesita aprobacion y sigue con lo demas."
         "8. Reporta con honestidad: que se verifico y que no."))
  "The style contract, one line per rule, in the section's language.")

(defparameter +messages+
  '(:en (("secrets" . "this touches secrets or credential files.")
         ("deletes" . "this deletes files or data.")
         ("destructive_git" . "this is a destructive git operation.")
         ("protected_push" . "this pushes or merges into a protected branch.")
         ("external_sends" . "this sends a message to the outside world.")
         ("expensive_models" . "this uses an expensive model."))
    :es (("secrets" . "esto toca secretos o archivos de credenciales.")
         ("deletes" . "esto borra archivos o datos.")
         ("destructive_git" . "esto es una operacion git destructiva.")
         ("protected_push" . "esto hace push o merge a una rama protegida.")
         ("external_sends" . "esto envia un mensaje al exterior.")
         ("expensive_models" . "esto usa un modelo caro.")))
  "Why each rule refused a call, in the section's language.")

(defun language ()
  (if (equal (string-downcase (or (setting :language) "en")) "es") :es :en))

(defun style-text ()
  (format nil "~{~a~^~%~}" (getf +style+ (language))))

(defun refusal-text (rule)
  "The refusal a call that broke RULE answers, in the section's language."
  (let ((why (or (cdr (assoc rule (getf +messages+ (language)) :test #'equal)) rule)))
    (if (eq (language) :es)
        (format nil "ERROR: refused by nodecode-bot-chat: ~a Pide aprobacion explicita al usuario antes de reintentar." why)
        (format nil "ERROR: refused by nodecode-bot-chat: ~a Ask the operator for explicit approval before retrying." why))))

;;; --- the hooks -------------------------------------------------------------

(defun guard-call (op next)
  "The :TOOL point: a call that breaks a rule answers the call's failure,
and NEXT never runs. Outside a bot-chat session nothing is read."
  (let ((rule (and (scoped-p (session-id))
                   (violation (getf op :name) (getf op :arguments)))))
    (if rule
        (nle:failure (refusal-text rule))
        (funcall next op))))

(defun style-section (next session-id)
  "The standing harness section: the style, in a session the mode is on for."
  (if (and (setting :style) (scoped-p session-id))
      (acons "bot-chat" (style-text)
             (remove "bot-chat" (funcall next session-id) :key #'car :test #'equal))
      (funcall next session-id)))

(defun one-question (next &rest args)
  "The core's ASK-OPERATOR, advised: in a bot-chat session the first card of
a turn is shown, and a second one in the same turn is answered unanswered."
  (let ((key (turn-key)))
    (if (or (null key) (not (scoped-p (session-id)))
            (not (rule-on-p "one_question")))
        (apply next args)
        (let ((first nil))
          (cond ((gethash key *questions*) nil)
                (t (setf first t)
                   (when (> (hash-table-count *questions*) 1024) (clrhash *questions*))
                   (setf (gethash key *questions*) t)))
          (if first (apply next args) (values nil :one-question))))))

;;; --- the operator's side ----------------------------------------------------

(defparameter +manual+
  "Bot chat is a mode for one session at a time: /bot-chat on turns it on in the session it is typed in, /bot-chat off turns it off, /bot-chat status reports it, and /bot-chat rules lists what the preset refuses. While it is on, every request carries a short style contract (act instead of narrating, short plain replies, never paste secrets, at most one question per turn), and a tool call that breaks a rule does not run: one that touches a credential file or prints the environment, deletes files or data, runs destructive git, pushes or merges into a protected branch, sends something to the outside world, or names an expensive model. A refused call is the model's failure result, with the reason and a line telling it to ask you first; the guard only ever blocks, and it never asks you anything on the model's behalf. Setting scope to all in the bot-chat section applies the mode to every session, and preset relaxed keeps only secrets, destructive git and protected pushes.")

(defun mode-line (session)
  (cond ((equal (setting :scope) "all") "on for every session (scope all)")
        ((scoped-p session) "on for this session")
        (t "off for this session: /bot-chat on turns it on")))

(defun status-text (session)
  "What /bot-chat status answers."
  (format nil "bot chat: ~a~%;preset: ~a, refusing: ~{~a~^, ~}~%;style on every request: ~:[no~;yes~]"
          (mode-line session) (setting :preset) (active-rules)
          (if (and (scoped-p session) (setting :style)) t nil)))

(defun rules-text ()
  "What /bot-chat rules answers."
  (format nil "preset ~a refuses: ~{~a~^, ~}; it never approves, allows or rewrites a call.~%;protected branches: ~{~a~^, ~}~%;external send tools: ~{~a~^, ~}"
          (setting :preset) (active-rules) (setting :protected-branches)
          (setting :external-send-tools)))

(defun switch (session on)
  "Turn the mode on or off for SESSION, and answer what changed."
  (cond ((or (null session) (zerop (length (or session ""))))
         "no session to turn bot chat on for: type /bot-chat on in the session that should behave this way.")
        (on (setf (gethash session *sessions*) t)
            (format nil "bot chat is on for this session (preset ~a).~%~%~a"
                    (setting :preset) (style-text)))
        (t (remhash session *sessions*)
           "bot chat is off for this session: calls run as they always did.")))

(defun run-bot-chat (args session-id)
  "The /bot-chat command: the operator's side of the mode."
  (let* ((text (nlk:trimmed (or args "")))
         (verb (string-downcase text)))
    (cond ((or (equal verb "") (equal verb "status")) (status-text session-id))
          ((equal verb "on") (switch session-id t))
          ((equal verb "off") (switch session-id nil))
          ((equal verb "rules") (rules-text))
          (t (format nil "Unknown /bot-chat argument ~s: use on, off, status or rules." text)))))

;;; --- the cell ---------------------------------------------------------------

(nle:define-cell bot-chat
  (:section ("bot-chat")
    (:guide "nothing here is required; the mode is off until /bot-chat on in a session, or scope all for every session; preset relaxed keeps only secrets, destructive git and protected pushes")
    ("scope" :choice :options '("sessions" "all") :default "sessions"
             :doc "sessions: only the ones /bot-chat on names; all: every session")
    ("style" :boolean :default t
             :doc "ride the short style contract on every request in a bot-chat session")
    ("language" :choice :options '("en" "es") :default "en"
                :doc "the language of the style contract and of a refusal")
    ("preset" :choice :options '("strict" "default" "relaxed") :default "default"
              :doc "strict and default refuse all seven rules; relaxed keeps secrets, destructive git and protected pushes")
    ("protected_branches" :list :default '("main" "master")
                           :doc "a push or merge into one of these waits for you")
    ("external_send_tools"
     :list :default '("send_message" "react_to_message" "send_email" "post_message"
                      "publish_post" "tweet")
     :doc "tool names that send something outside; names like send-email are matched too")
    ("expensive_models" :list :default '("\\bpro\\b" "opus" "\\bultra\\b" "-max\\b")
                        :doc "regular expressions over the model a call names")
    ("expensive_tools" :list :default '("delegate_task" "spawn_agent" "set_model"
                                         "agent-batch" "delegation" "model-router"
                                         "effort-router")
                        :doc "tool names whose model choice the expensive rule reads"))
  (:command "bot-chat" 'run-bot-chat
            :description "Bot chat: a concise style and a block-only safety guard in this session"
            :argument-hint "on | off | status | rules")
  (:help :bot-chat
         "bot-chat: a concise bot style plus a guard that blocks risky calls, in the sessions you turn it on in - /bot-chat on"
         +manual+)
  (:hook :tool #'guard-call)
  (:hook 'nle::read-harness-sections #'style-section)
  (:hook 'nle:ask-operator #'one-question)
  (:start (lambda ()
            (clrhash *sessions*)
            (clrhash *questions*)
            (nle:on-stop (lambda ()
                           (clrhash *sessions*)
                           (clrhash *questions*))))))
