;;;; agents.lisp --- the agents /models offers, and the one each session runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An agent is a command that speaks ACP on stdio. Three come named --
;;;; Claude Code and Codex through their ACP adapters (npx fetches each on
;;;; first use), Gemini CLI through its own flag -- and the section's `agents'
;;;; object adds any other, or changes a preset, by name. A Nodecode session on
;;;; acp/<agent> keeps one agent process, started in the session's folder at
;;;; its first turn and kept for the next; the agent's own session id is kept
;;;; in the home, so after a restart the next turn loads that session again
;;;; (session/load) where the agent offers it, and starts a new one where not.

(in-package #:nodecode-acp)

(defstruct (agent (:constructor make-agent (name title command args &key environment mode)))
  "One agent /models lists as acp/NAME."
  (name "" :type string)
  (title "" :type string)
  (command "" :type string)
  (args '() :type list)
  (environment '() :type list)
  (mode nil))

(defparameter +presets+
  (list (make-agent "claude-code" "Claude Code" "npx" '("-y" "@agentclientprotocol/claude-agent-acp"))
        (make-agent "codex" "Codex" "npx" '("-y" "@agentclientprotocol/codex-acp"))
        (make-agent "gemini" "Gemini CLI" "gemini" '("--experimental-acp")))
  "The agents offered with no config: each is the agent's own ACP entry,
run with the login its CLI already has.")

(defun refuse (control &rest arguments)
  "Refuse the section: a config refusal saying CONTROL with ARGUMENTS."
  (error 'nlk:config-refusal :detail (apply #'format nil control arguments)))

(defun string-list (value where)
  "VALUE, a JSON array of strings, as a list; a refusal naming WHERE else."
  (cond ((or (null value) (eq value :null)) '())
        ((and (vectorp value) (every #'stringp value)) (coerce value 'list))
        (t (refuse "acp: ~a must be an array of strings" where))))

(defun configured-agents (table)
  "The presets with the section's `agents' object TABLE laid over them:
each member names an agent, its object the command, args, env, title and
mode, or `\"enabled\": false' to take a preset off."
  (let ((agents (mapcar #'copy-agent +presets+)))
    (when (hash-table-p table)
      (maphash
       (lambda (name spec)
         (unless (and (stringp name) (plusp (length name)) (every (lambda (c) (or (alphanumericp c) (find c "-_."))) name))
           (refuse "acp: agents.~a: a name is letters, digits and - _ ." name))
         (unless (hash-table-p spec)
           (refuse "acp: agents.~a must be an object" name))
         (let* ((old (find name agents :key #'agent-name :test #'string=))
                (command (gethash "command" spec))
                (title (gethash "title" spec))
                (mode (gethash "mode" spec))
                (env (gethash "env" spec)))
           (setf agents (remove name agents :key #'agent-name :test #'string=))
           (unless (multiple-value-bind (enabled present) (gethash "enabled" spec)
                     (and present (null enabled)))
             (unless (or old (stringp command))
               (refuse "acp: agents.~a needs a command" name))
             (when (and env (not (hash-table-p env)))
               (refuse "acp: agents.~a.env must be an object of strings" name))
             (push (make-agent name
                               (if (stringp title) title (if old (agent-title old) name))
                               (if (stringp command) command (agent-command old))
                               (if (nth-value 1 (gethash "args" spec))
                                   (string-list (gethash "args" spec) (format nil "agents.~a.args" name))
                                   (if old (agent-args old) '()))
                               :environment (let ((pairs '()))
                                              (when (hash-table-p env)
                                                (maphash (lambda (key value)
                                                           (unless (stringp value)
                                                             (refuse "acp: agents.~a.env.~a must be a string" name key))
                                                           (push (format nil "~a=~a" key value) pairs))
                                                         env))
                                              (sort pairs #'string<))
                               :mode (if (stringp mode) mode (and old (agent-mode old))))
                   agents))))
       table))
    (sort agents #'string< :key #'agent-name)))

(defun find-agent (name)
  "The agent acp/NAME runs, or NIL."
  (find name (setting :agents) :key #'agent-name :test #'string=))

(defun process-environment (agent)
  "The environment AGENT's process starts with: this process's own, with
AGENT's env laid over it; NIL, the process's own as it is, when it adds none."
  (when (agent-environment agent)
    (nlk:environment-with (agent-environment agent))))

;;; --- the agent session each Nodecode session runs ----------------------------

(defstruct (link (:constructor make-link (conn agent session directory)))
  "A Nodecode session's agent: its connection and the agent's session id."
  conn agent session directory)

(defvar *links* (make-hash-table :test 'equal :synchronized t)
  "Nodecode session id -> its LINK.")

(defvar *sessions-lock* (bt2:make-lock :name "acp sessions"))

(defun sessions-path ()
  "Where the agent session of each Nodecode session is kept."
  (nlk:home "acp/sessions.json"))

(defun log-path (session-id)
  "Where the agent of SESSION-ID writes its stderr."
  (nlk:home (format nil "acp/logs/~a.log" session-id)))

(defun read-sessions ()
  "The kept sessions: Nodecode session id -> {agent, session, directory}."
  (let ((value (ignore-errors (nlk:decode-json (uiop:read-file-string (sessions-path))))))
    (if (hash-table-p value) value (make-hash-table :test 'equal))))

(defun remember-session (session-id agent session directory)
  "Keep SESSION, AGENT's session for SESSION-ID, for the next start."
  (bt2:with-lock-held (*sessions-lock*)
    (let ((all (read-sessions))
          (path (sessions-path)))
      (setf (gethash session-id all)
            (nle::json-object "agent" agent "session" session "directory" directory))
      (ensure-directories-exist path)
      (let ((temporary (make-pathname :type "tmp" :defaults path)))
        (with-open-file (out temporary :direction :output :if-exists :supersede
                                       :external-format :utf-8)
          (write-string (nlk:encode-json-object all) out))
        (rename-file temporary path)))))

(defun saved-session (session-id agent)
  "The agent session AGENT last ran for SESSION-ID, or NIL."
  (let ((saved (gethash session-id (read-sessions))))
    (and (hash-table-p saved)
         (equal (gethash "agent" saved) agent)
         (stringp (gethash "session" saved))
         (gethash "session" saved))))

(defun agent-offers-p (conn &rest path)
  "Whether the agent's initialize answer says yes at PATH under agentCapabilities."
  (let ((value (gethash "agentCapabilities" (conn-agent conn))))
    (dolist (key path (and value (not (eq value :false)) (not (eq value :null))))
      (setf value (and (hash-table-p value) (gethash key value))))))

(defun refusal-words (agent condition)
  "CONDITION, an ACP-ERROR from AGENT's session/new, in words that say what to do."
  (let ((detail (acp-error-detail condition)))
    (if (or (eql (acp-error-code condition) -32000)
            (search "auth" (string-downcase detail)))
        (format nil "~a asks you to sign in first: run its own CLI once and log in there, then send again (~a)"
                (agent-title agent) detail)
        detail)))

(defun start-session (conn agent session-id directory)
  "The agent session SESSION-ID runs in CONN: the kept one loaded again where
the agent offers session/load, else a new one in DIRECTORY."
  (let ((empty (vector)))
    (or (let ((kept (saved-session session-id (agent-name agent))))
          (when (and kept (agent-offers-p conn "loadSession"))
          ;; The agent replays the session's history as updates to a session
          ;; no sink stands behind yet: they fall on the floor, as they should.
          (handler-case
              (progn (call conn "session/load"
                           (nle::json-object "sessionId" kept "cwd" directory "mcpServers" empty)
                           :seconds (setting :start-seconds))
                     kept)
            (acp-error () nil))))
        (let* ((result (handler-case
                           (call conn "session/new"
                                 (nle::json-object "cwd" directory "mcpServers" empty)
                                 :seconds (setting :start-seconds))
                         (acp-error (condition)
                           (error 'acp-error :detail (refusal-words agent condition)))))
               (id (and (hash-table-p result) (gethash "sessionId" result))))
          (unless (stringp id)
            (error 'acp-error :detail (format nil "~a opened no session" (agent-title agent))))
          (when (agent-mode agent)
            (handler-case (call conn "session/set_mode"
                                (nle::json-object "sessionId" id "modeId" (agent-mode agent)))
              (acp-error (condition)
                (nle:notice (format nil "acp: ~a kept its own mode: ~a" (agent-title agent)
                                    (acp-error-detail condition))
                            :level :warning :key "nodecode-acp"))))
          id))))

(defun ensure-link (session-id agent directory)
  "SESSION-ID's agent, running: the one it has when that is AGENT in
DIRECTORY and still up, else a new process with its session started."
  (let ((link (gethash session-id *links*)))
    (when (and link (not (conn-closed (link-conn link)))
               (string= (link-agent link) (agent-name agent))
               (equal (link-directory link) directory))
      (setf (conn-used (link-conn link)) (get-universal-time))
      (return-from ensure-link link))
    (when link
      (close-connection (link-conn link) "replaced")
      (remhash session-id *links*))
    (let ((conn (open-connection (agent-title agent)
                                 (cons (agent-command agent) (agent-args agent))
                                 :directory directory
                                 :environment (process-environment agent)
                                 :log (log-path session-id))))
      (handler-case
          (let ((session (start-session conn agent session-id directory)))
            (remember-session session-id (agent-name agent) session directory)
            (setf (gethash session-id *links*)
                  (make-link conn (agent-name agent) session directory)))
        (error (condition)
          (close-connection conn (princ-to-string condition))
          (error condition))))))

(defun stop-links (&optional (why "Nodecode stopped the agent"))
  "Stop every agent process."
  (let ((links '()))
    (maphash (lambda (id link) (declare (ignore id)) (push link links)) *links*)
    (clrhash *links*)
    (dolist (link links) (close-connection (link-conn link) why))))

(defun reap-idle ()
  "Stop the agents no turn has used for idle_minutes."
  (let ((cut (- (get-universal-time) (* 60 (setting :idle-minutes))))
        (idle '()))
    (maphash (lambda (id link)
               (when (or (conn-closed (link-conn link))
                         (and (< (conn-used (link-conn link)) cut)
                              (zerop (hash-table-count (conn-sinks (link-conn link))))))
                 (push (cons id link) idle)))
             *links*)
    (loop for (id . link) in idle
          do (remhash id *links*)
             (close-connection (link-conn link) "idle"))))
