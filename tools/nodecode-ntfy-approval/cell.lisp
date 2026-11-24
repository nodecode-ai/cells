;;;; cell.lisp --- risky shell calls wait for a tap on the operator's phone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the ntfy approval transport for Hermes: the question a
;;;; dangerous call raises is an ntfy push notification with Approve once,
;;;; Approve for session and Deny, and the phone's tap is posted back to a
;;;; reply topic on the same server, so nothing listens on this machine.
;;;;
;;;; ONE HOOK. The cell installs one :TOOL hook. A call is read the way the
;;;; guard cell reads it -- the spawn calls in an EVAL form, their literal
;;;; arguments -- and only the commands a call runs meet the rule table; a
;;;; call showing no spawn reads nothing, and an ordinary command runs
;;;; without a network call at all.
;;;;
;;;; FAIL CLOSED. A matched call runs only on Approve once or Approve for
;;;; session. Deny, no answer before `timeout_seconds', a server that cannot
;;;; be reached, and a section with no topic all refuse the call, and the
;;;; refusal says which. That is the original transport's rule: silence is a
;;;; no.
;;;;
;;;; WHAT LEAVES THE MACHINE. The rule and its reason, the command's text
;;;; (skip it with send_command off), and one random code per button, to the
;;;; configured ntfy server only -- public ntfy.sh unless an own server is
;;;; set. The topic name is the secret there, so the cell refuses a topic
;;;; shorter than 16 characters when no access token is named, as the
;;;; original does.

(defpackage #:nodecode-ntfy-approval
  (:use #:cl))

(in-package #:nodecode-ntfy-approval)

;;; --- the seams tests bind ----------------------------------------------------

(defparameter *http* #'nlk:http
  "How one exchange is made: (funcall *HTTP* METHOD URL :headers :content :timeout)
=> (values BODY STATUS). Tests bind this to a fake server.")

(defparameter *clock* (lambda () (/ (get-internal-real-time)
                                    (float internal-time-units-per-second)))
  "Seconds, monotonic: when the wait ends.")

(defparameter *sleep* #'sleep "How the wait pauses between polls; tests make it free.")

;;; --- the wire ----------------------------------------------------------------

(defparameter +default-server+ "https://ntfy.sh")
(defparameter +reply-suffix+ "-reply")
(defparameter +max-topic-length+ (- 64 (length +reply-suffix+))
  "ntfy topics are 1-64 characters of [A-Za-z0-9_-]; the reply topic adds the suffix.")
(defparameter +min-open-topic-length+ 16
  "With no access token, knowing the topic is what lets someone answer a request.")
(defparameter +priorities+
  '(("min" . 1) ("low" . 2) ("default" . 3) ("high" . 4) ("urgent" . 5)))
(defparameter +reply-prefix+ "nodecode-approval v1"
  "The first two words of an answer; the rest is the request, the choice and the code.")
(defparameter +buttons+
  '(("once" . "Approve once") ("session" . "Approve for session") ("deny" . "Deny"))
  "The choices a notification offers, in order: three fit on a phone screen.")
(defparameter +publish-timeout+ 10)
(defparameter +poll-timeout+ 10)
(defparameter +withdraw-timeout+ 5)

(define-condition ntfy-error (error)
  ((text :initarg :text :reader ntfy-error-text))
  (:report (lambda (condition stream) (write-string (ntfy-error-text condition) stream))))

(defun trimmed (text)
  (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun non-empty (text)
  (let ((text (trimmed text)))
    (and text (plusp (length text)) text)))

(defun normalize-server (raw)
  "RAW as an http(s) base URL with no trailing slash."
  (let ((text (or (non-empty raw) +default-server+)))
    (unless (search "://" text)
      (setf text (concatenate 'string "https://" text)))
    (let* ((scheme-end (search "://" text))
           (scheme (string-downcase (subseq text 0 scheme-end))))
      (unless (member scheme '("http" "https") :test #'string=)
        (error 'ntfy-error :text (format nil "server ~s must be an http or https URL" text)))
      (let ((host (subseq text (+ scheme-end 3))))
        (when (or (not (plusp (length host))) (find #\Space host))
          (error 'ntfy-error :text (format nil "server ~s names no host" text)))))
    (string-right-trim "/" text)))

(defun topic-problem (topic token)
  "Why TOPIC cannot be used, or NIL."
  (cond ((null topic) "no topic is set: run /ntfy setup")
        ((> (length topic) +max-topic-length+)
         (format nil "the topic is longer than ~d characters" +max-topic-length+))
        ((not (every (lambda (c) (or (alphanumericp c) (member c '(#\- #\_)))) topic))
         "the topic holds a character ntfy does not allow (letters, digits, '-' and '_')")
        ((and (null token) (< (length topic) +min-open-topic-length+))
         (format nil "without an access token the topic is the only secret, so it must be at least ~d characters"
                 +min-open-topic-length+))))

;;; --- the section's values, read once per request -------------------------------

(defun token-value ()
  "The access token the section names: an environment variable's value, never
a value written in the config."
  (let ((name (non-empty (setting :token-env))))
    (and name (non-empty (uiop:getenv name)))))

(defun wire-settings ()
  "The one shape both the hook and /ntfy read.
=> (VALUES PLIST PROBLEM): PLIST holds :SERVER :TOPIC :TOKEN :PRIORITY."
  (let* ((topic (non-empty (setting :topic)))
         (token (token-value))
         (problem (topic-problem topic token)))
    (if problem
        (values nil problem)
        (handler-case
            (values (list :server (normalize-server (setting :server))
                          :topic topic
                          :token token
                          :priority (or (cdr (assoc (string-downcase (or (non-empty (setting :priority)) "high"))
                                                     +priorities+ :test #'string=))
                                        (cdr (assoc "high" +priorities+))))
                    nil)
          (ntfy-error (e) (values nil (ntfy-error-text e)))))))

(defun headers (token &optional (content-type "application/json"))
  (append (when token (list (cons "authorization" (format nil "Bearer ~a" token))))
          (list (cons "content-type" content-type)
                (cons "accept-encoding" "identity"))))

(defun json-text (object)
  (shasht:write-json object nil))

;;; --- one random string per button -----------------------------------------------

(defun random-text (length)
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (*random-state* (make-random-state t)))
    (coerce (loop repeat length collect (char chars (random (length chars)))) 'string)))

(defun new-topic ()
  "A topic private enough for a public server: 39 characters."
  (concatenate 'string "nodecode-" (random-text 30)))

(defun answer-codes (choices)
  "One code per offered choice, so seeing a Deny cannot be turned into an approval."
  (loop for choice in choices collect (cons choice (random-text 24))))

;;; --- what a call runs (read, never grepped) -------------------------------------

(defparameter +spawn-heads+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defun head-name (x)
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun literal-text (x strings)
  "The text X alone names, or :HIDDEN."
  (cond ((stringp x) x)
        ((and (consp x) (equal "$" (head-name (car x))) (stringp (second x)))
         (or (and (hash-table-p strings) (gethash (second x) strings)) :hidden))
        ((and (consp x) (member (head-name (car x)) '("list" "quote") :test #'string=))
         (let ((items (if (equal "quote" (head-name (car x))) (second x) (rest x))))
           (if (and (listp items) items (every #'stringp items))
               (format nil "~{~a~^ ~}" items)
               :hidden)))
        (t :hidden)))

(defun call-text (call strings)
  "CALL's command text, or NIL when an argument hides it."
  (let ((parts '()) (rest (rest call)))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest))
                 (let ((text (literal-text (first rest) strings)))
                   (when (eq text :hidden) (return-from call-text nil))
                   (push text parts)
                   (setf rest (cdr rest)))))
    (when parts (format nil "~{~a~^ ~}" (nreverse parts)))))

(defun commands-of (form-text strings)
  "The commands FORM-TEXT runs, one per line, and how they were read:
:COMMAND when the form shows them, :LOOSE when one is hidden, NIL when the
form runs nothing."
  (let ((commands '()) (saw nil) (hidden nil))
    (labels ((walk (x)
               (cond ((consp x)
                      (when (member (head-name (car x)) +spawn-heads+ :test #'string=)
                        (setf saw t)
                        (let ((text (call-text x strings)))
                          (if text (push text commands) (setf hidden t))))
                      (walk (car x))
                      (walk (cdr x)))
                     ((member (head-name x) +spawn-heads+ :test #'string=)
                      (setf saw t)))))
      (when (stringp form-text)
        (ignore-errors
          (with-input-from-string (in form-text)
            (let ((*read-eval* nil) (*package* (find-package :nodecode-ntfy-approval)))
              (loop for form = (read in nil :eof)
                    until (eq form :eof)
                    do (walk form))))))
      (cond ((and saw (not hidden))
             (values (format nil "~{~a~^~%~}" (nreverse commands)) :command))
            (saw (values form-text :loose))
            (t (values nil nil))))))

(defun command-lines (text)
  "TEXT's commands, one per line, trimmed: an unquoted ; & | or newline starts the next."
  (let ((lines '()) (start 0) (i 0) (n (length text)) (quote nil))
    (labels ((quote-char-p (c) (member c '(#\" #\')))
             (backslash-p (c) (char= c (code-char 92))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((backslash-p c) (incf i 2))
                       (quote (when (char= c quote) (setf quote nil)) (incf i))
                       ((quote-char-p c) (setf quote c) (incf i))
                       ((or (find c ";|&") (char= c #\Newline))
                        (push (subseq text start i) lines)
                        (setf start (1+ i))
                        (incf i))
                       (t (incf i))))))
    (push (subseq text start) lines)
    (format nil "~{~a~^~%~}"
            (remove "" (mapcar (lambda (line)
                                  (string-trim '(#\Space #\Tab #\Return) line))
                                (nreverse lines))))))

;;; --- the rule table ---------------------------------------------------------------

(defparameter +command-start+
  "(?:^|\\n)\\s*(?:\\S+=\\S+\\s+)*(?:(?:sudo|doas|env|nohup|exec|command|nice|time)\\s+)*(?:timeout\\s+\\S+\\s+)?"
  "The start of a command: leading assignments and wrappers stepped over, so a
danger word in an argument is never at a position a rule reads.")

(defun anchored (body)
  (concatenate 'string +command-start+ body))

(defparameter +rules+
  (list (list "rm-recursive" "it deletes a tree"
              (anchored "(?:rm|rmdir)\\s+(?:-[a-zA-Z-]+\\s+)*(?:-[a-zA-Z]*[rR][a-zA-Z]*\\b|--recursive\\b)"))
        (list "privilege" "it escalates privileges" (anchored "(?:sudo|doas)\\s"))
        (list "force-push" "it rewrites the remote"
              (anchored "git\\b[^\\n]*\\bpush\\b[^\\n]*(?:--force\\b|\\s-f\\b)"))
        (list "hard-reset" "it discards uncommitted work"
              (anchored "git\\b[^\\n]*\\breset\\b[^\\n]*--hard\\b"))
        (list "clean-force" "it deletes untracked files"
              (anchored "git\\b[^\\n]*\\bclean\\b[^\\n]*-[a-zA-Z]*f"))
        (list "mkfs" "it makes a filesystem" (anchored "mkfs\\b"))
        (list "device-write" "it writes onto a device node" "(?:>|of=)\\s*/dev/(?:sd|nvme|disk|mmcblk)")
        (list "host-power" "it changes the host's power state"
              (anchored "(?:shutdown|reboot|poweroff|halt)\\b|systemctl\\s+(?:poweroff|reboot|halt|shutdown)\\b"))
        (list "world-writable" "it makes a path world-writable"
              (anchored "chmod\\s+(?:-[a-zA-Z]+\\s+)*777\\b"))
        (list "pipe-to-shell" "it pipes a download into a shell"
              "\\b(?:curl|wget)\\b[^\\n]*\\|[^\\n]*(?:ba|z)?sh\\b"))
  "The dangerous shapes the original's host asked about, read against a command's own text.")

(defun custom-rules ()
  "The operator's own patterns, each a rule that reads a command's text."
  (loop for pattern in (setting :patterns)
        for index from 1
        collect (list (format nil "custom-~d" index)
                      (format nil "it matches the pattern ~s" pattern)
                      pattern)))

(defun matching-rule (text)
  "The first rule one of TEXT's commands earns. => (VALUES ID REASON), or NIL."
  (let ((rules (append +rules+ (custom-rules)))
        (lines (uiop:split-string (command-lines text) :separator '(#\Newline))))
    (dolist (line lines)
      (loop for (id reason pattern) in rules
            when (ignore-errors (cl-ppcre:scan pattern line))
              do (return-from matching-rule (values id reason))))
    nil))

;;; --- the notification -------------------------------------------------------------

(defun clip (text limit)
  (let ((data (if (stringp text) text (format nil "~a" text))))
    (if (<= (length data) limit)
        data
        (concatenate 'string (subseq data 0 (- limit 3)) "..."))))

(defun answer-window (seconds)
  (let ((seconds (max 0 (round seconds))))
    (if (< seconds 90) (format nil "~d s" seconds) (format nil "~d min" (round seconds 60)))))

(defun answer-body (request-id choice code)
  (format nil "~a ~a ~a ~a" +reply-prefix+ request-id choice code))

(defun build-notification (settings request-id codes message)
  "The JSON ntfy publishes: three http buttons, each posting its own code."
  (let ((reply-url (format nil "~a/~a~a" (getf settings :server) (getf settings :topic) +reply-suffix+)))
    (nlk:json-object
     "topic" (getf settings :topic)
     "title" "Nodecode needs your approval"
     "message" message
     "priority" (getf settings :priority)
     "tags" (coerce (list "warning") 'vector)
     "actions" (coerce
                (loop for (choice . label) in +buttons+
                      for code = (cdr (assoc choice codes :test #'string=))
                      when code
                        collect (nlk:json-object
                                 "action" "http" "label" label "url" reply-url
                                 "method" "POST"
                                 "body" (answer-body request-id choice code)
                                 "clear" t))
                'vector))))

(defun request-message (rule reason command timeout)
  (format nil "~a: ~a.~%~%~a~%~%Answer within ~a. No answer means deny."
          rule reason (clip command 2000) (answer-window timeout)))

;;; --- the round trip ----------------------------------------------------------------

(defun publish (server token body)
  "POST one notification. => its message hash table."
  (multiple-value-bind (text status)
      (funcall *http* "POST" (concatenate 'string server "/")
               :headers (headers token) :content body :timeout +publish-timeout+)
    (unless (and (integerp status) (<= 200 status 299))
      (error 'ntfy-error :text (format nil "the ntfy server answered HTTP ~a" status)))
    (let ((message (ignore-errors (shasht:read-json text))))
      (unless (and (hash-table-p message) (stringp (gethash "id" message)))
        (error 'ntfy-error :text "the ntfy server answered without a message id"))
      message)))

(defun withdraw (server token topic message-id)
  "Best effort: take the notification off every device (ntfy 2.16+)."
  (when message-id
    (ignore-errors
      (funcall *http* "DELETE" (format nil "~a/~a/~a" server topic message-id)
               :headers (headers token) :timeout +withdraw-timeout+))))

(defun reply-events (server token topic since)
  "The answer messages cached on the reply topic since SINCE."
  (let ((url (format nil "~a/~a~a/json?poll=1&since=~a" server topic +reply-suffix+ since)))
    (multiple-value-bind (text status)
        (funcall *http* "GET" url :headers (headers token) :timeout +poll-timeout+)
      (when (and (integerp status) (<= 200 status 299) (stringp text))
        (loop for line in (uiop:split-string text :separator '(#\Newline))
              for event = (ignore-errors (shasht:read-json line))
              when (and (hash-table-p event)
                        (equal "message" (gethash "event" event)))
                collect event)))))

(defun match-answer (text request-id codes)
  "The choice TEXT carries, if it answers this request with a code this request drew."
  (let ((parts (and (stringp text) (uiop:split-string text :separator " "))))
    (when (and (= 5 (length parts))
               (equal (format nil "~a ~a" (first parts) (second parts)) +reply-prefix+))
      (destructuring-bind (prefix version reply-id choice code) parts
        (declare (ignore prefix version))
        (let ((expected (cdr (assoc choice codes :test #'string=))))
          (when (and expected (equal reply-id request-id) (equal code expected))
            choice))))))

(defun answer-among (events request-id codes)
  (loop for event in events
        for choice = (match-answer (gethash "message" event) request-id codes)
        when choice return choice))

(defun wait-for-answer (server token topic since request-id codes deadline)
  "The choice tapped before DEADLINE, or NIL."
  (loop
    (let ((choice (answer-among (reply-events server token topic since) request-id codes)))
      (when choice (return choice)))
    (when (>= (funcall *clock*) deadline) (return nil))
    (funcall *sleep* 1)))

(defun ask (rule reason command)
  "Send one approval to the phone and wait. => (VALUES CHOICE PROBLEM):
CHOICE is \"once\", \"session\" or \"deny\"; a NIL CHOICE with a PROBLEM means
the call could not be approved."
  (multiple-value-bind (settings problem) (wire-settings)
    (if (null settings)
        (values nil (format nil "ntfy-approval is not set up: ~a" problem))
        (let* ((timeout (or (setting :timeout-seconds) 120))
               (request-id (random-text 16))
               (codes (answer-codes (mapcar #'car +buttons+)))
               (server (getf settings :server))
               (topic (getf settings :topic))
               (token (getf settings :token))
               (message (build-notification settings request-id codes
                                             (request-message rule reason
                                                              (if (setting :send-command)
                                                                  command
                                                                  "(the command is not sent: send_command is off)")
                                                              timeout)))
               (deadline (+ (funcall *clock*) timeout)))
          (handler-case
              (let* ((published (publish server token (json-text message)))
                     (since (gethash "time" published))
                     (id (gethash "id" published)))
                (unwind-protect
                     (values (wait-for-answer server token topic since request-id codes deadline) nil)
                  (withdraw server token topic id)))
            (ntfy-error (e) (values nil (ntfy-error-text e)))
            (error (e) (values nil (format nil "the ntfy server could not be reached (~a)" e))))))))

;;; --- the hook --------------------------------------------------------------------

(defparameter *session-allows* (make-hash-table :test #'equal)
  "Choices of `Approve for session': a (SESSION-ID . RULE-ID) here no longer asks.")

(defun session-id ()
  (getf (nle:turn) :session-id))

(defun allowed-this-session-p (rule)
  (gethash (cons (session-id) rule) *session-allows*))

(defun remember-for-session (rule)
  (setf (gethash (cons (session-id) rule) *session-allows*) t))

(defun refusal-text (rule reason choice problem timeout)
  "The model's words for a call the phone did not approve."
  (cond ((equal choice "deny")
         (format nil "ERROR: refused by nodecode-ntfy-approval: ~a (~a); the operator was asked on their phone and chose Deny; the call did not run -- do not retry it or work around it: ask them what they want instead"
                 reason rule))
        ((and (null choice) problem)
         (format nil "ERROR: refused by nodecode-ntfy-approval: ~a (~a); ~a; the call did not run -- give the operator the command to run themselves"
                 reason rule problem))
        (t (format nil "ERROR: refused by nodecode-ntfy-approval: ~a (~a); no answer on the phone within ~a, which is a deny; the call did not run -- give the operator the command to run themselves"
                   reason rule (answer-window timeout)))))

(defun gate (op next)
  "The :TOOL point: a matched call is asked about on the phone, and runs only on approval."
  (let* ((arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments))))
    (if (not (stringp form))
        (funcall next op)
        (multiple-value-bind (commands mode) (commands-of form strings)
          (declare (ignore mode))
          (multiple-value-bind (rule reason) (and commands (matching-rule commands))

            (cond ((null rule) (funcall next op))
                  ((allowed-this-session-p rule) (funcall next op))
                  (t (multiple-value-bind (choice problem) (ask rule reason (or commands form))
                       (cond ((equal choice "once") (funcall next op))
                             ((equal choice "session")
                              (remember-for-session rule)
                              (funcall next op))
                             (t (nle:failure (refusal-text rule reason choice problem
                                                           (or (setting :timeout-seconds) 120)))))))))))))

;;; --- /ntfy -----------------------------------------------------------------------

(defun shown-topic (topic)
  (if (> (length topic) 12) (concatenate 'string (subseq topic 0 12) "...") topic))

(defun status-text ()
  (multiple-value-bind (settings problem) (wire-settings)
    (if (null settings)
        (format nil "ntfy-approval: not set up -- ~a" problem)
        (format nil "ntfy-approval: ~a on ~a, ~a, token ~:[not set~;set~]; /ntfy test sends one sample"
                (shown-topic (getf settings :topic))
                (getf settings :server)
                (if (setting :send-command) "the command is sent" "only the reason is sent")
                (and (getf settings :token) t)))))

(defun setup-text ()
  (let* ((topic (new-topic))
         (server (handler-case (normalize-server (setting :server))
                   (ntfy-error () +default-server+))))
    (format nil "~{~a~^~%~}"
            (list
             "ntfy-approval setup"
             ""
             (format nil "1. Install the ntfy app (https://ntfy.sh/app) and subscribe to this topic on ~a:" server)
             ""
             (format nil "     ~a" topic)
             ""
             "   Anyone who knows this name can read your approval requests and answer them; keep it private."
             ""
             "2. Set it for this home, under ntfy-approval in /settings:"
             ""
             (format nil "     topic = ~a" topic)
             ""
             "   On a server with access control, name the variable holding the token in token_env:"
             "   it is read from the environment, never written in the config."
             ""
             "3. Check that a tap on your phone reaches Nodecode:"
             ""
             "     /ntfy test"))))

(defun test-text ()
  "Send one sample request -- nothing runs -- and report the tap."
  (multiple-value-bind (choice problem)
      (ask "test" "this is a test, nothing runs" "echo 'hello from /ntfy test'")
    (cond ((member choice '("once" "session") :test #'string=)
           (format nil "Your phone answered: ~a. Approvals from the phone work."
                   (cdr (assoc choice +buttons+ :test #'string=))))
          ((equal choice "deny") "Your phone answered: Deny. Approvals from the phone work.")
          (t (format nil "No approval came back: ~a. Is the phone subscribed to the topic on ~a?"
                     problem
                     (handler-case (normalize-server (setting :server))
                       (ntfy-error () +default-server+)))))))

(defun run-command (args session-id)
  "The answer to one /ntfy invocation: setup, test, or the current state."
  (declare (ignore session-id))
  (let ((action (string-downcase
                 (or (first (uiop:split-string (or args "") :separator '(#\Space #\Tab))) ""))))
    (cond ((equal action "setup") (setup-text))
          ((equal action "test") (test-text))
          ((zerop (length action)) (status-text))
          (t "usage: /ntfy [setup|test] -- setup makes a topic, test sends one sample approval"))))

;;; --- the cell ---------------------------------------------------------------------

(defun check-section (values table)
  "The declared values, with a topic that is set checked for shape at start."
  (declare (ignore table))
  (let* ((token-name (non-empty (getf values :token-env)))
         (topic (non-empty (getf values :topic)))
         (problem (and topic (topic-problem topic (and token-name (uiop:getenv token-name))))))
    (when problem (nlk:config-error "~a" problem))
    values))

(nlk:define-peripheral ntfy-approval :not-running t)

(nle:define-cell ntfy-approval
  (:section ("ntfy-approval")
    (:guide "topic is a long random name on the ntfy server the phone subscribes to (/ntfy setup makes one); token_env names the variable holding an access token, for a server with access control; a matched call runs only when a button is tapped -- no answer is a deny, so a public server's topic name is the secret")
    ("server" :string :default "https://ntfy.sh"
     :doc "the ntfy server the phone is subscribed on; an own server keeps approvals private")
    ("topic" :string
     :doc "the topic a notification is published to; treat it as a password on a public server")
    ("token_env" :env
     :doc "environment variable holding an ntfy access token, for a server with access control")
    ("priority" :choice :options '("min" "low" "default" "high" "urgent") :default "high"
     :doc "the notification's priority")
    ("send_command" :boolean :default t
     :doc "send the command's text; off sends only why the call was stopped")
    ("timeout_seconds" :integer :default 120 :min 10
     :doc "how long the call waits for a tap; no answer within it is a deny")
    ("patterns" :list
     :doc "extra regular expressions, matched against a command's text, that also need a tap"))
  (:settings #'check-section)
  (:hook :tool #'gate)
  (:help :ntfy-approval
         "ntfy-approval: risky shell calls wait for a tap on the operator's phone; /ntfy setup|test"
         "ntfy-approval asks the operator's phone, through ntfy, before a dangerous shell command runs. It reads the commands an eval call runs and matches them against a rule table (deleting a tree, sudo, a force push, a hard reset, mkfs, a write onto a device, a shutdown, chmod 777, a download piped into a shell) plus any `patterns` the section adds. A matched call becomes a push notification with three buttons -- Approve once, Approve for session, Deny -- and the call runs only when one of the first two is tapped. No answer within `timeout_seconds`, a server that cannot be reached, and a section with no topic all deny, and the refusal says which. `/ntfy setup` makes a topic and prints the steps; `/ntfy test` sends one sample approval and reports the tap; `/ntfy` alone shows the state. The notification carries the rule, its reason, the command's text (turn `send_command` off to withhold it) and one random code per button, to the configured ntfy server only. On a public server the topic name is the secret, so the cell refuses a topic shorter than 16 characters when no access token is named.")
  (:command "ntfy" #'run-command
            :description "Approve risky commands from your phone: setup, test"
            :argument-hint "setup|test")
  (:start (lambda ()
            (clrhash *session-allows*)
            (nle:on-stop (lambda () (clrhash *session-allows*))))))
