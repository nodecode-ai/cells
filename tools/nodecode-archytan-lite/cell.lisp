;;;; cell.lisp --- Archytan Lite: a policy gate on every tool call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of High-ArchyTech-Solutions's hermes-archytan-lite (see NOTICE).
;;;; Before a call runs, it is mapped to the actions it asks for -- the
;;;; programs it runs, the files it reads and writes, the hosts it fetches, the
;;;; tool itself -- and every one of them must be granted by the operator's
;;;; policy. One ungranted action holds the whole call, and the refusal is the
;;;; call's failure (NLE:FAILURE), so the transcript draws it failed.
;;;;
;;;; The original asks a gate out of process over HTTP and keeps an Ed25519
;;;; receipt chain. A Nodecode cell has no second process to trust, so the
;;;; policy file is the gate: the cell reads it (cached by write date), the
;;;; checks and the matching are the plugin's, and every decision is appended
;;;; to a JSONL journal. Edit the policy and the next call follows it.
;;;;
;;;; Fail-closed: no workspace, no policy, a policy that is not JSON, a
;;;; command the form does not show, or an action no rule names is a refusal,
;;;; never a pass.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "archytan-lite": {"workspace": "/home/me/work",
;;;;                "policy": "~/.nodecode/archytan/policy.json",
;;;;                "journal": "", "ungated": []}
;;;; The policy, one rule per action:
;;;;   {"version": 1,
;;;;    "actions": [{"action": "shell.run",
;;;;                 "grants": [{"resource_ids": ["ls", "git/status"]}],
;;;;                 "assertions": {"max_per_window": 60, "window_seconds": 60}}]}
;;;; A resource is a path, a program (git/push for a tool with subcommands),
;;;; a host (https/github.com) or a tool name; `*' is one path segment, `**'
;;;; the rest of one, and a match is case-insensitive. /archytan setup writes
;;;; a starter policy for the workspace.

(defpackage #:nodecode-archytan-lite
  (:use #:cl)
  (:nicknames #:archytan)
  (:export #:check #:status #:decisions
           ;; the one condition
           #:archytan-lite-error))

(in-package #:nodecode-archytan-lite)

(nlk:define-peripheral archytan-lite :not-running t)

;;; --- paths ------------------------------------------------------------------

(defun setting-text (value)
  "VALUE as the trimmed string it reads as; \"\" when it says nothing."
  (string-trim '(#\Space #\Tab) (princ-to-string (or value ""))))

(defun json-escape (text)
  "TEXT with the two characters JSON cannot carry raw escaped."
  (with-output-to-string (out)
    (loop for c across text
          do (cond ((char= c (code-char 92)) (write-string (format nil "~c~c" (code-char 92) (code-char 92)) out))
                   ((char= c (code-char 34)) (write-string (format nil "~c~c" (code-char 92) (code-char 34)) out))
                   (t (write-char c out))))))

(defun expand-path (text)
  "TEXT as an absolute pathname string: ~/ is the operator's home, and a
relative path is taken from the process's own directory."
  (let ((text (setting-text text)))
    (cond ((zerop (length text)) nil)
          ((uiop:string-prefix-p "~/" text)
           (namestring (merge-pathnames (subseq text 2) (user-homedir-pathname))))
          ((string= text "~") (namestring (user-homedir-pathname)))
          (t (namestring (uiop:ensure-pathname text :want-absolute t))))))

(defun normalize-path (text)
  "TEXT with `.`, `..` and repeated separators read lexically, as one path,
case kept: an absolute path stays absolute."
  (let* ((text (substitute #\/ (code-char 92) (princ-to-string text)))
         (drive (and (>= (length text) 2)
                     (alpha-char-p (char text 0))
                     (char= #\: (char text 1))
                     (subseq text 0 2)))
         (rest (if drive (subseq text 2) text))
         (absolute (or drive (and (plusp (length rest)) (char= #\/ (char rest 0)))))
         (stack '()))
    (dolist (part (uiop:split-string rest :separator '(#\/)))
      (cond ((or (string= part "") (string= part ".")) nil)
            ((string= part "..") (when stack (pop stack)))
            (t (push part stack))))
    (let* ((stack (nreverse stack))
           (body (format nil "~{~a~^/~}" stack)))
      (cond (drive (format nil "~a/~a" drive body))
            ((string= body "") (if absolute "/" "."))
            (absolute (format nil "/~a" body))
            (t body)))))

(defun absolute-path (text base)
  "TEXT read against BASE, or the home for ~, then normalized."
  (let ((text (setting-text text))
        (base (and base (setting-text base))))
    (cond ((zerop (length text)) nil)
          ((char= #\~ (char text 0)) (normalize-path (expand-path text)))
          ((char= #\/ (char text 0)) (normalize-path text))
          ((and (> (length text) 1) (char= #\: (char text 1))) (normalize-path text))
          ((or (null base) (zerop (length base))) (normalize-path (format nil "/~a" text)))
          (t (normalize-path (format nil "~a/~a" (string-right-trim "/" base) text))))))

(defun path-basename (text)
  "TEXT after its last separator."
  (let ((slash (position #\/ text :from-end t)))
    (if slash (subseq text (1+ slash)) text)))

(defun pathish-p (token)
  "T: TOKEN names a file or a directory rather than a word of a command."
  (and (stringp token)
       (plusp (length token))
       (or (char= #\/ (char token 0))
           (char= #\~ (char token 0))
           (uiop:string-prefix-p "./" token)
           (uiop:string-prefix-p "../" token)
           (and (> (length token) 1) (char= #\: (char token 1))))))

;;; --- the policy -------------------------------------------------------------

(defstruct (gate-rule (:constructor make-gate-rule (action resources max window)))
  "One action's rule: which RESOURCES it grants, and how many times in WINDOW
seconds MAX allows; MAX NIL is no limit."
  action resources max window)

(defstruct (gate (:constructor make-gate (&key workspace policy-path journal-path)))
  workspace policy-path journal-path
  (cached :missing)
  (stamp nil)
  (windows (make-hash-table :test #'equal)))

(defun path-setting (value fallback)
  "VALUE as a path, or the FALLBACK under this home when it says nothing."
  (let ((text (setting-text value)))
    (if (zerop (length text))
        (namestring (nlk:home fallback))
        (expand-path text))))

(defun make-gate-for (values)
  "The gate the section's VALUES describe."
  (let ((workspace (setting-text (getf values :workspace))))
    (make-gate :workspace (unless (zerop (length workspace)) (expand-path workspace))
               :policy-path (path-setting (getf values :policy) "archytan/policy.json")
               :journal-path (path-setting (getf values :journal) "archytan/decisions.jsonl"))))

(defun resource-ids (entry)
  "Every resource id ENTRY's grants name."
  (loop for grant across (or (nlk:json-array entry "grants") #())
        when (hash-table-p grant)
          append (loop for id across (or (nlk:json-array grant "resource_ids") #())
                       when (stringp id) collect id)))

(defun assertion (entry key)
  "ENTRY's assertions.KEY as an integer, or NIL."
  (let ((table (nlk:json-value entry :object "assertions")))
    (and (hash-table-p table) (nlk:json-value table :integer key))))

(defun parse-policy (path)
  "The rules the policy file at PATH declares, or :INVALID."
  (let ((document (and path (ignore-errors (nlk:decode-json (nlk:read-text path 1000000))))))
    (if (not (hash-table-p document))
        :invalid
        (let ((entries (nlk:json-array document "actions")))
          (if (or (null entries) (zerop (length entries)))
              :invalid
              (remove nil
                      (loop for entry across entries
                            when (hash-table-p entry)
                              collect (let ((action (nlk:config-string entry "action")))
                                        (and action
                                             (make-gate-rule action (resource-ids entry)
                                                             (assertion entry "max_per_window")
                                                             (or (assertion entry "window_seconds") 60)))))))))))

(defun file-stamp (path)
  "PATH's write date and size, or NIL when there is no file."
  (let ((file (and path (probe-file path))))
    (and file
         (cons (file-write-date file)
               (ignore-errors (with-open-file (in file :element-type '(unsigned-byte 8))
                                (file-length in)))))))

(defun gate-rules (gate)
  "The rules the policy file declares now: a list, :MISSING or :INVALID.
Re-read when the file's write date or size moved, so an edit applies to the
next call."
  (let* ((path (gate-policy-path gate))
         (stamp (file-stamp path)))
    (cond ((null stamp) (setf (gate-cached gate) :missing (gate-stamp gate) nil))
          ((not (equal stamp (gate-stamp gate)))
           (setf (gate-stamp gate) stamp
                 (gate-cached gate) (parse-policy path))))
    (gate-cached gate)))

(defun pattern-regex (pattern)
  "PATTERN as a regex: `**' is the rest of the path, `*' one segment."
  (let ((out (make-string-output-stream)))
    (loop with text = (string-downcase pattern)
          with i = 0
          with n = (length text)
          while (< i n)
          do (let ((c (char text i)))
               (cond ((and (char= c #\*) (< (1+ i) n) (char= #\* (char text (1+ i))))
                      (write-string ".*" out)
                      (incf i))
                     ((char= c #\*) (write-string "[^/]*" out))
                     ((find c ".+?()[]{}^$|~" :test #'char=)
                      (write-char (code-char 92) out)
                      (write-char c out))
                     (t (write-char c out))))
             (incf i))
    (format nil "\\A~a\\z" (get-output-stream-string out))))

(defun pattern-matches-p (pattern resource)
  "T: RESOURCE is PATTERN. Paths compare without case, so a policy written on
one platform reads the same on the next."
  (and (stringp pattern)
       (stringp resource)
       (plusp (length pattern))
       (cl-ppcre:scan (pattern-regex pattern) (string-downcase resource))))

(defun window-open-p (gate rule)
  "T: RULE's limit has room, counting only the uses inside its window."
  (let ((max (gate-rule-max rule)))
    (if (or (null max) (not (plusp max)))
        t
        (let* ((now (get-internal-real-time))
               (span (* (or (gate-rule-window rule) 60) internal-time-units-per-second))
               (kept (remove-if (lambda (at) (> (- now at) span))
                                (gethash (gate-rule-action rule) (gate-windows gate)))))
          (setf (gethash (gate-rule-action rule) (gate-windows gate)) kept)
          (< (length kept) max)))))

(defun note-use (gate rule)
  "Count one use of RULE, when it counts."
  (when (and (gate-rule-max rule) (plusp (gate-rule-max rule)))
    (push (get-internal-real-time) (gethash (gate-rule-action rule) (gate-windows gate)))))

(defun limit-text (rule)
  "How RULE's limit reads in a refusal."
  (format nil "~d ~a in ~d seconds are used" (gate-rule-max rule)
          (gate-rule-action rule) (or (gate-rule-window rule) 60)))

(defun decide (gate checks)
  "Every CHECK (ACTION RESOURCE WHY) against GATE's rules.
=> (values ALLOWED DECISIONS), one decision plist per check."
  (let ((rules (gate-rules gate)))
    (cond
      ((eq rules :missing)
       (values nil (list (list :action "policy" :resource (or (gate-policy-path gate) "")
                               :allowed nil :why "the policy file"
                               :reason (format nil "there is no policy file at ~a (run /archytan setup for a starter one)" (gate-policy-path gate))))))
      ((eq rules :invalid)
       (values nil (list (list :action "policy" :resource (or (gate-policy-path gate) "")
                               :allowed nil :why "the policy file"
                               :reason (format nil "~a is not JSON with an actions array" (gate-policy-path gate))))))
      (t
       (let ((decisions '()) (used '()) (all t))
         (dolist (check checks)
           (destructuring-bind (action resource why) check
             (let* ((rule (find action rules :key #'gate-rule-action :test #'string-equal))
                    (reason (cond ((null rule)
                                   (format nil "the policy has no rule for ~a" action))
                                  ((not (some (lambda (pattern) (pattern-matches-p pattern resource))
                                              (gate-rule-resources rule)))
                                   (format nil "the policy grants no ~a on \"~a\"" action resource))
                                  ((not (window-open-p gate rule)) (limit-text rule)))))
               (cond (reason (setf all nil))
                     (rule (push rule used)))
               (push (list :action action :resource resource :allowed (null reason)
                           :why why :reason (or reason ""))
                     decisions))))
         (when all
           (dolist (rule used) (note-use gate rule)))
         (values all (nreverse decisions)))))))

;;; --- the actions a call asks for ---------------------------------------------

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defparameter +write-operators+ '("write-file" "edit")
  "The heads whose first argument is a file the call writes.")

(defparameter +read-operators+ '("look" "read-file")
  "The heads whose first argument is a file the call reads.")

(defparameter +fetch-operators+ '("http" "fetch" "http-get" "http-post" "http-request")
  "The heads that reach a URL.")

(defparameter +subcommand-programs+ '("git" "docker" "gh" "cargo" "kubectl" "npm" "pnpm" "systemctl")
  "The programs whose first word after it is part of what it does: git/push.")

(defparameter +shell-breaks+ '(#\; #\& #\| #\Newline)
  "An unquoted one of these starts the next command.")

(defparameter +blanks+ '(#\Space #\Tab #\Return)
  "What separates one word of a command from the next.")

(defun operator-name (x)
  "X's symbol name, lowercased, or NIL."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun free-text (x strings)
  "X's text when the form shows it: a string, a ($ \"name\") the strings
object carries, or (list ...) of strings. NIL when it does not."
  (cond ((stringp x) x)
        ((and (consp x) (equal "$" (operator-name (car x))) (stringp (second x)))
         (let ((value (and (hash-table-p strings) (gethash (second x) strings))))
           (and (stringp value) value)))
        ((and (consp x) (member (operator-name (car x)) '("list" "quote") :test #'equal))
         (let ((items (if (equal "quote" (operator-name (car x))) (second x) (rest x))))
           (when (and (listp items) items (every #'stringp items))
             (format nil "~{~a~^ ~}" items))))))

(defun spawn-text (call strings)
  "CALL's command text, or :HIDDEN when one of its arguments hides it."
  (let ((args (loop for rest on (rest call)
                    when (not (keywordp (first rest))) collect (first rest)))
        (parts '()))
    (if (null args)
        :hidden
        (dolist (arg args (if parts (format nil "~{~a~^ ~}" (nreverse parts)) :hidden))
          (let ((text (free-text arg strings)))
            (if text (push text parts) (return :hidden)))))))

(defun unreadable-command-p (text)
  "T: only the shell settles what TEXT means -- a variable, a substitution, a
backtick or a heredoc -- so no check can be honest about it."
  (loop with open = nil
        for i from 0 below (length text)
        do (let ((c (char text i)))
             (cond ((and open (char= c open)) (setf open nil))
                   (open nil)
                   ((or (char= c #\') (char= c (code-char 34))) (setf open c))
                   ((char= c (code-char 92)) (incf i))
                   ((or (char= c #\$) (char= c #\`)) (return t))
                   ((and (char= c #\<) (< (1+ i) (length text))
                         (char= (char text (1+ i)) #\<))
                    (return t))))
        finally (return nil)))

(defun command-segments (text)
  "TEXT cut where one command ends and the next begins, quotes respected."
  (let ((segments '()) (start 0) (i 0) (n (length text)) (open nil))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((and open (char= c open)) (setf open nil))
                     (open nil)
                     ((or (char= c #\') (char= c (code-char 34))) (setf open c))
                     ((char= c (code-char 92)) (incf i))
                     ((member c +shell-breaks+ :test #'char=)
                      (push (subseq text start i) segments)
                      (setf start (1+ i))))
               (incf i)))
    (push (subseq text start) segments)
    (remove-if (lambda (segment) (zerop (length (string-trim " " segment))))
               (nreverse segments))))

(defun segment-tokens (text)
  "TEXT cut into words, quotes dropped, a backslash escaping the next character."
  (let ((tokens '()) (word (make-string-output-stream)) (open nil) (i 0) (n (length text)))
    (labels ((flush ()
               (let ((word (get-output-stream-string word)))
                 (when (plusp (length word)) (push word tokens)))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((and open (char= c open)) (setf open nil))
                       (open (write-char c word))
                       ((or (char= c #\') (char= c (code-char 34))) (setf open c))
                       ((char= c (code-char 92)) (incf i) (when (< i n) (write-char (char text i) word)))
                       ((member c +blanks+ :test #'char=) (flush))
                       (t (write-char c word))))
               (incf i))
      (flush))
    (nreverse tokens)))

(defun assignment-token-p (token)
  "T: TOKEN is NAME=value, an assignment before the command."
  (let ((equals (and (stringp token) (position #\= token))))
    (and equals (plusp equals)
         (loop for c across (subseq token 0 equals)
               always (or (alphanumericp c) (char= c #\_))))))

(defun redirect-token (token)
  "=> (values KIND TARGET) when TOKEN redirects, KIND :WRITE or :READ."
  (cond ((uiop:string-prefix-p "2>>" token) (values :write (subseq token 3)))
        ((uiop:string-prefix-p "2>" token) (values :write (subseq token 2)))
        ((uiop:string-prefix-p ">>" token) (values :write (subseq token 2)))
        ((uiop:string-prefix-p ">" token) (values :write (subseq token 1)))
        ((uiop:string-prefix-p "<" token) (values :read (subseq token 1)))
        (t (values nil nil))))

(defun program-id (program tokens)
  "PROGRAM as the policy names it: the basename, plus its subcommand when the
program has one."
  (let* ((name (string-downcase (or (path-basename program) program)))
         (sub (and (member name +subcommand-programs+ :test #'equal)
                   (find-if (lambda (token) (and (plusp (length token))
                                                 (not (char= #\- (char token 0)))))
                            tokens))))
    (if sub (format nil "~a/~a" name (string-downcase sub)) name)))

(defun segment-checks (segment base)
  "The checks one command of SEGMENT asks for."
  (let ((tokens (segment-tokens segment)) (checks '()))
    (loop while (and tokens (assignment-token-p (first tokens)))
          do (push (list "shell.env" (subseq (first tokens) 0 (position #\= (first tokens)))
                         "the environment it sets")
                   checks)
             (pop tokens))
    (when tokens
      (let ((program (pop tokens))
            (rest '()))
        (if (string-equal program "cd")
            (let ((target (pop tokens)))
              (when (and target (pathish-p target))
                (push (list "file.read" (absolute-path target base) "the directory it enters") checks)))
            (push (list "shell.run" (program-id program tokens) "the program it runs") checks))
        (setf rest tokens)
        (loop while rest
              do (let ((token (pop rest)))
                   (multiple-value-bind (kind target) (redirect-token token)
                     (cond
                       (kind
                        (let ((target (if (and target (plusp (length target))) target (pop rest))))
                          (when (and target (plusp (length target)) (not (char= #\& (char target 0))))
                            (push (list (if (eq kind :write) "file.write" "file.read")
                                        (absolute-path target base)
                                        (if (eq kind :write) "the file it writes" "the file it reads"))
                                  checks))))
                       ((pathish-p token)
                        (push (list "file.read" (absolute-path token base) "the file it reads") checks))))))))
  (nreverse checks)))

(defun command-checks (text base)
  "=> (values CHECKS UNREADABLE) for the shell command TEXT."
  (if (or (null text) (zerop (length text)) (unreadable-command-p text))
      (values nil t)
      (let ((checks '()))
        (dolist (segment (command-segments text))
          (dolist (check (segment-checks segment base)) (push check checks)))
        (values (nreverse checks) nil))))

(defun url-host (url)
  "URL as the policy names its host: https/github.com."
  (let* ((mark (search "://" url))
         (scheme (if mark (string-downcase (subseq url 0 mark)) "https"))
         (rest (subseq url (if mark (+ mark 3) 0)))
         (end (or (position-if (lambda (c) (find c "/?#:" :test #'char=)) rest) (length rest))))
    (format nil "~a/~a" scheme (string-downcase (subseq rest 0 end)))))

(defun url-checks (call)
  "The web.fetch checks every URL literal in CALL asks for."
  (let ((checks '()))
    (labels ((walk (x)
               (cond ((stringp x)
                      (dolist (url (cl-ppcre:all-matches-as-strings "https?://[^\\s\\\"<>)]+" x))
                        (push (list "web.fetch" (url-host url) "the host it fetches") checks)))
                     ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk (rest call)))
    checks))

(defun form-checks (text strings base)
  "=> (values CHECKS HIDDEN) for the form TEXT, read as code."
  (let ((checks '()) (hidden nil))
    (labels ((note (check) (push check checks))
             (walk (x)
               (when (consp x)
                 (let ((name (operator-name (car x))))
                   (cond
                     ((member name +spawn-operators+ :test #'equal)
                      (let ((command (spawn-text x strings)))
                        (if (eq command :hidden)
                            (setf hidden t)
                            (multiple-value-bind (found unreadable) (command-checks command base)
                              (when unreadable (setf hidden t))
                              (dolist (check found) (note check))))))
                     ((member name +write-operators+ :test #'equal)
                      (let ((path (free-text (second x) strings)))
                        (if (null path)
                            (setf hidden t)
                            (note (list "file.write" (absolute-path path base) "the file it writes")))))
                     ((member name +read-operators+ :test #'equal)
                      (let ((path (free-text (second x) strings)))
                        (when path
                          (note (list "file.read" (absolute-path path base) "the file it reads")))))
                     ((member name +fetch-operators+ :test #'equal)
                      (dolist (check (url-checks x)) (note check)))))
                 (mapc #'walk x))))
      (when (stringp text)
        (ignore-errors
          (with-input-from-string (in text)
            (let ((*read-eval* nil) (*package* (find-package :nodecode-archytan-lite)))
              (loop for form = (read in nil :eof)
                    until (eq form :eof)
                    do (walk form))))))
    (values (nreverse checks) hidden))))

(defun call-checks (tool form-text strings base)
  "The checks a call of TOOL asks for: what its form shows, else the tool."
  (multiple-value-bind (checks hidden) (form-checks form-text strings base)
    (when hidden
      (push (list "shell.run" "unreadable-command" "a command the form does not show") checks))
    (or checks (list (list "tool.call" tool "the tool it calls")))))

;;; --- the decision, and the journal -------------------------------------------

(defun decision-line (decision)
  "One decision as a person reads it."
  (format nil "~a on \"~a\" ~a" (getf decision :action) (getf decision :resource)
          (if (getf decision :allowed)
              "allowed"
              (format nil "refused (~a)" (getf decision :reason)))))

(defun refusal-text (gate decisions)
  "The model's words for a call that did not run."
  (let ((refused (remove-if (lambda (decision) (getf decision :allowed)) decisions)))
    (format nil "ERROR: refused by nodecode-archytan-lite: ~{~a~^; ~}. The call did not run -- do not retry it unchanged and do not work around it: tell the operator, who grants it in ~a."
            (mapcar #'decision-line refused) (or (gate-policy-path gate) "its policy"))))

(defun journal-decisions (gate tool decisions)
  "Append one line per decision to GATE's journal, when it has one."
  (let ((path (gate-journal-path gate)))
    (when (and path (plusp (length path)))
      (ignore-errors
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :append
                                  :if-does-not-exist :create :external-format :utf-8)
          (dolist (decision decisions)
            (write-line (nlk:encode-json-object
                         (nlk:json-object "ts" (nlk:iso-now) "tool" tool
                                          "action" (getf decision :action)
                                          "resource" (getf decision :resource)
                                          "decision" (if (getf decision :allowed) "allow" "block")
                                          "reason" (getf decision :reason)))
                        out)))))))

(defun journal-lines (&optional (limit 20))
  "The last LIMIT lines of the journal, oldest first."
  (let* ((path (and (setting :gate) (gate-journal-path (setting :gate))))
         (text (and path (nlk:read-text path 2000000)))
         (lines (and text (uiop:split-string text :separator '(#\Newline)))))
    (last (remove "" lines :test #'equal) limit)))

(defun journal-counts ()
  "=> (values ALLOW BLOCK) over the journal."
  (let ((allow 0) (block 0))
    (dolist (line (journal-lines 100000))
      (cond ((search "\"decision\":\"allow\"" line) (incf allow))
            ((search "\"decision\":\"block\"" line) (incf block))))
    (values allow block)))

;;; --- what the operator reads -------------------------------------------------

(defun grant-count (gate)
  "How many actions the policy names, or why it names none."
  (let ((rules (gate-rules gate)))
    (cond ((eq rules :missing) "no policy file")
          ((eq rules :invalid) "the policy does not read")
          (t (format nil "~d actions" (length rules))))))

(defun status ()
  "Where the policy is, what it grants, and what the journal says: no network."
  (let ((gate (setting :gate)))
    (multiple-value-bind (allow block) (journal-counts)
      (format nil "archytan-lite: workspace ~a; policy ~a (~a); ~d allow, ~d block in ~a~@[; no workspace is set -- every call is held until one is~]"
              (or (gate-workspace gate) "none") (gate-policy-path gate) (grant-count gate)
              allow block (gate-journal-path gate)
              (null (gate-workspace gate))))))

(defun log-text (limit)
  "The last LIMIT decisions, as the journal wrote them."
  (let ((lines (journal-lines limit)))
    (if lines
        (format nil "archytan-lite: the last ~d decision~:p:~%~{~a~%~}" (length lines) lines)
        "archytan-lite: the journal is empty.")))

(defun starter-policy-text (workspace)
  "A policy that grants ordinary work in WORKSPACE and nothing else."
  (let ((root (json-escape (or workspace ""))))
    (format nil "{~%  \"version\": 1,~%  \"actions\": [~%~a~%  ]~%}~%"
            (format nil "~{~a~^,~%~}"
                    (list
                     (format nil "    {\"action\": \"file.read\", \"grants\": [{\"resource_ids\": [\"~a/**\"]}]}" root)
                     (format nil "    {\"action\": \"file.write\", \"grants\": [{\"resource_ids\": [\"~a/**\"]}], \"assertions\": {\"max_per_window\": 120, \"window_seconds\": 60}}" root)
                     (concatenate 'string
                                  "    {\"action\": \"shell.run\", \"grants\": [{\"resource_ids\": ["
                                  "\"ls\", \"cat\", \"head\", \"tail\", \"wc\", \"grep\", \"rg\", \"sort\", \"uniq\","
                                  " \"diff\", \"echo\", \"pwd\", \"date\", \"true\", \"file\", \"stat\", \"du\","
                                  " \"mkdir\", \"cp\", \"mv\", \"git/status\", \"git/diff\", \"git/log\","
                                  " \"git/show\", \"git/branch\", \"git/rev-parse\"]}],"
                                  " \"assertions\": {\"max_per_window\": 60, \"window_seconds\": 60}}")
                     "    {\"action\": \"shell.env\", \"grants\": [{\"resource_ids\": [\"*\"]}]}"
                     "    {\"action\": \"web.fetch\", \"grants\": [{\"resource_ids\": [\"https/*\"]}], \"assertions\": {\"max_per_window\": 60, \"window_seconds\": 60}}"
                     "    {\"action\": \"tool.call\", \"grants\": [{\"resource_ids\": [\"*\"]}]}")))))

(defun setup-text ()
  "Write the starter policy when there is none, and say where it is."
  (let* ((gate (setting :gate))
         (path (gate-policy-path gate)))
    (cond
      ((null (gate-workspace gate))
       "archytan-lite: set workspace first (Settings, the archytan-lite section), then /archytan setup")
      ((probe-file path)
       (format nil "archytan-lite: kept ~a, which is already there; edit it and the next call follows it" path))
      (t (ensure-directories-exist path)
         (nlk:write-file-atomically path (starter-policy-text (gate-workspace gate)))
         (format nil "archytan-lite: wrote a starter policy at ~a; edit it and the next call follows it" path)))))

(defun panel ()
  "The dialog bare /archytan answers with."
  (nle:list-dialog
   :title "Archytan Lite" :summary "Every tool call is checked against the operator's policy before it runs."
   :rows (list (nle:ask-row "Show the last decisions" "How many" "/archytan log"
                             :placeholder "20" :verb "show")
               (nle:ask-row "Write a starter policy" "For the workspace" "/archytan setup"))))

(defun run-slash (args session-id)
  "/archytan typed alone: where the policy is and what the journal says. With
`setup' it writes a starter policy; with `log N' it shows the last N decisions."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (nlk:trimmed (or args ""))))
         (verb (string-downcase (or (first words) ""))))
    (cond ((equal verb "setup") (values (setup-text) (panel)))
          ((equal verb "log")
           (let ((limit (or (ignore-errors (parse-integer (or (second words) "") :junk-allowed t)) 20)))
             (values (log-text (max 1 (min 500 (or limit 20)))) (panel))))
          (t (let ((text (status))) (values text (panel)))))))

;;; --- the hook, and the verb the model can ask ---------------------------------

(defun gate-call (op next)
  "The :TOOL point: every action the call asks for must be granted by the
policy; one that is not answers the refusal, and NEXT never runs."
  (let* ((tool (string-downcase (princ-to-string (getf op :name))))
         (arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments))))
    (if (member tool (or (setting :ungated) '()) :test #'string-equal)
        (funcall next op)
        (let ((refusal (handler-case (refuse-if-ungranted tool form strings)
                         (error (condition)
                           (format nil "ERROR: refused by nodecode-archytan-lite: the gate could not read this call (~a); it did not run -- tell the operator."
                                   (nlk:one-line condition))))))
          (if refusal (nle:failure refusal) (funcall next op))))))

(defun refuse-if-ungranted (tool form strings)
  "The refusal TOOL's call earns, or NIL when every action is granted."
  (let* ((gate (setting :gate))
         (text (if (stringp form) form (format nil "~s" form))))
    (when (null (gate-workspace gate))
      (return-from refuse-if-ungranted
        (format nil "ERROR: refused by nodecode-archytan-lite: no workspace is set, so every call is held. The call did not run -- tell the operator to set the archytan-lite section's workspace (and run /archytan setup).")))
    (multiple-value-bind (allowed decisions)
        (decide gate (call-checks tool text strings (gate-workspace gate)))
      (journal-decisions gate tool decisions)
      (unless allowed (refusal-text gate decisions)))))

(define-verb check (action resource)
  "Would the policy grant ACTION on RESOURCE now? => a plist
(:ALLOWED BOOLEAN :REASON \"...\"), the same decision a tool call gets, without
running anything. (archytan:check \"shell.run\" \"git/push\")"
  (let ((gate (setting :gate)))
    (multiple-value-bind (allowed decisions)
        (decide gate (list (list action resource "asked directly")))
      (list :allowed allowed :reason (or (getf (first decisions) :reason) "")))))

(define-verb decisions (&key (limit 20))
  "The last LIMIT decisions this machine journalled, oldest first, as JSON lines."
  (journal-lines (max 1 limit)))

(defparameter +manual+
  "Archytan Lite holds a tool call until the operator's policy grants everything
the call asks for: the programs it runs, the files it reads and writes, the hosts it
fetches, and the tool itself. One ungranted action holds the whole call, and the
refusal says which rule and why; nothing runs on a guess.
  (archytan:check \"shell.run\" \"git/push\")   one action against the policy, now
  (archytan:decisions :limit 20)              the last decisions, as journalled
  /archytan                                    where the policy is, and the journal's counts
  /archytan log 50                             the last 50 decisions
  /archytan setup                              write a starter policy for the workspace
The policy is a file, one rule per action: file.read, file.write, shell.run (a
program, git/push for a tool with subcommands), shell.env, web.fetch (https/host),
tool.call. A resource id is matched with `*' for one path segment and `**' for the
rest; an action the policy does not name is refused, and so is a resource no grant
covers. `assertions' caps an action: max_per_window in window_seconds. The policy
is read when it changes, so an edit applies to the next call; a command the form
does not show ($VAR, $(...), a heredoc) and a machine with no policy or no
workspace are refused, never passed."
  "What (help :archytan) answers while the cell runs.")

(nle:define-cell archytan-lite
  (:section ("archytan-lite")
    (:guide "set workspace to the folder the agent may read and write; /archytan setup writes a starter policy; edit the policy and the next call follows it")
    ("workspace" :path :doc "the folder the agent may read and write; empty holds every gated call")
    ("policy" :string :default ""
     :doc "the policy file; empty is archytan/policy.json under this Nodecode home")
    ("journal" :string :default ""
     :doc "where decisions are appended; empty is archytan/decisions.jsonl under this home")
    ("ungated" :list :doc "tool names that skip the gate; empty gates every tool"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (append values (list :gate (make-gate-for values)))))
  (:help :archytan "archytan:check \"shell.run\" \"git/push\" asks whether the policy would grant one action now; every tool call is held until the policy grants all it asks for" +manual+)
  (:command "archytan" 'run-slash
            :description "The policy gate: where it is, what it decided, a starter policy"
            :argument-hint "[log N | setup]")
  (:hook :tool #'gate-call))
