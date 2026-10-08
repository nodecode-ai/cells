;;;; cli.lisp --- one run of the claude CLI: history in, one request out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every round starts a fresh `claude -p' speaking stream-json, the way the
;;;; Agent SDK drives it: no tools of its own (--tools ''), no settings, MCP
;;;; servers, CLAUDE.md or memory of the operator's, no session file, one
;;;; turn. Its ANTHROPIC_BASE_URL is the round's relay. The history is written
;;;; to its stdin one frame at a time, each earlier user frame acknowledged by
;;;; a zero-turn result before the next goes (unacknowledged, the CLI drops
;;;; frames: 181 of 305 messages arrived, 2026-09-27); the last frame asks, the
;;;; CLI POSTs, the relay keeps the request, and the CLI is killed. About
;;;; 0.4 s to start and 20 ms a replayed user frame, measured on 2.1.283.
;;;;
;;;; The login is the CLI's, read where the CLI reads it: this file never
;;;; touches a credential. Whatever in the environment would point the CLI
;;;; at another login or another backend is taken out of its environment.

(in-package #:nodecode-claude-code)

(defparameter +settled-environment+
  '("ENABLE_TOOL_SEARCH=false"
    "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1"
    "CLAUDE_CODE_MAX_RETRIES=0"
    "DISABLE_AUTO_COMPACT=1"
    "DISABLE_COMPACT=1"
    ;; the budget reminder is replayed into history and moves the cache prefix
    "CLAUDE_CODE_TOTAL_TOKENS_REMINDER=off"
    ;; the working directory's CLAUDE.md and the CLI's own memory are Claude
    ;; Code's context, not this session's
    "CLAUDE_CODE_DISABLE_AUTO_MEMORY=1"
    "CLAUDE_CODE_DISABLE_CLAUDE_MDS=1")
  "The environment every run starts from, beside ANTHROPIC_BASE_URL.")

(defparameter +withheld-prefixes+ '("ANTHROPIC_" "CLAUDE_CODE_" "CLAUDECODE=")
  "Inherited variables a run never sees: another key, another backend, and
the markers of a Claude Code session this process may itself run inside.")

(defstruct (run (:copier nil))
  pid process input output
  (partial (make-string-output-stream))
  (acks 0 :type integer)
  ;; what the CLI said went wrong, newest first: (CODE . TEXT)
  (failures '())
  (exit nil)
  (stderr nil))

(defun cli-program (command)
  "COMMAND's executable: a path as itself, a bare name found on PATH; NIL
when there is none."
  (if (find #\/ command)
      (probe-file command)
      (nlk::executable-on-path command)))

(defun cli-environment (relay max-tokens)
  "The run's environment: this process's, without +WITHHELD-PREFIXES+, with
the settled switches, the relay as the API and MAX-TOKENS as the output
ceiling the CLI checks thinking against."
  (append (list (format nil "ANTHROPIC_BASE_URL=~a" (relay-url relay)))
          (when (integerp max-tokens)
            (list (format nil "CLAUDE_CODE_MAX_OUTPUT_TOKENS=~d" max-tokens)))
          +settled-environment+
          (remove-if (lambda (entry)
                       (some (lambda (prefix) (uiop:string-prefix-p prefix entry))
                             +withheld-prefixes+))
                     (sb-ext:posix-environ))))

(defun cli-argv (program model system-file settings-file effort)
  "The run's command line."
  (append (list (uiop:native-namestring program) "-p"
                "--model" model
                "--input-format" "stream-json" "--output-format" "stream-json" "--verbose"
                "--tools" ""
                "--system-prompt-file" (uiop:native-namestring system-file)
                "--settings" (uiop:native-namestring settings-file)
                "--setting-sources" ""
                "--strict-mcp-config"
                "--disable-slash-commands"
                "--max-turns" "1"
                "--permission-mode" "dontAsk"
                "--no-session-persistence")
          ;; without it the CLI tells the model its own default level, over output_config
          (when effort (list "--effort" effort))))

(defun run-fail (detail &key (status nil) config)
  "Fail the round: a CONFIG failure stands (the operator must act) and a
statusless one is retried like any failed request."
  (if config
      (error 'nle::provider-config-error :status status :detail detail)
      (error 'nle::provider-error :status status :scope :request :detail detail)))

;;; --- reading the CLI -----------------------------------------------------------

(defun note-line (run line)
  "Take in one stdout LINE of RUN: an acknowledgment counts, a failure is kept."
  (let ((frame (ignore-errors (nlk:decode-json line))))
    (when (hash-table-p frame)
      (nlk:dispatch (nlk:json-value frame :string "type") equal
        ("result"
         (if (and (eql 0 (nlk:json-value frame :integer "num_turns"))
                  (not (nlk:json-value frame :boolean "is_error")))
             (incf (run-acks run))
             (push (cons (nlk:json-value frame :string "subtype")
                         (or (nlk:json-value frame :string "result") ""))
                   (run-failures run))))
        ("assistant"
         (let ((code (or (nlk:json-value frame :string "error")
                         (nlk:json-value frame :string "message" "error"))))
           (when code
             (push (cons code
                       (format nil "~{~a~^ ~}"
                               (loop for block across (or (nlk:json-value frame :array "message" "content") #())
                                     for text = (nlk:json-value block :string "text")
                                     when text collect text)))
                   (run-failures run)))))))))

(defun drain-output (run)
  "Take in every whole line RUN's stdout holds now; true at its end."
  (let ((stream (run-output run)))
    (loop
      (let ((char (read-char-no-hang stream nil :eof)))
        (cond ((eq char :eof) (return t))
              ((null char) (return nil))
              ((char= char #\Newline)
               (note-line run (get-output-stream-string (run-partial run))))
              (t (write-char char (run-partial run))))))))

(defun stderr-tail (run)
  "The last lines the CLI wrote to stderr, or NIL."
  (let ((text (ignore-errors (uiop:read-file-string (run-stderr run)))))
    (when (plusp (length text))
      (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) text)))
        (subseq text (max 0 (- (length text) 400)))))))

(defun logged-out-p (run)
  "Whether the CLI said it has no usable login."
  (some (lambda (failure)
          (or (equal (car failure) "authentication_failed")
              (ppcre:scan "(?i)not logged in|please run /login|invalid api key|oauth token has expired"
                          (cdr failure))))
        (run-failures run)))

(defun fail-ended (run)
  "Fail the round for a CLI that ended, or said it was done, before it sent
its request."
  (cond
    ((logged-out-p run)
     (run-fail "Claude Code is not logged in here: run `claude' in a terminal and /login, then try again"
               :status 403 :config t))
    (t
     (run-fail (format nil "the claude CLI ~:[stopped~;exited ~:*~d~] before sending its request~@[: ~a~]~@[ (stderr: ~a)~]"
                       (run-exit run)
                       (let ((failure (first (run-failures run))))
                         (and failure (format nil "~@[~a: ~]~a" (car failure) (cdr failure))))
                       (stderr-tail run))))))

;;; --- driving it ---------------------------------------------------------------

(defun check-turn ()
  "Unwind when the turn this round serves was cancelled."
  (let ((turn nle::*current-durable-turn*))
    (when turn (nlk:ensure-turn-not-cancelled turn))))

(defun pump (run relay deadline done-p)
  "Read RUN's stdout, and watch RELAY, until DONE-P answers true."
  ;; Waiting on the CLI's stdout a slice at a time: an acknowledgment is read
  ;; the moment it is written, and what the relay's thread took is looked at
  ;; between slices.
  (let ((fd (sb-sys:fd-stream-fd (run-output run))))
    (loop
      (when (relay-failure relay)
        (run-fail (format nil "the claude CLI's request could not be read: ~a" (relay-failure relay))))
      (let ((ended (drain-output run)))
        (when (funcall done-p) (return))
        (when (or ended (and (null (run-exit run))
                             (setf (run-exit run) (nlk:child-exit (run-pid run) (run-process run)))))
          (drain-output run)
          (when (funcall done-p) (return))
          (fail-ended run)))
      (when (run-failures run)
        (fail-ended run))
      (check-turn)
      (when (> (get-internal-real-time) deadline)
        (run-fail (format nil "the claude CLI wrote no request within ~d s" (setting :timeout-seconds))))
      (sb-sys:wait-until-fd-usable fd :input 0.01))))

(defun send-frame (run frame)
  "Write FRAME to the CLI as one line."
  (handler-case
      (let ((stream (run-input run)))
        (write-string (sb-ext:octets-to-string (nlk:encode-json-octets frame) :external-format :utf-8)
                      stream)
        (write-char #\Newline stream)
        (finish-output stream))
    (error (condition)
      (run-fail (format nil "the claude CLI stopped reading its history: ~a" condition)))))

(defun stop-run (run)
  "End RUN and whatever it started; reap it."
  (ignore-errors (close (run-input run)))
  (unless (run-exit run)
    (nlk:kill-tree (run-pid run))
    (ignore-errors (nlk:wait-for-child (run-pid run) (run-process run))))
  (ignore-errors (close (run-output run))))

(defun author-request (body &key model window directory)
  "(values HEADERS ENDPOINT OCTETS): the Messages request the claude CLI
writes for BODY — Nodecode's own for the round, decoded — run in DIRECTORY,
MODEL on WINDOW's route."
  (let* ((program (or (cli-program (setting :command))
                      (run-fail (format nil "the claude CLI is not installed here (no ~a on PATH): install Claude Code, run `claude' once and /login"
                                        (setting :command))
                                :status 404 :config t)))
         (frames (replay-frames body model))
         (queried (nlk:json-value (car (last frames)) :array "message" "content"))
         (scratch (uiop:ensure-directory-pathname
                   (merge-pathnames (format nil "nodecode-claude-code-~a" (hex-token 8))
                                    (uiop:temporary-directory))))
         (deadline (+ (get-internal-real-time)
                      (* (setting :timeout-seconds) internal-time-units-per-second)))
         (relay nil)
         (run nil))
    (unwind-protect
         (let ((system-file (merge-pathnames "system.md" scratch))
               (settings-file (merge-pathnames "settings.json" scratch)))
           (ensure-directories-exist scratch)
           #-win32 (sb-posix:chmod (uiop:native-namestring scratch) #o700)
           (with-open-file (out system-file :direction :output :external-format :utf-8)
             (write-string (system-text body) out))
           ;; CLAUDE_CODE_EXTRA_BODY rides the settings file: a tool list is
           ;; past what one environment string may hold
           (with-open-file (out settings-file :direction :output :element-type '(unsigned-byte 8))
             (write-sequence (nlk:encode-json-octets
                              (nlk:json-object "env" (nlk:json-object
                                                      "CLAUDE_CODE_EXTRA_BODY"
                                                      (sb-ext:octets-to-string
                                                       (nlk:encode-json-octets (extra-body body))
                                                       :external-format :utf-8))))
                             out))
           (setf relay (open-relay))
           (let ((stderr (merge-pathnames "stderr.log" scratch)))
             (nlk:bind (((pid output _ process input)
                         (nlk:spawn-program (cli-argv program (model-argument model window)
                                                      system-file settings-file (effort body))
                                            :directory directory
                                            :environment (cli-environment relay (gethash "max_tokens" body))
                                            :input :stream :error-output stderr)))
               (setf run (make-run :pid pid :process process :input input :output output
                                   :stderr stderr))))
           (let ((asked 0))
             (dolist (frame frames)
               (send-frame run frame)
               (when (nth-value 1 (gethash "shouldQuery" frame))
                 (incf asked)
                 (pump run relay deadline (lambda () (>= (run-acks run) asked))))))
           (ignore-errors (close (run-input run)))
           (pump run relay deadline (lambda () (relay-capture relay)))
           (let ((capture (relay-capture relay)))
             (values (captured-headers capture)
                     (captured-endpoint capture)
                     (nlk:encode-json-octets
                      (pin-breakpoint (nlk:decode-json (getf capture :body)) queried)))))
      (when run (stop-run run))
      (when relay (close-relay relay))
      (uiop:delete-directory-tree scratch :validate t :if-does-not-exist :ignore))))
