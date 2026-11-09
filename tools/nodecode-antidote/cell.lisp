;;;; cell.lisp --- nodecode-antidote: runtime guardrails for tool calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of llmAntidote's runtime half. One :TOOL hook reads
;;;; every call's arguments as text, names what the call touches (sensitive
;;;; material, an egress-capable command, an upload, a download piped into a
;;;; shell, a reverse shell), remembers that a sensitive read was let through,
;;;; and answers by `mode':
;;;;
;;;;   balanced  ask Allow once or Deny before a sensitive read and before a
;;;;             network-capable call; a call that carries sensitive material
;;;;             and egress at once, or that reaches the network after a
;;;;             sensitive read, is refused
;;;;   strict    refuse those instead of asking
;;;;   audit     allow everything and record what strict would have said
;;;;
;;;; Nothing is stored but metadata: the rule, the tool and the action, never
;;;; the call's text, a path, or a value read. The taint is the image's -- a
;;;; Nodecode process serves every session, and the :TOOL point carries no
;;;; session id -- so it is one memory, cleared by /antidote clear, by taint
;;;; minutes passing, or by a restart of the cell.
;;;;
;;;; LIKE GUARD, NOT A SECURITY BOUNDARY: the hook sees the text of a call,
;;;; so a command built out of pieces it cannot read is a command it cannot
;;;; judge. It closes the ordinary chain -- read a credential file, then send
;;;; it somewhere -- and the common download-and-run shapes.

(defpackage #:nodecode-antidote
  (:use #:cl))

(in-package #:nodecode-antidote)

;;; --- rules -----------------------------------------------------------------

(defun scanner (pattern)
  "PATTERN as a scanner: case ignored, ^ and $ per line, . across lines."
  (cl-ppcre:create-scanner pattern
                            :case-insensitive-mode t
                            :multi-line-mode t
                            :single-line-mode t))

(defun compile-rules (rules)
  "RULES, ((ID REASON PATTERN) ...), with each pattern compiled."
  (loop for (id reason pattern) in rules
        collect (list id reason (scanner pattern))))

(defun match-rule (rules text)
  "The ID and REASON of the first rule of RULES that matches TEXT, or NIL."
  (loop for (id reason scanner) in rules
        when (cl-ppcre:scan scanner text)
          do (return (values id reason))))

;; The material llmAntidote treats as sensitive: a dotenv or process
;; environment, a credential directory, an authentication file.
(defparameter *sensitive-rules*
  (compile-rules
   '(("sensitive:environment" "an environment or dotenv secret"
      "(?:^|[/\\s'\"(])(?:\\.env(?:\\.(?!example\\b)[a-z0-9_-]+)?|\\.envrc)(?:$|[/\\s'\";&|)\\]])|/proc/(?:self|\\d+)/environ\\b|\\b(?:printenv|getenv)\\b")
     ("sensitive:credential-file" "a credential directory"
      "(?:^|[/\\s'\"])(?:\\.ssh|\\.aws|\\.kube|\\.gnupg|\\.docker|\\.config/gcloud|\\.azure)(?:[/\\s'\"]|$)|(?:^|[/\\\\])\\.netrc\\b")
     ("sensitive:auth-material" "an authentication file"
      "(?:^|[/\\s'\"])(?:id_rsa|id_ed25519|id_ecdsa|credentials\\.json|service[_-]?account[^/\\\\\\s]*\\.json|auth\\.json|\\.credentials\\.json|private[_-]?key)(?:$|[/\\\\\\s'\";&|])")))
  "What a call reading this material is: the sensitive class.")

;; Anything that can leave the machine, in either language a call is written in.
(defparameter *network-rules*
  (compile-rules
   '(("egress:network" "a network-capable command"
      "\\b(?:curl|wget|https?|xh|nc|ncat|netcat|socat|scp|sftp|rsync|ftp)\\b|\\b(?:nlk:http|dexador|http-request|http-get|http-post|http-put|url-retrieve|urlopen)\\b|\\burllib\\.request\\.(?:urlopen|Request)\\s*\\(|\\b(?:requests|httpx)\\s*\\.\\s*(?:post|put|patch)\\s*\\(")))
  "A call whose text can open a socket.")

(defparameter *upload-rules*
  (compile-rules
   '(("egress:upload" "an upload or an outbound message"
      "\\bcurl\\b[^\\n]{0,1200}(?:\\s(?:-d|--data(?:-[a-z]+)?|-F|--form|-T|--upload-file)\\b|-X\\s*(?:POST|PUT|PATCH)\\b)|\\bwget\\b[^\\n]{0,1200}(?:--post-(?:data|file)|--method\\s*=?(?:POST|PUT|PATCH))|\\b(?:nc|ncat|netcat|socat|scp|sftp|rsync|ftp)\\b|\\b(?:dexador:post|dexador:put|http-post|http-put)\\b|\\bnlk:http\\b[^)]{0,300}:method\\s+:post")))
  "A call that sends a body somewhere, not only fetches one.")

(defparameter *reverse-rules*
  (compile-rules
   '(("shell:reverse" "a recognizable reverse shell"
      "/dev/(?:tcp|udp)/|\\b(?:nc|ncat|netcat)\\b.{0,500}\\s-e\\s*(?:/bin/)?(?:ba)?sh\\b|\\bsocat\\b.{0,500}\\bexec:(?:ba)?sh\\b|\\b(?:bash|sh)\\s+-i\\s+>&?\\s*/dev/(?:tcp|udp)/")))
  "A call that opens a shell back to whoever asked.")

(defparameter *download-rules*
  (compile-rules
   '(("shell:download-execute" "a download piped into a shell"
      "\\b(?:curl|wget)\\b.{0,1500}\\|\\s*(?:sudo\\s+)?(?:ba|z|k)?sh\\b|\\b(?:ba|z|k)?sh\\s*<\\(\\s*(?:curl|wget)\\b|\\bbase64\\b.{0,800}(?:--decode|-d)\\s*\\|\\s*(?:ba|z|k)?sh\\b")))
  "A call that runs what it just fetched.")

;;; --- the text of a call ----------------------------------------------------

(defun printed-form (text)
  "TEXT's forms printed back with their comments gone, or TEXT when it does
not read: a comment naming a secret is not a read of it."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil) (*package* (find-package :cl-user)) (parts '()))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push (write-to-string form :readably t :case :downcase) parts))
          (format nil "~{~a~^ ~}" (nreverse parts))))
    (error () text)))

(defun write-argument-strings (value out &optional (depth 0))
  "Write every string VALUE carries, one per line."
  (when (< depth 12)
    (cond ((stringp value) (format out "~%~a" value))
          ((hash-table-p value)
           (loop for item being the hash-values of value
                 do (write-argument-strings item out (1+ depth))))
          ((vectorp value)
           (loop for item across value
                 do (write-argument-strings item out (1+ depth))))
          ((consp value)
           (loop for item in value
                 do (write-argument-strings item out (1+ depth)))))))

(defun call-text (op)
  "The text a call of OP carries: its form as written, and every string it
hands over. A tool other than eval contributes the strings in its arguments."
  (let* ((arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments))))
    (with-output-to-string (out)
      (when (stringp form) (write-string (printed-form form) out))
      (when (hash-table-p strings)
        (loop for value being the hash-values of strings
              when (stringp value) do (format out "~%~a" value)))
      (unless (stringp form) (write-argument-strings arguments out)))))

;;; --- the policy ------------------------------------------------------------

(defstruct (policy (:constructor make-policy))
  (mode "balanced")
  (ask t)
  (ask-seconds 120)
  (taint-seconds (* 60 60))
  (max-scan-bytes 1048576)
  (capacity 200)
  (sensitive *sensitive-rules*)
  (allow-egress '())
  (tainted-until 0)
  (decisions '())
  (decided 0))

(defun taint-active-p (policy)
  "Whether a sensitive read was let through recently enough to gate egress."
  (> (policy-tainted-until policy) (get-universal-time)))

(defun taint-left-minutes (policy)
  "How many minutes the taint has left, rounded up, or 0."
  (max 0 (ceiling (- (policy-tainted-until policy) (get-universal-time)) 60)))

(defun mark-taint (policy)
  "Remember that a call touching sensitive material ran."
  (setf (policy-tainted-until policy) (+ (get-universal-time) (policy-taint-seconds policy))))

(defun allow-egress-p (policy tool)
  "Whether TOOL was named in allow_egress, exempting it from the egress rules."
  (member tool (policy-allow-egress policy) :test #'string-equal))

(defun record-decision (policy tool rule action)
  "Keep one metadata-only decision, newest first, bounded by audit_capacity."
  (incf (policy-decided policy))
  (push (list :at (get-universal-time) :tool tool :rule rule :action action)
        (policy-decisions policy))
  (when (> (length (policy-decisions policy)) (policy-capacity policy))
    (setf (policy-decisions policy)
          (subseq (policy-decisions policy) 0 (policy-capacity policy)))))

(defun sensitive-material (policy text)
  "The rule naming the sensitive material TEXT touches, or NIL."
  (values (match-rule (policy-sensitive policy) text)))

(defun mode-decision (policy rule message)
  "What MODE makes of a call RULE names: block, ask, or allow (audit)."
  (cond ((string-equal (policy-mode policy) "strict") (values :block rule message))
        ((string-equal (policy-mode policy) "audit") (values :allow rule message))
        (t (values :ask rule message))))

(defun decide (policy text tool)
  "What POLICY makes of TEXT, a call of TOOL.
=> ACTION (:allow :ask or :block), the RULE naming it, the MESSAGE the model
reads, and the sensitive RULE the call carries when it carries one."
  (multiple-value-bind (sensitive sensitive-why) (match-rule (policy-sensitive policy) text)
    (declare (ignore sensitive-why))
    (multiple-value-bind (network) (match-rule *network-rules* text)
      (multiple-value-bind (upload) (match-rule *upload-rules* text)
        (multiple-value-bind (reverse) (match-rule *reverse-rules* text)
          (multiple-value-bind (download) (match-rule *download-rules* text)
            (let ((egress (or upload network))
                  (exempt (allow-egress-p policy tool)))
              (cond
                (reverse
                 (values :block reverse "the call runs a recognizable reverse shell" sensitive))
                (download
                 (values :block download "the call runs a download piped into a shell" sensitive))
                ((and sensitive egress)
                 (values :block sensitive
                         "the call carries sensitive material and an egress-capable command at once"
                         sensitive))
                ((and egress (not exempt) (taint-active-p policy))
                 (values :block egress
                         "the call can reach the network after a sensitive read in this image"
                         sensitive))
                (sensitive
                 (multiple-value-bind (action rule message)
                     (mode-decision policy sensitive "the call reads potentially sensitive material")
                   (values action rule message sensitive)))
                ((and egress (not exempt))
                 (multiple-value-bind (action rule message)
                     (mode-decision policy egress "the call can reach the network")
                   (values action rule message nil)))
                (t (values :allow nil nil nil))))))))))

;;; --- asking the operator ----------------------------------------------------

(defparameter *ask* nil
  "A stand-in for the core's ASK-OPERATOR: (lambda (text &key detail choices
seconds) => (values answer why)). NIL asks the organism; a test binds one.")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun clipped (text limit)
  "TEXT cut to LIMIT characters."
  (if (> (length text) limit)
      (concatenate 'string (subseq text 0 limit) " ...")
      text))

(defun ask-operator (policy rule message tool text)
  "Ask Allow once or Deny in the surface that answers questions.
=> T when the operator allowed this one call."
  (let ((ask (and (policy-ask policy) (or *ask* (ask-operator-function)))))
    (and ask
         (equal "Allow once"
                (funcall ask (format nil "antidote stopped a ~a call: ~a" tool message)
                         :detail (format nil "~a~%~a" rule (clipped text 1200))
                         :choices (list "Allow once" "Deny")
                         :seconds (max 5 (policy-ask-seconds policy)))))))

;;; --- the hook --------------------------------------------------------------

(defun refusal-text (rule message why)
  "The model's words for a call that did not run."
  (format nil "ERROR: refused by nodecode-antidote: ~a (rule ~a~@[; ~a~]); ~
the call did not run -- do not retry it as it stands: ask the operator, or ~
work without that read or that destination"
          message rule why))

(defun gate (op next)
  "The :TOOL point: read the call, decide, ask when the mode says so, record
the decision, and answer a refusal through NLE:FAILURE; else let it run."
  (let* ((policy (setting :state))
         (tool (let ((name (getf op :name))) (if (stringp name) name (format nil "~a" name))))
         (text (call-text op)))
    (if (> (length text) (policy-max-scan-bytes policy))
        (progn
          (record-decision policy tool "scan:oversized" "block")
          (nle:failure (refusal-text "scan:oversized"
                                      "the call's arguments exceed the scan limit"
                                      nil)))
        (multiple-value-bind (action rule message sensitive) (decide policy text tool)
          (let* ((allowed (case action
                             (:allow t)
                             (:block nil)
                             (:ask (ask-operator policy rule message tool text))))
                 (why (when (and (eq action :ask) (not allowed))
                        "the operator was asked and did not allow it")))
            (record-decision policy tool (or rule "none")
                             (cond ((and allowed (eq action :ask)) "asked, allowed")
                                   ((eq action :ask) "asked, refused")
                                   (allowed "allowed")
                                   (t "blocked")))
            (when (and allowed sensitive) (mark-taint policy))
            (if allowed
                (funcall next op)
                (nle:failure (refusal-text (or rule "none") message why))))))))

;;; --- /antidote -------------------------------------------------------------

(defun status-text (policy)
  "What /antidote status shows: the mode, the taint, and the last decisions."
  (with-output-to-string (out)
    (format out "antidote: ~a mode~%" (policy-mode policy))
    (format out "- sensitive read remembered: ~a~%"
            (if (taint-active-p policy)
                (format nil "yes, ~d more minute~:p at most" (taint-left-minutes policy))
                "no"))
    (format out "- decisions recorded: ~d (last ~d kept)~%"
            (policy-decided policy) (length (policy-decisions policy)))
    (format out "- allowed to reach the network: ~a~%"
            (if (policy-allow-egress policy)
                (format nil "~{~a~^, ~}" (policy-allow-egress policy))
                "nothing extra"))
    (loop for entry in (subseq (policy-decisions policy)
                                0 (min 5 (length (policy-decisions policy))))
          do (format out "  ~a  ~a  ~a~%"
                     (or (getf entry :tool) "") (or (getf entry :rule) "")
                     (or (getf entry :action) "")))))

(defun run-slash (args session-id)
  "/antidote: status, or clear the remembered sensitive read and the record.
=> the answer's text."
  (declare (ignore session-id))
  (let ((policy (setting :state))
        (command (string-downcase (string-trim '(#\Space #\Tab) (or args "")))))
    (cond ((member command '("" "status") :test #'string=)
           (status-text policy))
          ((string= command "clear")
           (setf (policy-tainted-until policy) 0
                 (policy-decisions policy) '()
                 (policy-decided policy) 0)
           "antidote: forgot the sensitive read and the recorded decisions")
          (t "antidote: /antidote status | clear"))))

;;; --- the declaration -------------------------------------------------------

(defparameter +manual+
  "antidote: guardrails on what a tool call touches. Calls that read
sensitive material (.env, the environment, ~/.ssh, ~/.aws, cloud credentials,
key files), that can reach the network (curl, wget, nc, scp, nlk:http,
dexador, an http URL), that upload, that run a download through a shell, or
that open a reverse shell are judged before they run. In balanced mode the
first two kinds put a question up (Allow once or Deny) and are refused when
nobody answers; a call that does both at once, or that reaches the network
after a sensitive read was allowed, is refused. strict mode refuses instead
of asking; audit mode allows everything and records what would have been
refused. The memory is the image's, not a session's, and lasts taint_minutes.
/antidote status shows the mode, the memory and the last decisions;
/antidote clear forgets them. It reads the text of a call: a command built
out of pieces it cannot see is a command it cannot judge.")

(nlk:define-peripheral antidote :not-running t)

(nle:define-cell antidote
  (:section ("antidote")
    (:guide "mode is balanced (ask), strict (refuse) or audit (record only); taint_minutes is how long a sensitive read keeps gating egress; allow_egress names tools exempt from the network rules")
    ("mode" :choice :options '("balanced" "strict" "audit") :default "balanced"
     :doc "balanced asks before a sensitive read and before a network-capable call; strict refuses those; audit only records")
    ("ask" :boolean :default t
     :doc "in balanced mode, ask Allow once or Deny in the web page or the desktop app; with nobody there, or no answer, the call is refused")
    ("ask_seconds" :integer :default 120 :min 5 :doc "how long a question waits for an answer")
    ("taint_minutes" :integer :default 60 :min 1
     :doc "how long a sensitive read was let through keeps an egress-capable call gated")
    ("max_scan_bytes" :integer :default 1048576 :min 4096
     :doc "the longest call text read; a longer call is refused rather than half-read")
    ("sensitive" :list :doc "extra regular expressions naming material to treat as sensitive")
    ("allow_egress" :list :doc "tool names exempt from the network rules")
    ("audit_capacity" :integer :default 200 :min 1
     :doc "how many decisions /antidote status keeps, newest first"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (append (list :state (make-policy
                                      :mode (getf values :mode)
                                      :ask (getf values :ask)
                                      :ask-seconds (getf values :ask-seconds)
                                      :taint-seconds (* 60 (getf values :taint-minutes))
                                      :max-scan-bytes (getf values :max-scan-bytes)
                                      :capacity (getf values :audit-capacity)
                                      :allow-egress (mapcar #'string-downcase
                                                             (getf values :allow-egress))
                                      :sensitive (append
                                                  *sensitive-rules*
                                                  (compile-rules
                                                   (loop for pattern in (getf values :sensitive)
                                                         collect (list "sensitive:listed"
                                                                       "material the operator listed"
                                                                       pattern))))))
                       values)))
  (:hook :tool #'gate)
  (:help :antidote "antidote: guardrails on what a tool call reads and sends" +manual+)
  (:command "antidote" 'run-slash
            :description "The tool-call guard: status, or clear what it remembers"
            :argument-hint "status | clear"))
