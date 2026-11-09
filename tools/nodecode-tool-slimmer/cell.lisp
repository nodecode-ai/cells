;;;; cell.lisp --- the tool schemas one request needs, and no more.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of tool-slimmer (see NOTICE). The plugin trimmed the
;;;; tool schemas of each Hermes request to the few the turn's own words asked
;;;; for, kept a pinned set always, and reported the saving. Two of Nodecode's
;;;; function hooks carry the same thing: SUBMIT is the one ingress, so it is
;;;; where the user's words are kept, and TURN-PROMPT is the build of a
;;;; turn's prompt, so it is where the tool vector is trimmed against them. A
;;;; session whose words are not known is never trimmed -- fail open, as the
;;;; original's fail_open did.

(defpackage #:nodecode-tool-slimmer
  (:use #:cl))

(in-package #:nodecode-tool-slimmer)

(nlk:define-peripheral tool-slimmer :not-running t)

;;; --- what a request says ----------------------------------------------------

(defparameter +stop-words+
  '("the" "and" "for" "with" "from" "into" "onto" "that" "this" "these"
    "those" "than" "then" "there" "here" "are" "was" "were" "you" "your"
    "yours" "our" "ours" "its" "it" "not" "but" "can" "could" "should"
    "would" "will" "shall" "have" "has" "had" "use" "uses" "using" "used"
    "one" "two" "all" "any" "some" "please" "help" "need" "needs" "want"
    "wants" "make" "makes" "run" "runs" "get" "gets" "give" "gives" "when"
    "what" "which" "who" "how" "why" "where" "they" "them" "their" "also"
    "just" "like" "about" "over" "now" "again" "still" "only" "more")
  "Words a request carries that say nothing about which tool answers it.")

(defparameter +request-cap+ 2000
  "How much of one submitted request is kept: enough to rank it, no transcript.")

(defparameter +session-cap+ 256
  "How many sessions' requests are kept before the table is emptied.")

(defparameter +per-tool-overhead+ 48
  "Characters of provider wrapper (type, function, name key) one tool costs.")

(defun tokenize (text)
  "TEXT's words, lowercased: runs of letters and digits, split at anything
else -- an underscore or a dash in a tool's name is a word boundary -- and at
a camelCase hump."
  (let ((words '()) (start nil) (n (length text)))
    (labels ((emit (end)
               (when start
                 (push (string-downcase (subseq text start end)) words)
                 (setf start nil))))
      (loop for i from 0 below n
            for ch = (char text i)
            do (cond ((alphanumericp ch)
                      (when (and start (upper-case-p ch) (lower-case-p (char text (1- i))))
                        (emit i))
                      (unless start (setf start i)))
                     (t (emit i))))
      (emit n))
    (nreverse words)))

(defun relevant-tokens (text)
  "The words of TEXT worth ranking on: each once, none a stop word, none a
single letter."
  (remove-if (lambda (word) (or (< (length word) 2) (member word +stop-words+ :test #'equal)))
             (remove-duplicates (tokenize (or text "")) :test #'equal)))

;;; --- a tool entry, as the provider carries it or as a caller names it --------

(defun tool-function (entry)
  "ENTRY's function object: a provider entry nests it under \"function\"; a
plain (:name ... :description ...) list is its own."
  (cond ((hash-table-p entry)
         (let ((fn (gethash "function" entry))) (and (hash-table-p fn) fn)))
        (t entry)))

(defun tool-name (entry)
  "The name of the tool ENTRY declares."
  (let ((fn (tool-function entry)))
    (cond ((null fn) nil)
          ((hash-table-p fn) (gethash "name" fn))
          ((listp fn) (getf fn :name))
          (t nil))))

(defun tool-description (entry)
  "The description of the tool ENTRY declares."
  (let ((fn (tool-function entry)))
    (cond ((null fn) nil)
          ((hash-table-p fn) (gethash "description" fn))
          ((listp fn) (getf fn :description))
          (t nil))))

(defun tool-entry (name description)
  "A tool entry the ranking reads, from a name and a description."
  (list :name name :description (or description "")))

(defun entry-chars (entry)
  "Roughly how many characters ENTRY's schema costs the prompt."
  (+ +per-tool-overhead+
     (length (or (tool-name entry) ""))
     (length (or (tool-description entry) ""))))

(defun entries-chars (entries)
  "Roughly how many characters a list of tool entries costs."
  (reduce #'+ entries :key #'entry-chars :initial-value 0))

(defun approx-tokens (chars)
  "The rough token count of CHARS, the same bytes/4 rule the original used."
  (floor chars 4))

;;; --- ranking ----------------------------------------------------------------

(defun score-entry (entry tokens)
  "How much ENTRY answers TOKENS: a name word is worth four of a description
word, and a word found in neither is worth nothing."
  (let* ((name-tokens (tokenize (or (tool-name entry) "")))
         (description-tokens (tokenize (or (tool-description entry) "")))
         (score 0))
    (dolist (token tokens score)
      (cond ((member token name-tokens :test #'equal) (incf score 4))
            ((member token description-tokens :test #'equal) (incf score 1))))))

(defun lowered (names)
  "NAMES, each as a downcased string, whatever the section or a call wrote."
  (map 'list (lambda (name) (string-downcase (princ-to-string name))) names))

(defun slim-tools (tools text &key (top-k 8) keep (fail-open t))
  "TOOLS, the provider entries of a request, trimmed to what TEXT needs: the
KEEP names always, then the TOP-K highest scoring others. => (values SLIMMED
KEPT-NAMES DROPPED-NAMES). A FAIL-OPEN trim that keeps nothing keeps
everything: a request with no tool at all is worse than one with too many."
  (let* ((entries (coerce tools 'list))
         (tokens (relevant-tokens text))
         (keep (lowered keep))
         (pinned-p (lambda (entry)
                     (member (string-downcase (or (tool-name entry) "")) keep :test #'equal)))
         (others (remove-if pinned-p entries))
         (ranked (sort (remove-if-not (lambda (pair) (plusp (car pair)))
                                       (mapcar (lambda (entry) (cons (score-entry entry tokens) entry))
                                               others))
                       #'> :key #'car))
         (chosen (mapcar #'cdr (subseq ranked 0 (min (max 0 top-k) (length ranked)))))
         (kept (remove-if-not (lambda (entry) (or (funcall pinned-p entry) (member entry chosen)))
                               entries)))
    (when (and fail-open (null kept))
      (setf kept entries))
    (values (coerce kept 'vector)
            (mapcar #'tool-name kept)
            (mapcar #'tool-name (set-difference entries kept)))))

;;; --- the two hooks ----------------------------------------------------------

(defparameter *prompts* (make-hash-table :test #'equal :synchronized t)
  "Session id -> the last request the operator typed there, taken at SUBMIT.")

(defparameter *full-sessions* (make-hash-table :test #'equal :synchronized t)
  "Session ids whose next prompt build carries every tool, once, as
/tool-slimmer full asked.")

(defparameter *full-next* nil
  "When true, the next prompt build, of any session, carries every tool: what
the model's own tool_slimmer_request_full_tools asks for.")

(defparameter *last* nil
  "What the last trimmed request carried, as a plist for /tool-slimmer.")

(defun remember-prompt (next session-id prompt &rest keys)
  "SUBMIT advised: keep PROMPT as SESSION-ID's own words, then submit it."
  (when (and (stringp session-id) (stringp prompt))
    (when (> (hash-table-count *prompts*) +session-cap+)
      (clrhash *prompts*))
    (setf (gethash session-id *prompts*)
          (subseq prompt 0 (min (length prompt) +request-cap+))))
  (apply next session-id prompt keys))

(defun live-setting (key default)
  "KEY's setting while the cell runs, DEFAULT when it is on its way down."
  (if *tool-slimmer* (setting key) default))

(defun slim-request-tools (tools session-id)
  "TOOLS, the tool vector of SESSION-ID's request, trimmed against the words
the operator typed there. A vector the cell cannot judge is handed back
untouched, so a request always carries tools."
  (cond
    ((or (not (vectorp tools)) (zerop (length tools))) tools)
    ((or *full-next* (gethash session-id *full-sessions*))
     (when *full-next* (setf *full-next* nil))
     (remhash session-id *full-sessions*)
     (setf *last* (list :session session-id :every-tool t
                         :kept (length tools) :dropped 0
                         :chars-before (entries-chars (coerce tools 'list))
                         :chars-after (entries-chars (coerce tools 'list))))
     tools)
    ((null (gethash session-id *prompts*)) tools)
    (t
     (multiple-value-bind (slim kept dropped)
         (slim-tools tools (gethash session-id *prompts*)
                      :top-k (live-setting :top-k 8)
                      :keep (live-setting :keep '())
                      :fail-open (live-setting :fail-open t))
       (let ((before (entries-chars (coerce tools 'list)))
             (after (entries-chars (coerce slim 'list))))
         (setf *last* (list :session session-id :every-tool nil
                             :kept (length kept) :dropped (length dropped)
                             :names kept :left-out dropped
                             :chars-before before :chars-after after))
         slim)))))

(defun trim-turn-prompt (next session-id bindings)
  "TURN-PROMPT advised: the prompt's base, sections and tool vector, with the
tool vector trimmed to SESSION-ID's request."
  (multiple-value-bind (base sections tools) (funcall next session-id bindings)
    (values base sections (slim-request-tools tools session-id))))

;;; --- the model's three tools -------------------------------------------------

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

(defun json-string (text)
  "TEXT as a JSON string literal, quotes and backslashes escaped."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across (princ-to-string text)
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char ch out))))
    (write-char #\" out)))

(defun json-strings (names)
  "NAMES as a JSON array of string literals."
  (format nil "[~{~a~^, ~}]" (mapcar #'json-string names)))

(nlk:define-contract select-call
  (:doc "Rank candidate tools against a request: which of them a turn answering it would need. Answers the kept names in rank order with their scores, and the ones left out.")
  ("query" :text :required t :doc "the request, as the operator typed it")
  ("tools" :array :of :string :required t
           :doc "candidate tools, one per string: \"name\" or \"name: description\"")
  ("top_k" :integer :default 8 :doc "how many of them to keep"))

(nlk:define-contract full-call
  (:doc "Ask the next model request for every tool schema, when a tool this turn needed was not offered. The request after this call, in any session, carries them all once.")
  ("reason" :string :doc "what was missing, one sentence"))

(defun parse-tool-line (line)
  "One \"name: description\" line as a tool entry the ranking reads."
  (let* ((line (string-trim '(#\Space #\Tab) (princ-to-string line)))
         (colon (position #\: line)))
    (if colon
        (tool-entry (string-trim '(#\Space #\Tab) (subseq line 0 colon))
                    (string-trim '(#\Space #\Tab) (subseq line (1+ colon))))
        (tool-entry line ""))))

(defun select-tools (call)
  "The tool_slimmer_select body: rank the call's candidates for its query."
  (let* ((entries (mapcar #'parse-tool-line (coerce (select-call-tools call) 'list)))
         (tokens (relevant-tokens (select-call-query call)))
         (top-k (max 1 (select-call-top-k call)))
         (ranked (sort (remove-if-not (lambda (pair) (plusp (car pair)))
                                       (mapcar (lambda (entry) (cons (score-entry entry tokens) entry))
                                               entries))
                       #'> :key #'car))
         (kept (mapcar #'cdr (subseq ranked 0 (min top-k (length ranked))))))
    (format nil "{\"query\": ~a, \"scored\": [~{~a~^, ~}], \"kept\": ~a, \"left_out\": ~a}"
            (json-string (select-call-query call))
            (mapcar (lambda (pair)
                      (format nil "{\"name\": ~a, \"score\": ~a}"
                              (json-string (or (tool-name (cdr pair)) "")) (car pair)))
                    ranked)
            (json-strings (mapcar (lambda (entry) (or (tool-name entry) "")) kept))
            (json-strings (mapcar (lambda (entry) (or (tool-name entry) ""))
                                   (set-difference entries kept))))))

(defun status-text ()
  "The report /tool-slimmer and tool_slimmer_status answer."
  (let ((keep (map 'list #'princ-to-string (or (live-setting :keep '()) '()))))
    (format nil "{\"running\": ~:[false~;true~], \"top_k\": ~a, \"fail_open\": ~:[false~;true~], \"keep\": ~a, \"last\": ~a}"
            (and *tool-slimmer* t)
            (live-setting :top-k 8)
            (live-setting :fail-open t)
            (json-strings keep)
            (if (null *last*)
                "null"
                (format nil "{\"every_tool\": ~:[false~;true~], \"kept\": ~a, \"dropped\": ~a, \"approx_tokens_before\": ~a, \"approx_tokens_after\": ~a, \"approx_tokens_saved\": ~a, \"names\": ~a}"
                        (getf *last* :every-tool)
                        (getf *last* :kept)
                        (getf *last* :dropped)
                        (approx-tokens (getf *last* :chars-before 0))
                        (approx-tokens (getf *last* :chars-after 0))
                        (max 0 (- (approx-tokens (getf *last* :chars-before 0))
                                  (approx-tokens (getf *last* :chars-after 0))))
                        (json-strings (or (getf *last* :names) '())))))))

(defun status (arguments)
  "The tool_slimmer_status body: what the section says and what the last
request carried."
  (declare (ignore arguments))
  (status-text))

(defun request-full (call)
  "The tool_slimmer_request_full_tools body: every schema on the next request."
  (setf *full-next* t)
  (let ((reason (full-call-reason call)))
    (format nil "the next model request carries every tool schema, not just the ones this turn's words asked for~@[ (~a)~]"
            (and reason (plusp (length reason)) reason))))

;;; --- /tool-slimmer ----------------------------------------------------------

(defun run-command (args session-id)
  "One /tool-slimmer invocation: the report, or \"full\" to carry every tool
on the next request of this session."
  (let ((args (nlk:trimmed (or args ""))))
    (cond
      ((string-equal args "full")
       (if (stringp session-id)
           (progn (setf (gethash session-id *full-sessions*) t)
                  "the next request of this session carries every tool schema")
           "no session to give every tool: run it in a session"))
      ((string-equal args "reset")
       (setf *last* nil *full-next* nil)
       (clrhash *full-sessions*)
       "tool-slimmer's counters are cleared")
      ((and (plusp (length args)) (not (string-equal args "status")))
       (format nil "tool-slimmer: /tool-slimmer [status|full|reset]; ~a" (status-text)))
      (t
       (let ((last *last*))
         (format nil "~a~%~@[last request: ~a of ~a tool schemas kept, ~a dropped, about ~a tokens saved~]"
                 (status-text)
                 (and last (getf last :kept))
                 (and last (+ (getf last :kept) (getf last :dropped)))
                 (and last (getf last :dropped))
                 (and last (max 0 (- (approx-tokens (getf last :chars-before 0))
                                     (approx-tokens (getf last :chars-after 0)))))))))))

(defparameter +manual+
  "tool-slimmer keeps a request's tool list down to the tools its words ask
for, plus the ones the section pins, and reports the saving.

It works on every turn: the tool schemas of a request are the largest part of
a prompt that the operator never reads, and a catalog of forty tools costs
the same whether the request is about cron or about a pull request. The words
the operator typed are ranked against every tool's name (a name word counts
four) and description (one); the pinned names ride every request whatever
they typed.

A request whose words the cell has not seen -- a turn resumed at boot, a
channel prompt, a fork -- is never trimmed: it carries every schema, as the
section's fail_open says.

/tool-slimmer reports what the last request carried and the approximate
tokens it saved; /tool-slimmer full makes this session's next request carry
every schema, and /tool-slimmer reset clears the counters. The model asks
for the same thing with tool_slimmer_request_full_tools when a tool it
needed was not offered, and ranks a list of candidate tools itself with
tool_slimmer_select."
  "What (help :tool-slimmer) answers while the cell runs.")

(nle:define-cell tool-slimmer
  (:section ("tool-slimmer")
    (:guide "nothing here is required; keep names the tools every request must carry, top_k bounds the rest")
    ("keep" :list :default '("tool_slimmer_status" "tool_slimmer_select"
                              "tool_slimmer_request_full_tools")
            :doc "tool names every request carries, whatever the operator typed")
    ("top_k" :integer :default 8 :min 2
             :doc "how many more tool schemas one request may carry beside the kept ones")
    ("fail_open" :boolean :default t
                 :doc "when nothing in the request scores, offer every tool rather than none"))
  (:help :tool-slimmer
         "tool-slimmer: each request carries only the tool schemas its words ask for - (help :tool-slimmer)"
         +manual+)
  (:tool "tool_slimmer_select" (nlk:contract 'select-call) #'select-tools)
  (:tool "tool_slimmer_status"
         (no-arguments "What the last model request carried: how many tool schemas it kept and dropped, and the approximate tokens it saved. The section's top_k, keep and fail_open read back too.")
         #'status)
  (:tool "tool_slimmer_request_full_tools" (nlk:contract 'full-call) #'request-full)
  (:hook 'nle:submit #'remember-prompt)
  (:hook 'nle::turn-prompt #'trim-turn-prompt)
  (:command "tool-slimmer" 'run-command
            :description "Tool schemas: what the last request carried, or every one next"
            :argument-hint "[status|full|reset]")
  (:start (lambda ()
            (clrhash *prompts*)
            (clrhash *full-sessions*)
            (setf *full-next* nil *last* nil)
            (nle:notice (format nil "tool-slimmer: a request carries at most ~a tool schemas beside the ~a it always keeps"
                                (setting :top-k) (length (or (setting :keep) '())))
                        :key "nodecode-tool-slimmer")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-tool-slimmer"))))))
