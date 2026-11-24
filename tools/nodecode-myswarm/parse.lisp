;;;; parse.lisp --- an agents.md, as the agents it declares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four readings of one document, richest first: a shared MySwarm ASCII
;;;; block (the one the cell itself stamps, so a shared diagram imports back as
;;;; agents); the `## Name' sections with `- **Persona**:' and `- **Goal**:'
;;;; lines; a markdown table of agents; and bullet lines `- **Name** (role): goal'.
;;;; A file none of them fits is one monolithic persona, which is what a
;;;; single-agent workspace agents.md is.
;;;;
;;;; Nothing here reads the disk or calls a model: TEXT in, agents out, so the
;;;; reading is the same whatever model is talking and can be tested alone.

(in-package #:nodecode-myswarm)

;;; --- the shape -----------------------------------------------------------------

(defstruct (agent (:constructor make-agent (&key name persona goal (role "specialist") delegates tools)))
  "One agent an agents.md declares: its name, a persona and a goal abbreviated
for showing, its role, the agents it routes work to, and the tools it names."
  (name "" :type string)
  (persona "" :type string)
  (goal "" :type string)
  (role "specialist" :type string)
  delegates
  tools)

(defparameter +non-agent-headings+
  '("agents" "agent configuration" "agent configs" "swarm" "myswarm" "overview"
    "introduction" "summary" "architecture" "workflow" "rules" "guidelines"
    "conventions" "testing" "tests" "setup" "installation" "configuration"
    "context" "project context" "notes" "shared memory" "communication"
    "handoffs" "tools" "environment")
  "Headings that name a document's parts, never an agent.")

(defparameter +orchestrator-words+
  '("orchestrat" "coordinator" "lead" "manager" "router" "supervisor" "dispatcher" "chief")
  "A name, persona or goal carrying one of these is an orchestrator.")

(defparameter *abbreviation-chars* 220
  "How much of a persona or goal one line keeps before it is cut at a word.")

;;; --- small text helpers ----------------------------------------------------------

(defun trimmed (text)
  "TEXT without surrounding space, a newline or a tab."
  (string-trim '(#\Space #\Tab #\Return #\Linefeed) (or text "")))

(defun lines-of (text)
  "TEXT as its lines."
  (with-open-stream (in (make-string-input-stream (or text "")))
    (loop for line = (read-line in nil nil) while line collect line)))

(defun join (strings &optional (separator " "))
  (with-output-to-string (out)
    (loop for (s . rest) on strings
          do (write-string s out)
             (when rest (write-string separator out)))))

(defun split-on (char text)
  "TEXT split at every CHAR."
  (loop with start = 0
        for at = (position char text :start start)
        collect (subseq text start at)
        while at do (setf start (1+ at))))

(defun squeeze-space (text)
  "TEXT with every run of whitespace read as one space, trimmed."
  (with-output-to-string (out)
    (loop with space = nil
          for c across (trimmed text)
          do (if (member c '(#\Space #\Tab #\Return #\Linefeed))
                 (setf space t)
                 (progn (when space (write-char #\Space out))
                        (setf space nil)
                        (write-char c out))))))

(defun strip-markdown (text)
  "TEXT without the emphasis markers a heading or a field leaves on a value."
  (with-output-to-string (out)
    (loop for c across (or text "")
          unless (find c "*_`") do (write-char c out))))

(defun abbreviate (text &optional (max *abbreviation-chars*))
  "TEXT as one readable line: markdown folded away, whitespace one space, and
cut at a word boundary past MAX with an ellipsis."
  (let* ((clean (squeeze-space (strip-markdown (trimmed text))))
         (clean (trimmed (string-left-trim "-*#> " clean))))
    (cond ((zerop (length clean)) "Unspecified")
          ((<= (length clean) max) clean)
          (t (let* ((cut (subseq clean 0 (1- max)))
                    (space (position #\Space cut :from-end t)))
               (format nil "~a…" (trimmed (if space (subseq cut 0 space) cut))))))))

;;; --- names, roles and lists ------------------------------------------------------

(defun clean-agent-name (heading)
  "HEADING with its hashes, its number and its `Agent:' prefix taken off, and
only its primary name kept when it carries a subtitle."
  (let* ((text (trimmed (string-left-trim "#" (or heading ""))))
         (text (cl-ppcre:regex-replace "^[0-9]+[.)]\\s*" text ""))
         (text (cl-ppcre:regex-replace "(?i)^(?:agent|role|persona|bot|specialist|worker)\\s*[:\\-]\\s*" text ""))
         (text (trimmed (strip-markdown text))))
    (or (loop for separator in '(" — " " – " " - " ": ")
              for at = (cl-ppcre:scan (cl-ppcre:quote-meta-chars separator) text)
              when at
                do (let ((left (trimmed (subseq text 0 at))))
                     (when (<= 2 (length left) 36) (return left))))
        (subseq text 0 (min 40 (length text))))))

(defun infer-role (name persona goal delegates)
  "ORCHESTRATOR when DELEGATES is non-empty or the words say so, else SPECIALIST."
  (let ((blob (string-downcase (format nil "~a ~a ~a" name persona goal))))
    (if (or delegates
            (some (lambda (word) (cl-ppcre:scan word blob)) +orchestrator-words+))
        "orchestrator"
        "specialist")))

(defun parse-list-field (raw)
  "RAW as the items it lists: commas, semicolons, slashes and `and' divide."
  (let ((items (cl-ppcre:split "[,;/]|\\band\\b" (or raw ""))))
    (loop for item in items
          for cleaned = (trimmed (string-trim "`*_-[]()\"'" (trimmed item)))
          when (and (plusp (length cleaned))
                    (not (member (string-downcase cleaned) '("none" "n/a" "-") :test #'string=)))
            collect cleaned)))

;;; --- key/value lines -------------------------------------------------------------

(defparameter +persona-keys+
  '("persona" "personality" "character" "archetype" "identity" "style" "tone" "who" "vibe"))
(defparameter +goal-keys+
  '("goal" "goals" "objective" "objectives" "mission" "purpose" "responsibility"
    "responsibilities" "focus" "duty" "task"))
(defparameter +delegate-keys+
  '("delegates to" "delegate to" "subagent" "subagents" "handoff" "handoffs"
    "routes to" "route to" "collaborates with" "calls"))
(defparameter +tool-keys+ '("tool" "tools" "toolset" "toolsets" "capabilities"))

(defun key-line (line)
  "LINE as (KEY . VALUE) when it reads `**Key**: value', `- Key: value' or
`Key - value'; else NIL. KEY is lowercased, its emphasis and bullet stripped."
  (let* ((text (trimmed line))
         (text (if (and (plusp (length text)) (find (char text 0) "-*•")) (trimmed (subseq text 1)) text))
         (colon (position #\: text)))
    (flet ((answer (key value)
             (let ((key (string-downcase (trimmed (strip-markdown key)))))
               (when (and (plusp (length key)) (<= (length key) 30)
                          (plusp (length (trimmed value))))
                 (cons key (trimmed value))))))
      (cond ((and colon (answer (subseq text 0 colon) (subseq text (1+ colon))))
             (answer (subseq text 0 colon) (subseq text (1+ colon))))
            (t (loop for separator in '(" - " " – " " — ")
                     for at = (cl-ppcre:scan (cl-ppcre:quote-meta-chars separator) text)
                     when at
                       do (let ((answer (answer (subseq text 0 at) (subseq text (+ at (length separator))))))
                            (when answer (return answer)))))))))

(defun field-kind (key)
  "Which bucket KEY feeds: :PERSONA, :GOAL, :DELEGATES, :TOOLS, or NIL."
  (cond ((member key +persona-keys+ :test #'string=) :persona)
        ((member key +goal-keys+ :test #'string=) :goal)
        ((member key +delegate-keys+ :test #'string=) :delegates)
        ((member key +tool-keys+ :test #'string=) :tools)))
