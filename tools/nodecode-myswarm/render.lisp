;;;; render.lisp --- the list, the box-and-arrow topology, the copy-paste block.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three renderings of the same parsed swarm, none of them a screenshot: a
;;;; table of names with their personas and goals, a diagram of boxes and
;;;; arrows with the orchestrator above the specialists it routes to, and a
;;;; block of pure 7-bit ASCII small enough to paste into a chat. The third is
;;;; also an input: PARSE-ASCII-BLOCK reads it back, which is how a shared
;;;; diagram becomes an agents.md again.

(in-package #:nodecode-myswarm)

(defparameter *card-width* 46
  "A card's inner width in the topology diagram.")

(defparameter *copy-width* 78
  "The copy-paste block's width, borders included.")

;;; --- wrapping -------------------------------------------------------------------

(defun wrap-text (text width)
  "TEXT as lines of at most WIDTH characters, cut at spaces; a word longer
than WIDTH is cut itself, never dropped."
  (let ((words (remove-if #'zerop (cl-ppcre:split "\\s+" (or text "")) :key #'length))
        (out '()) (line ""))
    (when (zerop (length (trimmed text))) (return-from wrap-text (list "")))
    (dolist (word words)
      (cond
        ((zerop (length line)) (setf line word))
        ((<= (+ (length line) 1 (length word)) width) (setf line (format nil "~a ~a" line word)))
        (t (push line out) (setf line word))))
    (when (plusp (length line)) (push line out))
    (loop for line in (nreverse out)
          collect (if (<= (length line) width) line (subseq line 0 width)))))

(defun labeled-lines (label text width)
  "TEXT wrapped into WIDTH, every line but the first under LABEL's indent."
  (let* ((indent (make-string (length label) :initial-element #\Space))
         (wrapped (wrap-text text (max 8 (- width (length label))))))
    (loop for line in wrapped
          for index from 0
          collect (format nil "~a~a" (if (zerop index) label indent) line))))

(defun fit (text width)
  "TEXT in exactly WIDTH characters: padded, or cut with an ellipsis."
  (let ((text (or text "")))
    (cond ((= (length text) width) text)
          ((< (length text) width) (format nil "~a~a" text (make-string (- width (length text))
                                                                         :initial-element #\Space)))
          (t (format nil "~a…" (subseq text 0 (max 0 (1- width))))))))

(defun pad (text width)
  (format nil "~a~a" text (make-string (max 0 (- width (length text))) :initial-element #\Space)))

;;; --- the list --------------------------------------------------------------------

(defun list-text (title agents &key (source ""))
  "AGENTS as the table a user reads: one row each, an asterisk on the
orchestrator, every persona and goal abbreviated to one line."
  (if (null agents)
      (format nil "No agents found in ~a."
              (if (plusp (length source)) source "the configuration"))
      (let* ((name-w (min 24 (max (length "Agent")
                                   (loop for a in agents maximize (+ 2 (length (agent-name a)))))))
             (persona-w (min 38 (max (length "Persona") (loop for a in agents maximize (length (agent-persona a))))))
             (goal-w (min 44 (max (length "Goal") (loop for a in agents maximize (length (agent-goal a))))))
             (rule (make-string name-w :initial-element #\─))
             (top (format nil "┌─~a─┬─~a─┬─~a─┐" rule
                          (make-string persona-w :initial-element #\─)
                          (make-string goal-w :initial-element #\─)))
             (header (format nil "│ ~a │ ~a │ ~a │" (fit "Agent" name-w) (fit "Persona" persona-w) (fit "Goal" goal-w)))
             (separator (format nil "├─~a─┼─~a─┼─~a─┤" rule
                                (make-string persona-w :initial-element #\─)
                                (make-string goal-w :initial-element #\─)))
             (bottom (format nil "└─~a─┴─~a─┴─~a─┘" rule
                             (make-string persona-w :initial-element #\─)
                             (make-string goal-w :initial-element #\─))))
        (with-output-to-string (out)
          (format out "Swarm Config: ~a (~d agent~:p)~%~a~%~a~%~a~%"
                  (if (plusp (length source)) source "(in-memory)") (length agents)
                  top header separator)
          (dolist (agent agents)
            (format out "│ ~a │ ~a │ ~a │~%"
                    (fit (format nil "~a~a" (if (string-equal (agent-role agent) "orchestrator") "* " "  ")
                                  (agent-name agent))
                         name-w)
                    (fit (agent-persona agent) persona-w)
                    (fit (agent-goal agent) goal-w)))
          (format out "~a~%(★ = Orchestrator / Coordinator)~%" bottom)))))

;;; --- the topology ----------------------------------------------------------------

(defun badge (agent)
  (if (string-equal (agent-role agent) "orchestrator") "[ORCHESTRATOR]" "[AGENT]"))

(defun card (agent width)
  "AGENT as a box of WIDTH inner characters: its badge and name, its persona,
its goal, and who it routes to."
  (let* ((persona (labeled-lines "Persona: " (agent-persona agent) (- width 2)))
         (goal (labeled-lines "Goal   : " (agent-goal agent) (- width 2)))
         (routes (when (agent-delegates agent)
                   (labeled-lines "Routes : " (join (agent-delegates agent) ", ") (- width 2))))
         (rule (make-string width :initial-element #\─))
         (title (fit (format nil "~a ~a" (badge agent) (agent-name agent)) (- width 2))))
    (append (list (format nil "┌~a┐" rule)
                  (format nil "│ ~a │" title)
                  (format nil "├~a┤" rule))
            (mapcar (lambda (line) (format nil "│ ~a │" (pad line (- width 2)))) persona)
            (mapcar (lambda (line) (format nil "│ ~a │" (pad line (- width 2)))) goal)
            (mapcar (lambda (line) (format nil "│ ~a │" (pad line (- width 2)))) routes)
            (list (format nil "└~a┘" rule)))))

(defun card-row (agents width)
  "AGENTS (one or two) side by side, the shorter card padded to the taller."
  (let* ((cards (mapcar (lambda (agent) (card agent width)) agents))
         (height (loop for c in cards maximize (length c)))
         (padded (mapcar (lambda (c)
                           (append c (loop repeat (- height (length c))
                                           collect (make-string (+ width 2) :initial-element #\Space))))
                         cards))
         (gap (make-string 4 :initial-element #\Space)))
    (loop for row from 0 below height
          collect (join (mapcar (lambda (c) (nth row c)) padded) gap))))

(defun topology-text (title agents)
  "AGENTS as the box-and-arrow diagram: the orchestrators above the
specialists they route to, and every delegation named under it."
  (if (null agents)
      "Empty swarm configuration (0 agents)."
      (let* ((orchestrators (remove-if-not (lambda (a) (string-equal (agent-role a) "orchestrator")) agents))
             (specialists (remove-if (lambda (a) (string-equal (agent-role a) "orchestrator")) agents))
             (banner (format nil " MySwarm Topology: ~a (~d Agent~:p) " title (length agents)))
             (banner-w (max (+ 4 (length banner)) 62))
             (out '()))
        (labels ((emit (line) (push line out))
                 (emit-all (lines) (dolist (line lines) (push line out))))
          (emit (format nil "┌~a┐" (make-string banner-w :initial-element #\─)))
          (emit (format nil "│~a│" (let* ((text (trimmed banner))
                                            (left (floor (- banner-w (length text)) 2)))
                                       (format nil "~a~a~a" (make-string left :initial-element #\Space) text
                                               (make-string (- banner-w left (length text))
                                                            :initial-element #\Space)))))
          (emit (format nil "└~a┘" (make-string banner-w :initial-element #\─)))
          (emit "")
          (when orchestrators
            (emit-all (card-row (subseq orchestrators 0 (min 2 (length orchestrators))) *card-width*))
            (when specialists
              (let ((center (floor (+ *card-width* 2) 2)))
                (cond ((= 1 (length specialists))
                       (emit (format nil "~a│" (make-string center :initial-element #\Space)))
                       (emit (format nil "~a▼" (make-string center :initial-element #\Space))))
                      (t (let ((second-center (+ (+ *card-width* 2) 4 (floor (+ *card-width* 2) 2))))
                           (emit (format nil "~a┌~a┐" (make-string center :initial-element #\Space)
                                         (make-string (- second-center center 1) :initial-element #\─)))
                           (emit (format nil "~a▼~a▼" (make-string center :initial-element #\Space)
                                         (make-string (- second-center center 1) :initial-element #\Space)))))))))
          (unless orchestrators (setf specialists agents))
          (loop for pair on specialists by #'cddr
                for index from 0
                do (emit-all (card-row pair *card-width*))
                   (when (cddr pair)
                     (emit (format nil "~a│" (make-string (floor (+ *card-width* 2) 2) :initial-element #\Space)))
                     (emit (format nil "~a▼" (make-string (floor (+ *card-width* 2) 2) :initial-element #\Space)))))
          (let ((flows (loop for agent in agents
                             when (agent-delegates agent)
                               append (mapcar (lambda (target) (format nil "  ~a ──▶ ~a" (agent-name agent) target))
                                              (agent-delegates agent)))))
            (when flows
              (emit "")
              (emit "Delegation Flows:")
              (emit-all flows))))
        (join (nreverse out) (string #\Newline)))))

;;; --- the copy-paste block --------------------------------------------------------

(defparameter +icons+
  '(("orchestrator" . "[\\^/]")
    ("security" . "[#!#]") ("audit" . "[#!#]") ("guard" . "[#!#]") ("trust" . "[#!#]") ("privacy" . "[#!#]")
    ("code" . "[{/}]") ("engineer" . "[{/}]") ("dev" . "[{/}]") ("build" . "[{/}]") ("forge" . "[{/}]")
    ("research" . "[?=?]") ("search" . "[?=?]") ("data" . "[?=?]") ("doc" . "[?=?]") ("analy" . "[?=?]"))
  "The pure-ASCII badge an agent's role earns, most specific first.")

(defun agent-icon (agent)
  "A 7-bit badge for AGENT: the orchestrator's, one its words earn, or none."
  (if (string-equal (agent-role agent) "orchestrator")
      "[\\^/]"
      (let ((blob (string-downcase (format nil "~a ~a ~a" (agent-name agent)
                                           (agent-persona agent) (agent-goal agent)))))
        (or (loop for (word . icon) in +icons+
                  when (cl-ppcre:scan word blob) return icon)
            "[o_o]"))))

(defun ascii-only (text)
  "TEXT with every character above 126 read as a plain equivalent."
  (with-output-to-string (out)
    (loop for c across (or text "")
          do (write-char (let ((code (char-code c)))
                           (if (<= code 126) c
                               (cond ((member c '(#\… )) #\.)
                                     ((and (<= 8216 code) (<= code 8217)) #\')
                                     ((and (<= 8220 code) (<= code 8221)) #\")
                                     ((member c '(#\— #\–)) #\-)
                                     (t #\?))))
                         out))))

(defun copy-paste-text (title agents)
  "AGENTS as a block of pure 7-bit ASCII, one card a person, stamped so a
reader knows where it came from and PARSE-ASCII-BLOCK can read it back."
  (if (null agents)
      "+--- MySwarm (0 agents) ---+"
      (let* ((width *copy-width*)
             (inner (- width 2))
             (clean-title (trimmed (ascii-only title)))
             (header (format nil "MYSWARM // ~a (~d Agents)"
                             (if (plusp (length clean-title)) clean-title "Agent Swarm")
                             (length agents)))
             (ordered (append (remove-if-not (lambda (a) (string-equal (agent-role a) "orchestrator")) agents)
                              (remove-if (lambda (a) (string-equal (agent-role a) "orchestrator")) agents)))
             (out '())
             (rule (make-string inner :initial-element #\-)))
        (labels ((emit (line) (push line out))
                 (framed (line) (emit (format nil "| ~a |" (pad (fit (ascii-only line) (- inner 2)) (- inner 2))))))
          (emit (format nil "+~a+" (make-string inner :initial-element #\=)))
          (framed header)
          (emit (format nil "+~a+" (make-string inner :initial-element #\=)))
          (loop for agent in ordered
                for index from 0
                do (when (plusp index)
                     (emit "       |")
                     (emit "       v"))
                   (emit (format nil "+~a+" rule))
                   (framed (format nil "~a ~a [~a]" (agent-icon agent) (agent-name agent)
                                   (if (string-equal (agent-role agent) "orchestrator") "ORCHESTRATOR" "SPECIALIST")))
                   (emit (format nil "+~a+" rule))
                   (dolist (line (labeled-lines "  * Persona : " (agent-persona agent) (- inner 2)))
                     (framed line))
                   (dolist (line (labeled-lines "  * Goal    : " (agent-goal agent) (- inner 2)))
                     (framed line))
                   (when (agent-delegates agent)
                     (dolist (line (labeled-lines "  * Routes  : ===> " (join (agent-delegates agent) ", ") (- inner 2)))
                       (framed line)))
                   (emit (format nil "+~a+" rule)))
          (let ((flows (loop for agent in ordered
                             when (agent-delegates agent)
                               collect (format nil "~a ==>> ~a" (agent-name agent)
                                               (join (agent-delegates agent) ", ")))))
            (when flows
              (dolist (line (labeled-lines "FLOW: " (join flows " | ") (- inner 2)))
                (framed line))
              (emit (format nil "+~a+" rule))))
          (emit (format nil "~a" (pad "[Shared via MySwarm]" width))))
        (join (nreverse out) (string #\Newline)))))
