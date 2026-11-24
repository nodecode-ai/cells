;;;; parse2.lisp --- the four readings, and the one document converter.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-myswarm)

;;; --- a heading's section ---------------------------------------------------------

(defun headings-of (text)
  "TEXT as a list of (LEVEL TITLE BODY-LINES), a heading's body the lines
under it and before the next heading of any level. HTML comments are gone."
  (let* ((clean (cl-ppcre:regex-replace-all "(?s)<!--.*?-->" (or text "") ""))
         (lines (lines-of clean))
         (out '()))
    (labels ((emit (level title body)
               (when level (push (list level title (reverse body)) out))))
      (let ((level nil) (title nil) (body '()))
        (dolist (line lines)
          (let ((groups (nth-value 1 (cl-ppcre:scan-to-strings "^(#{1,4})\\s+(.+)$" line))))
            (if groups
                (progn (emit level title body)
                       (setf level (length (aref groups 0))
                             title (trimmed (aref groups 1))
                             body '()))
                (push line body))))
        (emit level title body)))
    (nreverse out)))

(defun agent-section-p (title body)
  "Does TITLE's section declare an agent: explicit fields, a subtitle, or a body
that talks about one."
  (or (some (lambda (line) (field-kind (car (key-line line)))) body)
      (some (lambda (separator) (cl-ppcre:scan (cl-ppcre:quote-meta-chars separator) title))
            '(" — " " – " " - " ": "))
      (cl-ppcre:scan
       "(?i)\\b(you are|act as|persona|goal|objective|responsible for|specialist|agent|auditor|architect|researcher|engineer|writer|reviewer|planner)\\b"
       (join body (string #\Newline)))))

(defun parse-section (title body)
  "TITLE's section as an AGENT, or NIL when it declares none."
  (let* ((clean-title (cl-ppcre:regex-replace "[^a-z0-9\\s]" (string-downcase title) ""))
         (clean-title (trimmed clean-title)))
    (unless (and (member clean-title +non-agent-headings+ :test #'string=)
                 (null (some (lambda (line) (field-kind (car (key-line line)))) body)))
      (when (agent-section-p title body)
        (let ((persona-lines '()) (goal-lines '()) (delegates '()) (tools '())
              (prose '()) (role nil) (bucket nil)
              (persona-text (or (loop for separator in '(" — " " – " " - " ": ")
                                      for at = (cl-ppcre:scan (cl-ppcre:quote-meta-chars separator) title)
                                      when at do (let ((right (trimmed (subseq title (+ at (length separator))))))
                                                   (when (plusp (length right)) (return right))))
                                 "")))
          (dolist (raw body)
            (let ((line (trimmed raw)))
              (cond ((or (zerop (length line)) (cl-ppcre:scan "^```" line)) (setf bucket nil))
                    ((and (plusp (length line)) (char= #\| (char line 0))) nil)
                    (t (let ((field (key-line line)))
                         (cond
                           (field
                            (let ((kind (field-kind (car field))))
                              (cond ((eq kind :persona) (push (cdr field) persona-lines) (setf bucket :persona))
                                    ((eq kind :goal) (push (cdr field) goal-lines) (setf bucket :goal))
                                    ((eq kind :delegates) (setf delegates (append delegates (parse-list-field (cdr field)))
                                                              bucket nil))
                                    ((eq kind :tools) (setf tools (append tools (parse-list-field (cdr field)))
                                                              bucket nil))
                                    ((string= (car field) "role")
                                     (when (cl-ppcre:scan "(?i)orchestrat" (cdr field)) (setf role "orchestrator"))
                                     (setf bucket nil))
                                    (t (push line prose)))))
                           ((and bucket (cl-ppcre:scan "^[-*•]\\s+" line))
                            (push (cl-ppcre:regex-replace "^[-*•]\\s+" line "") 
                                  (ecase bucket (:persona persona-lines) (:goal goal-lines))))
                           (t (push line prose))))))))
          (let* ((prose (reverse prose))
                 (fallback (or (first prose) "Specialized swarm agent"))
                 (persona (or (and persona-lines (join (reverse persona-lines)))
                              (and (plusp (length persona-text)) persona-text)
                              fallback))
                 (goal (cond (goal-lines (join (reverse goal-lines)))
                             ((cdr prose) (second prose))
                             (prose (first prose))
                             ((plusp (length persona-text)) persona-text)
                             (t "Execute assigned domain tasks")))
                 (name (clean-agent-name title)))
            (when (plusp (length name))
              (make-agent :name name
                          :persona (abbreviate persona)
                          :goal (abbreviate goal)
                          :role (or role (infer-role name persona goal delegates))
                          :delegates delegates
                          :tools tools))))))))

(defun parse-sections (text)
  "Every agent TEXT declares by heading, in file order."
  (loop for (level title body) in (headings-of text)
        when (or (>= level 2)
                 (and (= level 1) (cl-ppcre:scan "^agent\\s*[:-]" title)))
          append (let ((agent (parse-section title body)))
                   (when agent (list agent)))))

;;; --- a table of agents -----------------------------------------------------------

(defparameter +table-separator+ "^\\|[\\s\\-:|]+\\|$"
  "A markdown table's second line: the dashes under its header.")

(defun table-column (headers words)
  "The index of the first header carrying one of WORDS, or NIL."
  (loop for header in headers
        for index from 0
        when (some (lambda (word) (cl-ppcre:scan word (string-downcase header))) words)
          return index))

(defun table-cells (line)
  (mapcar #'trimmed (split-on #\| (string-trim "|" (trimmed line)))))

(defun parse-tables (text)
  "Every agent TEXT declares in a table with a name and a persona or goal column."
  (let ((lines (lines-of (or text "")))
        (agents '()))
    (loop for rest on lines
          for line = (trimmed (first rest))
          for next = (trimmed (second rest))
          when (and next (plusp (length line))
                    (char= #\| (char line 0)) (char= #\| (char line (1- (length line))))
                    (cl-ppcre:scan +table-separator+ next))
            do (let* ((headers (table-cells line))
                      (name-i (table-column headers '("agent" "name" "id" "bot")))
                      (persona-i (table-column headers '("persona" "role" "character" "style" "identity" "vibe")))
                      (goal-i (table-column headers '("goal" "objective" "mission" "purpose" "task" "responsibilit")))
                      (delegates-i (table-column headers '("delegate" "subagent" "routes" "handoff"))))
                 (when (and name-i (or persona-i goal-i))
                   (loop for row in (cddr rest)
                         for cells = (table-cells row)
                         while (and (plusp (length (trimmed row))) (char= #\| (char (trimmed row) 0)))
                         do (let* ((name (and (< name-i (length cells)) (clean-agent-name (nth name-i cells)))))
                              (when (and name (not (member (string-downcase name)
                                                           '("agent" "name" "---") :test #'string=)))
                                (let* ((persona (or (and persona-i (< persona-i (length cells)) (nth persona-i cells))
                                                    "Specialized autonomous agent"))
                                       (goal (or (and goal-i (< goal-i (length cells)) (nth goal-i cells))
                                                 persona))
                                       (delegates (if (and delegates-i (< delegates-i (length cells)))
                                                      (parse-list-field (nth delegates-i cells))
                                                      '())))
                                  (push (make-agent :name name
                                                    :persona (abbreviate persona)
                                                    :goal (abbreviate goal)
                                                    :role (infer-role name persona goal delegates)
                                                    :delegates delegates)
                                        agents))))))))
    (nreverse agents)))

;;; --- bullets ---------------------------------------------------------------------

(defun parse-bullets (text)
  "Every agent TEXT declares as `- **Name** (persona): goal'."
  (let ((agents '()))
    (loop for line in (lines-of (or text ""))
          for groups = (nth-value 1
                                (cl-ppcre:scan-to-strings
                                 "^(?:-|\\*|•)\\s+\\*\\*([A-Za-z0-9_\\-\\s]{2,35})\\*\\*\\s*(?:\\(([^)]+)\\))?\\s*[:\\-–—]\\s*(.+)$"
                                 (trimmed line)))
          when groups
            do (let* ((key (trimmed (aref groups 0)))
                      (paren (aref groups 1))
                      (rest (trimmed (aref groups 2))))
                 (unless (member (string-downcase key)
                                 '("persona" "goal" "goals" "role" "objective" "tools" "delegates to"
                                   "note" "warning" "important" "input" "output" "style")
                                 :test #'string=)
                   (let ((persona (if (and paren (plusp (length (trimmed paren))))
                                      (trimmed paren)
                                      (let ((at (cl-ppcre:scan " — |; " rest)))
                                        (if at (trimmed (subseq rest 0 at)) (format nil "~a specialist" key)))))
                         (goal (let ((at (cl-ppcre:scan " — |; " rest)))
                                 (if (and at (plusp (length (trimmed paren))))
                                     (trimmed (subseq rest (+ at (if (char= #\Space (char rest at)) 3 2))))
                                     rest))))
                     (push (make-agent :name key
                                       :persona (abbreviate persona)
                                       :goal (abbreviate goal)
                                       :role (infer-role key persona goal '()))
                           agents)))))
    (nreverse agents)))

;;; --- the shared ASCII block -------------------------------------------------------

(defun ascii-block-p (text)
  "Is TEXT a MySwarm copy-paste block rather than markdown?"
  (and (or (cl-ppcre:scan "MYSWARM //" text)
           (cl-ppcre:scan "\\[Shared via MySwarm" text))
       t))
(defun parse-ascii-block (text)
  "TEXT, a shared MySwarm ASCII block, as (VALUES TITLE AGENTS), or NIL."
  (when (ascii-block-p text)
    (let ((title "MySwarm Agent Configuration")
          (agents '())
          (name nil) (role "specialist") (persona "") (goal "") (routes '()) (field nil))
      (flet ((head-line (line)
               (cl-ppcre:scan-to-strings
                "^\\|\\s*\\[[^]]*\\]\\s+(.+?)\\s+\\[(ORCHESTRATOR|SPECIALIST)\\]\\s*\\|$" line))
             (field-line (line)
               (cl-ppcre:scan-to-strings
                "^\\|\\s*\\*\\s*(Persona|Goal|Routes)\\s*:\\s*(?:===>\\s*)?(.+?)\\s*\\|$" line))
             (cont-line (line)
               (cl-ppcre:scan-to-strings "^\\|\\s{6,}([^|*].*?)\\s*\\|$" line))
             (finish ()
               (when name
                 (push (make-agent :name name
                                   :persona (if (plusp (length persona)) persona
                                                 (format nil "~a specialist" name))
                                   :goal (if (plusp (length goal)) goal
                                             (format nil "Execute ~a tasks" name))
                                   :role role
                                   :delegates routes)
                       agents))
               (setf name nil role "specialist" persona "" goal "" routes '() field nil)))
        (dolist (raw (lines-of text))
          (let ((line (trimmed raw)))
            (cond
              ((zerop (length line)) nil)
              ((char= #\+ (char line 0)) (setf field nil))
              ((cl-ppcre:scan "MYSWARM //" line)
               (setf title (trimmed (cl-ppcre:regex-replace "\\(.*$" 
                                                             (cl-ppcre:regex-replace "^.*MYSWARM //" line "")
                                                             ""))))
              ((head-line line)
               (multiple-value-bind (whole groups) (head-line line)
                 (declare (ignore whole))
                 (finish)
                 (setf name (trimmed (aref groups 0))
                       role (if (string-equal (aref groups 1) "orchestrator")
                                "orchestrator" "specialist"))))
              ((and name (field-line line))
               (multiple-value-bind (whole groups) (field-line line)
                 (declare (ignore whole))
                 (let ((kind (string-downcase (aref groups 0)))
                       (value (trimmed (aref groups 1))))
                   (setf field kind)
                   (cond ((string= kind "persona") (setf persona value))
                         ((string= kind "goal") (setf goal value))
                         ((string= kind "routes")
                          (setf routes (mapcar #'trimmed (split-on #\, value))))))))
              ((and name field (cont-line line))
               (multiple-value-bind (whole groups) (cont-line line)
                 (declare (ignore whole))
                 (let ((value (trimmed (aref groups 0))))
                   (cond ((string= field "persona")
                          (setf persona (trimmed (format nil "~a ~a" persona value))))
                         ((string= field "goal")
                          (setf goal (trimmed (format nil "~a ~a" goal value))))
                         ((string= field "routes")
                          (setf routes (append routes (mapcar #'trimmed (split-on #\, value))))))))))))
        (finish))
      (when agents (values title (nreverse agents))))))
