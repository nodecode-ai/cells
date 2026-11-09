;;;; cell.lisp --- Poteto's engineering rigor stack as one cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes plugin of skills only: an orchestrator skill,
;;;; poteto-mode, that routes a task to a playbook, the playbooks beside it,
;;;; and 51 supporting skills under skills/. Hermes loads a skill whole and
;;;; lists every one of them to the model; here the prose sits in prose/,
;;;; unchanged, and one `pstack' tool reads one file when it is asked for --
;;;; so none of the 516K rides a request until the model or the operator wants
;;;; it. The catalogue names every playbook with the line it opens with, and
;;;; every skill with its own description.
;;;;
;;;; Nothing is rewritten in the prose itself. A served file carries a short
;;;; header above it saying where it came from and how the original's
;;;; `skill_view(name='pstack:X', file_path='playbooks/Y.md')' reads here:
;;;; pstack(name="X") and pstack(name="Y").

(defpackage #:nodecode-pstack
  (:use #:cl))

(in-package #:nodecode-pstack)

(nlk:define-peripheral pstack :not-running t)

;;; --- where the prose is ---------------------------------------------------------

(defun prose-root ()
  "The folder's prose/ directory: the original's skills/ tree, unchanged."
  (asdf:system-relative-pathname "nodecode-pstack" "prose/"))

(defun skills-root ()
  "prose/skills/: the original's skills/ folder, one directory per skill."
  (merge-pathnames "skills/" (prose-root)))

(defun playbook-directory ()
  (merge-pathnames "poteto-mode/playbooks/" (skills-root)))

(defparameter +router+ "poteto-mode"
  "The orchestrator skill: the file a task should be routed through first.")

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun squeeze (value)
  "VALUE as one line of text: runs of whitespace collapsed to one space."
  (if (stringp value)
      (with-output-to-string (out)
        (let ((space nil))
          (loop for char across (string-trim '(#\Space #\Tab #\Newline #\Return) value)
                do (if (whitespace-p char)
                       (setf space t)
                       (progn (when space (write-char #\Space out))
                              (setf space nil)
                              (write-char char out))))))
      ""))

(defun shorten (text limit)
  "TEXT cut at LIMIT characters on a word boundary, with an ellipsis when it was cut."
  (if (<= (length text) limit)
      text
      (let ((space (position #\Space text :end limit :from-end t)))
        (format nil "~a..." (subseq text 0 (or space (max 0 (- limit 3))))))))

(defun unmarkup (text)
  "TEXT with the markdown emphasis removed: `**', backticks and lone underscores."
  (with-output-to-string (out)
    (loop with chars = (coerce text 'vector)
          for i from 0 below (length chars)
          do (let ((char (aref chars i)))
               (cond ((and (char= char #\*) (< (1+ i) (length chars)) (char= (aref chars (1+ i)) #\*))
                      (incf i))
                     ((member char '(#\* #\` #\_) :test #'char=))
                     (t (write-char char out)))))))

;;; --- what the prose says about itself -------------------------------------------

(defun front-text (line prefix)
  "LINE's value after PREFIX, unquoted."
  (let ((value (string-trim '(#\Space #\Tab #\Return) (subseq line (length prefix)))))
    (if (and (>= (length value) 2)
             (member (char value 0) '(#\" #\') :test #'char=)
             (char= (char value 0) (char value (1- (length value)))))
        (subseq value 1 (1- (length value)))
        value)))

(defun frontmatter (path)
  "SKILL.md's own name and description, as (values NAME DESCRIPTION), or NILs
when it carries no frontmatter."
  (with-open-file (in path :external-format :utf-8)
    (let ((name nil) (description nil))
      (when (string= (string-trim '(#\Space #\Tab #\Return) (read-line in nil "")) "---")
        (loop for line = (read-line in nil nil)
              while line
              until (string= (string-trim '(#\Space #\Tab #\Return) line) "---")
              do (cond ((uiop:string-prefix-p "name:" line) (setf name (front-text line "name:")))
                       ((uiop:string-prefix-p "description:" line)
                        (setf description (front-text line "description:"))))))
      (values name description))))

(defun first-impression (path)
  "A playbook's own opener: its first bolded line, or the first prose line
under its heading, squeezed and cut at 180 characters."
  (with-open-file (in path :external-format :utf-8)
    (loop repeat 40
          for line = (read-line in nil nil)
          while line
          for text = (string-trim '(#\Space #\Tab #\Return) line)
          when (search "**" text)
            return (shorten (squeeze (unmarkup text)) 180)
          when (and (plusp (length text))
                    (not (member (char text 0) '(#\# #\- #\| #\>) :test #'char=)))
            return (shorten (squeeze (unmarkup text)) 180))))

;;; --- the catalogue ---------------------------------------------------------------

(defstruct (piece (:constructor make-piece (name kind path summary)))
  "One file the model can ask for: what it is called, what it is, where it
lives and the one line that says when to read it."
  name kind path summary)

(defun directory-name (path)
  (let ((parts (pathname-directory path)))
    (string-downcase (or (first (last parts)) ""))))

(defun path-relative (path)
  "PATH below prose/, as `poteto-mode/playbooks/feature.md'."
  (let ((root (namestring (truename (prose-root))))
        (full (namestring (truename path))))
    (if (uiop:string-prefix-p root full) (subseq full (length root)) full)))

(defun discover ()
  "Every skill and playbook under prose/. => (values PLAYBOOKS SKILLS PROBLEMS)."
  (let ((root (skills-root)) (playbooks '()) (skills '()) (problems '()))
    (if (not (probe-file root))
        (values '() '() (list (format nil "~a is missing: no playbook can be read" (namestring root))))
        (progn
          (dolist (dir (uiop:subdirectories root))
            (let* ((name (directory-name dir))
                   (file (merge-pathnames "SKILL.md" dir)))
              (if (probe-file file)
                  (multiple-value-bind (declared description) (frontmatter file)
                    (push (make-piece (if (and declared (plusp (length declared))) declared name)
                                      :skill file (shorten (squeeze description) 150))
                          skills))
                  (push (format nil "~a/ has no SKILL.md" name) problems))))
          (let ((directory (playbook-directory)))
            (when (probe-file directory)
              (dolist (file (uiop:directory-files directory))
                (when (equal "md" (pathname-type file))
                  (push (make-piece (string-downcase (pathname-name file)) :playbook file
                                    (first-impression file))
                        playbooks)))))
          (setf playbooks (sort playbooks #'string< :key #'piece-name)
                skills (sort skills #'string< :key #'piece-name))
          (values playbooks skills (nreverse problems))))))

(defun playbooks () (setting :playbooks))

(defun skills () (setting :skills))

;;; --- finding and reading one -------------------------------------------------------

(defun inside-prose-p (path)
  "Whether PATH, followed through every link, stays under prose/."
  (let ((root (namestring (truename (prose-root))))
        (full (ignore-errors (namestring (truename path)))))
    (and full (uiop:string-prefix-p root full))))

(defun escapes-prose-p (name)
  "Whether NAME, read as a relative path, could escape prose/."
  (let ((parts (uiop:split-string name :separator "/")))
    (or (zerop (length name))
        (char= (char name 0) #\/)
        (member ".." parts :test #'string=)
        (member "." parts :test #'string=))))

(defun piece-by-path (name)
  "NAME read as a file under prose/skills/, as `reflect/judgment-reviewer', or NIL."
  (unless (escapes-prose-p name)
    (let* ((parts (uiop:split-string name :separator "/"))
           (last-part (car (last parts)))
           (relative (if (find #\. last-part) name (concatenate 'string name ".md")))
           ;; A skill's own references/ folder may be left out of the path:
           ;; `reflect/judgment-reviewer' finds reflect/references/judgment-reviewer.md.
           (spelled (when (> (length parts) 1)
                      (format nil "~a/references/~a" (first parts)
                              (format nil "~{~a~^/~}" (rest parts))))))
      (flet ((piece-at (relative)
               (let ((path (merge-pathnames relative (merge-pathnames "skills/" (prose-root)))))
                 (when (and (probe-file path) (inside-prose-p path)
                            (not (uiop:directory-pathname-p path)))
                   (make-piece name :file path (first-impression path))))))
        (or (piece-at relative)
            (and spelled (piece-at (if (find #\. last-part) spelled
                                        (concatenate 'string spelled ".md")))))))))

(defun find-piece (name)
  "The piece NAME names, or NIL."
  (let ((wanted (string-downcase (squeeze name))))
    (or (find wanted (append (playbooks) (skills)) :key #'piece-name :test #'string=)
        (piece-by-path wanted))))

(defun unknown-piece (name)
  (fail "~a is not a pstack playbook or skill: pstack() lists every name (playbooks: ~{~a~^, ~})"
        name (mapcar #'piece-name (playbooks))))

(defun read-text (path limit)
  "PATH's text, cut at LIMIT characters on a line boundary.
=> (values TEXT CUT-P)."
  (let ((text (uiop:read-file-string path :external-format :utf-8)))
    (if (<= (length text) limit)
        (values text nil)
        (let ((cut (or (position #\Newline text :end limit :from-end t) limit)))
          (values (subseq text 0 cut) t)))))

(defun header (piece)
  "The two lines above a served file: what it is, where it came from, and how
the original's names read here."
  (format nil ";; pstack: ~a (~(~a~)), prose/~a~%;; The original's text, unchanged. Where it says skill_view(name='pstack:X') read pstack(name=\"X\"), and playbooks/Y.md is pstack(name=\"Y\"). pstack() lists every name."
          (piece-name piece) (piece-kind piece) (path-relative (piece-path piece))))

(defparameter *read-text* 'read-text
  "What a piece's text is read with: (PATH LIMIT) => (values TEXT CUT-P). A
symbol, so a test or another cell can substitute one.")

(defun serve (name)
  "NAME's file, whole or cut at pstack.max_chars, under its header."
  (let* ((piece (or (find-piece name) (unknown-piece name)))
         (limit (setting :max-chars)))
    (multiple-value-bind (text cut) (funcall *read-text* (piece-path piece) limit)
      (format nil "~a~%~%~a~:[~;~%~%;; cut at ~d characters: the rest is in prose/~a. Read that file, or raise pstack.max_chars.~]"
              (header piece) text cut limit (path-relative (piece-path piece))))))

;;; --- the catalogue as one answer -----------------------------------------------------

(defun catalogue-lines (title pieces width)
  (with-output-to-string (out)
    (format out "~a~%" title)
    (dolist (piece pieces)
      (format out "  ~va  ~a~%" width (piece-name piece) (or (piece-summary piece) "")))))

(defun catalogue ()
  "What pstack() answers: how to use it, every playbook with the line it
opens with, and every skill with its own description."
  (with-output-to-string (out)
    (format out "pstack: Poteto's engineering rigor stack. ~d playbooks, ~d skills.~%"
            (length (playbooks)) (length (skills)))
    (format out "Read one whole with pstack(name=\"feature\"). Start with pstack(name=\"~a\"): the router,~%its ladder and principles, and which task goes to which playbook.~%~%" +router+)
    (format out "~a" (catalogue-lines "Playbooks:" (playbooks) 20))
    (format out "~%~a" (catalogue-lines "Skills (read with pstack(name=\"how\")):" (skills) 34))
    (format out "~%A reference file a skill names is read by its path: pstack(name=\"reflect/judgment-reviewer\").~%")
    (when (setting :problems)
      (format out "~%Not read: ~{~a~^; ~}~%" (setting :problems)))))

;;; --- the manual ---------------------------------------------------------------------

(defparameter +primer+
  "pstack is Poteto's engineering rigor stack: an orchestrator and the playbooks and
skills under it. Call the `pstack' tool with no name for the catalogue: every
playbook with the line it opens with, every skill with its own description.
Then call it with one name to read that file whole, before starting the work.
Start with pstack(name=\"poteto-mode\"): it routes a task to a playbook and
carries the ladder, the principles and the autonomy rules. The usual route:
investigation to understand, bug-fix or feature or perf-issue or refactoring to
change, figure-it-out when none fits, then opening-a-pr and shipping (and
babysit, autopilot or orchestrate for a queue of pull requests). A skill is read
the same way: pstack(name=\"architect\"), pstack(name=\"interrogate\"),
pstack(name=\"principle-prove-it-works\"). A reference file a skill names is read
by its path: pstack(name=\"reflect/judgment-reviewer\").
The text is the original plugin's, unchanged; a served file says where it sits
under prose/. Where it says skill_view(name='pstack:X') or a playbook path,
that is pstack(name=\"X\"). Where it says delegate_task, that is Nodecode's
delegation; where it says cronjob, the cron cell.
The pstack section of the config has max_chars (default 24000): how much of one
file one call returns, the rest named in the answer. The operator reads the same
two answers with /pstack and /pstack NAME."
  "What (help :pstack) answers while the cell runs.")

;;; --- the tool -----------------------------------------------------------------------

(defun playbook-names () (mapcar #'piece-name (playbooks)))

(defun tool-schema ()
  (list :description
        (format nil "Poteto's engineering playbooks: call with a name to read one whole before starting the work, or with no name for the catalogue (~d playbooks, ~d skills)~@[. Playbooks: ~{~a~^, ~}~]."
                (length (playbooks)) (length (skills)) (playbook-names))
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "name"
          (nlk:json-object
           "type" "string"
           "description" "A playbook name (feature, bug-fix, investigation, ...), a skill name (poteto-mode, how, architect, interrogate, principle-prove-it-works, ...), or a reference file's path (reflect/judgment-reviewer). Omit to list them all."))
         "required" (vector))))

(defun pstack-tool (arguments)
  "The `pstack' tool: ARGUMENTS the decoded call, a hash table."
  (let ((name (nlk:json-value arguments :string "name")))
    (if (or (null name) (zerop (length (squeeze name))))
        (catalogue)
        (serve name))))

;;; --- /pstack --------------------------------------------------------------------------

(defun run-slash (args session-id)
  "Typing /pstack alone: the catalogue. /pstack NAME: that file, whole."
  (declare (ignore session-id))
  (let ((name (squeeze (or args ""))))
    (if (zerop (length name))
        (values (catalogue)
                (nle:list-dialog
                 :title "pstack"
                 :rows (list (nle:run-row "Read the router" "/pstack poteto-mode")
                             (nle:run-row "Read the feature playbook" "/pstack feature"))))
        (serve name))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell pstack
  (:section ("pstack")
    (:guide "Nothing to set: pstack reads the playbooks in its own folder. max_chars caps one answer, the rest named in it")
    ("max_chars" :integer :default 24000 :min 200
                 :doc "how much of one playbook or skill a call returns; the rest of the file is named in the answer"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (multiple-value-bind (playbooks skills problems) (discover)
                 (append values (list :playbooks playbooks :skills skills :problems problems)))))
  (:help :pstack
         "pstack: Poteto's engineering playbooks - call pstack() for the catalogue, pstack(name) to read one"
         +primer+)
  (:command "pstack" 'run-slash :catches 'pstack-error
            :description "Poteto's playbooks: every name, or one whole"
            :argument-hint "PLAYBOOK")
  (:start (lambda ()
            (when (setting :problems)
              (nle:notice (format nil "pstack: ~{~a~^; ~}" (setting :problems))
                          :level :warning :key "nodecode-pstack")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-pstack"))))
            (when (or (playbooks) (skills))
              (nle:register-tool "pstack" (tool-schema) #'pstack-tool)
              (nle:on-stop (lambda () (nle:unregister-tool "pstack")))))))
