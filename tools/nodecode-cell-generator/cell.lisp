;;;; cell.lisp --- a cell that writes cells: scaffold one, then add to it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of iruzen-dono/hermes-plugin-gen. The original wrote
;;;; a Hermes plugin folder from a description (plugin.yaml, __init__.py,
;;;; schemas.py, tools.py) under ~/.hermes/plugins, then patched a folder it
;;;; had written to add one tool or one bundled skill. Here the same three
;;;; verbs write the same kind of folder for THIS organism -- a cell folder
;;;; under the home's cells/, which the next boot loads -- and patch one of
;;;; them: nodecode-NAME.asd (the system and its test system), cell.lisp
;;;; (package, one schema and one handler a tool, the NLE:DEFINE-CELL
;;;; declaration with its section, its tools, its command and its manual
;;;; line), README.md, and test/ (support.lisp and cell-test.lisp, the
;;;; lifecycle tests every cell's slice carries).
;;;;
;;;; The one thing the original could not promise is the one that matters
;;;; for a folder the loader runs: a name is letters, digits and dashes --
;;;; no dot, no slash -- so a scaffolded folder is always inside the cells
;;;; folder, and an existing folder is refused, never overwritten. A tool or
;;;; a command added later is spliced in at a marker line the scaffold
;;;; writes, so a hand-written cell that carries no markers refuses by name
;;;; instead of being half-patched.

(in-package #:nodecode-cell-generator)

;;; --- the markers a scaffolded cell.lisp carries ------------------------------

(defparameter *tools-end* ";;; --- end of tools ---"
  "The line a scaffolded cell.lisp ends its tool handlers at.")

(defparameter *tools-anchor* "  ;; more tools here"
  "The line inside a scaffolded cell's NLE:DEFINE-CELL that one more :tool clause goes above.")

(defparameter *commands-end* ";;; --- end of commands ---"
  "The line a scaffolded cell.lisp ends its command functions at.")

(defparameter *commands-anchor* "  ;; more commands here"
  "The line inside a scaffolded cell's NLE:DEFINE-CELL that one more :command clause goes above.")

(defparameter *param-types* '("string" "number" "integer" "boolean" "array" "object")
  "The JSON-Schema types a tool parameter may name; anything else reads as string.")

;;; --- the manual, and the one command ------------------------------------------
;;; (help :cell-generator) answers +MANUAL+ while the cell runs; every request
;;; carries the one line the :HELP clause below gives, never this text.

(defparameter +manual+
  "A cell here is a folder under the home's cells/ that carries an .asd, which
Nodecode loads at the next boot (or at (restart-cells)). This cell writes one,
and adds to one it wrote, through three tools and the cell-generator: package:
  cell_scaffold        NAME, DESCRIPTION, TOOLS (name, description, params),
      SETTINGS (name, description), WITH_COMMAND. Writes nodecode-NAME.asd,
      cell.lisp, README.md and test/ under the cells folder, or the section's
      directory, and answers every file. It refuses a name already there.
  cell_add_tool        CELL, NAME, DESCRIPTION, PARAMS: one more schema, one
      more handler, one more :tool clause, spliced in at the scaffold's markers.
  cell_add_command     CELL, NAME, DESCRIPTION: one more slash command.
A name is letters, digits and dashes -- no dot, no slash -- so a scaffolded
folder is always inside the cells folder. The handler of a fresh tool answers a
placeholder: edit cell.lisp to make it answer for real, then (restart-cells
\"nodecode-NAME\") loads it now instead of at the next boot."
  "What (help :cell-generator) answers while the cell runs.")

(defun report (args session-id)
  "The /cell-generator answer: where a folder goes, and what one holds."
  (declare (ignore args session-id))
  (format nil "scaffolded cells go in ~a, one folder a cell: nodecode-<name>.asd, cell.lisp, README.md and test/. cell_scaffold writes one; cell_add_tool and cell_add_command add to one it wrote.~@[ max_tools is ~d.~]"
          (namestring (cell-directory))
          (ignore-errors (setting :max-tools))))

;;; --- names -------------------------------------------------------------

(defun cat (&rest parts)
  "PARTS joined, with ~% in one read as a line break."
  ;; A string literal's ~% is how a template says "a newline here": the
  ;; backslash escapes of a Lisp string cannot carry one.
  (with-output-to-string (out)
    (dolist (part parts)
      (loop for i from 0 below (length part)
            for c = (char part i)
            do (if (and (char= c #\~)
                        (< (1+ i) (length part))
                        (char= (char part (1+ i)) #\%))
                   (progn (write-char #\Newline out) (incf i))
                   (write-char c out))))))

(defun dashes (text)
  "TEXT with spaces and underscores as dashes."
  (map 'string (lambda (c) (if (or (char= c #\Space) (char= c #\_)) #\- c)) text))

(defun bare-name (raw)
  "RAW trimmed and lowercased, spaces and underscores as dashes, a leading
nodecode- dropped -- no check."
  (let* ((name (dashes (string-downcase (nlk:trimmed (princ-to-string (or raw ""))))))
         (name (if (and (> (length name) 9) (string= "nodecode-" name :end2 9))
                   (subseq name 9)
                   name)))
    name))

(defun cell-name (raw)
  "RAW as a cell's name, or a refusal: letters, digits and dashes, at most
40, a letter or digit first. No dot and no slash, so no name walks out of
the cells folder."
  (let ((name (bare-name raw)))
    (unless (and (<= 1 (length name) 40)
                 (< (char-code (char name 0)) 128)
                 (alphanumericp (char name 0))
                 (every (lambda (c)
                          (and (< (char-code c) 128)
                               (or (alphanumericp c) (char= c #\-))))
                        name))
      (fail "~s is no cell name: letters, digits and dashes, at most 40, a letter or digit first"
            raw))
    name))

(defun wire-name (name)
  "NAME with dashes as underscores: what a model calls the tool."
  (substitute #\_ #\- name))

(defun tool-name (raw)
  "RAW as a tool's wire name, or a refusal."
  (wire-name (cell-name raw)))

(defun schema-var (tool)
  "TOOL's schema variable, as one name: +look_up-schema+."
  (cat "+" (getf tool :name) "-schema+"))

;;; --- where a folder goes -----------------------------------------------------

(defun cell-directory ()
  "The folder a cell goes in: the section's DIRECTORY when it names one,
else this home's cells/."
  (running-settings)                    ; refuses, in this cell's words, while it is not started
  (let ((directory (ignore-errors (setting :directory))))
    (if (and (stringp directory) (plusp (length (nlk:trimmed directory))))
        (uiop:ensure-directory-pathname (nlk:trimmed directory))
        (nlk:home "cells/"))))

(defun cell-folder (name)
  "The folder cell NAME lives in."
  (merge-pathnames (format nil "nodecode-~a/" name) (cell-directory)))

(defun existing-cell-folder (name)
  "The folder of the cell called NAME, or a refusal naming the cells folder."
  (let ((folder (cell-folder name)))
    (unless (probe-file (merge-pathnames "cell.lisp" folder))
      (fail "no cell called ~a under ~a: cell_scaffold writes one"
            name (namestring (cell-directory))))
    folder))

;;; --- text in and out ---------------------------------------------------------

(defun write-text (path text)
  "PATH with TEXT in it, its parents made. => PATH's namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  (namestring path))

(defun lisp-string (text)
  "TEXT as the inside of a Lisp string literal."
  (with-output-to-string (out)
    (loop for c across (or text "")
          do (when (member c '(#\" #\\)) (write-char #\\ out))
             (write-char c out))))

(defun one-line (text)
  "TEXT trimmed, its line breaks and runs of blanks collapsed to one space."
  (let ((text (nlk:trimmed (or text ""))))
    (with-output-to-string (out)
      (loop for c across text
            for space = nil then space
            do (cond ((member c '(#\Space #\Tab #\Return #\Linefeed))
                      (unless space (write-char #\Space out))
                      (setf space t))
                     (t (write-char c out) (setf space nil)))))))

(defun insert-before (text marker block)
  "TEXT with BLOCK above the first line that is exactly MARKER, or a refusal."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (at (position marker lines :test #'string=)))
    (unless at
      (fail "this cell.lisp carries no ~s line to insert at: cell_scaffold writes one (it holds ~d lines; the first is ~s)"
            marker (length lines) (first lines)))
    (format nil "~{~a~%~}"
            (append (subseq lines 0 at)
                    (list (string-right-trim '(#\Newline) block))
                    (subseq lines at)))))

;;; --- reading what the model sent ---------------------------------------------

(defun arg-string (args key &optional default)
  "ARGS' KEY as a non-empty trimmed string, or DEFAULT."
  (let ((value (nlk:json-value args :string key)))
    (if (and (stringp value) (plusp (length (nlk:trimmed value))))
        (nlk:trimmed value)
        default)))

(defun arg-boolean (args key default)
  "ARGS' KEY when it is there, else DEFAULT."
  (multiple-value-bind (value present) (nlk:json-value args :boolean key)
    (if present value default)))

(defun object-list (value)
  "VALUE, a decoded JSON array, as the objects in it."
  (if (vectorp value)
      (loop for item across value when (hash-table-p item) collect item)
      '()))

(defun read-params (array)
  "ARRAY, a decoded JSON array of parameter objects, as plists."
  (loop for item in (object-list array)
        for name = (ignore-errors (tool-name (arg-string item "name")))
        when name
          collect (list :name name
                        :type (let ((type (arg-string item "type")))
                                (if (member type *param-types* :test #'string=) type "string"))
                        :description (one-line (arg-string item "description"))
                        :required (nlk:json-value item :boolean "required"))))

(defun read-tools (array limit)
  "ARRAY, a decoded JSON array of tool objects, as plists; more than LIMIT refuses."
  (let ((tools (loop for item in (object-list array)
                     for name = (ignore-errors (tool-name (arg-string item "name")))
                     when name
                       collect (list :name name
                                     :description (one-line (or (arg-string item "description")
                                                                "what this tool does"))
                                     :params (read-params (nlk:json-value item :array "params"))))))
    (when (> (length tools) limit)
      (fail "~d tools asked for; the section's max_tools is ~d" (length tools) limit))
    (let ((seen '()))
      (dolist (tool tools)
        (when (member (getf tool :name) seen :test #'string=)
          (fail "two tools called ~a" (getf tool :name)))
        (push (getf tool :name) seen)))
    tools))

(defun secret-name-p (name)
  "Whether a setting NAME reads as a secret, as the original's REQUIRES_ENV did."
  (let ((name (string-downcase name)))
    (some (lambda (word) (search word name))
          '("key" "token" "secret" "password"))))

(defun read-settings (array)
  "ARRAY, a decoded JSON array of {name, description}, as section fields."
  (loop for item in (object-list array)
        for name = (arg-string item "name")
        when name
          collect (list :name (string-downcase (bare-name name))
                        :description (one-line (or (arg-string item "description")
                                                   "what this setting is for"))
                        :env (secret-name-p name))))

;;; --- the schema and the handler of one tool -----------------------------------

(defun schema-source (tool)
  "TOOL's schema defparameter: what the provider is shown, and a call is held to."
  (let* ((params (getf tool :params))
         (properties
           (with-output-to-string (out)
             (dolist (param params)
               (format out "~&                                   \"~a\" (nlk:json-object \"type\" \"~a\"~@[
                                                            \"description\" \"~a\"~])~%"
                       (getf param :name) (getf param :type)
                       (let ((text (getf param :description)))
                         (and text (plusp (length text)) (lisp-string text)))))))
         (required (remove-if-not (lambda (param) (getf param :required)) params)))
    (cat "(defparameter " (schema-var tool) "~%"
         "  (list :description \"" (lisp-string (getf tool :description)) "\"~%"
         "        :parameters (nlk:json-object~%"
         "                     \"type\" \"object\"~%"
         (if params
             (cat "                     \"properties\" (nlk:json-object~%"
                  properties
                  "                                   )~%")
             "")
         "                     \"required\" (vector"
         (if required
             (cat " "
                  (format nil "~{\"~a\"~^ ~}" (mapcar (lambda (p) (getf p :name)) required)))
             "")
         "))))~%")))

(defun handler-source (tool)
  "TOOL's handler: it takes the decoded arguments and answers a string."
  (let ((name (getf tool :name)))
    (cat "(defun " name " (args)~%"
         "  \"" (lisp-string (getf tool :description)) "\"~%"
         "  ;; TODO: answer from the arguments, as~%"
         "  ;; (nlk:json-value args :string \"" (or (getf (first (getf tool :params)) :name) "param") "\").~%"
         "  (declare (ignore args))~%"
         "  (format nil \"" name ": not implemented yet\"))~%")))

;;; --- the files a scaffold writes ----------------------------------------------

(defun asd-source (name description)
  "The system and the test system of cell NAME."
  (cat ";;;; nodecode-" name ".asd --- " description "~%"
       ";;;;~%"
       ";;;; SPDX-License-Identifier: MIT~%"
       ";;;;~%"
       ";;;; Scaffolded by nodecode-cell-generator: the system, and the test~%"
       ";;;; system the folder's own slice runs.~%"
       "~%"
       "(defsystem \"nodecode-" name "\"~%"
       "  :description \"" (lisp-string description) "\"~%"
       "  :license \"MIT\"~%"
       "  :version \"0.1.0\"~%"
       "  :depends-on (\"nodecode\")~%"
       "  :serial t~%"
       "  :components ((:file \"cell\"))~%"
       "  :in-order-to ((test-op (test-op \"nodecode-" name "/test\"))))~%"
       "~%"
       "(defsystem \"nodecode-" name "/test\"~%"
       "  :description \"" name " tests.\"~%"
       "  :license \"MIT\"~%"
       "  :depends-on (\"nodecode-" name "\" \"nodecode/test\")~%"
       "  :pathname \"test/\"~%"
       "  :serial t~%"
       "  :components ((:file \"support\")~%"
       "               (:file \"cell-test\"))~%"
       "  :perform (test-op (operation component)~%"
       "                    (uiop:symbol-call '#:nodecode.test '#:run-" name "-tests)))~%"))

(defun section-source (name settings)
  "NAME's (:section ...) clause, with one field a setting."
  (cat "  (:section (\"" name "\")~%"
       "    (:guide \"Nothing here is required: the tools answer a placeholder until you write them.\")~%"
       (if settings
           (format nil "~{    (\"~a\" :~a :doc \"~a\")~%~}"
                   (loop for setting in settings
                         append (list (getf setting :name)
                                      (if (getf setting :env) "env" "string")
                                      (lisp-string (getf setting :description)))))
           "")
       "  )~%"))

(defun command-source (fn description)
  "One command function: FN answers the command's text."
  (cat "(defun " fn " (args session-id)~%"
       "  \"" (lisp-string description) "\"~%"
       "  (declare (ignore session-id))~%"
       "  (format nil \"" (lisp-string description) "~@[ (~a)~]\" (nlk:trimmed (or args \"\"))))~%"))

(defun primer (name tools command)
  "The one line (help :NAME) is summarised by, and the text it answers."
  (cat "A cell scaffolded by cell_scaffold"
       (if tools (cat ": the tools " (format nil "~{~a~^, ~}" (mapcar (lambda (t2) (getf t2 :name)) tools))) "")
       (if command (cat ", and the /" name " command") "")
       ". Each answers a string; edit cell.lisp to make them answer for real."))

(defun cell-source (name description tools settings command)
  "The whole cell.lisp of cell NAME."
  (let ((primer (primer name tools command)))
    (cat ";;;; cell.lisp --- " description "~%"
         ";;;;~%"
         ";;;; SPDX-License-Identifier: MIT~%"
         ";;;;~%"
         ";;;; Scaffolded by nodecode-cell-generator. One schema and one handler a~%"
         ";;;; tool, one function a command, and the NLE:DEFINE-CELL declaration~%"
         ";;;; that puts them in place. Edit the handlers; leave the markers, which~%"
         ";;;; is where cell_add_tool and cell_add_command splice in.~%"
         "~%"
         "(defpackage #:nodecode-" name "~%"
         "  (:use #:cl)~%"
         "  (:export #:start-cell))~%"
         "~%"
         "(in-package #:nodecode-" name ")~%"
         "~%"
         "(nlk:define-peripheral " name " :not-running t)~%"
         "~%"
         "(defparameter +primer+~%"
         "  \"" (lisp-string name) ": the manual line of a scaffolded cell, built from what it~%"
         "declares. Nothing here is required; set the section in /settings when a~%"
         "tool needs a key, and edit the handlers below to answer with something~%"
         "real.\")~%"
         "~%"
         ";;; --- tools: one schema and one handler each ---~%"
         "~%"
         (format nil "~{~a~%~}" (mapcar (lambda (tool)
                                           (cat (schema-source tool) "~%" (handler-source tool)))
                                         tools))
         ";;; --- end of tools ---~%"
         "~%"
         ";;; --- commands: one function each ---~%"
         "~%"
         (if command (command-source "run" (or description "the cell's command")) "")
         ";;; --- end of commands ---~%"
         "~%"
         "(nle:define-cell " name "~%"
         (section-source name settings)
         (format nil "~{  (:tool \"~a\" ~a #'~a)~%~}" (loop for tool in tools
                                                             append (list (getf tool :name)
                                                                          (schema-var tool)
                                                                          (getf tool :name))))
         "  ;; more tools here~%"
         (if command (cat "  (:command \"" name "\" #'run :description \""
                          (lisp-string (one-line (or description "the cell's command"))) "\")~%") "")
         "  ;; more commands here~%"
         "  (:help :" name " \"" (lisp-string primer) "\" +primer+))~%")))

(defun readme-source (name description tools command)
  "The README of cell NAME."
  (cat "# nodecode-" name "~%~%"
       description "~%~%"
       "A Nodecode cell, scaffolded from a description by nodecode-cell-generator.~%~%"
       "## Get it~%~%"
       "The folder sits under your home's `cells/`, and Nodecode loads it at the next~%"
       "boot. Keep it in a repository of its own to share it.~%~%"
       "## Use it~%~%"
       (if tools
           (cat "The tools the model can call: "
                (format nil "~{`~a`~^, ~}" (mapcar (lambda (tool) (getf tool :name)) tools))
                ".~%")
           "It contributes no tool yet.~%")
       (if command (cat "The command `/" name "` answers in the shell.~%") "")
       "~%Set the section in `/settings` (or the Settings tab) when a tool needs a key.~%"))

(defun support-source (name)
  "The folder's test support: its slice, and its start."
  (cat ";;;; support.lisp --- " name " tests.~%"
       ";;;;~%"
       ";;;; SPDX-License-Identifier: MIT~%"
       "~%"
       "(in-package #:nodecode.test)~%"
       "~%"
       "(define-test-slice \"" name "\" \"" (string-upcase (cat name "-CELL-")) "\"~%"
       "  :start nodecode-" name ":start-cell)~%"))

(defun cell-test-source (name tools command)
  "The folder's lifecycle tests."
  (cat ";;;; cell-test.lisp --- " name ": what a start installs and a stop takes back.~%"
       ";;;;~%"
       ";;;; SPDX-License-Identifier: MIT~%"
       "~%"
       "(in-package #:nodecode.test)~%"
       "~%"
       "(define-cell-lifecycle-tests \"" name "\"~%"
       "  (:help :" name ")~%"
       (if command (cat "  (:command \"" name "\")~%") "")
       (if tools
           (cat "  (:running (is (stringp (nodecode-" name "::" (getf (first tools) :name)
                               " (nlk:json-object)))))~%")
           "")
       "  (:idle nodecode-" name "::" name "-error (nodecode-" name "::running-settings)))~%"))

;;; --- the answer the model reads ----------------------------------------------

(defun answer (&rest pairs)
  "PAIRS, key then value, as one JSON object: a string."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    (nlk:encode-json-object object)))

;;; --- the three verbs ------------------------------------------------------------

(defun scaffold (args)
  "Create a cell folder from a description: NAME, DESCRIPTION, TOOLS (one
{name, description, params} an object), SETTINGS (one {name, description}
an object) and WITH_COMMAND (default true). Answers the files it wrote."
  (let* ((name (cell-name (arg-string args "name")))
         (description (one-line (or (arg-string args "description")
                                    (format nil "the ~a cell" name))))
         (folder (cell-folder name))
         (limit (or (ignore-errors (setting :max-tools)) 8))
         (tools (read-tools (nlk:json-value args :array "tools") limit))
         (settings (read-settings (nlk:json-value args :array "settings")))
         (command (arg-boolean args "with_command" t)))
    (when (probe-file folder)
      (fail "a folder is already there: ~a" (namestring folder)))
    (let ((files (list (cons (merge-pathnames (format nil "nodecode-~a.asd" name) folder)
                             (asd-source name description))
                       (cons (merge-pathnames "cell.lisp" folder)
                             (cell-source name description tools settings command))
                       (cons (merge-pathnames "README.md" folder)
                             (readme-source name description tools command))
                       (cons (merge-pathnames "test/support.lisp" folder)
                             (support-source name))
                       (cons (merge-pathnames "test/cell-test.lisp" folder)
                             (cell-test-source name tools command)))))
      (answer "status" "created"
              "name" name
              "path" (namestring folder)
              "files" (coerce (mapcar (lambda (file) (write-text (car file) (cdr file))) files)
                              'vector)
              "tools" (coerce (mapcar (lambda (tool) (getf tool :name)) tools) 'vector)
              "next" (format nil "the next boot loads it; (restart-cells \"nodecode-~a\") tries it now~@[, then /~a in the shell~]"
                              name (and command name))))))

(defun add-tool (args)
  "Add one tool to a cell this cell scaffolded: CELL, NAME, DESCRIPTION, PARAMS."
  (let* ((name (cell-name (arg-string args "cell")))
         (tool (list :name (tool-name (arg-string args "name"))
                      :description (one-line (or (arg-string args "description")
                                                 "what this tool does"))
                      :params (read-params (nlk:json-value args :array "params"))))
         (file (merge-pathnames "cell.lisp" (existing-cell-folder name)))
         (text (nlk:read-text file)))
    (when (search (format nil "(:tool \"~a\"" (getf tool :name)) text)
      (fail "nodecode-~a already has a tool called ~a" name (getf tool :name)))
    (setf text (insert-before text *tools-end*
                               (cat "~%" (schema-source tool) "~%" (handler-source tool))))
    (setf text (insert-before text *tools-anchor*
                               (format nil "  (:tool \"~a\" ~a #'~a)"
                                       (getf tool :name) (schema-var tool) (getf tool :name))))
    (write-text file text)
    (answer "status" "tool_added"
            "cell" name
            "tool" (getf tool :name)
            "files" (vector (namestring file))
            "next" (format nil "implement ~a in cell.lisp; (restart-cells \"nodecode-~a\") loads it"
                            (getf tool :name) name))))

(defun add-command (args)
  "Add one slash command to a cell this cell scaffolded: CELL, NAME, DESCRIPTION."
  (let* ((name (cell-name (arg-string args "cell")))
         (command (cell-name (arg-string args "name")))
         (description (one-line (or (arg-string args "description")
                                    "what this command does")))
         (file (merge-pathnames "cell.lisp" (existing-cell-folder name)))
         (text (nlk:read-text file))
         (fn (format nil "run-~a" command)))
    (when (search (format nil "(:command \"~a\"" command) text)
      (fail "nodecode-~a already has a command called /~a" name command))
    (setf text (insert-before text *commands-end* (cat "~%" (command-source fn description))))
    (setf text (insert-before text *commands-anchor*
                               (format nil "  (:command \"~a\" #'~a :description \"~a\")"
                                       command fn (lisp-string description))))
    (write-text file text)
    (answer "status" "command_added"
            "cell" name
            "command" command
            "files" (vector (namestring file))
            "next" (format nil "type /~a in the shell after (restart-cells \"nodecode-~a\")"
                            command name))))

;;; --- the declaration -----------------------------------------------------------

(nle:define-cell cell-generator
  (:section ("cell-generator")
    (:guide "Nothing is required. directory says where a generated folder goes (empty: this home's cells folder); max_tools caps one scaffold; a generated cell loads at the next boot, and (restart-cells) tries one now.")
    ("directory" :path :doc "where a scaffolded cell folder is written; empty means this home's cells folder")
    ("max_tools" :integer :default 8 :min 1 :doc "the most tools one cell_scaffold writes"))
  (:tool "cell_scaffold"
         (list :description
               "Create a complete Nodecode cell folder from a description: the system and its test system, cell.lisp with a schema and a handler for each tool, README.md, and lifecycle tests. Optionally a slash command and config settings. Writes under the home's cells/ (or the section's directory) and answers every file it wrote."
               :parameters
               (nlk:json-object
                "type" "object"
                "properties"
                (nlk:json-object
                 "name" (nlk:json-object "type" "string"
                                          "description" "the cell's name: letters, digits and dashes, e.g. 'weather'")
                 "description" (nlk:json-object "type" "string"
                                                 "description" "one line on what the cell does")
                 "tools" (nlk:json-object
                          "type" "array"
                          "description" "the tools it contributes, one object each"
                          "items" (nlk:json-object
                                   "type" "object"
                                   "properties"
                                   (nlk:json-object
                                    "name" (nlk:json-object "type" "string"
                                                             "description" "the tool's wire name, e.g. check_weather")
                                    "description" (nlk:json-object "type" "string"
                                                                    "description" "what the tool does")
                                    "params" (nlk:json-object
                                              "type" "array"
                                              "description" "the tool's parameters"
                                              "items" (nlk:json-object
                                                       "type" "object"
                                                       "properties"
                                                       (nlk:json-object
                                                        "name" (nlk:json-object "type" "string"
                                                                                 "description" "the parameter's name")
                                                        "type" (nlk:json-object "type" "string"
                                                                                 "description" "string, number, integer, boolean, array or object")
                                                        "description" (nlk:json-object "type" "string"
                                                                                        "description" "what the parameter is")
                                                        "required" (nlk:json-object "type" "boolean"
                                                                                     "description" "whether a call must carry it")))))))
                 "settings" (nlk:json-object
                             "type" "array"
                             "description" "the cell's config fields, one object each; a name holding key, token, secret or password reads as an environment variable's name"
                             "items" (nlk:json-object
                                      "type" "object"
                                      "properties"
                                      (nlk:json-object
                                       "name" (nlk:json-object "type" "string"
                                                                "description" "the config member, e.g. api_key")
                                       "description" (nlk:json-object "type" "string"
                                                                       "description" "what the setting is for"))))
                 "with_command" (nlk:json-object "type" "boolean"
                                                  "description" "also register /<name> in the shell; default true"))
                "required" (vector "name")))
         #'scaffold)
  (:tool "cell_add_tool"
         (list :description
               "Add one tool to a cell cell_scaffold wrote: a schema, a handler, and the :tool clause that registers it. The cell's cell.lisp must still carry the scaffold's markers; one that does not refuses by name."
               :parameters
               (nlk:json-object
                "type" "object"
                "properties"
                (nlk:json-object
                 "cell" (nlk:json-object "type" "string"
                                          "description" "the cell to patch, e.g. 'weather'")
                 "name" (nlk:json-object "type" "string"
                                          "description" "the tool's wire name, e.g. check_weather")
                 "description" (nlk:json-object "type" "string"
                                                 "description" "what the tool does")
                 "params" (nlk:json-object
                           "type" "array"
                           "description" "the tool's parameters, as in cell_scaffold"
                           "items" (nlk:json-object "type" "object")))
                "required" (vector "cell" "name")))
         #'add-tool)
  (:tool "cell_add_command"
         (list :description
               "Add one slash command to a cell cell_scaffold wrote: its function, its :command clause, and the answer the shell shows."
               :parameters
               (nlk:json-object
                "type" "object"
                "properties"
                (nlk:json-object
                 "cell" (nlk:json-object "type" "string"
                                          "description" "the cell to patch, e.g. 'weather'")
                 "name" (nlk:json-object "type" "string"
                                          "description" "the command's spelling, without the slash, e.g. 'forecast'")
                 "description" (nlk:json-object "type" "string"
                                                 "description" "what the command does"))
                "required" (vector "cell" "name")))
         #'add-command)
  (:command "cell-generator" 'report
            :description "What cell_scaffold writes, and where")
  (:help :cell-generator
         "cell-generator: scaffold a Nodecode cell from a description, and add a tool or a command to one - (cell-generator:scaffold ...) through eval, or the cell_scaffold tools"
         +manual+))
